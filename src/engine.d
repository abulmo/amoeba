/*
 * File engine.d
 * Manage a UCI Engine through pipes
 *
 * © 2016-2020 Richard Delorme
  */

module engine;

import game, board, move, util;
import std.algorithm, std.conv, std.math, std.process, std.string, std.stdio, std.uni;


/*
 * Engine
 */
final class Engine {
private:
	string [] cmd;
	ProcessPipes pipe;
	std.stdio.File log;
	Board board;
	size_t multipv = 1;

	/* send a message to an engine */
	void send(T...)(T args) {
		pipe.stdin.writeln(args);
		pipe.stdin.flush();
		if (log.isOK) log.writeln(name, "< ", args);
	}

	/* receive a message from an engine */
	string receive() {
		string l = pipe.stdout.readln();
		l = chomp(l);
		if (log.isOK) log.writeln(name, "> ", l);
		return l.dup;
	}

	/* check & set spin option */
	void setSpinOption(string [] words, string name, int value) {
		int d, m, M;
		foreach(i; 0 .. words.length - 1) {
			if (words[i] == "default") d = to!int(words[i + 1]);
			if (words[i] == "min") m = to!int(words[i + 1]);
			if (words[i] == "max") M = to!int(words[i + 1]);
		}
		value = clamp(value, m, M);
		if (value != d) send("setoption name ", name, " value ", value);
	}

	/* print a number with a unit */
	static void prettyNumber(double n, std.stdio.File f) {
		char unit = '*';
		if (n > 1e12) { unit = 'T'; n /= 1e12; }
		else if (n > 1e9) { unit = 'G'; n /= 1e9; }
		else if (n > 1e6) { unit = 'M'; n /= 1e6; }
		else if (n > 1e3) { unit = 'K'; n /= 1e3; }
		if (unit != '*') f.writef("%7.3f%c  ", n, unit); else f.writef("%7.0f   ", n);
	}

	/* print time nicely as D HH:MM:SS.mmm */
	static void prettyTime(double t, std.stdio.File f) {
		int d, h, m;
		d = cast (int) floor(t / 86400.0); t -= d * 86400.0;
		h = cast (int) floor(t /  3600.0); t -= h *  3600.0;
		m = cast (int) floor(t /    60.0); t -= m *    60.0;

		if (d > 0.0) write(d, "d ");
		if (d + h > 0) writef("%02d:", h);
		writef("%02d:%06.3f  ", m, t);
	}

	/* nicely print the pv on several lines */
	static void prettyPV(string text, const int margin, const int width, std.stdio.File f) {
		size_t a, b;
		foreach (i; 0 .. text.length) {
			if (text[i].isSpace()) {
				if (i >= a + width - margin) {
					if (b == a) b = i;
					writeln(text[a .. b]);
					a = b + 1;
					if (a < text.length) foreach (j; 0 .. margin) write(' ');
				} else b = i;
			}
		}
		if (a < text.length) writeln(text[a .. $]);
	}

public:
	bool analyseModeSupport, clearHashSupport;

	/* information about the search */
	struct Info {
		ulong nodes = 0;
		double time = 0.0, lockTime = 0.0;
		int score = int.min;
		int depth = 0;
		int multipv = 0;
		Line pv;

		/* bestmove from the pv */
		Move bestmove() const {
			return pv.move[0];
		}

		/* convert to game info */
		Game.Info get() const {
			Game.Info g;
			g.time = time;
			g.score = cast (short) clamp(score, short.min, short.max);
			g.depth = cast (ubyte) depth;

			return g;
		}

		/* read search info */
		void read(string [] words) {
			Move m = bestmove();

			foreach(i; 0 .. words.length - 1) {
				if (words[i] == "depth") depth = clamp(to!int(words[i + 1]), 0, 8192);
				else if (words[i] == "cp") score = to!int(words[i + 1]);
				else if (words[i] == "mate") {
					const int matein = to!int(words[i + 1]);
					if (matein < 0) score = (-Score.mate - matein * 2);
					else if (matein > 0) score = (Score.mate - matein * 2 + 1);
				} else if (words[i] == "time") time = 0.001 * to!int(words[i + 1]);
				else if (words[i] == "nodes") nodes = to!ulong(words[i + 1]);
				else if (words[i] == "multipv") multipv = clamp(to!int(words[i + 1]) - 1, 0, 255);
				else if (words[i] == "pv") {
					pv.clear();
					foreach(j; i + 1 .. words.length) pv.push(fromPan(words[j]));
					if (m != bestmove()) lockTime = time;
				}
			}
		}

		/* write the search info */
		void write(Board board, const int width = 120, std.stdio.File f = stdout) const {
			f.writef("%3d ", depth);
			f.writef("%3d ", multipv + 1);
			if (score > Score.high) f.write("  +M", (Score.mate + 1 - score) / 2, "    ");
			else if (score < Score.low) f.write("  -M", -(Score.mate + score) / 2, "    ");
			else f.writef("%+7.2f  ", 0.01 * score);
			prettyNumber(nodes, f);
			prettyTime(time, f);
			prettyNumber(nodes / time, f);
			prettyPV(pv.toSan(board), 48, width, f);
		 }
	}
	string name;
	Info [256] info;

	/* constructor */
	this(string [] c) {
		cmd = c;
		name = cmd[0];
		board = new Board;
	}

	/* constructor */
	this(string s) {
		name = s;
		cmd ~= s;
		board = new Board;
	}

	/* wait for an engine to be ready */
	void ready() {
		string l;
		send("isready");
		do {
			l = receive();
		} while (l != "readyok");
	}

	/* start an engine */
	void start(bool showDebug, const int hashSize = 64, const int nThreads = 1, const int multipv = 1) {
		string l, lineThreads, lineMultipv, lineHash;
		bool hasLog;

		pipe = pipeProcess(cmd);
		analyseModeSupport = false;
		send("uci");
		do {
			l = receive();
			if (l.skipOver("id name")) name = l.strip();
			if (l.skipOver("option name Log type check")) hasLog = true;
			if (l.skipOver("option name Hash type spin")) lineHash = l;
			if (l.skipOver("option name Threads type spin")) lineThreads = l;
			if (l.skipOver("option name MultiPV type spin")) lineMultipv = l;
			if (l.skipOver("option name UCI_AnalyseMode type check")) analyseModeSupport = true;
			if (l.skipOver("option name Clear Hash type button")) clearHashSupport = true;
		} while (l != "uciok");
		ready();
		if (lineHash) setSpinOption(lineHash.split(), "Hash", hashSize);
		if (lineThreads) setSpinOption(lineThreads.split(), "Threads", nThreads);
		if (lineMultipv) setSpinOption(lineMultipv.split(), "MultiPV", multipv);
		if (hasLog && showDebug) send("setoption name Log value true");
		if (showDebug) send("debug");
		ready();
	}

	void analyse() {
		if (analyseModeSupport) send("setoption name UCI_AnalyseMode value true");
	}

	void startDebugging(string header) {
		log.open(header ~ "-" ~ to!string(thisProcessID) ~ ".log", "w");
	}

	/* end an engine */
	void end() {
		send("quit");
		wait(pipe.pid);
		if (log.isOK) log.close();
	}

	/* stop an engine */
	void stop() {
		send("stop");
		ready();
	}

	/* new game */
	void newGame() {
		send("ucinewgame");
		ready();
		board.set();
	}

	/* clear hash */
	void clearHash() {
		if (clearHashSupport) send("setoption name Clear Hash");
	}

	/* set a new position */
	void position(string fen, const shared Move [] moves = []) {
		string s = "position ";
		if (fen is null) {
			s ~= "startpos";
			board.set();
		} else {
			s ~= "fen " ~ fen;
			board.set(fen);
		}

		if (moves.length > 0) {
			s ~= " moves";
			foreach(m; moves[0 .. $]) s ~= " " ~ m.toPan();
		}
		send(s);
		ready();
	}

	/* go */
	Move go(const string options, const bool verbose = false, const int width = 120) {
		Chrono chrono;
		string l;

		info[] = Info.init;
		chrono.start();

		send("go", options);
		if (verbose) writeln("depth mpv score     nodes       time     speed  pv\n------------------------------------------------------");

		while (true) {
			l = receive();

			if (l.skipOver("info ")) {
				Info r;

				r.read(l.split());
				if ((r.time > info[r.multipv].time || r.depth > info[r.multipv].depth) && r.score > int.min) {
					if (verbose) r.write(board, width);
					info[r.multipv] = r;
				}
			}

			if (l.skipOver("bestmove ")) {
				info[0].time = chrono.time();
				Move m  = l.strip().fromPan();
				if (m != info[0].bestmove) info[0].lockTime = info[0].time;
				if (verbose) writeln("bestmove ", m.toSan(board));
				return m;
			}
		}
	}

}

