/*
 * File engine.d
 * Manage a UCI Engine through pipes
 *
 * © 2016-2019 Richard Delorme
  */

module engine;

import game, move, util;
import std.process, std.string, std.algorithm, std.conv, std.stdio;


/*
 * Engine
 */
class Engine {
private:
	string [] cmd;
	ProcessPipes pipe;
	std.stdio.File log;
	bool analyseModeSupport;

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
		return l;
	}

	/* read game info */
	void getInfo(string [] words) {
		foreach(i; 0 .. words.length - 1) {
			if (words[i] == "depth") info.depth = cast (ubyte) max(0, min(to!int(words[i + 1]), 255));
			else if (words[i] == "cp") info.score = to!short(words[i + 1]);
			else if (words[i] == "mate") {
				const int matein = to!int(words[i + 1]);
				if (matein < 0) info.score = cast (short) (-Score.mate - matein * 2);
				else if (matein > 0) info.score = cast (short)  (Score.mate - matein * 2 + 1);
			} else if (words[i] == "time") info.time = 0.001 * to!int(words[i + 1]);
			else if (words[i] == "pv") {
				pv.clear();
				foreach(j; i + 1 .. words.length) pv.push(fromPan(words[j]));
			}
		}
	}

public:
	string name;
	Game.Info info;
	Line pv;

	/* constructor */
	this(string [] c) {
		cmd = c;
		name = cmd[0];
	}

	/* constructor */
	this(string s) {
		name = s;
		cmd ~= s;
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
	void start(bool showDebug, int hashSize = 64, int nThreads = 1) {
		string l;

		pipe = pipeProcess(cmd);
		analyseModeSupport = false;		
		send("uci");
		do {
			l = receive();
			if (l.skipOver("id name")) name = l.strip();
			if (showDebug && l.skipOver("option name Log type check")) send("setoption name Log value true");
			if (hashSize != 64 && l.skipOver("option name Hash type spin")) send("setoption name Hash value ", hashSize);
			if (nThreads != 1 && l.skipOver("option name Threads type spin")) send("setoption name Threads value ", nThreads);
			if (l.skipOver("option name UCI_AnalyseMode type check")) analyseModeSupport = true;
		} while (l != "uciok");
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

	/* new game */
	void newGame() {
		send("ucinewgame");
		ready();
	}

	/* set a new position */
	void position(string fen, const shared Move [] moves) {
		string s = "position ";
		if (fen is null) s ~= "startpos";
		else s ~= "fen " ~ fen;

		if (moves.length > 0) {
			s ~= " moves";
			foreach(m; moves[0 .. $]) s ~= " " ~ m.toPan();
		}
		send(s);
	}

	/* go */
	Move go(const string options) {
		string l;
		info = Game.Info.init;
		send("go ", options);
		while(true) {
			l = receive();
			if (l.skipOver("info ")) {
				getInfo(l.split());
			}
			if (l.skipOver("bestmove ")) {
				Move m  = l.strip().fromPan();
				return m;
			}
		}
	}

}

