/*
 * File uci.d
 * Universal Chess Interface.
 * © 2016 Richard Delorme
 */

module uci;

import board, eval, move, search, util;
import std.algorithm, std.stdio, std.string, std.conv, std.array, std.concurrency, std.process;

/* spawnable function */
void eventLoop(shared Event e) {
	e.loop();
}

/* uci class */
class Uci {
	string name;
	struct Time {
		double remaining, increment;
		void clear() {
			remaining = increment = 0.0;
		}
	}
	Search search;
	Board board;
	Moves moves;
	shared Event event;	
	int depthMax;
	int movesToGo;
	Time [Color.size] time;
	bool canPonder;
	bool isPondering;
	std.stdio.File log;
	Chrono chrono;

	/* constructor */
	this() {
		chrono.start();
		name = "Amoeba 1.3";
		search = new Search;
		search.event = event = new shared Event;
		board = new Board;
		ucinewgame();
		canPonder = false;
		search.option.verbose = true;
	}

	/* send */
	void send(T...) (T args) {
		writeln(args);
		if (log.isOpen) {
			log.writef("[%8.3f] %s> ", chrono.time(), name);
			log.writeln(args);
		}
		stdout.flush();
	}

	/* set thinking time */
	double setTime() {
		immutable  p = board.player;
		double t = time[p].remaining;
		int todo = 40;

		if (t) {
			if (movesToGo > 0) todo = movesToGo;
			t += time[p].increment * todo;
			t = max(t - 1.0, 0.95 * t) / todo;
		} else {
			t = time[p].increment;
			t = max(t - 1.0, 0.95 * t);
		}

		return t;
	}

	/* set max time to use in hard (failing low) position */
	double setExtraTime() {
		immutable  p = board.player;
		return (time[p].remaining + time[p].increment) * 0.1;
	}

	/* uci command */
	void uci() {
		send("id name " ~ name);
		send("id author Richard Delorme");
		send("option name Ponder type check default false");
		send("option name Hash type spin default 64 min 1 max 4096");
		send("option name Log type check default false");
		// add more options here...
		send("uciok");
	}

	/* setoption command */
	void setoption(string line) {
		string name = findBetween(line.chomp(), "name", "value").strip().toLower();
		findSkip(line, "value");
		string value = line.strip().toLower();
		if (name == "ponder") canPonder = to!bool(value);
		else if (name == "hash") search.resize(to!size_t(value) * 1024 * 1024);
		else if (name == "log") {
			if (to!bool(value)) {
				log.open(name ~ to!string(thisProcessID) ~ ".log", "w");
				search.logFile = log;
			} else {
				if (log.isOpen) log.close();
			}
		}
			
	}

	/* ucinewgame command: clear the search state */
	void ucinewgame() {
		search.clear();
		board.set();
		search.set(board);
	}

	/* set a news position */
	void position(string line) {
		if (findSkip(line, "startpos")) board.set();
		else if (findSkip(line, "fen")) board.set(line);
		if (findSkip(line, "moves")) {
			auto words = line.split();
			foreach(w ; words) board.update(toMove(w));
		}
		search.set(board);
	}

	/* search only some moves */
	void searchmoves(string [] words) {
		foreach(w ; words) moves.push(toMove(w));
	}

	/* set bestmove */
	void bestmove() {
		if (search.hint != 0 && canPonder) send("bestmove ", search.bestMove.toString(), " ponder ", search.hint.toString());
		else send("bestmove ", search.bestMove.toString());
	}

	/* go */
	void go(string line) {
		string [] words = line.split();
		depthMax = Limits.plyMax;
		foreach(c ; Color.white .. Color.size) time[c].clear();
		isPondering = false;
		foreach(i, ref w ; words) {
			if (w == "searchmoves") searchmoves(words);
			else if (w == "ponder") isPondering = true;
			else if (w == "wtime" && i + 1 < words.length) time[Color.white].remaining = 0.001 * to!double(words[i + 1]);
			else if (w == "btime" && i + 1 < words.length) time[Color.black].remaining = 0.001 * to!double(words[i + 1]);
			else if (w == "winc" && i + 1 < words.length) time[Color.white].increment = 0.001 * to!double(words[i + 1]);
			else if (w == "binc" && i + 1 < words.length) time[Color.black].increment = 0.001 * to!double(words[i + 1]);
			else if (w == "movestogo" && i + 1 < words.length) movesToGo = to!int(words[i + 1]);
			else if (w == "depth" && i + 1 < words.length) depthMax = to!int(words[i + 1]);
			else if (w == "movetime" && i + 1 < words.length) time[board.player].increment = 0.001 * to!double(words[i + 1]);
			else if (w == "infinite") time[board.player].increment = double.infinity;
		}
		search.go(depthMax, setTime(), setExtraTime(), isPondering); //TODO: add SearchMoves + Multipv
		if (!isPondering) bestmove();
	}

	/* stop */
	void stop() {
		if (isPondering) bestmove();
	}

	/* main loop */
	void loop() {
		spawn(&eventLoop, event);
		while (true) {
			auto line = event.wait();
			if (log.isOpen) log.writefln("[%8.3f] uci> %s", chrono.time(), line);
			if (line == null) break;
			else if (line == "" || line[0] == '#') continue;
			else if (findSkip(line, "uci")) uci();
			else if (findSkip(line, "setoption")) setoption(line);
			else if (findSkip(line, "ucinewgame")) ucinewgame();
			else if (findSkip(line, "isready")) send("readyok");
			else if (findSkip(line, "position")) position(line);
			else if (findSkip(line, "go")) go(line);
			else if (findSkip(line, "stop")) stop();
			else if (findSkip(line, "ponderhit")) stop();
			else if (findSkip(line, "quit")) break;
			else if (findSkip(line, "perft")) perft(line.split, board);
		}
		if (log.isOpen) {
			log.close();
		}
	}
}

