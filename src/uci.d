/*
 * File uci.d
 * Universal Chess Interface.
 * © 2016-2017 Richard Delorme
 */

module uci;

import board, eval, move, search, util;
import std.algorithm, std.array, std.conv, std.concurrency, std.process, std.stdio, std.string;

/* Some information about the compilation */
string arch() @property {
	string a;
	version (Windows) a = "w";
	else version (linux) a = "l";
	else version (OSX) a = "m";
	else a = "u";

	a ~= to!string(8 * size_t.sizeof);
	
	version (withPopCount) a ~= "p";

	return a;
}

/* spawnable event loop function */
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
	std.stdio.File log;
	Chrono chrono;
	Time [Color.size] time;
	int depthMax, movesToGo, multipv;
	ulong nodesMax;
	bool canPonder, isPondering, easy;

	/* constructor */
	this() {
		chrono.start();
		name = "Amoeba.191." ~ arch;
		search = new Search;
		search.event = event = new shared Event;
		board = new Board;
		ucinewgame();
		canPonder = false;
		search.option.verbose = true;
		multipv = 1;
		easy = true;
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
		const p = board.player;
		double t = time[p].remaining;
		int todo = 40;

		if (t > 0) {
			if (movesToGo > 0) todo = movesToGo;
			t += time[p].increment * todo;
			t = max(t - 1.0, 0.95 * t) / todo;
		} else {
			t = time[p].increment;
			if (t > 0) {
				t = max(t - 1.0, 0.95 * t);
			} else t = double.infinity;
		}

		return t;
	}

	/* set max time to use const hard (failing low) position */
	double setExtraTime(const double maxTime) {
		const p = board.player;
		double t;

		t = (time[p].remaining + time[p].increment) * 0.1;
		if (t <= 0) t = double.infinity;
		t = min(maxTime, max(2.0 * maxTime, t));

		return t;
	}

	/* uci command */
	void uci() {
		send("id name " ~ name);
		send("id author Richard Delorme");
		send("option name Ponder type check default false");
		send("option name Hash type spin default 64 min 1 max 4096");
		send("option name Log type check default false");
		send("option name MultiPV type spin default 1 min 1 max 256");
		send("option name UCI_AnalyseMode type check default false");
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
		else if (name == "multipv") multipv = to!int(value);
		else if (name == "uci_analysemode") easy = !to!bool(value);
		else if (name == "log") {
			if (to!bool(value)) {
				log.open(name ~ "-" ~ to!string(thisProcessID) ~ ".log", "w");
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

	/* set a new position */
	void position(string line) {
		if (findSkip(line, "startpos")) board.set();
		else if (findSkip(line, "fen")) board.set(line);
		if (findSkip(line, "moves")) {
			auto words = line.split();
			foreach(w ; words) board.update(fromPan(w));
		}
		search.set(board);
	}

	/* search only some moves */
	void searchmoves(string [] words) {
		foreach(w ; words) {
			Move m = fromPan(w);
			if (board.isLegal(m)) moves.push(m);
		}
	}

	/* set bestmove */
	void bestmove() {
		if (search.hint != 0 && canPonder) send("bestmove ", search.bestMove.toPan(), " ponder ", search.hint.toPan());
		else send("bestmove ", search.bestMove.toPan());
	}

	/* go */
	void go(string line) {
		Termination termination;
		string [] words = line.split();

		moves.clear();
		termination.depth.max = Limits.plyMax;
		termination.nodes.max = ulong.max;
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
			else if (w == "depth" && i + 1 < words.length) termination.depth.max = to!int(words[i + 1]);
			else if (w == "nodes" && i + 1 < words.length) termination.nodes.max = to!ulong(words[i + 1]);
			else if (w == "mate" && i + 1 < words.length) termination.depth.max = to!int(words[i + 1]); /* TODO: turnoff selective search? */
			else if (w == "movetime" && i + 1 < words.length) time[board.player].increment = 0.001 * to!double(words[i + 1]);
			else if (w == "infinite") termination.depth.max =  Limits.plyMax;
		}
		termination.time.max = setTime();
		termination.time.extra = setExtraTime(termination.time.max);

		search.go(termination, moves, (easy && multipv == 1), multipv, isPondering);
		if (!isPondering) bestmove();
	}

	/* stop */
	void stop() {
		if (isPondering) bestmove();
	}

	/* show */
	void show(string line) {
		string [] words = line.split();
		foreach(i, ref w ; words) {
			if (w == "board") writeln(board);
			else if (w == "moves") {
				Moves moves;
				moves.generate(board);
				writeln(moves);
			}
			else if (w == "search") search.showSetting();
			else if (w == "eval") search.eval.show(board);
			else if (w == "weights") search.eval.showWeight();
		}
		stdout.flush();
	}	

	/* main loop */
	void loop(const bool readStdin = true) {
		if (readStdin) spawn(&eventLoop, event);
		while (true) {
			auto line = event.wait();
			if (log.isOpen) log.writefln("[%8.3f] uci> %s", chrono.time(), line);
			if (line == null) break;
			else if (line == "" || line[0] == '#') continue;
			else if (findSkip(line, "ucinewgame")) ucinewgame();
			else if (findSkip(line, "uci")) uci();
			else if (findSkip(line, "isready")) send("readyok");
			else if (findSkip(line, "setoption")) setoption(line);
			else if (findSkip(line, "position")) position(line);
			else if (findSkip(line, "go")) go(line);
			else if (findSkip(line, "stop")) stop();
			else if (findSkip(line, "ponderhit")) stop();
			else if (findSkip(line, "quit")) break;
			// unused
			else if (findSkip(line, "debug")) {}
			else if (findSkip(line, "register")) {}
			// extension
			else if (findSkip(line, "perft")) perft(line.split, board);
			else if (findSkip(line, "show")) show(line);
			else if (log.isOpen) log.writeln("error> unknown command: %s", line);
		}
		if (log.isOpen) {
			log.close();
		}
	}
}

/* unittest */
unittest {
	stderr.writeln("Testing uci protocol");
	Uci uci = new Uci();
	uci.event.push("uci");
	uci.event.push("ucinewgame");
	uci.event.push("position startpos moves e2e4");
	uci.event.push("show board");
	uci.event.push("show moves");
	uci.event.push("show weights");
	uci.event.push("show eval");
	uci.event.push("isready");
	uci.event.push("go depth 15");
	uci.event.push("show search");
	uci.event.push("position fen 8/k7/3p4/p2P1p2/P2P1P2/8/8/K7 w - -");
	uci.event.push("show board");
	uci.event.push("show moves");
	uci.event.push("show eval");
	uci.event.push("go movetime 15000");
	uci.event.push("show search");
	uci.event.push("position fen 8/k7/3n4/1Q6/8/8/8/K7 b - -");
	uci.event.push("show board");
	uci.event.push("show moves");
	uci.event.push("show eval");
	uci.event.push("go btime 1000 binc 100");
	uci.event.push("quit");
	uci.loop(false);
}


