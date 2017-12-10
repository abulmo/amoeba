/*
 * File uci.d
 * Universal Chess Interface.
 * © 2016-2017 Richard Delorme
 */

module uci;

import board, eval, move, search, util;
import std.algorithm, std.array, std.conv, std.concurrency, std.stdio, std.string;

/* version */
enum string versionNumber = "2.7";

/* Some information about the compilation */
string arch() @property {
	string a;
	// OS
	version (Windows) a = "w";
	else version (linux) a = "l";
	else version (OSX) a = "m";
	else a = "?";

	// CPU
	a ~= to!string(8 * size_t.sizeof);
	version (withPopCount) a ~= "p";
	version(ARM) a ~='a';
	else version(AARCH64) a ~='a';

	// Compiler
	a ~= '-';
	version (LDC) a ~= 'l';
	else version (GNU) a ~= 'g';
	else version (DigitalMars) a ~= 'd';
	else version (SDC) a ~= 's';
	else a ~= '?';

	return a;
}

/* spawnable message loop function */
void messageLoop(shared util.Message m) {
	m.loop();
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
	shared util.Message message;	
	Time [Color.size] time;
	int depthMax, movesToGo, multipv;
	ulong nodesMax;
	bool canPonder, isPondering, infiniteSearch, easy;

	/* constructor */
	this(const bool dbg = false) {
		name = "Amoeba " ~ versionNumber ~ '.' ~ arch;
		search = new Search;
		search.message = message = new shared util.Message(name);
		if (dbg) message.logOn();
		board = new Board;
		ucinewgame();
		canPonder = false;
		search.option.verbose = true;
		multipv = 1;
		easy = true;
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
		t = max(maxTime, min(2.0 * maxTime, t));

		return t;
	}

	/* uci command */
	void uci() {
		message.send("id name " ~ name);
		message.send("id author Richard Delorme");
		message.send("option name Ponder type check default false");
		message.send("option name Hash type spin default 64 min 1 max ", 4096 * Entry.sizeof);
		message.send("option name Log type check default ", message.isLogging());
		message.send("option name MultiPV type spin default 1 min 1 max 256");
		message.send("option name UCI_AnalyseMode type check default false");
		// add more options here...
		message.send("uciok");
	}

	/* setoption command */
	void setoption(string line) {
		const string option = findBetween(line.chomp(), "name", "value").strip().toLower();
		findSkip(line, "value");
		string value = line.strip().toLower();
		if (option == "ponder") canPonder = to!bool(value);
		else if (option == "hash") search.resize(to!size_t(value) * 1024 * 1024);
		else if (option == "multipv") multipv = to!int(value);
		else if (option == "uci_analysemode") easy = !to!bool(value);
		else if (option == "log") {
			if (to!bool(value)) message.logOn();
			else message.logOff();
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
		if (search.hint != 0 && canPonder) message.send("bestmove ", search.bestMove.toPan(), " ponder ", search.hint.toPan());
		else message.send("bestmove ", search.bestMove.toPan());
	}

	/* go */
	void go(string line) {
		Termination termination;
		string [] words = line.split();
		bool doPrune = true;

		moves.clear();
		termination.depth.max = Limits.ply.max;
		termination.nodes.max = ulong.max;
		foreach(c ; Color.white .. Color.size) time[c].clear();
		isPondering = infiniteSearch = false;
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
			else if (w == "mate") {
				doPrune = false;
				if (i + 1 < words.length) termination.depth.max = to!int(words[i + 1]);
			} else if (w == "movetime" && i + 1 < words.length) time[board.player].increment = 0.001 * to!double(words[i + 1]);
			else if (w == "infinite") { termination.depth.max =  Limits.ply.max; infiniteSearch = true; }
		}
		termination.time.max = setTime();
		termination.time.extra = setExtraTime(termination.time.max);

		search.go(termination, moves, (easy && multipv == 1), multipv, isPondering, doPrune);
		if (!isPondering && !infiniteSearch) bestmove();
	}

	/* play a move after stop has been received or ponderHit */
	void play() {
		if (isPondering || infiniteSearch) bestmove();
	}

	/* main loop */
	void loop(const bool readStdin = true) {
		if (readStdin) spawn(&messageLoop, message);
		while (stdin.isOpen) {
			auto line = message.retrieve();
			if (line is null || line == "" || line[0] == '#') continue;
			else if (findSkip(line, "ucinewgame")) ucinewgame();
			else if (findSkip(line, "uci")) uci();
			else if (findSkip(line, "isready")) message.send("readyok");
			else if (findSkip(line, "setoption")) setoption(line);
			else if (findSkip(line, "position")) position(line);
			else if (findSkip(line, "go")) go(line);
			else if (findSkip(line, "stop")) play();
			else if (findSkip(line, "ponderhit")) play();
			else if (findSkip(line, "quit")) break;
			// unused
			else if (findSkip(line, "debug")) {}
			else if (findSkip(line, "register")) {}
			// error
			else message.log("error unknown command: '%s'", line);
		}
	}
}

