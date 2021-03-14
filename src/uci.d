/*
 * File uci.d
 * Universal Chess Interface.
 * © 2016-2020 Richard Delorme
 */

module uci;

import board, eval, move, search, tt, util;
import std.algorithm, std.array, std.conv, std.concurrency, std.parallelism, std.stdio, std.string;
import core.thread;

/* version */
enum string versionNumber = "3.3";

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

/* uci class */
final class Uci {
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
	util.Message message;
	Time [Color.size] time;
	int forcedDepth = -1, depthMax, movesToGo, multipv;
	double forcedTime = 0.0;
	ulong nodesMax;
	size_t hashSize = 64;
	int nThreads = 1;
	bool canPonder, isPondering, infiniteSearch, easy;
	string affinity;

	/* constructor */
	this(const bool dbg = false) {
		name = "Amoeba " ~ versionNumber ~ '.' ~ arch;
		message = new util.Message(name);
		if (dbg) message.logOn();
		board = new Board;
		search = Search(hashSize.MBytes, 1, message);
		ucinewgame();
		canPonder = false;
		search.option.verbose = true;
		multipv = 1;
		easy = true;
		affinity = "0:0";
	}

	/* set Hash Size */
	void resize(const size_t s) {
		const size_t hashSize = clamp(s, 1, 4096 * Entry.sizeof);
		search.resize(hashSize.MBytes);
	}

	/* set number of threads */
	void threads(const int t) {
		const int nThreads = clamp(t, 1, 256);
		search.threads(nThreads);
		search.position(board);
	}

	/* set thinking time */
	double setTime() {
		const p = board.player;
		double t = time[p].remaining;
		int todo = 40;

		if (t > 0) {
			if (movesToGo > 0) todo = movesToGo;
			t = min(t, (t + time[p].increment * (todo - 1)) / todo);
			t = max(t - 1.0, 0.95 * t);
		} else {
			t = time[p].increment;
			if (t > 0) {
				t = max(t - 1.0, 0.95 * t) - 0.015;
			} else t = double.infinity;
		}
		t = max(0.01, t);

		return t;
	}

	/* set max time to use const hard (failing low) position */
	double setExtraTime(const double maxTime) {
		return max(maxTime, min(2.0 * maxTime, time[board.player].remaining * 0.1)); 
	}

	/* uci command */
	void uci() {
		message.send("id name " ~ name);
		message.send("id author Richard Delorme");
		message.send("option name Ponder type check default false");
		message.send("option name Hash type spin default ", hashSize, " min 1 max ", 4096 * Entry.sizeof);
		message.send("option name Clear Hash type button");
		message.send("option name Threads type spin default ", nThreads, " min 1 max 256");
		message.send("option name Affinity type string default ", affinity);
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
		else if (option == "hash") resize(to!size_t(value));
		else if (option == "clear hash") search.clear();
		else if (option == "threads") threads(to!int(value));
		else if (option == "multipv") multipv = clamp(to!int(value), 1, 256);
		else if (option == "uci_analysemode") easy = !to!bool(value);
		else if (option == "affinity") affinity = value;
		else if (option == "log") {
			if (to!bool(value)) message.logOn();
			else message.logOff();
		}
		else message.send("info error ", option, " is not a known option");
			
	}

	/* ucinewgame command: clear the search state */
	void ucinewgame() {
		search.clear();
		board.set();
		search.position(board);
	}

	/* set a new position */
	void position(string line) {
		if (findSkip(line, "startpos")) board.set();
		else if (findSkip(line, "fen")) board.set(line);
		if (findSkip(line, "moves")) {
			auto words = line.split();
			foreach(w ; words) {
				Move m = fromPan(w);
				if (board.isLegal(m)) board.update(m);
				else message.send("info error ", w, " is not a legal move");
			}
		}
		search.position(board);
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
		Move hint = search.hint;
		if (hint != 0 && canPonder) message.send("bestmove ", search.bestMove.toPan(), " ponder ", hint.toPan());
		else message.send("bestmove ", search.bestMove.toPan());
	}

	/* go */
	void go(string line) {
		Option option;
		string [] words = line.split();

		moves.clear();
		option.depth.end = Limits.ply.max;
		option.nodes.max = ulong.max;
		foreach(c ; Color.white .. Color.size) time[c].clear();
		isPondering = infiniteSearch = false;
		// interpret the go command parameters
		foreach(i, ref w ; words) {
			if (w == "searchmoves") searchmoves(words);
			else if (w == "ponder") isPondering = true;
			else if (w == "wtime" && i + 1 < words.length) time[Color.white].remaining = 0.001 * to!double(words[i + 1]);
			else if (w == "btime" && i + 1 < words.length) time[Color.black].remaining = 0.001 * to!double(words[i + 1]);
			else if (w == "winc" && i + 1 < words.length) time[Color.white].increment = 0.001 * to!double(words[i + 1]);
			else if (w == "binc" && i + 1 < words.length) time[Color.black].increment = 0.001 * to!double(words[i + 1]);
			else if (w == "movestogo" && i + 1 < words.length) movesToGo = to!int(words[i + 1]);
			else if (w == "depth" && i + 1 < words.length) option.depth.end = to!int(words[i + 1]);
			else if (w == "mate" && i + 1 < words.length) option.depth.end = to!int(words[i + 1]);
			else if (w == "nodes" && i + 1 < words.length) option.nodes.max = to!ulong(words[i + 1]);
			else if (w == "movetime" && i + 1 < words.length) time[board.player].increment = 0.001 * to!double(words[i + 1]);
			else if (w == "infinite") { option.depth.end =  Limits.ply.max; infiniteSearch = true; }
		}
		// option overriding from the commandline
		if (forcedDepth >= 0) option.depth.end = forcedDepth;
		if (forcedTime > 0.0) {
			time[board.player].clear();
			time[board.player].increment = 0.001 * forcedTime;
		}
		// set options
		option.time.max = setTime();
		option.time.extra = setExtraTime(option.time.max);
		message.log("time> ", time[board.player].remaining, "/", movesToGo, "+", time[board.player].increment, ", time.max: ", option.time.max, ", time.extra: ", option.time.extra);
		option.easy = (easy && multipv == 1 && time[board.player].remaining > 0.0);
		option.multiPv = multipv;
		option.isPondering = isPondering;
		option.verbose = true;
		option.cpu.max = totalCPUs;
		option.cpu.affinity.set(affinity);
		message.log("search options: ", option);
		search.go(option, moves);
		if (!isPondering && !infiniteSearch) bestmove();
	}

	/* play a move after stop or ponderHit has been received */
	void play() {
		if (isPondering || infiniteSearch) bestmove();
	}

	/* main loop */
	void loop(const bool readStdin = true) {
		if (readStdin) message.daemon();
		while (stdin.isOpen) {
			auto line = message.retrieve();
			if (line is null || line == "" || line[0] == '#') continue;
			else if (findSkip(line, "debug")) message.logOn();
			else if (findSkip(line, "go")) go(line);
			else if (findSkip(line, "isready")) message.send("readyok");
			else if (findSkip(line, "position")) position(line);
			else if (findSkip(line, "ponderhit")) play();
			else if (findSkip(line, "quit")) break;
			else if (findSkip(line, "ucinewgame")) ucinewgame();
			else if (findSkip(line, "uci")) uci();
			else if (findSkip(line, "setoption")) setoption(line);
			else if (findSkip(line, "stop")) play();
			// extension
			else if (startsWith(line, "bench")) bench(line.split(), search);
			else if (startsWith(line, "perft")) perft(line.split(), board);
			// unused
			else if (findSkip(line, "register")) {}
			// xboard
			else if (findSkip(line, "xboard")) {
				message.send("tellusererror ", name, " only supports the UCI protocol");
				break;
			}
			// error
			else message.log("error unknown command: '%s'", line);
		}
	}
}

