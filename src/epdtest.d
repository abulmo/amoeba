/*
 * epdtest.d
 * epdtest analysis of a set of positions
 * © 2016-2020 Richard Delorme
 */

module epdtest;

import board, move, util, engine;
import std.algorithm, std.parallelism, std.conv, std.format, std.getopt, std.math, std.stdio, std.string, std.uni;

/* result found during the test */
enum Solution { notAvailable = 0, found = 1, notFound = 2 };

/* stats */
struct Stats {
	double x;
	double n;

	/* return the ration x / n */
	double v() const { return 100.0 * x / n; }

	/* return stats as a formated string */
	string toString() const {
		return n ? format("%.0f/%.0f (%.2f%%)", x, n, v) : "";
	}
	/* operator overloading: a + b; c * d; etc. apply the operator to each member data */
	Value opBinary(string op)(const Stats s) const {
		Stats r = { mixin("x " ~ op ~ " s.x"), mixin("n " ~ op ~ " s.n") };
		return r;
	}
	/* assignment operator overloading: apply the operator to each member data */
	void opOpAssign(string op)(const Stats s) {
		mixin("x" ~ op ~ "= s.x;");
		mixin("n" ~ op ~ "= s.n;");
	}
}

/* */
double timeScale(double t, double tMax) {
	const double a = 2.0 / (log(0.01 * tMax) - log(0.8 * tMax));
	const double b = 3.0 - a * log(0.01 * tMax);

	t = clamp(t, 0.01 * tMax, 0.8 * tMax);
	return a * log(t) + b;
}

/* Compute a MEA score according to Ed Shröder & F. Mosca */
Stats MEAScore(string epd, Board b, const Move found, const double scale, const double maxScale) {
	string c0 = epd.findBetween("c0", ";").findBetween("\"", "\"");
	string[] solutions = c0.split(",");
	Stats stats = Stats(0.0, 0.0);
	foreach(s; solutions) {
		string[] pair = s.split("=");
		if (pair.length == 3) {
			pair[0] ~= "=" ~ pair[1];
			pair[1] = pair[2];
		}
		const Move m = fromSan(pair[0].strip, b);
		int score = to!int(pair[1].strip);
		if (m == found) stats.x = score * scale;
		stats.n = max(stats.n, score * maxScale);
	}
	return stats;
}

/* Look if epd best moves match the best move found. */
Solution checkGoodMove(string epd, Board b, const Move found, const bool quiet) {
	if (!quiet) write("found ", found.toSan(b));
	string [] solutions = epd.findBetween(" bm ", ";").split();
	if (solutions.length > 0) {
		if (!quiet) write("; expected = ", solutions);
		foreach (s; solutions) {
			const Move m = fromSan(s, b);
			if (m == found) return Solution.found;
		}
		return Solution.notFound;
	} 
	return Solution.notAvailable;
}

/* Look if epd avoid moves match the best move found. */
Solution checkBadMove(string epd, Board b, const Move found, const bool quiet) {
	string [] mistakes = epd.findBetween(" am ", ";").split();
	if (mistakes.length > 0) {
		if (!quiet) write("; unexpected = ", mistakes);
		foreach (s; mistakes) {
			const Move m = fromSan(s, b);
			if (m == found) return Solution.found;
		}
		return Solution.notFound;
	}
	return Solution.notAvailable;
}

/* Look for a mate found at the right distance */
Solution checkMate(string epd, Board b, const int score, const bool quiet) {
	string solution = epd.findBetween(" dm ", ";");

	if (solution.length > 0) {
		int dm = Score.mate, em = to!int(solution.strip);

		if (score > Score.high) dm = (Score.mate + 1 - score) / 2;
		else if (score < Score.low) dm = -(Score.mate + score) / 2;

		if (!quiet) write(em <= 0 ? "; mated in " : "; mate in ", abs(em), ", ");
		if (dm < em && dm * em > 0) write("better ");
		if (dm <= em) {
			if (!quiet) write("found in ", abs(dm));
			return Solution.found;
		} else {
			if (!quiet) {
				write("not found");
				if (abs(dm - em) < 10) write(" in ", abs(dm));
			}
			return Solution.notFound;
		}
	}
	return Solution.notAvailable;
}

/* print the final result of solution counts */
string total(string type, ulong [3] n) {
	ulong N = n[Solution.found] + n[Solution.notFound];
	if (N) return format(" %d %s found / %d problems (%.2f%%);", n[Solution.found], type, N, 100.0 * n[Solution.found] / N);
	return "";
}

/* main function: read options start engine & loop over epd problems */
void main(string [] arg) {
	double tMax = double.infinity;
	int dMax = Limits.ply.max, nCpu = 1, loop = 1, multipv = 1, ttSize = 256;
	ulong nMax = ulong.max, n = 0;
	string executable, epdFile, cmd;
	bool showDebug, showHelp, showVersion, analyseMode, verbose, quiet, MEA, timeBonus;
	Engine engine;
	ulong [3] nGoodMoves, nMates, nBadMoves;
	Stats sMEA, tMEA = Stats(0.0, 0.0); 
	Move bestMove;
	double time = 0.0, lockTime = 0.0, T = 0.0, D = 0.0, N = 0.0, L = 0.0;
	int score;
	Moves moves;

	// read arguments
	getopt(arg, std.getopt.config.caseSensitive, "engine|e", &executable, "file|f", &epdFile,
		"movetime|t", &tMax, "depth|d", &dMax, "nodes|n", &nMax,
		"hash|H", &ttSize, "cpu|c", &nCpu, "analyse|a", &analyseMode,
		"MEA|M", &MEA, "bonus|b", &timeBonus,
		"loop|l", &loop, "verbose|v", &verbose, "quiet|q", &quiet,
		 "debug|g", &showDebug, "help|h", &showHelp, "version|V", &showVersion);

	// show help & exit
	if (showHelp || epdFile == null || executable == null) {
		stderr.writefln("%s --engine|-e <chess engine> --file|-f <epd file> [options]", arg[0]);
		stderr.writeln("test an engine with a set of epd files");
		stderr.writeln ("\t--file|-f <epd file>  EPD file to test");
		stderr.writeln ("\t--engine|-e <engine>  Use an external engine executable (default: use embedded amoeba)");
		stderr.writefln("\t--movetime|-t <time>  Maximum time per move in seconds (default: %s)", tMax);
		stderr.writefln("\t--depth|-d <depth>    Maximum depth per move (default: %s)", dMax);
		stderr.writefln("\t--nodes|-n <nodes>    Maximum nodes per move (default: %s)", nMax);
		stderr.writefln("\t--hash|-H <size>      Hash size in Mb (default %s MB)", ttSize);
		stderr.writefln("\t--analyse|-a          Set Engine into analyse mode", analyseMode);
		stderr.writefln("\t--cpu|-c <threads>    Cpu number (default %s cpu)", nCpu);
		stderr.writefln("\t--MEA|-M              MEA score (default %s)", MEA);
		stderr.writefln("\t--bonus|-b            Use time bonus in MEA Score (default %s)", timeBonus);
		stderr.writefln("\t--loop|-l <repeat>    Repeat the test several times (default ×%s)", loop);
		stderr.writefln("\t--verbose|-v          More verbose output (default: %s)", verbose);
		stderr.writefln("\t--quiet|-q            Less verbose output (default: %s)", quiet);
		stderr.writefln("\t--debug|-g            Log on to a debug file (default: %s)", showDebug);
		stderr.writeln ("\t--help|-h             Display this help\n");
		stderr.writeln ("  --version|-V          Show version number");
		return;
	}

	// show version
	if (showVersion) {
		writeln("Epdtest 1.0 (c) 2020 - Richard Delorme");
	}

	// initialisations
	writeln("*** ", arg[0], " threads: ", nCpu, " depth: ", dMax, " time: ", tMax, ", nodes: ", nMax, " memory: ", ttSize, " MB ***");

	timeBonus = (timeBonus && dMax == Limits.ply.max && nMax == ulong.max);

	if (executable.length > 0) {
		engine = new Engine(executable);
		if (showDebug) engine.startDebugging("epdtest");
		if (analyseMode) engine.analyse();
		engine.start(showDebug, ttSize, nCpu, multipv);
		writeln(engine.name, " used for analysis");
	} else {
		stderr.writeln("Fatal Error: You need to mention a chess engine to test");
		return;
	}
	
	Board b = new Board;

	// set go options (as cmd)
	if (dMax < Limits.ply.max) cmd = " depth " ~ to!string(dMax);
	if (nMax < ulong.max) cmd ~= " nodes " ~ to!string(dMax);
	if (tMax != double.infinity) cmd ~= " movetime " ~ to!string(cast(long) (1000 * tMax));

	// loop 'loop' times
	foreach (l; 0 .. loop) {
		auto f = std.stdio.File(epdFile);
		// loop over the epd file
		foreach (line; f.byLine()) {
			// clear Hash if possible else uci new game
			if (engine.clearHashSupport) engine.clearHash(); else engine.newGame();

			// set the board
			string epd = to!string(line).chomp();
			if (epd == "" || epd[0] == '#') continue;
			b.set(epd);
			if (verbose) {
				writeln();
				writeln(b);
			}
			engine.position(b.toFen());

			// test a position
			bestMove = engine.go(cmd, verbose);
			score = engine.info[0].score;
			L += lockTime = engine.info[0].lockTime;
			T += time = engine.info[0].time;
			N += engine.info[0].nodes;
			D += engine.info[0].depth;

			// update stats
			++n;
			nGoodMoves[checkGoodMove(epd, b, bestMove, quiet)]++;
			nBadMoves[checkBadMove(epd, b, bestMove, quiet)]++;
			nMates[checkMate(epd, b, score, quiet)]++;
			if (!quiet) writeln(": ", total("best moves", nGoodMoves), total("bad Moves", nBadMoves), total("mates", nMates));
			if (MEA) {
				sMEA = MEAScore(epd, b, bestMove, timeBonus ? timeScale(lockTime, tMax) : 1.0, timeBonus ? 3.0 : 1.0);
				tMEA += sMEA; 
				if (!quiet) writeln("MEA: ", sMEA.toString, ", ",tMEA.toString);
			}
			if (verbose) writefln("total: %.0f nodes in %.3fs : %.0f nps, depth = %.2f, avg time = %.3fs", N, T, N / T, D / n, L / n);
		}
		f.close();
	}
	// show global results
	writeln("------------------------------------------------------------------");
	write("epd: ", total("best moves", nGoodMoves), total("bad Moves", nBadMoves), total("mates", nMates));
	writef(" %.0f nodes in %.3fs (%.0f nps), depth = %.2f, locktime = %.3fs", N, T, N / T, D / n, L / n);
	if (MEA) write(", MEA:", tMEA.toString);
	writeln();

	if (engine) engine.end();
}

