/*
 * File tourney.d
 * Organise a match beween two programs using UCI protocol
 * Play it until one program is found stronger than the other using the sprt approach.
 * © 2016-2019 Richard Delorme
 */

import util, board, move, engine, game;
import core.atomic;
import std.algorithm, std.conv, std.format, std.getopt, std.parallelism, std.process, std.range, std.stdio, std.string;
import std.math, std.mathspecial, std.random;

/*
 * Time
 */
struct Time {
private:
	double base;
	int movesPerPeriod;
	double increment;
	double [Color.size] availableTime;
	int movesToGo;

	/* convert a time in [[hh]:mm:]s into seconds */
	static double convert(string time) {
		double t = 0.0;
		if (time.length > 0) {
			long m = -1;
			long h = indexOf(time, ":");
			if (h > -1) {
				m = indexOf(time[h + 1 .. $], ":");
				if (m == -1) {
					m = h;
					h = -1;
				}
			}
			if (h > -1) t += 3600.0 * to!int(time[0 .. h]);
			if (m > -1) t += 60.0 * to!int(time[h + 1 .. m]);
			t += to!double(time[m + 1 .. $]);
		}
		return t;
	}

	/* convert a time setting */
	this(const string time) {
		string b, mpp, i;
		long s0, s1;
		s0 = indexOf(time, '/');
		if (s0 > -1) {
			b = time[0 .. s0];
			s1 = indexOf(time[s0 + 1 .. $], '+');
			if (s1 > -1) {
				mpp = time[s0 + 1 .. s1];
				i = time[s1 + 1 .. $];
			} else mpp = time[s0 + 1 .. $];
		} else {
			mpp = "0";
			s1 = indexOf(time, '+');
			if (s1 > -1) b = time[0 .. s1];
			i = time[s1 + 1 .. $];
		}
		base = convert(b);
		movesPerPeriod = to!int(mpp);
		increment = convert(i);
		writeln("Time settings: base: ", base, " s; period: ", movesPerPeriod, " moves,  inc: ", increment, " s");
		availableTime = [base, base];
		movesToGo = movesPerPeriod;
	}

	/* return the string formatted to the go command */
	string goString() const {
		string s;
		if (base == 0.0) {
			s = format("movetime %.0f", 1000 * increment);
		} else {
			if (increment > 0.0) {
				s = format("wtime %.0f winc %.0f btime %.0f binc %.0f", 1000 * availableTime[Color.white], 1000 * increment, 1000 * availableTime[Color.black], 1000 * increment);
			} else  {
				s = format("wtime %.0f btime %.0f", 1000 * availableTime[Color.white], 1000 * availableTime[Color.black]);
			}
			if (movesPerPeriod > 0) {
				s ~= format(" movesToGo %d", movesToGo);
			}
		}
		return s;
	}

	/* to string */
	string toString() const {
		if (base == 0.0) return "?";
		if (movesPerPeriod) return format("%.0f/%d", base, movesPerPeriod);
		else return format("%.0f+%.1f", base, increment);
	}

	/* update the available time & movestogo after a move. return false if all the alloted time has been consumed */
	bool update(const Color player,  double time, double margin) {
		if ((base > 0.0 && availableTime[player] < -margin) || (base == 0.0 && time > increment + margin)) {
			writeln("time: ", base, "/", movesPerPeriod, "+", increment);
			writeln("time: ", availableTime[player], "/", movesToGo, "+", increment, "->", time);
			return false;
		}
		availableTime[player] += increment - time;
		if (movesPerPeriod > 0) {
			if (player == Color.black) --movesToGo;
			if (movesToGo == 0) movesToGo += movesPerPeriod;
		}
		return true;
	}

	/* correct movesToGo after playing the opening */
	void set(const Board b) {
		const ply = (b.ply + b.plyOffset) / 2;
		if (movesPerPeriod) movesToGo = (movesPerPeriod - ply % movesPerPeriod);
	}
}



/*
 * a Match between two opponents
 */
class Match {
	Engine [Color.size] engine;
	shared Game game;
	Time time;
	double margin;

	/* create a new match setting */
	this (Engine [Color.size] e, const shared Game opening, const Time t, const double m) {
		engine = e;
		game = new shared Game(opening, true);
		time = t;
		margin = m;
	}

	/* run a match */
	int run(const int round) {
		Chrono chrono;
		Board b = new Board;
		Result r;
		Move m;
		string fen = null;

		foreach (t; game.tags) if (t.name == "FEN") fen = t.value;
		if (fen is null) b.set(); else b.set(fen);
		b.update(game.moves);
		time.set(b);

		engine[Color.white].newGame();
		engine[Color.black].newGame();

		while((r = b.isGameOver) == Result.none) {
			engine[b.player].position(fen, game.moves);
			chrono.start();
			m = engine[b.player].go(time.goString);
			chrono.stop();
			if (m == 0 || !b.isLegal(m)) {
				b.write();
				writefln("%s played the illegal move %s", engine[b.player].name, m.toPan());
				r = [Result.whiteIllegalMove, Result.blackIllegalMove][b.player];
				break;
			}
			game.push(m, engine[b.player].info);
			if (!time.update(b.player, chrono.time, margin)) {
				writefln("%s loses on time", engine[b.player].name);
				r = [Result.whiteLossOnTime, Result.blackLossOnTime][b.player];
				break;
			}
			b.update(m);
		}

		// game infos
		game.result = r;
		game.tags = null;
		game.push("Event", "Tourney");
		game.push("Site", "??");
		game.push("Date", date());
		game.push("Round", format("%d", round + 1));
		game.push("White", engine[Color.white].name);
		game.push("Black", engine[Color.black].name);
		game.push("Result", r.fromResult!(LongFormat.off)());
		if (fen !is null) {
			game.push("FEN", fen);
			game.push("SetUp", "1");
		}
		game.push("TimeControl", time.toString());

		if (r == Result.whiteWin || r == Result.blackLossOnTime || r == Result.blackIllegalMove) return 2;
		else if (r == Result.blackWin || r == Result.whiteLossOnTime || r == Result.whiteIllegalMove) return 0;
		else return 1;
	}
}

enum Var { none, trinomial, pentanomial, all }

/*
 * Sequential Probability Ratio test (SPRT)
 *  - SPRT is computed directly from the wdl scores, not the elo
 *  - SPRT use a 5-nomial distribution to compute the variance
 * See the following discussion in talkchess for rationality:
 *
 */
struct SPRT {
	double score0, score1;
	double llr0, llr1;
	double Φ;
	ulong [5] n;
	ulong w, d, l;
	string [2] name;
	std.stdio.File logfile;

private:
	/* elo from winning probability */
	static double elo(const double p) {
		if (p >= 1.0) return 1000;
		else if (p <= 0.0) return -1000;
		return -400.0 * log10(1.0 / p - 1.0);
	}

	/* winning probability from elo */
	static double proba(const double e) pure {
		return 1.0 / (1.0 + pow(10.0, -e / 400.0));
	}

	/* 5-nomial variance of the mean score (take care of game pair)*/
	double var5() const {
		const double N = n[0] + n[1] + n[2] + n[3] + n[4];
		const double m = (n[1] * 0.5 + n[2] + n[3] * 1.5 + n[4] * 2.0) / N;
		const double v = (n[1] * 0.25 + n[2] + n[3] * 2.25 + n[4] * 4.0) / N - (m ^^ 2);
		return v / (4 * N); // 4 is to rescale the variance as for one game
	}

	/* 3-nomial variance of the mean score*/
	double var3() const {
		const double N = w + d + l;
		const double m = (w + d * 0.5) / N;
		const double v = (w + d * 0.25) / N - (m ^^ 2);
		return v / N;
	}

	/* send formatted */
	void sendf(T...) (string format, T args) {
		writefln(format, args);
		logfile.writefln(format, args);
		logfile.flush();
	}

	/* send */
	void send(T...) (T args) {
		writeln(args);
		logfile.writeln(args);
		logfile.flush();
	}

public:
	/* constructor */
	this(const double elo0, const double elo1, const double α, const double β) {
		Φ =  normalDistribution(1 - α * 0.5);
		score0 = proba(elo0);
		score1 = proba(elo1);
		llr0 = log(β / (1 - α));
		llr1 = log((1.0 - β) / α);
		logfile.open("tourney.log", "w");

	}

	/* (re)set engine names */
	void setEngineNames(const string [2] engine) {
		name[0] = engine[0];
		name[1] = engine[1];
	}

	/* record a new game */
	void record(const int [2] result) {
		int s;

		foreach (r; result) {
			switch (r) {
			case 0:
				writeln(name[1], " wins");
				++l;
				break;
			case 1:
				writeln("draw");
				++d;
				break;
			case 2:
				writeln(name[0], " wins");
				++w;
				break;
			default:
				return;
			}
			s += r;
		}
		++n[s];

		send(name[0], " vs ", name[1]);
		sendf("results: %d games", w + d + l);
		sendf("wdl:    w: %d, d: %d, l: %d", w, d, l);
		sendf("pair:   0: %d, 0.5: %d, 1: %d, 1.5: %d, 2: %d", n[0], n[1], n[2], n[3], n[4]);
	}

	/* compute llr for a given variance */
	int LLR(const double v) {
		const ulong N = w + d + l;
		const double score = (w + 0.5 * d) / N;
		const double σ = sqrt(v);
		const double llr = v > 0.0 ? 0.5 * (score1 - score0) * (2 * score - score0 - score1) / v : 0.0;
		const double los = v > 0.0 ? normalDistribution((min(max(score, 0.5 / N), 1 - 0.5 / N) - 0.5) / σ) : 0.5;
		int end = 0;

		sendf("Elo: %.1f [%.1f, %.1f]", elo(score), elo(score -  Φ * σ), elo(score + Φ * σ));
		sendf("LOS: %.2f %%", 100.0 * los);
		sendf("LLR: %.3f [%.3f, %.3f]", llr, llr0, llr1);

		if (llr < llr0) {
			send("test rejected");
			end = -1;
		} else if (llr > llr1) {
			send("test accepted");
			end = 1;
		} else {
			end = 0;
		}
		send();

		return end;
	}

	/* stop if llr condition are met */
	int stop(const Var v) {
		int end = 0;

		if (v & Var.pentanomial) {
			send("Using variance of the pentanomial distribution of game pairs:");
			end = LLR(var5());
		}

		if (v & Var.trinomial) {
			send("Using variance of the trinomial distribution of single games:");
			const int e = LLR(var3());
			if ((v & Var.pentanomial) && (e != end)) end = 0; else end = e;
		}

		return end;
	}
}

/*
 * A pool of engines running in parallel...
 */
class EnginePool {
private:
	shared GameBase openings;
	Engine [] player, opponent;
	std.stdio.File output;
	SPRT sprt;
	Time time;
	double margin = 0.0;

	/* play a pair of matches, each engine playing white & black */
	int match(const int i, const shared Game opening, const Var v) {
		auto w = taskPool.workerIndex;
		auto m1 = new Match([player[w], opponent[w]], opening, time, margin);
		auto m2 = new Match([opponent[w], player[w]], opening, time, margin);
		auto s1 = m1.run(i);
		auto s2 = m2.run(i);
		synchronized {
			if (output.isOpen) {
				m1.game.write(output); output.writeln();
				m2.game.write(output); output.writeln();
			}
			sprt.record([s1, 2 - s2]);
			return sprt.stop(v);
		}
	}

public:
	/* constructor */
	this(const string [] engineName, const string [] openingFile, string outputFile, const double elo0, const double elo1, const double α, const double β) {
		openings = new shared GameBase;
		foreach (o; openingFile) {
			const string ext = o[$ - 3..$].toLower();
			if (ext == "pgn") openings.read(o);
			else if (ext == "fen" || ext == "epd") openings.read!false(o);
		}
		if (openings.length == 0) openings ~= new shared Game;
		if (outputFile) output.open(outputFile, "w");

		sprt = SPRT(elo0, elo1, α, β);

		foreach(i; 0 .. taskPool.size + 1) {
			player ~= new Engine(engineName[0]);
			opponent ~= new Engine(engineName[1]);
		}
	}

	/* constructor */
	this(const string [] engineName, const int depth, const int nOpenings, string outputFile, const double elo0, const double elo1, const double α, const double β) {
		Board b = new Board;
		Random r;

		openings = new shared GameBase;
		r.seed(unpredictableSeed);
		foreach (o; 0 .. nOpenings) {
			shared Game game = new shared Game;
			game.random(b, r, depth);
			openings ~= game;
		}
		if (openings.length == 0) openings ~= new shared Game;
		if (outputFile) output.open(outputFile, "w");

		sprt = SPRT(elo0, elo1, α, β);

		foreach(i; 0 .. taskPool.size + 1) {
			player ~= new Engine(engineName[0]);
			opponent ~= new Engine(engineName[1]);
		}
	}

	/* start a pool of UCI engines */
	void start(const bool showDebug, const int hashSize, const int nThreads) {
		foreach (ref e; player) e.start(showDebug, hashSize, nThreads);
		foreach (ref e; opponent) e.start(showDebug, hashSize, nThreads);

		sprt.setEngineNames([player[0].name, opponent[0].name]);
	}

	/* end a pool of UCI engines */
	void end() {
		foreach (ref e; player) e.end();
		foreach (ref e; opponent) e.end();
	}

	/* loop const parallel thru the games */
	void loop(const int nGames, const string t, const double m, const Var v) {
		shared bool done = false;
		time = Time(t);
		margin = m;
		foreach (i; taskPool.parallel(iota(nGames))) {
			if (!done) {
				bool r = (match(i, openings.next(Loop.on), v) != 0);
				done = atomicOp!"|"(done, r);
			}
		}
		taskPool.finish(true);
	}
}

/*
 * sprt Simulation
 */
void simulation(const uint nSimulation, const uint nGames, const double draw, const double whiteAdvantage, const double H0, const double H1, const double α, const double β, const Var v) {
	Random r;
	double [3] W;
	double [3] B;
	double p;
	int [2] s;
	int result;
	std.stdio.File f;
	ulong [3] outcome;
	ulong tGames;

	r.seed(unpredictableSeed);

	f.open("simulation.txt", "w");

	for (double elo = -20; elo <= 20; elo += 0.1)  {
		const double a = 0.005 * whiteAdvantage, d = 0.01 * draw;
		const double w = max(0, min(1, (SPRT.proba(elo) - d / 2)));

		outcome = [0, 0, 0];
		tGames = 0;

		W[0] = max(0, min(1, w + a));
		W[1] = min(1 - W[0], d);
		W[2] = 1 - W[0] - W[1];

		B[0] = max(0, min(1, w - a));
		B[1] = min(1 - B[0], d);
		B[2] = 1 - B[0] - B[1];

		foreach (i; 0 .. nSimulation) {

			SPRT sprt = SPRT(H0,  H1, α, β);
			sprt.setEngineNames(["engine 1", "engine 2"]);

			foreach (j; 0 .. nGames) {
				++tGames;
				p = uniform01(r);
				if (p < W[0])  s[0] = 2; else if (p < W[0] + W[1]) s[0] = 1; else s[0] = 0;
				p = uniform01(r);
				if (p < B[0])  s[1] = 2; else if (p < B[0] + B[1]) s[1]= 1; else s[1] = 0;
				sprt.record(s);
				if ((result = sprt.stop(v)) != 0) break;
			}

			++outcome[result + 1];
		}

		const double n = outcome[0] + outcome[1] + outcome[2];

		stderr.writefln("Simulation for elo diff: %.2f; draw percentage %.2f%%; white advantage %.2f%%", elo, draw, whiteAdvantage);
		stderr.writeln("White proba: ", W, " ; Black proba: ", B);
		stderr.writefln("%s simulations, %s game pairs", nSimulation, tGames);
		stderr.writefln("rejected: %.4f, undecided: %.4f, accepted: %.4f", outcome[0] / n, outcome[1] / n, outcome[2] / n);
		f.writeln(elo, ", ", outcome[0] / n, ", ", outcome[1] / n, ", ", outcome[2] / n, ", ", tGames);
	}

}


/*
 * main: play a tournament between two UCI chess engines
 */
void main(string [] args) {
	EnginePool engines;
	int nGames = 30_000, nCpu = 1, nRandom, nSimulation = 0, hashSize = 64, nThreads = 1;
	bool showVersion, showHelp, showDebug, elo;
	string [] engineName, openingFile;
	string outputFile, var, time="10+0.1";
	double H0 = -2.0, H1 = 2.0, α = 0.05, β = 0.05, draw = 40.0, white = 10.0, win = 0.0, loss = 0.0, margin = 0.0;
	Var v;

	// read arguments
	getopt(args, "engine|e", &engineName, "time|t", &time, "margin|m", &margin, "hash", &hashSize, "threads", &nThreads,
		"book|b", &openingFile, "random|r", &nRandom, "output|o", &outputFile, "games|g", &nGames, "cpu|n", &nCpu,
		"simulation|s", &nSimulation, "draw|d", &draw, "white", &white,
		"elo0", &H0, "elo1", &H1, "alpha", &α, "beta", &β, "variance|v", &var,
		"elo", &elo, "win|w", &win, "loss|l", &loss,
		"debug", &showDebug, "help|h", &showHelp, "version", &showVersion);

	if (showVersion) writeln("tourney version 1.7\n© 2017-2019 Richard Delorme");

	if (showHelp) {
		writeln("\nRun a tournament between two UCI engines using Sequential Probability Ratio Test as stopping condition.");
		writeln("\ntourney --engine|-e <cmd> --engine|-e <cmd>  [optional settings]") ;
		writeln("    --engine|-e <cmd>        launch an engine with <cmd>. 2 engines should be loaded");
		writeln("    --time|-t <time>         time ([h:][m:]s[/period]+[m:]s) to play a game (default 10+0.1)");
		writeln("    --margin|-m <seconds>    time tolerance before declaring a time loss (default 0.0)");
		writeln("    --hash <MB>              hash size in MB (default 64)");
		writeln("    --threads <n>            number of threads allowed per engine (default 1)");
		writeln("    --book|-b <pgn|epd file> opening book");
		writeln("    --random|-r depth>       random opening moves up to <depth>");
		writeln("    --output|-o <pgn file>   save the played games");
		writeln("    --games|-g <games>       max number of game pairs to play (default 30000)");
		writeln("    --cpu|-n <cpu>           number of games to play in parallel (default 1)");
		writeln("    --elo0  <elo>            H0 hypothesis (default = 0)");
		writeln("    --elo1  <elo>            H1 hypothesis (default = 5)");
		writeln("    --alpha <alpha>          type I error (default = 0.05)");
		writeln("    --beta  <beta>           type II error (default = 0.05)");
		writeln("    --variance|-v <type>     none|3nomial|5nomial|all (default=all) ");
		writeln("    --simulation|s <n>       run <n> simulations (default: 0)");
		writeln("        --draw|-d <%draw>        draw percentage to use during a simulation (0 to 100: default 40)");
		writeln("        --white  <%draw>         white advantage to use during a simulation (0 to 30: default 10)");
		writeln("    --elo                    compute elo");
		writeln("        --win|-w <n>             number of win ");
		writeln("        --draw|-d <n>            number of draw ");
		writeln("        --loss|-l <n>            number of loss ");
		writeln("    --debug                  allow debugging by the engine to a log file");
		writeln("    --help|-h                display this help");
		writeln("    --version                show version number");
		writeln("\nFor example:\n$ tourney -e amoeba-2.1 -e amoeba-2.0 -g 30000 -b opening.pgn -t 0.1 -n 3 -o game.pgn -v 5nomial");
		writeln("[...]");
		writeln("Amoeba 2.1-l64p vs Amoeba 2.0.l64p");
		writeln("results: 3524 games");
		writeln("wdl:    w: 1058, d: 1545, l: 921");
		writeln("pair:   0: 112, 0.5: 395, 1: 651, 1.5: 452, 2: 152");
		writeln("Elo: 13.5 [9.9, 17.1]");
		writeln("LOS: 99.92 %");
		writeln("LLR: 2.992 [-2.944, 2.944]");
		writeln("test accepted");
	}

	if (nSimulation == 0 && !elo  && engineName.length != 2) {
		if (!showVersion && !showHelp) stderr.writeln("Two engines and only two needed");
		return;
	}

	var = var.toLower;
	if (var == "pentanomial" || var == "5-nomial" || var == "5nomial") v = Var.pentanomial;
	else if (var == "trinomial" || var == "3-nomial" || var == "3nomial") v = Var.trinomial;
	else if (var == "none") v = Var.none;
	else v = Var.all;

	// run a simulation
	if (nSimulation > 0) {
		white = max(0, min(30, white));
		draw = max(0, min(100, draw));
		simulation(nSimulation, nGames, draw, white, H0,  H1, α, β, v);

	} else if (elo) {
		SPRT sprt = SPRT(H0, H1, α, β);
		sprt.w = cast (ulong) win;
		sprt.d = cast (ulong) draw;
		sprt.l = cast (ulong) loss;
		sprt.LLR(sprt.var3());

	// run a tournament between 2 engines
	} else {
		// init
		nCpu = max(0, min(nCpu - 1, totalCPUs - 1));
		defaultPoolThreads(nCpu);
		if (nRandom > 0) engines = new EnginePool(engineName, nRandom, nGames, outputFile, H0, H1, α, β);
		else engines = new EnginePool(engineName, openingFile, outputFile, H0, H1, α, β);

		// run the tournament
		engines.start(showDebug, hashSize, nThreads);
		engines.loop(nGames, time, margin, v);
		engines.end();
	}
}

unittest {
	SPRT sprt = SPRT(0, 5, α, β);
	sprt.w = 47; sprt.d = 356; sprt.l = 25;
	sprt.n = [0, 15, 163, 35, 1];
	sprt.stop(v);
}

