/*
 * File tourney.d
 * Organise a match beween two programs using UCI protocol
 * Play it until one program is found stronger than the other using the sprt approach.
 * © 2016-2018 Richard Delorme
 */

import util, board, move, game;
import core.atomic;
import std.algorithm, std.conv, std.format, std.getopt, std.parallelism, std.process, std.range, std.stdio, std.string;
import std.math, std.mathspecial, std.random;

/*
 * Engine
 */
class Engine {
private:
	string [] cmd;
	string name;
	ProcessPipes pipe;
	Game.Info info;

	/* send a message to an engine */
	void send(T...)(T args) {
		pipe.stdin.writeln(args);
		pipe.stdin.flush();
	}

	/* receive a message from an engine */
	string receive() {
		string l = pipe.stdout.readln();
		l = chomp(l);
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
		}
	}

public:
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
	void start(bool showDebug, int hashSize = 64) {
		string l;

		pipe = pipeProcess(cmd);
		send("uci");
		do {
			l = receive();
			if (l.skipOver("id name")) name = l.strip();
			if (showDebug && l.skipOver("option name Log type check")) send("setoption name Log value true");
			if (hashSize != 64 && l.skipOver("option name Hash type spin")) send("setoption name Hash value ", hashSize);
		} while (l != "uciok");
	}

	/* end an engine */
	void end() {
		send("quit");
		wait(pipe.pid);
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
	Move go(const int ms) {
		string l;
		info = Game.Info.init;
		send("go movetime ", ms);
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


/*
 * a Match between two opponents
 */
class Match {
	Engine [Color.size] engine;
	shared Game game;
	int ms;

	/* create a new match setting */
	this (Engine [Color.size] e, const shared Game opening, const double t) {
		engine = e;
		game = new shared Game(opening, true);
		ms = cast (int) (1000 * t + 0.5);
	}

	/* run a match */
	int run(const int round) {
		Board b = new Board;
		Result r;
		Move m;
		string fen = null;

		foreach (t; game.tags) if (t.name == "FEN") fen = t.value;
		if (fen is null) b.set(); else b.set(fen);
		b.update(game.moves);

		engine[Color.white].newGame();
		engine[Color.black].newGame();

		while((r = b.isGameOver) == Result.none) {
			engine[b.player].position(fen, game.moves);
			m = engine[b.player].go(ms);
			if (m == 0 || !b.isLegal(m)) {
				if (b.player == Color.white) r = Result.blackWin; else r = Result.whiteWin;
				b.write();
				writefln("%s played the illegal move %s", engine[b.player].name, m.toPan());
				break;
			}
			game.push(m, engine[b.player].info);
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
		game.push("Result", r.fromResult!false());
		if (fen !is null) {
			game.push("FEN", fen);
			game.push("SetUp", "1");
		}

		if (r == Result.whiteWin) return 2;
		else if (r == Result.blackWin) return 0;
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

public:
	/* constructor */
	this(const double elo0, const double elo1, const double α, const double β) {
		Φ =  normalDistribution(1 - α * 0.5);
		score0 = proba(elo0);
		score1 = proba(elo1);
		llr0 = log(β / (1 - α));
		llr1 = log((1.0 - β) / α);
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

		writeln(name[0], " vs ", name[1]);
		writefln("results: %d games", w + d + l);
		writefln("wdl:    w: %d, d: %d, l: %d", w, d, l);
		writefln("pair:   0: %d, 0.5: %d, 1: %d, 1.5: %d, 2: %d", n[0], n[1], n[2], n[3], n[4]);
	}

	/* compute llr for a given variance */
	int LLR(const double v) const {
		const ulong N = w + d + l;
		const double score = (w + 0.5 * d) / N;
		const double σ = sqrt(v);
		const double llr = v > 0.0 ? 0.5 * (score1 - score0) * (2 * score - score0 - score1) / v : 0.0;
		const double los = v > 0.0 ? normalDistribution((min(max(score, 0.5 / N), 1 - 0.5 / N) - 0.5) / σ) : 0.5;
		int end = 0;

		writefln("Elo: %.1f [%.1f, %.1f]", elo(score), elo(score -  Φ * σ), elo(score + Φ * σ));
		writefln("LOS: %.2f %%", 100.0 * los);
		writefln("LLR: %.3f [%.3f, %.3f]", llr, llr0, llr1);

		if (llr < llr0) {
			writeln("test rejected");
			end = -1;
		} else if (llr > llr1) {
			writeln("test accepted");
			end = 1;
		} else {
			end = 0;
		}
		writeln();

		return end;
	}


	/* stop if llr condition are met */
	int stop(const Var v) const {
		int end = 0;

		if (v & Var.pentanomial) {
			writeln("Using variance of the pentanomial distribution of game pairs:");
			end = LLR(var5());
		}

		if (v & Var.trinomial) {
			writeln("Using variance of the trinomial distribution of single games:");
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

	/* play a pair of matches, each engine playing white & black */
	int match(const int i, const shared Game opening, const double time, const Var v) {
		auto w = taskPool.workerIndex;
		auto m1 = new Match([player[w], opponent[w]], opening, time);
		auto m2 = new Match([opponent[w], player[w]], opening, time);
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
	void start(const bool showDebug, const int hashSize) {
		foreach (ref e; player) e.start(showDebug, hashSize);
		foreach (ref e; opponent) e.start(showDebug, hashSize);

		sprt.setEngineNames([player[0].name, opponent[0].name]);
	}

	/* end a pool of UCI engines */
	void end() {
		foreach (ref e; player) e.end();
		foreach (ref e; opponent) e.end();
	}

	/* loop const parallel thru the games */
	void loop(const int nGames, const double time, const Var v) {
		shared bool done = false;
		foreach (i; taskPool.parallel(iota(nGames))) {
			if (!done) {
				bool r = (match(i, openings.next(true), time, v) != 0);
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
	double time = 0.1;
	int nGames = 30_000, nCpu = 1, nRandom, nSimulation = 0, hashSize = 64;
	bool showVersion, showHelp, showDebug, elo;
	string [] engineName, openingFile;
	string outputFile, var;
	double H0 = -2.0, H1 = 2.0, α = 0.05, β = 0.05, draw = 40.0, white = 10.0, win = 0.0, loss = 0.0;
	Var v;

	// read arguments
	getopt(args, "engine|e", &engineName, "time|t", &time, "hash", &hashSize,
		"book|b", &openingFile, "random|r", &nRandom, "output|o", &outputFile, "games|g", &nGames, "cpu|n", &nCpu,
		"simulation|s", &nSimulation, "draw|d", &draw, "white", &white,
		"elo0", &H0, "elo1", &H1, "alpha", &α, "beta", &β, "variance|v", &var,
		"elo", &elo, "win|w", &win, "loss|l", &loss,
		"debug", &showDebug, "help|h", &showHelp, "version", &showVersion);

	if (showVersion) writeln("tourney version 1.6\n© 2017-2018 Richard Delorme");

	if (showHelp) {
		writeln("\nRun a tournament between two UCI engines using Sequential Probability Ratio Test as stopping condition.");
		writeln("\ntourney --engine|-e <cmd> --engine|-e <cmd>  [optional settings]") ;
		writeln("    --engine|-e <cmd>        launch an engine with <cmd>. 2 engines should be loaded");
		writeln("    --time|-t <movetime>     time (in seconds) to play a move (default 0.1s)");
		writeln("    --hash <MB>              hash size in MB (default 64)");
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
		engines.start(showDebug, hashSize);
		engines.loop(nGames, time, v);
		engines.end();
	}
}

unittest {
	SPRT sprt = SPRT(0, 5, α, β);
	sprt.w = 47; sprt.d = 356; sprt.l = 25;
	sprt.n = [0, 15, 163, 35, 1];
	sprt.stop(v);
}

