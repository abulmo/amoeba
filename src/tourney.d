/*
 * File tourney.d
 * Organise a match beween two programs using UCI protocol
 * Play it until one program is found stronger than the other using the sprt approach.
 * © 2016-2017 Richard Delorme
 */

import util, board, move, game;
import std.conv, std.getopt, std.math, std.mathspecial, std.parallelism, std.process, std.stdio, std.range, std.string, std.format, std.algorithm;

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
				int matein = to!int(words[i + 1]);
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
	void start(bool showDebug) {
		string l;

		pipe = pipeProcess(cmd);
		send("uci");
		do {
			l = receive();
			if (l.skipOver("id name")) name = l.strip();
			if (showDebug && l.skipOver("option name Log type check")) send("setoption name Log value true");
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
		game = new shared Game(opening);
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

enum Var { none, trinomial, pentanomial, all };

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
		double N = n[0] + n[1] + n[2] + n[3] + n[4];
		double m = (n[1] * 0.5 + n[2] + n[3] * 1.5 + n[4] * 2.0) / N;
		double v = (n[1] * 0.25 + n[2] + n[3] * 2.25 + n[4] * 4.0) / N - (m ^^ 2);
		return v / (4 * N); // 4 is to rescale the variance as for one game
	}

	/* 3-nomial variance of the mean score*/
	double var3() const {
		double N = w + d + l;
		double m = (w + d * 0.5) / N;
		double v = (w + d * 0.25) / N - (m ^^ 2);
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

		foreach(r; result) {
			switch(r) {
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
	bool LLR(const double v) const {
		ulong n = w + d + l;
		double score = (w + 0.5 * d) / n;
		bool end = true;
		double σ = sqrt(v);
		double llr = v > 0.0 ? 0.5 * (score1 - score0) * (2 * score - score0 - score1) / v : 0.0;
		double los = v > 0.0 ? normalDistribution((min(max(score, 0.5 / n), 1 - 0.5 / n) - 0.5) / σ) : 0.5;

		writefln("Elo: %.1f [%.1f, %.1f]", elo(score), elo(score -  Φ * σ), elo(score + Φ * σ));
		writefln("LOS: %.2f %%", 100.0 * los);
		writefln("LLR: %.3f [%.3f, %.3f]", llr, llr0, llr1);

		if (llr < llr0) writeln("test rejected");
		else if (llr > llr1) writeln("test accepted");
		else end = false;
		writeln();

		return end;
	}


	/* stop if llr condition are met */
	bool stop(const Var v) const {
		bool end = false;

		if (v & Var.pentanomial) {
			writeln("Using variance of the pentanomial distribution of game pairs:");
			end = LLR(var5());
		}

		if (v & Var.trinomial) {
			writeln("Using variance of the trinomial distribution of single games:");
			end = LLR(var3());
		}

		return end;
	}
}


/*
 * A pool of engines running const parallel...
 */
class EnginePool {
private:
	shared GameBase openings;
	Engine [] player, opponent;
	std.stdio.File output;
	SPRT sprt;

	/* play a pair of matches, each engine playing white & black */
	bool match(const int i, const shared Game opening, const double time, const Var v) {
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
			string ext = o[$ - 3..$].toLower();
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

	/* start a pool of UCI engines */
	void start(const bool showDebug) {
		foreach (ref e; player) e.start(showDebug);
		foreach (ref e; opponent) e.start(showDebug);

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
			if (!done) done = match(i, openings.next(true), time, v);
		}
		taskPool.finish(true);
	}
}


/*
 * main: play a tournament between two UCI chess engines
 */
void main(string [] args) {
	EnginePool engines;
	double time = 0.1;
	int nGames = 30000, nCpu = 1;
	bool showVersion, showHelp, showDebug;
	string [] engineName, openingFile;
	string outputFile, var;
	double H0 = 0.0, H1 = 5.0, α = 0.05, β = 0.05;
	Var v = Var.all;

	// read arguments
	getopt(args, "engine|e", &engineName, "time|t", &time,
		"book|b", &openingFile, "output|o", &outputFile, "games|g", &nGames, "cpu|n", &nCpu,
		"elo0", &H0, "elo1", &H1, "alpha", &α, "beta", &β, "variance|v", &var,
		"debug|d", &showDebug, "help|h", &showHelp, "version", &showVersion);

	if (showVersion) writeln("tourney version 1.2\n© 2017 Richard Delorme");

	if (showHelp) {
		writeln("\nRun a tournament between two UCI engines using Sequential Probability Ratio Test as stopping condition.");
		writeln("\ntourney --engine|-e <cmd> --engine|-e <cmd>  [optional settings]") ;
		writeln("    --engine|-e <cmd>        launch an engine with <cmd>. 2 engines should be loaded");
		writeln("    --time|-t <movetime>     time (const seconds) to play a move (default 0.1s)");
		writeln("    --book|-b <pgn|epd file> opening book");
		writeln("    --output|-o <pgn file>   save the played games");
		writeln("    --games|-g <games>       max number of game pairs to play (default 30000)");
		writeln("    --cpu|-n <cpu>           number of games to play in parallel (default 1)");
		writeln("    --elo0  <elo>            H0 hypothesis (default = 0)");
		writeln("    --elo1  <elo>            H1 hypothesis (default = 5)");
		writeln("    --alpha <alpha>          type I error (default = 0.05)");
		writeln("    --beta  <beta>           type II error (default = 0.05)");
		writeln("    --variance|-v <type>     none|3nomial|5nomial|all (default=all) ");
		writeln("    --debug|-d               allow debugging by the engine to a log file");
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

	if (engineName.length != 2) {
		if (!showVersion && !showHelp) stderr.writeln("Two engines and only two needed");
		return;
	}

	var = var.toLower;
	if (var == "pentanomial" || var == "5-nomial" || var == "5nomial") v = Var.pentanomial;
	else if (var == "trinomial" || var == "3-nomial" || var == "3nomial") v = Var.trinomial;
	else if (var == "all") v = Var.all;

	// init
	nCpu = max(0, min(nCpu - 1, totalCPUs - 1));
	defaultPoolThreads(nCpu);
	engines = new EnginePool(engineName, openingFile, outputFile, H0, H1, α, β);

	// run the tournament
	engines.start(showDebug);
	engines.loop(nGames, time, v);
	engines.end();
}

unittest {
	SPRT sprt = SPRT(0, 5, α, β);
	sprt.w = 47; sprt.d = 356; sprt.l = 25;
	sprt.n = [0, 15, 163, 35, 1];
	sprt.stop(v);
}

