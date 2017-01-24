/*
 * File tourney.d
 * Organise a match beween two programs using UCI protocol
 * Play it until one program is found stronger than the other using the sprt approach.
 * © 2016-2017 Richard Delorme
 */

import std.stdio, std.conv, std.range, std.string, std.format, std.algorithm;
import std.math, std.mathspecial, std.process, std.parallelism, std.getopt;
import util, board, move, game;

/*
 * Engine
 */
class Engine {
private:
	string [] cmd;
	string name;
	ProcessPipes pipe;


public:
	/* constructor */
	this(string [] c) {
		cmd = c;
		name = cmd[0];
	}

	/* constructor */
	this(in string s) {
		name = s;
		cmd ~= s;
	}
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

	/* wait for an engine to be ready */
	void ready() {
		string l;
		send("isready");
		do {
			l = receive();
		} while (l != "readyok");
	}

	/* start an engine */
	void start() {
		string l;

		pipe = pipeProcess(cmd);
		send("uci");
		do {
			l = receive();
			if (l.skipOver("id name")) name = l.strip();
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
	void position(in shared Game g) {
		string s = "position startpos";

		if (g.moves.length > 0) {
			s ~= " moves";
			foreach(m; g.moves[0 .. $]) {
				s ~= " " ~ m.toPan();
			}
			send(s);
		}
	}

	/* go */
	Move go(in int ms) {
		string l;
		send("go movetime ", ms);
		while(true) {
			l = receive();
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
	this (Engine [Color.size] e, in shared Game opening, in double t) {
		engine = e;
		game = new shared Game(opening);
		ms = cast (int) (1000 * t + 0.5);
	}

	/* run a match */
	int run(in int round) {
		Board b = new Board;
		Result r;
		Move m;
		
		b.set();
		b.update(game.moves);
		
		engine[Color.white].newGame();
		engine[Color.black].newGame();
	
		while((r = b.isGameOver) == Result.none) {
			engine[b.player].position(game);
			m = engine[b.player].go(ms);
			if (m == 0 || !b.isLegal(m)) {
				if (b.player == Color.white) r = Result.blackWin; else r = Result.whiteWin;
				b.write();
				writefln("%s played the illegal move %s", engine[b.player].name, m.toPan());
				break;
			}
			b.update(m);
			game.push(m);
		}

		// game infos
		game.result = r;
		game.tags = null;
		game.push("Event", "Tourney");
		game.push("Site", "??");
		game.push("Date", date());
		game.push("Round", format("%d", round + 1));
		game.push("Black", engine[Color.black].name);
		game.push("White", engine[Color.white].name);
		game.push("Result", r.fromResult!false());

		if (r == Result.whiteWin) return 2;
		else if (r == Result.blackWin) return 0;
		else return 1;
	}
}

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
	static double elo(in double p) {
		if (p >= 1.0) return 1000;
		else if (p <= 0.0) return -1000;
		return -400.0 * log10(1.0 / p - 1.0);
	}

	/* winning probability from elo */
	static double proba(in double e) pure {
		return 1.0 / (1.0 + pow(10.0, -e / 400.0));
	}

	/* 5-nomial variance of the mean score (take care of game pair)*/
	double var5() const {
		double N = n[0] + n[1] + n[2] + n[3] + n[4];
		double m = (n[1] * 0.5 + n[2] + n[3] * 1.5 + n[4] * 2.0) / N;
		double v = (n[1] * 0.25 + n[2] + n[3] * 2.25 + n[4] * 4.0) / N - (m ^^ 2);
		return v / (4 * N); // 4 is to rescale the variance as for 1 game
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
	this(in double elo0, in double elo1, in double α = 0.05, in double β = 0.05) {
		Φ =  normalDistribution(1 - α * 0.5);
		score0 = proba(elo0);
		score1 = proba(elo1);
		llr0 = log(β / (1 - α));
		llr1 = log((1.0 - β) / α);
	}

	/* (re)set engine names */
	void setEngineNames(in string [2] engine) {
		name[0] = engine[0];
		name[1] = engine[1];
	}		

	/* record a new game */
	void record(in int [2] result) {
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

	/* stop if llr condition are met */
	bool stop() const {
		ulong n = w + d + l;
		double score = (w + 0.5 * d) / n;
		double v = var5(), σ = sqrt(v);
		double llr = v > 0.0 ? 0.5 * (score1 - score0) * (2 * score - score0 - score1) / v : 0.0;
		double los = normalDistribution((min(max(score, 0.5 / n), 1 - 0.5 / n) - 0.5) / σ);

		writefln("Elo: %.1f [%.1f, %.1f]", elo(score), elo(score -  Φ * σ), elo(score + Φ * σ));
		writefln("LOS: %.2f %%", 100.0 * los);
		writefln("LLR: %.3f [%.3f, %.3f]", llr, llr0, llr1);

		if (llr < llr0) writeln("test rejected");
		else if (llr > llr1) writeln("test accepted");
		writeln();
		
		return (llr < llr0 || llr > llr1); 
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
	bool match(in int i, in shared Game opening, in double time) {
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
			return sprt.stop();
		}
	}

public:
	/* constructor */
	this(in string [] engineName, in string [] openingFile, in string outputFile, in double elo0, in double elo1) {
		openings = new shared GameBase;
		foreach (o; openingFile) openings.read(o);

		output.open(outputFile, "w");
		
		sprt = SPRT(elo0, elo1);

		foreach(i; 0 .. taskPool.size + 1) {
			player ~= new Engine(engineName[0]);
			opponent ~= new Engine(engineName[1]);
		}
	}

	/* start a pool of UCI engines */
	void start() {
		foreach (ref e; player) e.start();
		foreach (ref e; opponent) e.start();
		sprt.setEngineNames([player[0].name, opponent[0].name]);
	}

	/* end a pool of UCI engines */
	void end() {
		foreach (ref e; player) e.end();
		foreach (ref e; opponent) e.end();
	}

	/* loop in parallel thru the games */
	void loop(in int nGames, in double time) {
		bool done = false;
		foreach (i; taskPool.parallel(iota(nGames))) {
			if (!done) done = (match(i, openings.next(true), time) && i >= 99);
		}
		taskPool.finish(true);
	}
}


/*
 * main: play a tournament between two UCI chess engines
 */
void main (string [] args) {
	EnginePool engines;
	double time = 0.1;
	int nGames = 30000, nCpu = 1;
	bool showVersion, showHelp;
	string [] engineName, openingFile;
	string outputFile;
	double H0 = 0.0, H1 = 5.0;
	
	// init
	getopt(args, "engine|e", &engineName, "time|t", &time, "book|b", &openingFile, "output|o", &outputFile, 
		"games|g", &nGames,	"cpu|n", &nCpu, "elo0|H0", &H0, "elo1|H1", &H1,
		"help|h", &showHelp, "version|v", &showVersion);

	if (showHelp) {
		writeln("tourney --engine|-e <cmd> --engine|-e <cmd>  --time|-t <movetime> --book|-b <pgn file> --games|g <games> --cpu|-n <cpu> --elo0|-H0 <elo> --elo1|-H1 <elo>") ;
		writeln("\t--engine|-e <cmd>      launch an engine with <cmd>. 2 engines should be loaded");
		writeln("\t--time|-t <movetime>   time to play a move");
		writeln("\t--book|-b <pgn file>   opening book");
		writeln("\t--output|-o <pgn file> opening book");
		writeln("\t--games|-g <games>     max number of game pairs to play");
		writeln("\t--cpu|-n <cpu>         number of games to play in parallel");
		writeln("\t--elo0|-H0 <elo>       H0 hypothesis (default = 0)");
		writeln("\t--elo1|-H1 <elo>       H1 hypothesis (default = 5)");
		writeln("\t--help|-h              display this help");
		writeln("\t--version|-v           show version number");
	}
		
	if (showVersion) writeln("tourney version 1.0\nRichard Delorme © 2017");
	if (engineName.length != 2) {
		stderr.writeln("Two engines and only two needed");
		return;
	}
	nCpu = max(0, min(nCpu - 1, totalCPUs - 1));
	defaultPoolThreads(nCpu);

	engines = new EnginePool(engineName, openingFile, outputFile, H0, H1);

	engines.start();
	engines.loop(nGames, time);
	engines.end();
}

