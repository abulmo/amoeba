/*
 * postmortem.d
 * Posmortem analysis of a set of games
 * © 2016-2020 Richard Delorme
 */

module postmortem;

import game, search, board, move, util, uci, engine;
import std.stdio, std.getopt, std.algorithm, std.format, std.uni, std.conv;

/*
 * Analyse a game;
 */

void analyse(shared Game game, Search *search, Engine engine, const ref search.Option option, const size_t width, std.stdio.File epd) {
	Board board = new Board;
	Moves moves;
	int [2] score;
	int length = cast (int) game.moves.length;
	string fen, cmd;
	Result r;
	Line pv;

	// return a comparison character 
	char cmp() {	
		int µ = (Score.low < score[0] && score[0] < Score.high) ? 10 : 0;
		if (score[0] > score[1] + µ) return '>'; 
		else if (score[0] < score[1] - µ) return '<';
		else if (score[0] == score[1]) return '=';
		else return '~';
	}

	// nice string format for the score
	string nice(const int s) {
		if (s > Score.high) return format("+M%d", (Score.mate + 1 - s) / 2); 
		if (s < Score.low) return format("-M%d", (Score.mate + s) / 2); 
		else return format("%+5.2f", 0.01 * s);
	}

	// check if the move was bad
	string bad() {
		int µ = (Score.low < score[0] && score[0] < Score.high) ? 10 : 0;
		if (score[0] < score[1] - 100) return "??";
		else if (score[0] < score[1] - µ) return "? ";
		else return "  ";
	}

	// print the Principal variation on a single or several aligned lines.
	void pvPrint(string text) {
		const int margin = 32;
		size_t a, b;
		foreach (i; 0 .. text.length) {
			if (text[i].isSpace()) {
				if (i >= a + width - margin) {
					if (b == a) b = i;
					writeln(text[a .. b]);
					a = b + 1;
					if (a < text.length) foreach (j; 0 .. margin) write(' ');
				} else b = i;
			}
		}
		if (a < text.length) writeln(text[a .. $]);
	}

	// save a bad move to an epd file
	void save(const Move bm, const Move am) {
		static int id;
		if (epd.isOK && score[0] < score[1] - 100) {
			epd.writeln(board.toFen!(LongFormat.off)(), " bm ", bm.toSan(board), "; am = ", am.toSan(board), "; id \"", ++id, "\";");
		}
	}

	// write opponent names
	writeln();
	write("game: ");
	foreach (t; game.tags) if (t.name == "White") write(t.value, " vs ");
	foreach (t; game.tags) if (t.name == "Black") write(t.value); 
	foreach (t; game.tags) if (t.name == "Date") writeln(" - ", t.value); 

	// title
	writeln();
	writeln("     Played Move      |   Best Alternative");
	writeln("ply    move     score |   score pv");
	writeln("----------------------+--------------------");

	// init: okay the game to the end
	if (search) search.clear(true);
	else if (engine) engine.newGame();
	foreach (t; game.tags) if (t.name == "FEN") fen = t.value;
	if (fen is null) board.set(); else board.set(fen);
	foreach (m; game.moves) board.update(m);

	// initial score
	r = board.isGameOver();
	if (r == Result.none) {
		if (engine) {
			engine.position(fen, game.moves);
			if (option.depth.end < Limits.ply.max) cmd = "depth " ~ to!string(option.depth.end);
			else cmd = "movetime " ~ to!string(cast(int) (1000 * option.time.max));
			engine.go(cmd);
			score[0] = engine.info.score;
			pv.set(engine.pv);
		} else if (search) {
			search.position(board);
			search.go(option, moves);
			score[0] = search.score();
			pv = search.pv();
		}
	} else {
		writeln(fromResult!(LongFormat.on)(r));
		if (r == Result.whiteWin) score[0] = Score.mate;
		else if (r == Result.blackWin) score[0] = -Score.mate;
		else score[0] = 0;
		if (board.player == Color.black) score[0] = -score[0];
	}

	// loop over all move from the end to the begining of the game
	foreach_reverse(m; game.moves) {
		board.restore(m);
		--length;

		// correct the score for the current ply & player
		score[0] = -score[0];
		if (score[0] > Score.high) --score[0]; else if (score[0] < Score.low) ++score[0];

		// search for the best second move
		moves.generate(board);
		if (moves.length > 1) { 
			moves.exclude(m);
			if (engine) {
				engine.position(fen, game.moves[0 .. length]);
				if (option.depth.end < Limits.ply.max) cmd = "depth " ~ to!string(option.depth.end);
				else cmd = "movetime " ~ to!string(cast(int) (1000 * option.time.max));
				cmd ~= " searchmoves " ~ moves.toString();
				engine.go(cmd);
				score[1] = engine.info.score;
				pv.set(engine.pv);
			} else if (search) {
				search.position(board);
				search.go(option, moves);
				score[1] = search.score();
				pv = search.pv;
			}

			// print the result of the analyze
			if (board.player == Color.white) writef("%3d. ", (board.ply + board.plyOffset) / 2 + 1); else write("     ");
			writef("%6s%s %7s %c %7s ", m.toSan(board), bad(), nice(score[0]), cmp(), nice(score[1]));
			pvPrint(pv.toSan(board));

			// save to epd file
			if (cmp() == '<') save(pv.move[0], m);

			// keep the best score
			if (score[1] > score[0]) score[0] = score[1];

		} else { // special case: no alternative move
			if (board.player == Color.white) writef("%3d. ", (board.ply + board.plyOffset) / 2 + 1); else write("     ");
			writefln("%6s   %7s", m.toSan(board), nice(score[0]));
		}
	}
}


void main(string [] arg) {
	double tMax = double.infinity;
	int dMax = Limits.ply.max;
	int nCpu = 1;
	int width = 120;
	int ttSize = 256;
	search.Option option;
	Moves moves;
	string gameFile, executable, epdFile;
	bool showDebug, showHelp, showVersion, analyseMode;
	Search *search;
	Engine engine;
	std.stdio.File epd;

	// read arguments
	getopt(arg, std.getopt.config.caseSensitive, "engine|e", &executable, "movetime|t", &tMax, "depth|d", &dMax, "hash|H", &ttSize,
		"cpu|c", &nCpu, "width|w", &width, "file|f", &gameFile, "analyse|a", &analyseMode, "epd|o", &epdFile, 
		 "debug|g", &showDebug, "help|h", &showHelp, "version|v", &showVersion);

	if (showHelp) {
		writeln("postmortem <options>");
		writeln("  --engine|-e <engine>    use an external engine executable (default: use embedded amoeba)");
 		writeln("  --movetime|-t <time>    time alloted to analyze each move (default: infinite)"); 
		writeln("  --depth|-d <depth>      max depth to analyze each move (default: ", Limits.ply.max, ")");
		writeln("  --hash|-H <size>        size of the transposition table in Mb (default: 256)");
		writeln("  --cpu|-c <cpu>          set the number of cpus to use (default: 1)");
		writeln("  --width|-w <width>      set the width of the printed lines (default: 120)");
		writeln("  --file[-f <pgn>         name of the game file to analyze");
		writeln("  --analyse|-a            set UCI_AnalyseMode option to true, if supported");
		writeln("  --epd|-o <file>         output found errors into this epd file");
		writeln("  --debug|-g              switch debugging on");
		writeln("  --help|-h               display this help");
		writeln("  --version               show version number");
		return;
	}
	if (showVersion) {
		writeln("Postmortem version 1.1 (c) 2019-2020 - Richard Delorme");
	}

	option.time.max = option.time.extra = tMax;
	option.depth.end = min(dMax, Limits.ply.max);
	option.nodes.max = long.max;
	option.easy = false;
	option.multiPv = 1;
	option.verbose = false;
	option.doPrune = !analyseMode;
	if (executable.length > 0) {
		engine = new Engine(executable);
		if (showDebug) engine.startDebugging("postmortem");
		if (analyseMode) engine.analyse();
		engine.start(showDebug, ttSize, nCpu);
		writeln(engine.name, " used for analysis");
	} else {
		ttSize = clamp(ttSize, 1, 65_536);
		size_t ttSizeInBytes = (cast(size_t) ttSize) * 1_024 * 1_024;
		search = new Search(ttSizeInBytes, nCpu, new Message("postmortem"));
		if (showDebug) search.message.logOn();
		writeln("Internal engine used for analysis: amoeba " ~ versionNumber);
	}
	shared GameBase base = new shared GameBase;
	base.read(gameFile);

	if (epdFile.length > 0) epd.open(epdFile, "w");

	foreach(game; base.games) {
		analyse(game, search, engine, option, width, epd);		
	}

	if (engine) engine.end();
}

