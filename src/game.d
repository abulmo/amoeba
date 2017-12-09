/*
 * File game.d
 * PGN game reader / writer
 * © 2016-2017 Richard Delorme
 */

import board, move, util;
import std.algorithm, std.format, std.math, std.random, std.stdio, std.string, std.uni;
import core.atomic;

/*
 * A buffered file reader
 */
struct Reader {
	string line;
	std.stdio.File file;
	bool skip;

	/* open a file to read */
	void open(string fileName) {
		file.open(fileName, "r");
		writeln("reading ", fileName);
	}

	/* close the file */
	void close() {
		file.close();
	}

	/* read a line */
	string read() {
		if (!skip) line = file.readln();
		skip = false;
		return line;
	}

	/* unread a line (keep it in the buffer) */
	void unread() {
		skip = true;
	}

	/* check if the reader is ok to be read */
	bool isOK() {
		return skip || file.isOK();
	}
}


/*
 * PGN tag
 */
struct Tag {
private:
	/* get the name of the tag  */
	static string getName(string line) {
		foreach (i; 1 .. line.length) {
			if (isSpace(line[i])) return line[1 .. i];
		}
		unreachable();
		return null;
	}

	/* get the value of the tag */
	static string getValue(string line) {
		size_t a = 1, b = line.length - 1;
		foreach (i; 1 .. line.length) {
			if (line[i] == '"') {
				a = i + 1;
				break;
			}
		}

		foreach (i; a.. line.length) {
			if (line[i] == '"') {
				b = i;
				break;
			}
		}

		return line[a .. b];
	}

public:
	string name, value;

	/* try to read a tag */
	bool read(ref Reader r) {
		string line;

		line = r.read();
		if (line is null || line[0] != '[') {
			r.unread();
			return false;
		}

		name = getName(line);
		value = getValue(line);

		return true;
	}

	/* write a tag */
	void write(std.stdio.File file = stdout) shared {
		file.writeln("[", name, " \"", value, "\"]");
	}
}

/*
 * Game
 */
shared class Game {
	struct Info {
		float time = 0.0;
		short score = 0;
		ubyte depth = 0;
		bool book = false;
	}
private:
	/* skip spaces */
	string skipSpaces(ref string text) {
		size_t i;

		while (text[i].isSpace()) ++i;

		return text[i .. $];
	}

	/* skip annotation */
	string skipAnnotation(ref string text) {
		size_t i;

		text = skipSpaces(text);
		if (text[i] == '$') while (!text[i].isSpace()) ++i;

		return text[i .. $];
	}

	/* skip nested ariation */
	string skipVariation(ref string text) {
		size_t i;

		text = skipSpaces(text);
		if (text[i] == '(') {
			++i;
			for (auto counter = 1; counter > 0 && i < text.length; ++i) {
				counter += (text[i] == '(') - (text[i] == ')');
			}
			if (text[i] == ')') ++i;
		}

		return text[i .. $];
	}

	/* skip comment */
	string skipComment(ref string text) {
		size_t i;

		text = skipSpaces(text);
		if (text[i] == '{') {
			while (++i < text.length && text[i] != '}') {}
			if (text[i] == '}') ++i;
		}

		return text[i .. $];
	}

	/* read thinking comment of the form {score/depth time} */
	Info readInfo(ref string text) {
		size_t b;
		Info i = {0.0, 0, 0, false};
		int matein, depth;
		double score, time;
		string s;


		text = text.strip();
		if (text[b] == '{') {
			while (++b < text.length && text[b] != '}') {}
			if (text[b] == '}') ++b;

			s = text[1 .. b].strip();
			text = text[b .. $];

			if (s == "book") i.book = true;
			else {
				if (formattedRead(s, "%s/%s %s",&score, &depth, &time) == 3) {
				} else if (s[1] == 'M') {
					s = s[2 .. $];
					if (formattedRead(s, "%s/%s %s",&matein, &depth, &time) == 3) {
						if (s[0] == '-') matein = -matein;
					} else return i;
				}
				if (matein > 0) i.score = cast (short) (Score.mate - matein);
				else if (matein < 0) i.score = cast (short) (matein - Score.mate);
				else i.score = cast (short) (100 * score);
				i.depth = cast (ubyte) max(0, min(255, depth));
				i.time = cast (float) time;
			}
		}

		return i;
	}

	/* read a move */
	Move readMove(ref string text, Board board) {
		size_t a, b;
		Move m;

		text = skipSpaces(text);
		if (text[a].isNumber()) {
			while (a + 1 < text.length && text[++a] != '.') {}
			while (text[a] == '.') ++a;
		}
		while (text[a].isSpace()) ++a;
		for (b = a; b < text.length && !text[b].isSpace; ++b) {}
		try {
			m = fromSan(text[a .. b], board);
		} catch (Exception e) {
			stderr.writeln(e);
			stderr.writeln("text: ", text);
			stderr.writeln("move: ", text[a .. b]);
			stderr.writeln("rest: ", text[b .. $]);
			throw e;
		}
		text = text[b .. $];

		return m;
	}

	/* get the ply number */
	int ply(const Board b) {
		return 1 + (b.ply + b.plyOffset) / 2;
	}

public:
	Tag [] tags;
	Move [] moves;
	Info [] infos;
	Result result;

	/* default constructor */
	this() {
		clear();
	}

	/* copy constructor */
	this(const shared Game g, bool isOpening = false) {
		clear();
		tags = g.tags.dup;
		moves = g.moves.dup;
		infos = g.infos.dup;
		if (isOpening) foreach(i; infos) i.book = true;
	}

	/* copy constructor */
	this(ref Reader reader) {
		this.read(reader);
	}

	/* reset the number of moves to 0 */
	void clear() {
		tags.length = 0;
		moves.length = 0;
		infos.length = 0;
		result = Result.none;
	}

	/* append a move */
	void push(const Move m, const Info i = Info.init) {
		moves ~= m;
		infos ~= i;
	}

	/* append a tag */
	void push(string name, string value) {
		Tag t;
		t.name = name; t.value = value;
		tags ~= t;
	}

	void random(Board b, ref Random r, const int depth) {
		Moves randomMoves = void;
		Move m;
		Info i = {book: true};

		b.set();
		foreach (d; 0 .. depth) {
			randomMoves.generate(b);
			if (randomMoves.length == 0) break;
			m = randomMoves[uniform(0, randomMoves.length, r)];
			push(m, i);
			b.update(m);
		}
	}

	/* read a game from a simple PGN (no #annotation field) */
	void read(ref Reader reader) {
		string line, text;
		Move m;
		Info i;
		Board board = new Board;
		Result r;
		Tag tag;

		board.set();
		clear();

		// read tags
		while (tag.read(reader)) {
			tags ~= tag;
			if (tag.name == "FEN") board.set(tag.value);
			else if (tag.name == "Result") toResult(tag.value, result);
		}

		// read the sequence of moves
		while (true) {
			line = reader.read();
			if (line is null) break;
			else if (line[0] == '[') {
				reader.unread();
				break;
			} else text ~= line.chomp() ~ " ";
		}

		// interpret it
		while (text.length > 0) {
			text = skipComment(text);
			text = skipVariation(text);
			text = skipAnnotation(text);
			if (toResult(text, r)) { // check result
				if (r != result) throw new Exception("Inconsistant result ?");
				return;
			} else { // read a move ?
				m = readMove(text, board);
				i = readInfo(text);
				push(m, i);
				board.update(m);
			}
		}
	}

	/* write a game */
	void write(std.stdio.File f = stdout) {
		Board board = new Board;
		size_t a, b;
		string text;
		bool hasResult;

		// tags
		foreach (t; tags) {
			t.write(f);
			if (t.name == "FEN") board.set(t.value);
			else if (t.name == "Result" && t.value == result.fromResult!false()) hasResult = true;
		}

		// game
		if (board.player == Color.black) text = format("%d... ", ply(board));
		foreach (i ; 0 .. moves.length) {
			if (board.player == Color.white) text ~= format("%d. ", ply(board));
			text ~= toSan(moves[i], board) ~ ' ';
			if (i < infos.length) {
				if (infos[i].book) text ~= "{book} ";
				else if (infos[i].score > Score.high) text ~= format("{+M%d/%s %.3f} ", Score.mate - infos[i].score, infos[i].depth, infos[i].time);
				else if (infos[i].score < Score.low) text ~= format("{-M%d/%s %.3f} ", Score.mate + infos[i].score, infos[i].depth, infos[i].time);
				else if (infos[i].depth > 0) text ~= format("{%.2f/%s %.3f} ", 0.01 * infos[i].score, infos[i].depth, infos[i].time);
			}
			board.update(moves[i]);
		}
		if (!hasResult) {
			result = board.isGameOver;
			f.writeln("[Result \"", result.fromResult!false(), "\"]");
		}
		text ~= result.fromResult!true() ~ " ";

		foreach (i; 0 .. text.length) {
			if (text[i].isSpace()) {
				if (i >= a + 80) {
					if (b == a) b = i;
					f.writeln(text[a .. b]);
					a = b + 1;
				} else b = i;
			}
		}
		if (a < text.length) f.writeln(text[a .. $]);
		f.flush();
	}
}

/*
 * GameBase a collection of Game.
 */
shared class GameBase {
	Game [] games;
	size_t index;
	class Lock {}
	Lock lock;

	this() {
		lock = new shared Lock;
	}

	size_t length() const @property {
		return games.length;
	}

	void opOpAssign(string op)(shared Game game) {
		if (op == "~") games ~= game;
		else assert(0);
	}

	/* read all games (not thread safe) */
	void read(bool pgn = true)(string fileName, const int minimalLength = 0) {
		Reader r;
		shared Game g;
		ulong n;
		string line;
		static if (!pgn) {
			string fen;
			Board b = new Board;
		}
		Chrono t;

		t.start();
		r.open(fileName);
		while (r.isOK) {
			if ((line = r.read()) == "") continue;
			try {
				g = new shared Game;
				static if (pgn) {
					r.unread();
					g.read(r);
				} else {
					b.set(line);
					fen = b.toFen();
					g.push("FEN", fen);
					g.push("SetUp", "1");
				}
				n += g.moves.length;
			} catch (Exception e) {
				static if (pgn) {
					stderr.writeln("bad pgn:", e);
					stderr.writeln("moves: ", g.moves);
				} else {
					stderr.writeln("bad fen:", e);
				}
				break;
			}
			if (g.moves.length >= minimalLength) games ~= g;
		}
		writeln("read ", games.length, " games & ", n, " moves in ", t.time(), " s."); stdout.flush();
	}


	/* write all games (not thread safe) */
	void write(string fileName) {
		std.stdio.File f;

		f.open(fileName);
		foreach (g; games) {
			g.write(f);
			f.writeln();
		}
	}

	/* reset index to 0 */
	void clear() {
		index = 0;
	}

	/* get next game */
	shared(Game) next(const bool loop = false) {
		synchronized (lock) {
			if (index < games.length) atomicOp!"+="(index, 1);
			else {
				if (loop) index = 1; else return null;
			}
			return games[index - 1];
		}
	}
}

