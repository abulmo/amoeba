/*
 * File game.d
 * PGN game reader / writer
 * © 2016-2017 Richard Delorme
 */

import board, move, util;
import std.stdio, std.string, std.algorithm, std.uni;
import core.atomic;

/*
 * A buffered file reader
 * TODO: RAII ?
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
}
	

/*
 * PGN tag
 */
struct Tag {
private:
	/* get the name of the tag  */
	static string getName(in string line) {
		foreach (i; 1 .. line.length) {
			if (isSpace(line[i])) return line[1 .. i];
		}
		claim(0);
		return null;
	}

	/* get the value of the tag */
	static string getValue(in string line) {
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
		float time;
		short score;
		ubyte depth;
		bool book;
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

	/* skip comment */
	string readComment(ref string text) {
		size_t i;

		text = skipSpaces(text);
		if (text[i] == '{') {
			while (++i < text.length && text[i] != '}') {}
			if (text[i] == '}') ++i;
		}

		return text[i .. $];
	}

	/* read a move */
	Move getMove(ref string text, Board board) {
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
	int ply(in Board b) {
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
	this(in shared Game g) {
		clear();
		foreach(m; g.moves[0..$]) push(m);
	}

	/* copy constructor */
	this(ref Reader reader) {
		this.read(reader);
	}

	/* reset the number of moves to 0 */
	void clear() {
		tags.length = 0;
		moves.length = 0;
		result = Result.none;
	}

	/* append a move */
	void push(in Move m) {
		moves ~= m;
	}	
	
	/* append a tag */
	void push(in string name, in string value) {
		Tag t;
		t.name = name; t.value = value;
		tags ~= t;
	}	

	/* read a game from a simple PGN (no #annotation field) */
	void read(ref Reader reader) {
		string line, text;
		string word; 
		Move m;
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
				m = getMove(text, board);
				push(m);
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
		foreach (m; moves) {
			if (board.player == Color.white) text ~= format("%d. ", ply(board));
			text ~= toSan(m, board) ~ ' ';
			board.update(m);
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

	/* read all games (not thread safe) */
	void read (in string fileName, in int minimalLength = 0) {
		Reader r;
		shared Game g;
		ulong n;

		r.open(fileName);
		do {
			try {
				g = new shared Game(r);
				n += g.moves.length;
			} catch (Exception e) {
				stderr.writeln("bad pgn:", e);
				stderr.writeln("moves: ", g.moves);
				g.moves.length = 0;
			}
			if (g.moves.length > minimalLength) games ~= g;
		} while (g.moves.length > 0);
		writeln("read ", games.length, " games & ", n, " positions"); stdout.flush();
		games ~= g;
	}

	/* write all games */
	void write(in string fileName) {
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
	ref shared(Game) next(in bool loop = false) {
		synchronized (lock) {
			if (index < games.length) atomicOp!"+="(index, 1);
			else if (loop) index = 1;
			return games[index - 1];
		}
	}
}

