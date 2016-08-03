/*
 * File game.d
 * PGN game reader
 * © 2016 Richard Delorme
 */

import board, move, util;
import std.stdio, std.string, std.random, std.getopt, std.algorithm;

/* 
 * Game
 */
class Game {
	string [Color.size] name;
	Result result;
	Move [] moves;
	string line;

	/* default constructor */
	this() {
		clear();
	}

	/* copy constructor */
	this(in Game g) {
		clear();
		foreach(m; g.moves[0..$]) push(m);
	}

	/* copy constructor */
	this(std.stdio.File f) {
		read(f);
	}

	/* reset the number of moves to 0 */
	void clear() {
		name[Color.white] = "";
		name[Color.black] = "";
		moves.length = 0;
		result = Result.none;
	}

	/* append a move */
	void push(in Move m) {
		moves ~= m;
	}	
	
	/* read a line */
	private bool readLine(std.stdio.File f) {
		do {
			line = f.readln();
		} while (line != null && (line[0] == '%' || line[0] == ';'));
		return line != null;
	}		

	/* game result */
	bool getResult(in string s, out Result r) const {
		if (s == "1-0") r = Result.whiteWin;
		else if (s == "0-1") r = Result.blackWin;
		else if (s == "1/2-1/2") r = Result.draw;
		else if (s == "*") r = Result.none;
		else return false;
		return true;
	}

		
	/* read a game from a simple PGN (no FEN field) */
	void read(std.stdio.File f) {
		string text;
		string [] words;
		size_t n;
		Move m;
		Board b = new Board;
		Result r;

		b.set();
		clear();

		// read tags
		do {
			if (line != null && line.length > 10 && line[0] == '[') {
				if (line[1..7] == "White ") name[Color.white] = line[7..$].findBetween("\"", "\"");
				if (line[1..7] == "Black ") name[Color.black] = line[7..$].findBetween("\"", "\"");
				if (line[1..8] == "Result ") {
					string s = line[8..$].findBetween("\"", "\"");
					getResult(s, result);
				}
			}
		} while (readLine(f) && line[0] == '[');

		// read the sequence of moves
		do {
			text ~= line.chomp() ~ " ";
		} while (readLine(f) && line[0] != '[');

		// remove {comments} & (variations)
		do {
			n = text.length;
			text = text.removeBetween("{", "}");
			text = text.removeBetween("(", ")");
		} while (n > text.length);

		// read legal moves
		words = text.split();
		foreach(w; words) {
			w.findSkip("."); // remove move number
			w.strip();	     // remove spaces ?
			try {
				if (w == "") {   // skip empty word
				} else if (getResult(w, r)) { // check result
					if (r != result) throw new Error("Inconsistant result");
					break;
				} else { // read & append moves
					m = fromSan(w, b);
					push(m);
					b.update(m);
				}
			} catch(Error e) {
				stderr.writeln("BAD pgn:", e.msg);
				stderr.writeln("text: ", text);
				stderr.writeln("words ", words);
				stderr.writeln(b);
			}
		}
	}
}

