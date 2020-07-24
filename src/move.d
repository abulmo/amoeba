/*
 * File move.d
 * move, list of moves & sequence of moves.
 * © 2016-2020 Richard Delorme
 */

module move;

import board, util;
import std.stdio, std.array, std.ascii, std.format,  std.math, std.string, std.algorithm;

/*
 * Move
 */
alias Move = ushort;

/* from square */
Square from(const Move move) @property {
	return cast (Square) (move & 63);
}

/* to square */
Square to(const Move move) @property {
	return cast (Square) ((move >> 6) & 63);
}

/* to Move */
Move toMove(const int from, const int to, const int promotion = Piece.none) {
	return cast (Move) (promotion << 12 | to << 6 | from);
}

/* Promoted piece (if any) */
Piece promotion(const Move move) @property {
	return cast (Piece) ((move >> 12) & 7);
}

/* convert a move into a string using Pure Algebraic Coordinate Notation (PAN) */
string toPan(const Move move) {
	if (move.promotion) return format("%s%s%c", move.from, move.to, toChar(move.promotion));
	else if (move) return format("%s%s", move.from, move.to);
	else return "0000";
}

/* convert a string (encoded using PAN) into a move */
Move fromPan(string s) {
	if (s.length < 4) return 0;
	Piece promotion;
	Square from = toSquare(s[0..2]);
	Square to = toSquare(s[2..4]);
	if (s.length > 4) promotion = toPiece(s[4]);
	return cast (Move) (from | to << 6 | promotion << 12);
}

/* convert a string using standard algebraic notation (SAN) into a move */
Move fromSan(string s, const Board b) {
	int i, r = -1, f = -1;
	Square from, to;
	Piece promotion = Piece.none;
	Move move;
	Piece p;
	bool isCapture;

	bool hasString(string t) {return  s.length >= t.length && s[0 .. t.length] == t; }
	bool hasChar(const int j, char c) { return j < s.length && s[j] == c; }
	bool hasAlpha(const int j) { return j < s.length && isAlpha(s[j]); }
	bool hasDigit(const int j) { return j < s.length && isDigit(s[j]); }

	if (hasString("O-O-O") || hasString("0-0-0")) {
		from = b.xKing[b.player];
		to = cast (Square) (from - 2);
		move = toMove(from, to);
	} else if (hasString("O-O") || hasString("0-0")) {
		from = b.xKing[b.player];
		to = cast (Square) (from + 2);
		move = toMove(from, to);
	} else {
		if (isUpper(s[i])) p = toPiece(s[i++]);
		else p = Piece.pawn;
		isCapture = true;
		if (hasChar(i, 'x')) ++i;
		else if (hasChar(i + 1, 'x')) {
			if (hasAlpha(i)) f = s[i] - 'a';
			else if (hasDigit(i)) r = s[i] - '1';
			i += 2;
		} else if (hasChar(i + 2, 'x')) {
			if (hasAlpha(i)) f = s[i] - 'a';
			if (hasDigit(i + 1)) r = s[i + 1] - '1';
			i += 3;
		} else {
			isCapture = false;
			if (hasAlpha(i + 1)) {
				if (hasAlpha(i)) f = s[i] - 'a';
				else if (hasDigit(i)) r = s[i] - '1';
				i += 1;
			} else if (hasAlpha(i + 2) && hasDigit(i + 3)) {
				if (hasAlpha(i)) f = s[i] - 'a';
				if (hasDigit(i + 1)) r = s[i + 1] - '1';
				i += 2;
			}
		}
		to = toSquare(s[i..i+2]); i += 2;
		if (hasChar(i, '=')) {
			promotion = toPiece(s[i + 1]);
			i += 2;
		}
	}

	if (move == 0) move = b.guess(p, to, f, r, promotion, isCapture);

	if (b.isLegal(move)) return move;

	move = fromPan(s);

	if (b.isLegal(move)) {
		stderr.writeln("warning: expected SAN instead of PAN '", s, "'");
		return move;
	} else {
		stderr.writeln("error: bad SAN '", s, "' -> ", toPan(move));
	}

	return 0;
}

/* convert a move to a string using Standard Algebraic Notation (SAN) */
string toSan(const Move move, Board board) {
	string f = "abcdefgh", r = "12345678", s;
	int nSameFile, nSameTo;
	Moves moves = void;
	const Piece p = toPiece(board[move.from]);

	if (move == 0) s = "@@@@";
	else if (p == Piece.king && move.to == move.from + 2) s = "O-O";
	else if (p == Piece.king && move.to == move.from - 2) s = "O-O-O";
	else {
		if (p != Piece.pawn) {
			moves.generate(board);
			foreach (m; moves) {
				if (move.to == m.to && p == toPiece(board[m.from]) && move.from != m.from) {
					++nSameTo;
					if (file(move.from) == file(m.from)) ++nSameFile;
				}
			}
			s ~= p.toChar();
		}
		if ((p == Piece.pawn && file(move.from) != file(move.to)) || nSameTo > nSameFile) s ~= f[file(move.from)];
		if (nSameFile) s ~= r[rank(move.from)];
		if ((p == Piece.pawn && file(move.from) != file(move.to)) || board[move.to]) s ~= 'x';
		s ~= f[file(move.to)]; s ~= r[rank(move.to)];
		if (move.promotion) s ~= "=" ~ toChar(move.promotion);
	}

	if (board.giveCheck(move)) {
		board.update(move);
			moves.generate(board);
			if (moves.length) s ~= '+'; else s ~= '#';
		board.restore(move);
	}
	return s;
}

/*
 * Search history
 */
struct History {
	ushort [Square.size][CPiece.size] goodMoves, badMoves;
	enum int max = 16_384;

	/* rescale history */
	void rescale(const int d = 2) {
		foreach (p; CPiece.wpawn .. CPiece.size)
		foreach (x; Square.a1 .. Square.size) {
			goodMoves[p][x] /= d;
			badMoves[p][x] /= d;
		}
	}

	/* update history for a good move */
	void updateGood(const Board board, const Move m, const uint δ) {
		if ((goodMoves[board[m.from]][m.to] += δ) > max) rescale();
	}

	/* update history for bad moves */
	void updateBad(const Board board, const Move m, const uint δ) {
		if ((badMoves[board[m.from]][m.to] += δ) > max) rescale();
	}

	/* clear the history tables */
	void clear() {
		foreach (p; CPiece.wpawn .. CPiece.size)
		foreach (x; Square.a1 .. Square.size) {
			goodMoves[p][x] = badMoves[p][x] = 0;
		}
	}

	/* return the history score */
	short value(const CPiece p, const Square to) const {
		const int g = goodMoves[p][to], b = badMoves[p][to];
		if (g + b == 0) return cast (short) -max / 2;
		else return cast (short) ((g * max) / (g + b) - max);
	}

	/* return the history score */
	bool isGood(const CPiece p, const Square to) const {
		return goodMoves[p][to] > badMoves[p][to];
	}
}

/*
 * MoveItem: a Move and a sorting value
 */
struct MoveItem {
	Move move;
	short value;

	/* Constructor */
	this(const Move m, const short v) {
		move = m;
		value = v;
	}

	/* toString */
	string toString(Board b) {
		return "(" ~ (b ? move.toSan(b) : move.toPan()) ~ ", " ~ format("%d", value) ~ ")";
	 }
}

/* insertion Sort */
void insertionSort(MoveItem [] items) {
	const size_t n = items.length;
	foreach (i; 1 .. n) {
		size_t j;
		const tmp = items[i];
		for (j = i ; j > 0 && tmp.value > items[j - 1].value; j--) {
			items[j] = items[j - 1];
		}
		items[j] = tmp;
	}
}


/*
 * Moves : an array of legal moves
 */
struct Moves {
public:
	MoveItem [Limits.moves.max] item;
	size_t index;
private:
	enum Stage {ttMove1, ttMove2, captureGeneration, captureSelection, killer1, killer2, refutation, quietGeneration, evasionGeneration, moveSelection }
	size_t n;
	bool captureOnly;
	Move [2] ttMove;
	Move [2] killer;
	Move refutation;
	const(History) *history;
	Stage stage;
	static immutable short [Piece.size] vPiece = [0, 1, 2, 3, 4, 5, 6];
	static immutable short [Piece.size] vPromotion = [0, 0, 48, 16, 32, 64, 0];
	static immutable short [Piece.size] vCapture = [0, 256, 512, 768, 1024, 1280, 1536];
	static immutable short [Square.size] center = allSquares.map!(cdist).array;
	enum short badSeeMalus = -History.max - vCapture[Piece.king];
	enum short ttBonus = 10_000;
	enum short killerBonus = 10;
	enum short to7thRankBonus = 12;
	enum short doublon = short.min;

	static int dist(const Square x, const Square y) { return abs(rank(x) - rank(y)) + abs(file(x) - file(y)); }
	static short cdist(const Square x) { with(Square) return cast (short) min(dist(a1, x), dist(a8, x), dist(h1, x), dist(h8, x)); }

	/* insert item j into i */
	void insert(const size_t i, const size_t j) {
		if (i < j) {
			const MoveItem tmp = item[j];
			foreach_reverse (k; i .. j) item[k + 1] = item[k];
			item[i] = tmp;
		}
	}

	/* include a move & its value at the current index position */
	void include(const Move m, const short v) {
		item[n] = MoveItem(m, v);
		insert(index, n);
		n++;
	}

	/* generate & score captures using MVVLVA & punishing bad captures */
	void generateCapture(Board board) {
		const size_t o = n;
		board.generateMoves!(Generate.capture)(this);
		foreach (ref i; item[o .. n]) {
			auto m = i.move;
			if (m == ttMove[0]) i.value = doublon;
			else if (m == ttMove[1]) i.value = doublon;
			else {
				auto p = toPiece(board[m.from]);
				auto victim = toPiece(board[m.to]);
				i.value = cast (short) (vCapture[victim] + vPromotion[m.promotion] - vPiece[p] + center[m.to]);
				if (i.value == -vPiece[Piece.pawn]) {
					if (board.isEnpassant(m)) i.value += vCapture[Piece.pawn]; // en passant
					else i.value = to7thRankBonus; // push to 7
				}
			}
		}
		insertionSort(item[o .. n]);
		item[n] = MoveItem.init;
	}

	/* generate & score quiet moves */
	void generateQuiet(Board board) {
		const size_t o = n;
		board.generateMoves!(Generate.quiet)(this);
		foreach (ref i; item[o .. n]) {
			if (i.move == ttMove[0]) i.value = doublon;
			else if (i.move == ttMove[1]) i.value = doublon;
			else if (i.move == killer[0]) i.value = doublon;
			else if (i.move == killer[1]) i.value = doublon;
			else if (i.move == refutation) i.value = doublon;
			else i.value = cast (short) (history.value(board[i.move.from], i.move.to) + center[i.move.to]);
		}
		insertionSort(item[index .. n]);
		item[n] = MoveItem.init;
	}

	/* generate & score check evading moves */
	void generateEvasions(Board board) {
		board.generateEvasions(this);
		foreach (ref i; item[0 .. n]) {
			if (i.move == ttMove[0]) i.value = ttBonus;
			else if (i.move == ttMove[1]) i.value = ttBonus - 1;
			else {
				auto p = toPiece(board[i.move.from]);
				auto victim = toPiece(board[i.move.to]);
				if (victim || i.move.promotion) {
					i.value = cast (short) (vCapture[victim] + vPromotion[i.move.promotion] - vPiece[p]);
					if (board.see(i.move) < 0 && board.giveCheck(i.move) < 2) i.value += badSeeMalus;
				} else {
					i.value = (p == Piece.king) ? 1 : -vPiece[p];
				}
			}
		}
		insertionSort(item[0 .. n]);
		item[n] = MoveItem.init;
	}


public:

	/* reset to initial state to loop over the moves again */
	void reset() {
		index = 0;
	}

	/* init (from main search) */
	void setup(const bool inCheck, const ref Move [2] ttm, const ref Move[Color.size] k, const ref Move r, const ref History h) {
		ttMove = ttm;
		killer = k;
		refutation = r;
		history = &h;
		stage = inCheck ? Stage.evasionGeneration : Stage.ttMove1;
		captureOnly = false;
		index = n = 0;
	}

	/* init (from quiescence search) */
	void setup(const bool inCheck, const ref Move [2] ttm) {
		ttMove = ttm;
		stage = inCheck ? Stage.evasionGeneration : Stage.ttMove1;
		captureOnly = true;
		index = n = 0;
	}

	/* staged - move generation (aka spaghetti code) */
	ref MoveItem selectMove(Board board) return {
		final switch (stage) {
		// best move from transposition table
		case Stage.ttMove1:
			stage = Stage.ttMove2;
			if (board.isLegal(ttMove[0])) {
				push(ttMove[0], ttBonus);
				break;
			} else goto case;

		// second best move from transposition table
		case Stage.ttMove2:
			stage = Stage.captureGeneration;
			if (board.isLegal(ttMove[1])) {
				push(ttMove[1], ttBonus - 1);
				break;
			} else goto case;
		// capture & promotion generation & scoring
		case Stage.captureGeneration:
			stage = captureOnly ? Stage.moveSelection :  Stage.captureSelection;
			generateCapture(board);
			if (captureOnly) goto case Stage.moveSelection; else goto case;

		// good-capture selection from best to worst
		case Stage.captureSelection:
			if (index == n || item[index].value < 0) { // end of good capture
				stage = Stage.killer1;
				goto case;
			} else {
				foreach (i; index .. n) {
					if (board.see(item[i].move) >= 0 || board.giveCheck(item[i].move) == 2) {
						insert(index, i);
						break;
					} else if (item[i].value >= 0) item[i].value += badSeeMalus;
				}
				if (item[index].value < 0) { // end of good capture
					stage = Stage.killer1;
					goto case;
				} else 	break;
			}
		// killer 1
		case Stage.killer1:
			stage = Stage.killer2;
			if (board.isLegal(killer[0]) && !board.isTactical(killer[0]) && killer[0] != ttMove[0] && killer[0] != ttMove[1]) {
				include(killer[0], killerBonus);
				break;
			} else goto case;

		// killer 2
		case Stage.killer2:
			stage = Stage.refutation;
			if (board.isLegal(killer[1]) && !board.isTactical(killer[1]) && killer[1] != ttMove[0] && killer[1] != ttMove[1]) {
				include(killer[1], killerBonus - 1);
				break;
			} else goto case;

		// refutation
		case Stage.refutation:
			stage = Stage.quietGeneration;
			if (board.isLegal(refutation) && !board.isTactical(refutation) && refutation != ttMove[0] && refutation != ttMove[1] && refutation != killer[0] && refutation != killer[1]) {
				include(refutation, killerBonus - 2);
				break;
			} else goto case;

		// quiet move generation
		case Stage.quietGeneration:
			stage = Stage.moveSelection;
			generateQuiet(board);
			goto case Stage.moveSelection;

		// check evading move generation
		case Stage.evasionGeneration:
			stage = Stage.moveSelection;
			generateEvasions(board);
			goto case;

		// move selection from best to worst
		case Stage.moveSelection:
			if (item[index].value == doublon) { // already done, stop here.
				item[index] = MoveItem.init;
				n = index;
			}
			break;
		}

		return item[index++];
	}

	/* length of the array */
	size_t length() const @property {
		return n;
	}

	/* insert a move as ith move */
	void setBest(const Move m, const size_t i = 0) {
		foreach (j; 0 .. n) if (m == item[j].move) {
			insert(i, j);
			break;
		}
	}

	/* remove all moves */
	void clear() {
		index = n = 0;
	}

	/* get front move */
	Move front() const {
		return item[index].move;
	}

	/* popFront */
	void popFront() {
		++index;
	}

	/* empty */
	bool empty() @property const {
		return index == n;
	}

	/* append a move built from origin & destination squares */
	void push(const Square from, const Square to) {
		item[n++].move = toMove(from, to);
	}

	/* append a move */
	void push(const Move m, const short v = 0) {
		item[n++] = MoveItem(m, v);
	}

	/* append promotions from origin & destination squares */
	void pushPromotions(const Square from, const Square to) {
		item[n++].move = toMove(from, to, Piece.queen);
		item[n++].move = toMove(from, to, Piece.knight);
		item[n++].move = toMove(from, to, Piece.rook);
		item[n++].move = toMove(from, to, Piece.bishop);
	}

	/* generate all moves */
	void generate(const Board board) {
		index = n = 0;
		if (board.inCheck) board.generateEvasions(this);
		else board.generateMoves(this);
	}

	/* convert to string */
	string toString() const {
		string s;
		foreach (ref i; item[0..n]) s ~= i.move.toPan() ~ " ";
		return s;
	}

	/* convert to string using SAN */
	string toSan(Board board) const {
		string s;
		foreach (ref i; item[0..n]) s ~= i.move.toSan(board) ~ " ";
		return s;
	}

	/* dump */
	void dump(std.stdio.File f = stdout) const {
		foreach (ref i; item[0 .. n]) f.write(i.move.toPan(), " [", i.value, "], ");
		f.writeln();
		f.writeln("stage = ", stage, " index = ", index, " n = ", n);
		f.writeln("ttMove = ", ttMove[0].toPan, ", ", ttMove[1].toPan);
		f.writeln("killer = ", killer[0].toPan, ", ", killer[1].toPan, " ; refutation = ", refutation.toPan);
	}

	/* is the first move ? */
	bool isFirst(const Move m) const {
		return m == item[0].move;
	}

	/* opIndex */
	Move opIndex(const size_t i) const {
		 return item[i].move;
	}

	/* exclude a move */
	void exclude(Move m) {
		foreach (i; 0 .. n) if (m == item[i].move) {
			foreach (j; i + 1 .. n) item[j - 1] = item[j];
			--n;
			break;
		}
	}
}


/*
 * struct Line: a sequence of moves
 */
struct Line {
	Move [Limits.ply.max + 1] move;
	int n;

	/* length of the sequence */
	size_t length() const @property {
		return n;
	}

	/* clear */
	void clear() {
		n = 0;
	}

	/* append a move */
	void push(const Move m) {
		move[n++] = m;
	}

	/* remove the last pushed move & return it */
	void pop() {
		--n;
	}

	/* append another line */
	void push(const ref Line l) {
		foreach (m; l.move[0 .. l.n]) push(m);
	}

	/* set */
	void set(const Move m, const ref Line l) {
		clear(); push(m); push(l);
	}

	/* set */
	void set(const ref Line l) {
		clear(); push(l);
	}

	/* Convert it to a string */
	string toString() const {
		string s;
		foreach (m; move[0 .. n]) s ~= m.toPan() ~ " ";
		return s;
	}

	/* Convert it to a string */
	string toSan(Board board) const {
		string s;
		if (board.player == Color.black) s = format("%d... ", (board.ply + board.plyOffset) / 2 + 1);
		foreach (m; move[0 .. n]) {
			if (board.player == Color.white) s ~= format("%d.", (board.ply + board.plyOffset) / 2 + 1);
			s ~= m.toSan(board) ~ " ";
			board.update(m);
		}
		foreach_reverse (m; move[0 .. n]) board.restore(m);

		return s;
	}


	/* last pushed move */
	Move top() const @property {
		return n > 0 ? move[n - 1] : 0;
	}

	/* swap */
	void swap(ref Line line) {
		Line tmp = void;
		tmp.set(line);
		line.set(this);
		set(tmp);
	}
}

