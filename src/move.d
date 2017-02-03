/*
 * File move.d
 * move, list of moves & sequence of moves.
 * © 2016-2017 Richard Delorme
 */

module move;

import board, util;
import std.stdio, std.ascii, std.format, std.string, std.algorithm;

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
Move fromSan(string s, Board b) {
	int r, f;
	Square from, to;
	Piece promotion = Piece.none;
	Moves moves = void;
	Piece p;
	int i;
	f = r = Square.none;

	bool hasChar(const int j, char c) { return j < s.length && s[j] == c; }
	bool hasAlpha(const int j) { return j < s.length && isAlpha(s[j]); }
	bool hasDigit(const int j) { return j < s.length && isDigit(s[j]); }

	if (s.length >= 5 && (s[0..5] == "O-O-O" || s[0..5] == "0-0-0")) {
		from = b.xKing[b.player];
		to = cast (Square) (from - 2);
		p = Piece.king;
	} else if (s.length >= 3 && (s[0..3] == "O-O" || s[0..3] == "0-0")) {
		from = b.xKing[b.player];
		to = cast (Square) (from + 2);
		p = Piece.king;
	} else {
		p = Piece.pawn;
		if (isUpper(s[i])) p = toPiece(s[i++]);
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

	moves.generate(b);
	foreach (m; moves) {
		if (p == toPiece(b[m.from]) && to == m.to && promotion == m.promotion
		&& (f == Square.none || file(m.from) == f) && (r == Square.none || rank(m.from) == r)) {
			return m;
		}
	}

	debug stderr.writeln(p, ":", to, ", ", r, ", ", f, ", ", promotion);
	debug stderr.writeln("moves: ", moves);
	throw new Exception("Bad SAN : '" ~ s ~ "'");
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
		s ~= format("%s", move.to);
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
	ushort [Square.size][CPiece.size] h;
	enum max = 32768;
	
	void update(const Board board, const Move m, const uint δ) {
		debug claim (board.isTactical(m) == false);

		if ((h[board[m.from]][m.to] += δ) > max) {
			foreach (p; CPiece.wpawn .. CPiece.size)
			foreach (x; Square.a1 .. Square.size) {
				h[p][x] /= 2;
			}
		}
	}

	void clear() {
		foreach (p; CPiece.wpawn .. CPiece.size)
		foreach (x; Square.a1 .. Square.size) {
			h[p][x] = 0;
		}
	}

	short value(const CPiece p, const Square to) const {
		return cast (short) (h[p][to] - max);
	}
}	

/*
 *
 */
struct MoveItem {
	Move move;
	int value;

	this(const Move m, const int v) {
		move = m;
		value = v;
	}
}

/*
 * Moves : an array of legal moves
 */

struct Moves {
public:
	MoveItem [Limits.movesMax] item;
	size_t index;
private:
	size_t n;
	enum Stage {ttMove1, ttMove2, captureGeneration, captureSelection, killer1, killer2, refutation, quietGeneration, evasionGeneration, moveSelection };
	bool captureOnly;
	Move [2] ttMove;
	Move [2] killer;
	Move refutation;
	const(History) *history;
	Stage stage;
	
	enum badSeeMalus = -History.max - vCapture[Piece.king];
	static immutable short [Piece.size] vPiece = [0, 1, 2, 3, 4, 5, 6];
	static immutable short [Piece.size] vPromotion = [0, 0, 48, 16, 32, 64, 0];
	static immutable short [Piece.size] vCapture = [0, 256, 512, 768, 1024, 1280, 1536];
	enum ttBonus = 10000;
	enum killerBonus = 10;
	enum doublon = int.min;

	/* select the best move according to its value */
	void selectValuableMove() {
		size_t k = index;
		foreach (i; index + 1 .. n) if (item[i].value > item[k].value) k = i;
		if (k > index) swap(item[index], item[k]);
	}
	
	/* insert a move & its value at the current index position */
	void insert(const Move m, const short v) {
		item[n] = MoveItem(m, v);
		swap(item[index], item[n]);
		n++;
	}

	/* generate & score captures using MVVLVA & punishing bad captures */
	void generateCapture(Board board) {
		size_t o = n;
		board.generateMoves!(Generate.capture)(this);
		foreach(ref i; item[o .. n]) {
			auto m = i.move;
			if (m == ttMove[0]) i.value = doublon;
			else if (m == ttMove[1]) i.value = doublon;
			else {
				auto p = toPiece(board[m.from]);
				auto victim = toPiece(board[m.to]);			
				i.value = vCapture[victim] + vPromotion[m.promotion] - vPiece[p];
				if (i.value == -vPiece[Piece.pawn]) i.value += vCapture[Piece.pawn]; // en passant
				if ((board.see(m) < 0 && board.giveCheck(m) < 2) || (m.promotion > Piece.pawn && m.promotion < Piece.queen)) {
					if (captureOnly) i.value = doublon;
					else i.value += badSeeMalus;
				}
			}
		}
		item[n] = MoveItem.init;
	}

	/* generate & score quiet moves */
	void generateQuiet(Board board) {
		size_t o = n;
		board.generateMoves!(Generate.quiet)(this);
		foreach (ref i; item[o .. n]) {
			if (i.move == ttMove[0]) i.value = doublon;
			else if (i.move == ttMove[1]) i.value = doublon;
			else if (i.move == killer[0]) i.value = doublon;
			else if (i.move == killer[1]) i.value = doublon;
			else if (i.move == refutation) i.value = doublon;
			else {
				i.value = history.value(board[i.move.from], i.move.to);
			}
		}
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
					i.value = vCapture[victim] + vPromotion[i.move.promotion] - vPiece[p];
					if (board.see(i.move) < 0 && board.giveCheck(i.move) < 2) i.value += badSeeMalus;
				} else {
					i.value = (p == Piece.king) ? 1 : -vPiece[p];
				}
			}
		}
		item[n] = MoveItem.init;
	}


public:

	/* reset to initial state to loop over the moves again */
	void reset() {
		index = 0;
	}

	/* init (from main search) */
	void init(const bool inCheck, const ref Move [2] ttm, const ref Move[Color.size] k, const ref Move r, const ref History h) {
		ttMove = ttm;
		killer = k;
		refutation = r;
		history = &h;
		stage = inCheck ? Stage.evasionGeneration : Stage.ttMove1;
		captureOnly = false;
		index = n = 0;
	}
	
	/* init (from quiescence search) */
	void init(const bool inCheck, const ref Move [2] ttm) {
		ttMove = ttm;
		killer[] = 0;
		refutation = 0;
		history = null;
		stage = inCheck ? Stage.evasionGeneration : Stage.ttMove1;
		captureOnly = true;
		index = n = 0;
	}

	/* staged - move generation (aka spaghetti code) */
	ref MoveItem selectMove(Board board) {
		const Stage oldStage = stage;

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
			selectValuableMove();
			if (index == n || item[index].value < 0) { // end of good capture
				stage = Stage.killer1;
				goto case;
			} else break;

		// killer 1
		case Stage.killer1:
			stage = Stage.killer2;
			if (board.isLegal(killer[0]) && !board.isTactical(killer[0]) && killer[0] != ttMove[0] && killer[0] != ttMove[1]) {
				insert(killer[0], killerBonus);
				break;
			} else goto case;

		// killer 2
		case Stage.killer2:
			stage = Stage.refutation;
			if (board.isLegal(killer[1]) && !board.isTactical(killer[1]) && killer[1] != ttMove[0] && killer[1] != ttMove[1]) {
				insert(killer[1], killerBonus - 1);
				break;
			} else goto case;

		// refutation
		case Stage.refutation:
			stage = Stage.quietGeneration;
			if (board.isLegal(refutation) && !board.isTactical(refutation) && refutation != ttMove[0] && refutation != ttMove[1] && refutation != killer[0] && refutation != killer[1]) {
				insert(refutation, killerBonus - 2);
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
			selectValuableMove();
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
			auto tmp = item[j];
			foreach_reverse (k; i .. j) item[k + 1] = item[k];
			item[i] = tmp;
		}
	}

	/* remove all moves */
	void clear() {
		index = n = 0;
	}

	/* get front move */
	ref const(Move) front() const {
		return item[index].move;
	}

	/* pop first move */
	void popFront() {
		++index;
	}

	/* empty */
	bool empty() @property const {
		return index == n;
	}

	/* append a move built from origin & destination squares */
	ref Moves push(const Square from, const Square to) {
		item[n++].move = (from | to << 6);
		return this;
	}

	/* append a move */
	void push(const Move m, const short v = 0) {
		item[n++] = MoveItem(m, v);
	}

	/* append promotions from origin & destination squares */
	ref Moves pushPromotions(const Square from, const Square to) {
		item[n++].move = (from | to << 6 | Piece.queen << 12);
		item[n++].move = (from | to << 6 | Piece.knight << 12);
		item[n++].move = (from | to << 6 | Piece.rook << 12);
		item[n++].move = (from | to << 6 | Piece.bishop << 12);
		return this;
	}

	/* generate all moves */
	ref Moves generate(Board board) {
		index = n = 0;
		if (board.inCheck) board.generateEvasions(this);
		else board.generateMoves(this);
		return this;
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
	void dump() const {
		foreach (ref i; item[0 .. n]) write(i.move.toPan(), " [", i.value, "], ");
		writeln();
		writeln("stage = ", stage, " index = ", index, " n = ", n);
		writeln("ttMove = ", ttMove[0].toPan, ", ", ttMove[1].toPan);
		writeln("killer = ", killer[0].toPan, ", ", killer[1].toPan, " ; refutation = ", refutation.toPan);

	}

	/* is the first move ? */
	bool isFirst(const Move m) const {
		return m == item[0].move;
	}

	/* opIndex */
	ref const(Move) opIndex(const size_t i) const {
		 return item[i].move;
	}	
}


/*
 * struct Line: a sequence of moves
 */
struct Line {
	enum plyMax = 100;
	Move [plyMax] move;
	int n;

	/* length of the sequence */
	size_t length() const @property {
		return n;
	}

	/* clear */
	ref Line clear() {
		n = 0;
		return this;
	}

	/* append a move */
	ref Line push(const Move m) {
		debug claim(n < plyMax);
		move[n++] = m;
		return this;
	}

	/* remove the last pushed move & return it */
	ref Line pop() {
		debug claim(n > 0);
		--n;
		return this;
	}

	/* append another line */
	ref Line push(const ref Line l) {
		foreach (m; l.move[0 .. l.n]) push(m);
		return this;
	}

	/* set */
	ref Line set(const Move m, const ref Line l) {
		return clear().push(m).push(l);
	}

	/* set */
	ref Line set(const ref Line line) {
		return clear().push(line);
	}

	/* Convert it to a string */
	string toString() const {
		string s;
		foreach (m; move[0 .. n]) s ~= m.toPan() ~ " ";
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

