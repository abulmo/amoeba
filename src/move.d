/*
 * File move.d
 * move, list of moves & sequence of moves.
 * © 2016 Richard Delorme
 */

module move;

import board, util;
import std.stdio, std.ascii, std.format, std.string, std.algorithm;

/*
 * Move
 */
alias Move = ushort;

/* from square */
Square from(in Move move) @property {
	return cast (Square) (move & 63);
}

/* to square */
Square to(in Move move) @property {
	return cast (Square) ((move >> 6) & 63);
}

/* Promoted piece (if any) */
Piece promotion(in Move move) @property {
	return cast (Piece) ((move >> 12) & 7);
}

/* convert a move into a string */
string toString(in Move move) {
	if (move.promotion) return format("%s%s%c", move.from, move.to, toChar(move.promotion));
	else if (move) return format("%s%s", move.from, move.to);
	else return "0000";
}

/* convert a string into a move */
Move toMove(string s) {
	if (s.length < 4) return 0;
	Piece promotion;
	Square from = toSquare(s[0..2]);
	Square to = toSquare(s[2..4]);
	if (s.length > 4) promotion = toPiece(s[4]);
	return cast (Move) (from | to << 6 | promotion << 12);
}

/* convert a string using standard algebraic notation (SAN) into a move */
Move fromSan(in string s, Board b) {
	int r, f;
	Square from, to;
	Piece promotion = Piece.none;
	Moves moves = void;
	Piece p;
	int i;
	f = r = Square.none;

	bool hasChar(in int j, char c) { return j < s.length && s[j] == c; }
	bool hasAlpha(in int j) { return j < s.length && isAlpha(s[j]); }
	bool hasDigit(in int j) { return j < s.length && isDigit(s[j]); }

	if (s.length >= 5 && (s[0..5] == "O-O-O" || s[0..5] == "0-0-0")) {
		from = firstSquare(b.color[b.player] & b.piece[Piece.king]);
		to = cast (Square) (from - 2);
		p = Piece.king;
	} else if (s.length >= 3 && (s[0..3] == "O-O" || s[0..3] == "0-0")) {
		from = firstSquare(b.color[b.player] & b.piece[Piece.king]);
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
	throw new Error("Bad SAN : '" ~ s ~ "'");
}

/* convert a move to a string using Standard Algebraic Notation (SAN) */
string toSan(in Move move, Board board) {
	string f = "abcdefgh", r = "12345678", s;
	int nSameFile, nSameTo;
	Moves moves = void;
	immutable Piece p = toPiece(board[move.from]);

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
 * Moves
 */

struct Moves {
public:
	Move[Limits.movesMax] move;
	int [Limits.movesMax] value;
	size_t index;
private:
	size_t n;
	enum Stage {ttMove1, ttMove2, captureGeneration, captureSelection, killer1, killer2, refutation, quietGeneration, evasionGeneration, moveSelection };
	bool captureOnly;
	Move [2] ttMove;
	Move [2] killer;
	Move refutation;
	const(ushort) *history;
	Stage stage;
	
	static immutable int badSeeMalus = -Limits.historyMax - vCapture[Piece.king];
	static immutable int [Piece.size] vPiece = [0, 1, 2, 3, 4, 5, 6];
	static immutable int [Piece.size] vPromotion = [0, 0, 48, 16, 32, 64, 0];
	static immutable int [Piece.size] vCapture = [0, 256, 512, 768, 1024, 1280, 1536];
	static immutable int ttBonus = 10000;
	static immutable int killerBonus = 10;
	static immutable int doublon = int.min;

	/* select the best move according to its value */
	void selectValuableMove() {
		size_t k = index;
		foreach (i; index + 1 .. n) if (value[i] > value[k]) k = i;
		if (k > index) exchange(index, k);
	}
	
	/* exchange two moves */
	void exchange(in size_t i, in size_t k) {
		swap(move[i], move[k]);
		swap(value[i], value[k]);
	}

	/* insert a move & its value at the current index position */
	void insert(in Move m, in int v) {
		move[n] = m;
		value[n] = v;
		exchange(index, n);
		n++;
	}

	/* generate & score captures using MVVLVA & punishing bad capture */
	void generateCapture(Board board) {
		board.generateMoves!(Generate.capture)(this);
		foreach(i; index .. n) {
			if (move[i] == ttMove[0]) value[i] = doublon;
			else if (move[i] == ttMove[1]) value[i] = doublon;
			else {
				auto p = toPiece(board[move[i].from]);
				auto victim = toPiece(board[move[i].to]);			
				value[i] = vCapture[victim] + vPromotion[move[i].promotion] - vPiece[p];
				if (value[i] == -vPiece[Piece.pawn]) value[i] += vCapture[Piece.pawn]; // en passant
				if ((board.see(move[i]) < 0 && board.giveCheck(move[i]) < 2) || (move[i].promotion > Piece.pawn && move[i].promotion < Piece.queen)) {
					if (captureOnly) value[i] = doublon;
					else value[i] += badSeeMalus;
				}
			}
		}
		move[n] = 0;
		value[n] = 0;
	}

	/* generate & score quiet moves */
	void generateQuiet(Board board) {
		size_t oldN = n;
		board.generateMoves!(Generate.quiet)(this);
		foreach (i; oldN .. n) {
			if (move[i] == ttMove[0]) value[i] = doublon;
			else if (move[i] == ttMove[1]) value[i] = doublon;
			else if (move[i] == killer[0]) value[i] = doublon;
			else if (move[i] == killer[1]) value[i] = doublon;
			else if (move[i] == refutation) value[i] = doublon;
			else {
				auto h = board[move[i].from] * Square.size + move[i].to;
				value[i] = history[h] - Limits.historyMax;
			}
		}
		move[n] = 0;
		value[n] = 0;
	}

	/* generate & score check evading moves */
	void generateEvasions(Board board) {
		board.generateEvasions(this);
		foreach (i; 0 .. n) {
			if (move[i] == ttMove[0]) value[i] = ttBonus;
			else if (move[i] == ttMove[1]) value[i] = ttBonus - 1;
			else {
				auto p = toPiece(board[move[i].from]);
				auto victim = toPiece(board[move[i].to]);			
				if (victim || move[i].promotion) {
					value[i] = vCapture[victim] + vPromotion[move[i].promotion] - vPiece[p];
					if (board.see(move[i]) < 0) value[i] += badSeeMalus;
				} else {
					value[i] = (p == Piece.king) ? 1 : -vPiece[p];
				}
			}
		}
		move[n] = 0;
	}


public:

	/* reset to initial state so that foreach loop can be called again */
	void reset() {
		index = 0;
	}

	/* init (from main search) */
	void init(in bool inCheck, const ref Move [2] ttm, const ref Move[Color.size] km, const ref Move r, const ref ushort [Square.size * CPiece.size] h) {
		ttMove = ttm;
		killer = km;
		refutation = r;
		history = &h[0];
		stage = inCheck ? Stage.evasionGeneration : Stage.ttMove1;
		captureOnly = false;
		index = n = 0;
	}
	
	/* init (from quiescence search) */
	void init(in bool inCheck, const ref Move [2] ttm) {
		ttMove = ttm;
		killer[] = 0;
		refutation = 0;
		history = null;
		stage = inCheck ? Stage.evasionGeneration : Stage.ttMove1;
		captureOnly = true;
		index = n = 0;
	}

	/* staged - move generation (aka spaghetti code) */
	ref Move selectMove(Board board) {
		immutable Stage oldStage = stage;

		final switch(stage) {
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
			if (index == n || value[index] < 0) { // end of good capture
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
			if (value[index] == doublon) { // already done, stop here.
				move[index] = 0;
				n = index;
			}
			break;
		}

		return move[index++];
	}

	/* length of the array */
	size_t length() const @property {
		return n;
	}

	/* insert a move as ith move */
	void setBest(in Move m, in size_t i = 0) {
		foreach (j; 0 .. n) if (m == move[j]) {
			foreach_reverse (k; i .. j) move[k + 1] = move[k];
			move[i] = m;
			auto v = value[j];
			foreach_reverse (k; i .. j) value[k + 1] = value[k];
			value[i] = v;
		}
	}

	/* remove all moves */
	void clear() {
		index = n = 0;
	}

	/* get front move */
	ref const(Move) front() {
		return move[index];
	}

	/* pop first move */
	void popFront() {
		++index;
	}

	/* empty */
	bool empty() @property {
		return index == n;
	}

	/* append a move built from origin & destination squares */
	ref Moves push(in Square from, in Square to) {
		move[n++] = (from | to << 6);
		return this;
	}

	/* append a move */
	void push(in Move m, in int v = 0) {
		move[n] = m;
		value[n] = v;
		++n;
	}

	/* append promotions from origin & destination squares */
	ref Moves pushPromotions(in Square from, in Square to) {
		move[n++] = (from | to << 6 | Piece.queen << 12);
		move[n++] = (from | to << 6 | Piece.knight << 12);
		move[n++] = (from | to << 6 | Piece.rook << 12);
		move[n++] = (from | to << 6 | Piece.bishop << 12);
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
		foreach(m; move[0..n]) s ~= m.toString() ~ " ";
		return s;
	}

	/* dump */
	void dump() const {
		foreach(i; 0 .. n) write(move[i].toString(), " [", value[i], "], ");
		writeln();
		writeln("stage = ", stage, " index = ", index, " n = ", n);
		writeln("ttMove = ", ttMove[0].toString, ", ", ttMove[1].toString);
		writeln("killer = ", killer[0].toString, ", ", killer[1].toString, " ; refutation = ", refutation.toString);

	}

	/* is the first move ? */
	bool isFirst(in Move m) const {
		return m == move[0];
	}

	/* opIndex */
	ref const(Move) opIndex(in size_t i) const {
		 return move[i];
	}	
}


/*
 * struct Line: a sequence of moves
 */
struct Line {
	immutable plyMax = 100;
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

	/* add a move */
	ref Line push(in Move m) {
		assert(n < plyMax);
		move[n++] = m;
		return this;
	}

	/* remove the last pushed move & return it */
	ref Line pop() {
		assert(n > 0);
		--n;
		return this;
	}

	/* add another line */
	ref Line push(const ref Line l) {
		foreach (m; l.move[0 .. l.n]) push(m);
		return this;
	}

	/* set */
	ref Line set(in Move m, const ref Line l) {
		return clear().push(m).push(l);
	}

	/* set */
	ref Line set(const ref Line line) {
		return clear().push(line);
	}

	/* Convert it to a string */
	string toString() const {
		string s;
		foreach (m; move[0 .. n]) s ~= m.toString() ~ " ";
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

