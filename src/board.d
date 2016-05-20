/*
 * File board.d
 * Chess board representation, move generation, etc.
 * © 2016 Richard Delorme
 */

module board;

import move, util;
import std.stdio, std.ascii, std.format, std.string, std.conv;
import std.algorithm, std.getopt, std.math, std.random;

/* limits */
enum Limits {plyMax = 100, gameSize = 4096, moveSize = 4096, moveMask = 4095, historyMax = 32768}

/*
 * Color
 */
enum Color : ubyte {white, black, size, none}

// Opponent color
Color opponent(in Color c) pure {
	return cast (Color) !c;
}

// Conversion from a char
Color toColor(in char c) pure {
	auto i = indexOf("wb", c);
	if (i == -1) i = Color.size;
	return cast (Color) i;
}

/*
 * Piece
 */
/* Piece enumeration */
enum Piece : ubyte {none, pawn, knight, bishop, rook, queen, king, size}

/* Convert to a piece from a char */
Piece toPiece(in char c) pure {
	auto i = indexOf(".pnbrqk", c, CaseSensitive.no);
	if (i == -1) i = 0;
	return cast (Piece) i;
}

char toChar(in Piece p) pure {
	return ".PNBRQK?"[p];
}

/*
 * Colored Piece
 */
/* CPiece enumeration */
enum CPiece : ubyte {none, wpawn, bpawn, wknight, bknight, wbishop, bbishop, wrook, brook, wqueen, bqueen, wking, bking, size}

/* Conversion from piece & color */
CPiece toCPiece(in Piece p, in Color c) pure {
	return cast (CPiece) (2 * p + c - 1);
}

/* Conversion from a char */
CPiece toCPiece(in char c) pure {
	auto i = indexOf(".PpNnBbRrQqKk", c);
	if (i == -1) i = 0;
	return cast (CPiece) i;
}

/* Get the color of a colored piece */
Color toColor(in CPiece p) pure {
	static immutable Color[CPiece.size] c= [Color.none,
		Color.white, Color.black, Color.white, Color.black, Color.white, Color.black,
		Color.white, Color.black, Color.white, Color.black, Color.white, Color.black];
	return c[p];
}

/* Get the piece of a colored piece */
Piece toPiece(in CPiece p) pure {
	return cast (Piece) ((p + 1) / 2);
}

/* Get the opponent colored piece */
CPiece opponent(in CPiece p) pure {
	return cast (CPiece) (((p - 1) ^ 1) + 1);
}

/*
 * Square
 */
/* Square enumeration */
enum Square : ubyte {
	none = 65,
	a1 = 0, b1, c1, d1, e1, f1, g1, h1,
	a2, b2, c2, d2, e2, f2, g2, h2,
	a3, b3, c3, d3, e3, f3, g3, h3,
	a4, b4, c4, d4, e4, f4, g4, h4,
	a5, b5, c5, d5, e5, f5, g5, h5,
	a6, b6, c6, d6, e6, f6, g6, h6,
	a7, b7, c7, d7, e7, f7, g7, h7,
	a8, b8, c8, d8, e8, f8, g8, h8,
	size,
}

/* Mirror square for black */
Square forward(in Square x, in Color c) pure {
	return cast (Square) (x ^ (56 * c));
}

/* Get rank */
int rank(in Square x) pure {
	return x >> 3;
}

/* Get file */
int file(in Square x) pure {
	return x & 7;
}

/* Square from file & rank */
Square toSquare(in int f, in int r) pure {
	if (0 <= f && f < 8 && 0 <= r && r < 8) return cast (Square) ((r << 3) + f);
	else return Square.none;
}

/* Square from string */
Square toSquare(in string s) pure {
	if(s.length > 1) return toSquare(s[0] - 'a', s[1] - '1');
	else return Square.none;

}

/* Square from bitboard */
Square popSquare(ref ulong b) pure {
	return cast (Square) popBit(b);
}

/* Square from bitboard */
Square firstSquare(in ulong b) pure {
	return cast (Square) firstBit(b);
}


/*
 * Miscs
 */

/* File mask TODO: en minuscule */
enum File {
	A = 0x0101010101010101,
	B = 0x0202020202020202,
	C = 0x0404040404040404,
	D = 0x0808080808080808,
	E = 0x1010101010101010,
	F = 0x2020202020202020,
	G = 0x4040404040404040,
	H = 0x8080808080808080
}

/* Rank mask */
enum Rank {
	r1 = 0x00000000000000ffUL,
	r2 = 0x000000000000ff00UL,
	r3 = 0x0000000000ff0000UL,
	r4 = 0x00000000ff000000UL,
	r5 = 0x000000ff00000000UL,
	r6 = 0x0000ff0000000000UL,
	r7 = 0x00ff000000000000UL,
	r8 = 0xff00000000000000UL
}

/* Castling */
enum Castling : ubyte {none = 0, K = 1, Q = 2, k = 4, q = 8, size = 16}

int toCastling(in char c) pure {
	auto i = indexOf("KQkq", c);
	if (i == -1) return 0;
	else return 1 << i;
}

/*
 * Zobrist key
 */
struct Key {
	ulong code;

	static immutable ulong [Square.size][CPiece.size] square;
	static immutable ulong [Castling.size] castling;
	static immutable ulong [Square.none + 1] enpassant;
	static immutable ulong [Color.size] color;
	static immutable ulong play;

	/* initialize Zobrist keys with pseudo random numbers */
	static this () pure {
		Mt19937 r;
		r.seed(19937);
		foreach (p; CPiece.wpawn .. CPiece.size)
		foreach (x; Square.a1 .. Square.size) square[p][x] = uniform(ulong.min, ulong.max, r);
		foreach (c; Castling.K .. Castling.size) castling[c] = uniform(ulong.min, ulong.max, r);
		foreach (x; Square.a4 .. Square.a6) enpassant[x] = uniform(ulong.min, ulong.max, r);
		foreach (c; Color.white .. Color.size) color[c] = uniform(ulong.min, ulong.max, r);
		play = color[Color.white] ^ color[Color.black];
	}

	/* set the key from a position */
	void set(in Board board) pure {
		const Board.Stack *s = &board.stack[board.ply];
		code = color[board.player];
		foreach (Square x; Square.a1 .. Square.size) code ^= square[board[x]][x];
		code ^= enpassant[s.enpassant];
		code ^= castling[s.castling];
	}

	/* update the key with a move */
	void update(in Board board, Move move) pure {
		Square x = Square.none;
		immutable Color player = board.player;
		immutable Color enemy = opponent(player);
		immutable CPiece p = board[move.from];
		const Board.Stack *s = &board.stack[board.ply];

		code = s.key.code;
		code ^= play;
		if (move != 0) {
			code ^= square[p][move.from] ^ square[p][move.to];
			code ^= square[board[move.to]][move.to];
			if (toPiece(p) == Piece.pawn) {
				if (move.promotion) code ^= square[p][move.to] ^ square[toCPiece(move.promotion, player)][move.to];
				else if (s.enpassant == move.to) code ^= square[toCPiece(Piece.pawn, enemy)][toSquare(file(move.to), rank(move.from))];
				else if (abs(move.to - move.from) == 16 && (board.mask[move.to].enpassant & (board.color[enemy] & board.piece[Piece.pawn]))) {
					x = cast (Square) ((move.from + move.to) / 2);
				}
			} else if (toPiece(p) == Piece.king) {
				CPiece r = toCPiece(Piece.rook, board.player);
				if (move.to == move.from + 2) code ^= square[r][move.from + 3] ^ square[r][move.from + 1];
				else if (move.to == move.from - 2) code ^= square[r][move.from - 4] ^ square[r][move.from - 1];
			}
			code ^= enpassant[s.enpassant] ^ enpassant[x];
			code ^= castling[s.castling] ^ castling[s.castling & board.mask[move.from].castling & board.mask[move.to].castling];
		}
	}
}


/*
 * Zobrist pawn key
 */
struct PawnKey {
	ulong code;

	/* set the key from a position */
	void set(in Board board) pure {
		ulong b = board.piece[Piece.pawn];
		while (b) {
			auto x = popSquare(b);
			code ^= Key.square[board[x]][x];
		}
	}

	/* update the key with a move */
	void update(in Board board, Move move) pure {
		Square x = Square.none;
		immutable Color player = board.player;
		immutable CPiece p = board[move.from];
		const Board.Stack *s = &board.stack[board.ply];

		code = s.pawnKey.code;
		if (toPiece(p) == Piece.pawn) {
			code ^= Key.square[p][move.from];
			if (!move.promotion) code ^= Key.square[p][move.to];
			if (s.enpassant == move.to) code ^= Key.square[opponent(p)][toSquare(file(move.to), rank(move.from))];
		}
		if (toPiece(board[move.to]) == Piece.pawn) code ^= Key.square[board[move.to]][move.from];			
	}
}

/*
 * Bitmask
 */
struct Mask {
	ulong bit;
	ulong diagonal;
	ulong antidiagonal;
	ulong file;
	ulong [Color.size] pawnAttack;
	ulong [Color.size] pawnPush;
	ulong [Color.size] openFile;
	ulong [Color.size] passedPawn;
	ulong isolatedPawn;
	ulong enpassant;
	ulong knight;
	ulong king;
	ulong [Square.size] between;
	ubyte [Square.size] direction;
	ubyte castling;
}

/* Bitmask init */
private immutable(Mask[Square.size]) maskInit() {
	int r, f, i, j, c, y, z;
	byte [Square.size][Square.size] d;
	Mask [Square.size] mask;
	immutable ubyte [6] castling = [13, 12, 14, 7, 3, 11];
	immutable Square [6] castlingX = [Square.a1, Square.e1, Square.h1, Square.a8, Square.e8, Square.h8];

	foreach (x; Square.a1 .. Square.size) {

		for (i = -1; i <= 1; ++i)
		for (j = -1; j <= 1; ++j) {
			if (i == 0 && j == 0) continue;
			f = x & 07;
			r = x >> 3;
			for (r += i, f += j; 0 <= r && r < 8 && 0 <= f && f < 8; r += i, f += j) {
		 		y = 8 * r + f;
				d[x][y] = cast (byte) (8 * i + j);
				mask[x].direction[y] = abs(d[x][y]);
		 	}
		}

		for (y = 0; y < Square.size; ++y) {
			i = d[x][y];
			if (i) {
				for (z = x + i; z != y; z += i) mask[x].between[y] |= 1UL << z;
			}
		}

		mask[x].bit = 1UL << x;

		for (y = x - 9; y >= 0 && d[x][y] == -9; y -= 9) mask[x].diagonal |= 1UL << y;
		for (y = x + 9; y < Square.size && d[x][y] == 9; y += 9) mask[x].diagonal |= 1UL << y;

		for (y = x - 7; y >= 0 && d[x][y] == -7; y -= 7) mask[x].antidiagonal |= 1UL << y;
		for (y = x + 7; y < Square.size && d[x][y] == 7; y += 7) mask[x].antidiagonal |= 1UL << y;

		for (y = x - 8; y >= 0; y -= 8) mask[x].file |= 1UL << y;
		for (y = x + 8; y < Square.size; y += 8) mask[x].file |= 1UL << y;

		f = x & 07;
		r = x >> 3;
		for (i = -1, c = 1; i <= 1; i += 2, c = 0) {
			for (j = -1; j <= 1; j += 2) {
				if (0 <= r + i && r + i < 8 && 0 <= f + j && f + j < 8) {
					 y = (r + i) * 8 + (f + j);
					 mask[x].pawnAttack[c] |= 1UL << y;
				}
			}
			if (0 <= r + i && r + i < 8) {
				y = (r + i) * 8 + f;
				mask[x].pawnPush[c] = 1UL << y;
			}
		}
		for (i = -1, c = 1; i <= 1; i += 2, c = 0) {
			if (0 < r && r < 7) {
				for (y = 8 * (r + i) + f; 8 <= y && y < 56 ; y += i * 8) {
					mask[x].openFile[c] |= 1UL << y;
					mask[x].passedPawn[c] |= 1UL << y;
					if (f > 0) mask[x].passedPawn[c] |= 1UL << (y - 1);
					if (f < 7) mask[x].passedPawn[c] |= 1UL << (y + 1);
				}
			}
		}
		if (f > 0) {
			for (y = x - 1; y < 56; y += 8) mask[x].isolatedPawn |= 1UL << y;
			for (y = x - 9; y >= 8; y -= 8) mask[x].isolatedPawn |= 1UL << y;
		}
		if (f < 7) {
			for (y = x + 1; y < 56; y += 8) mask[x].isolatedPawn |= 1UL << y;
			for (y = x - 7; y >= 8; y -= 8) mask[x].isolatedPawn |= 1UL << y;
		}
		if (r == 3 || r == 4) {
			if (f > 0) mask[x].enpassant |=  1UL << x - 1;
			if (f < 7) mask[x].enpassant |=  1UL << x + 1;
		}

		for (i = -2; i <= 2; i = (i == -1 ? 1 : i + 1))
		for (j = -2; j <= 2; ++j) {
			if (i == j || i == -j || j == 0) continue;
			if (0 <= r + i && r + i < 8 && 0 <= f + j && f + j < 8) {
		 		y = 8 * (r + i) + (f + j);
		 		mask[x].knight |= 1UL << y;
			}
		}

		for (i = -1; i <= 1; ++i)
		for (j = -1; j <= 1; ++j) {
			if (i == 0 && j == 0) continue;
			if (0 <= r + i && r + i < 8 && 0 <= f + j && f + j < 8) {
		 		y = 8 * (r + i) + (f + j);
		 		mask[x].king |= 1UL << y;
			}
		}

		mask[x].castling = 15;	
	}

	foreach (k; 0 .. 6) mask[castlingX[k]].castling = castling[k];

	return mask;
}

private immutable(bool [Limits.moveSize][Piece.size]) legalInit() {
	bool [Limits.moveSize][Piece.size] legal;

	void set(in Piece p, in int f, in int t) { legal[p][(t << 6) | f] = true;}
	
	foreach (x; Square.a2 .. Square.a8) {
		set(Piece.pawn, x, x + 8);
		if (rank(x) == 1) set(Piece.pawn, x, x + 16);
		if (file(x) < 7) set(Piece.pawn, x, x + 9);
		if (file(x) > 0) set(Piece.pawn, x, x + 7);
	}

	foreach (x; Square.a1 .. Square.size) {
		int f = file(x), r = rank(x);
		int i, j, to;

		foreach (c; Color.white .. Color.size) {
			for (i = -2; i <= 2; ++i)
			for (j = -2; j <= 2; ++j) {
				if (i == 0 || i == j || i == -j || j == 0) continue;
				if (0 <= r + i && r + i < 8 && 0 <= f + j && f + j < 8) {
			 		set(Piece.knight, x, toSquare(f + j, r + i));
				}
			}

		for (i = f - 1, j = r - 1; i >= 0 && j >= 0; --i, --j) set(Piece.bishop, x, toSquare(i, j));
		for (i = f + 1, j = r - 1; i <= 7 && j >= 0; ++i, --j) set(Piece.bishop, x, toSquare(i, j));
		for (i = f - 1, j = r + 1; i >= 0 && j <= 7; --i, ++j) set(Piece.bishop, x, toSquare(i, j));
		for (i = f + 1, j = r + 1; i <= 7 && j <= 7; ++i, ++j) set(Piece.bishop, x, toSquare(i, j));

		for (i = f - 1; i >= 0; --i) set(Piece.rook, x, toSquare(i, r));
		for (i = f + 1; i <= 7; ++i) set(Piece.rook, x, toSquare(i, r));
		for (j = r - 1; j >= 0; --j) set(Piece.rook, x, toSquare(f, j));
		for (j = r + 1; j <= 7; ++j) set(Piece.rook, x, toSquare(f, j));

		for (i = f - 1, j = r - 1; i >= 0 && j >= 0; --i, --j) set(Piece.queen, x, toSquare(i, j));
		for (i = f + 1, j = r - 1; i <= 7 && j >= 0; ++i, --j) set(Piece.queen, x, toSquare(i, j));
		for (i = f - 1, j = r + 1; i >= 0 && j <= 7; --i, ++j) set(Piece.queen, x, toSquare(i, j));
		for (i = f + 1, j = r + 1; i <= 7 && j <= 7; ++i, ++j) set(Piece.queen, x, toSquare(i, j));
		for (i = f - 1; i >= 0; --i) set(Piece.queen, x, toSquare(i, r));
		for (i = f + 1; i <= 7; ++i) set(Piece.queen, x, toSquare(i, r));
		for (j = r - 1; j >= 0; --j) set(Piece.queen, x, toSquare(f, j));
		for (j = r + 1; j <= 7; ++j) set(Piece.queen, x, toSquare(f, j));

		for (i = f - 1; i <= f + 1; ++i)
		for (j = r - 1; j <= r + 1; ++j) 
			if (0 <= i && i <= 7 && 0 <= j && j <= 7 && toSquare(i, j) != x) set(Piece.king, x, toSquare(i, j));
		}
		set(Piece.king, Square.e1, Square.g1);
		set(Piece.king, Square.e1, Square.c1);
	}

	return legal;
}

/* rank table init */
private immutable(ubyte [512]) rankInit()
{
	int x, y, o, f, b;
	ubyte[512] r;

	for (o = 0; o < 64; ++o) {
		for (f = 0; f < 8; ++f) {
			y = 0;
			for (x = f - 1; x >= 0; --x) {
				b = 1 << x;
				y |= b;
				if (((o << 1) & b) == b) break;
			}
			for (x = f + 1; x < 8; ++x) {
				b = 1 << x;
				y |= b;
				if (((o << 1) & b) == b) break;
			}
			r[o * 8 + f] = cast (ubyte) y;
		}
	}

	return r;
}	


/* Game result */
enum Result {none = 0, draw, repetitionDraw, fiftyDraw, insufficientMaterialDraw, stalemateDraw, whiteWin, blackWin, size}

/* Kind of move Generation */
enum Generate {all, capture, quiet}

/*
 * Class board
 */
class Board {

public:
	struct Stack {
		ulong pins;
		ulong checkers;
		Key key;
		PawnKey pawnKey;
		Square enpassant = Square.none;
		Piece victim;
		byte fifty;
		Castling castling;
	}
	/* members */
	ulong [Piece.size] piece;
	ulong [Color.size] color;
	CPiece [Square.size] cpiece;
	Stack [Limits.gameSize] stack;
	Color player;
	int ply, plyOffset;

	/* static values */
	static immutable Mask [Square.size] mask;
	static immutable bool [Limits.moveSize][Piece.size] legal;
private:
	static immutable ubyte [512] ranks;
	static immutable Castling [Color.size] kingside = [Castling.K, Castling.k];
	static immutable Castling [Color.size] queenside = [Castling.Q, Castling.q];
	static immutable int [Piece.size] seeValue = [0, 1, 3, 3, 5, 9, 300];

	static this() {
		mask = maskInit();
		ranks = rankInit();
		legal = legalInit();
	}

	/* can castle kingside ? */
	bool can_castle_kingside() pure const {
		return (stack[ply].castling & kingside[player]) != 0;
	}

	/* can castle queenside ? */
	bool can_castle_queenside() pure const {
		return (stack[ply].castling & queenside[player]) != 0;
	}

	/* slider attack function for the file, diagonal & antidiagonal directions */
	static ulong attack(in ulong occupancy, in Square x, in ulong m) pure  {
		immutable ulong o = occupancy & m;
		immutable ulong r = swapBytes(o);
		return ((o - mask[x].bit) ^ swapBytes(r - mask[x ^ 56].bit)) & m;
	}

	/* slider attack function along a rank */
	static ulong rankAttack(in ulong occupancy, in Square x) pure {
		immutable int f = x & 7;
		immutable int r = x & 56;
		immutable ulong o = (occupancy >> r) & 126;
		return ulong(ranks[o * 4  + f]) << r;
	}

	/* Slider attack along a file */
	static ulong fileAttack(in ulong occupancy, in Square x) pure {
		return attack(occupancy, x, mask[x].file);
	}

	/* Slider attack along a diagonal */
	static ulong diagonalAttack(in ulong occupancy, in Square x) pure {
		return attack(occupancy, x, mask[x].diagonal);
	}

	/* Slider attack along an antidiagonal */
	static ulong antidiagonalAttack(in ulong occupancy, in Square x) pure {
		return attack(occupancy, x, mask[x].antidiagonal);
	}

	/* Compute pins & checkers */
	void setPinsCheckers(out ulong checkers, out ulong pins) pure const {
		immutable Color enemy = opponent(player);
		immutable ulong K = piece[Piece.king] & color[player];
		immutable Square k = firstSquare(K);
		immutable ulong bq = (piece[Piece.bishop] + piece[Piece.queen]) & color[enemy];
		immutable ulong rq = (piece[Piece.rook] + piece[Piece.queen]) & color[enemy];
		immutable ulong occupancy = ~piece[Piece.none];
		ulong partialCheckers;
		ulong b;
		Square x;

		pins = 0;
		// bishop or queen
		b = coverage!(Piece.bishop)(k, occupancy);
		checkers = partialCheckers = b & bq;
		b &= color[player];
		if (b) {
			b = attack!(Piece.bishop)(k, bq ^ partialCheckers, occupancy ^ b);
			while (b) {
				x = popSquare(b);
				pins |= mask[k].between[x] & color[player];
			}
		}

		// rook or queen: all square reachable from the king square.
		b = coverage!(Piece.rook)(k, occupancy);
		checkers |= partialCheckers = b & rq;
		b &= color[player];
		if (b) {
			b = attack!(Piece.rook)(k, rq ^ partialCheckers, occupancy ^ b);
			while (b) {
				x = popSquare(b);
				pins |= mask[k].between[x] & color[player];
			}
		}
		// other occupancy (no more pins)
		checkers |= attack!(Piece.knight)(k, piece[Piece.knight]);
		// checkers |= mask[k].pawnAttack[player] & piece[Piece.pawn]; 
		checkers |= attack!(Piece.pawn)(k, piece[Piece.pawn], occupancy, player);
		checkers &= color[enemy];
	}

	/* Deplace a piece on the board */
	void deplace(in int from, in int to, in Piece p) pure {
		immutable ulong M = mask[from].bit | mask[to].bit;
		piece[Piece.none] ^= M;
		piece[p] ^= M;
		color[player] ^= M;
		cpiece[to] = cpiece[from];
		cpiece[from] = CPiece.none;
	}

	/* check if a square is attacked */
	bool isSquareAttacked(in Square x, in Color player) pure const {
		immutable ulong occupancy = ~piece[Piece.none];
		immutable ulong P = color[player];

		return attack!(Piece.bishop)(x, P & (piece[Piece.bishop] | piece[Piece.queen]), occupancy)
			|| attack!(Piece.rook)(x, P & (piece[Piece.rook] | piece[Piece.queen]), occupancy)
			|| attack!(Piece.knight)(x, P & piece[Piece.knight])
			|| attack!(Piece.pawn)(x, P & piece[Piece.pawn], occupancy, opponent(player))
			|| attack!(Piece.king)(x, P & piece[Piece.king]);
	}

	/* generate all moves from a square */
	static void generateMoves(ref Moves moves, ulong attack, in Square from) pure {
		Square to;

		while (attack) {
			to = popSquare(attack);
			moves.push(from, to);
		}
	}

	/* generate promotion */
	static void generatePromotions(ref Moves moves, ulong attack, in int dir) pure {
		while (attack) {
			Square to = popSquare(attack);
			Square from = cast (Square) (to - dir);
			moves.pushPromotions(from, to);
		}
	}

	/* generate pawn moves */
	static void generatePawnMoves (ref Moves moves, ulong attack, in int dir) pure {
		while (attack) {
			Square to = popSquare(attack);
			Square from = cast (Square) (to - dir);
			moves.push(from, to);
		}
	}

	/* generate all white pawn moves */
	void generateWhitePawnMoves(Generate type) (ref Moves moves, in ulong attacker, in ulong enemies, in ulong empties) pure const {
		ulong attack;

		static if (type != Generate.quiet) {
			attack = ((attacker & ~File.A) << 7) & enemies;
			generatePromotions(moves, attack & Rank.r8, 7);
			generatePawnMoves(moves, attack & ~Rank.r8, 7);
			attack = ((attacker & ~File.H) << 9) & enemies;
			generatePromotions(moves, attack & Rank.r8, 9);
			generatePawnMoves(moves, attack & ~Rank.r8, 9);
		}
		attack = attacker << 8 & piece[Piece.none];
		static if (type != Generate.quiet) {
			generatePromotions(moves, attack & Rank.r8 & empties, 8);
		}
		static if (type != Generate.capture) {
			generatePawnMoves(moves, attack & ~Rank.r8 & empties, 8);
			attack = ((attack & Rank.r3) << 8) & empties;
			generatePawnMoves(moves, attack, 16);
		}
	}

	/* generate all black pawn moves */
	void generateBlackPawnMoves(Generate type) (ref Moves moves, in ulong attacker, in ulong enemies, in ulong empties) pure const {
		ulong attack;

		if (type != Generate.quiet) {
			attack = ((attacker & ~File.A) >> 9) & enemies;
			generatePromotions(moves, attack & Rank.r1, -9);
			generatePawnMoves(moves, attack & ~Rank.r1, -9);
			attack = ((attacker & ~File.H) >> 7) & enemies;
			generatePromotions(moves, attack & Rank.r1, -7);
			generatePawnMoves(moves, attack & ~Rank.r1, -7);
		}
		attack = (attacker >> 8) & piece[Piece.none];
		if (type != Generate.quiet) {
			generatePromotions(moves, attack & Rank.r1 & empties, -8);
		}
		if (type != Generate.capture) {
			generatePawnMoves(moves, attack & ~Rank.r1 & empties, -8);
			attack = ((attack & Rank.r6) >> 8) & empties;
			generatePawnMoves(moves, attack, -16);
		}
	}

public:
	/* board coverage by a piece */
	static ulong coverage(Piece p)(in Square x, in ulong occupancy = 0, in Color c = Color.white) pure {
		static if (p == Piece.pawn) return (mask[x].pawnPush[c] & ~occupancy) + (mask[x].pawnAttack[c] & occupancy); // capture & push here
		else static if (p == Piece.knight) return mask[x].knight;
		else static if (p == Piece.bishop) return diagonalAttack(occupancy, x) + antidiagonalAttack(occupancy, x);
		else static if (p == Piece.rook) return fileAttack(occupancy, x) + rankAttack(occupancy, x);
		else static if (p == Piece.queen) return diagonalAttack(occupancy, x) + antidiagonalAttack(occupancy, x) + fileAttack(occupancy, x) + rankAttack(occupancy, x);
		else static if (p == Piece.king) return mask[x].king;
	}

	/* masked attack */
	static ulong attack(Piece p)(in Square x, in ulong target, in ulong occupancy = 0, in Color c = Color.white) pure {
		static if (p == Piece.pawn) return (mask[x].pawnAttack[c] & target); // only capturing move here!
		else return coverage!p(x, occupancy, c) & target;
	}

	/* Clear the board */
	Board clear() pure {
		foreach (p; Piece.none .. Piece.size) piece[p] = 0;
		foreach (c; Color.white .. Color.size) color[c] = 0;
		foreach (x; Square.a1 .. Square.size) cpiece[x] = CPiece.none;
		stack[0] = Stack.init;
		player = Color.white;
		ply = 0;
		return this;
	}

	/* Invert the board */
	Board mirror() pure {
		foreach (p; Piece.none .. Piece.size) piece[p] = swapBytes(piece[p]);
		foreach (c; Color.white .. Color.size) color[c] = swapBytes(color[c]);
		swap(color[Color.white], color[Color.black]);
		foreach (x; Square.a1 .. Square.size) if (x < (x ^ 56)) swap(cpiece[x], cpiece[x ^ 56]);
		foreach (x; Square.a1 .. Square.size) if (cpiece[x]) cpiece[x] = toCPiece(toPiece(cpiece[x]), opponent(toColor(cpiece[x])));
		player = opponent(player);

		foreach(i; 0 .. ply + 1) {
			stack[i].pins = swapBytes(stack[i].pins);
			stack[i].checkers = swapBytes(stack[i].checkers);
			stack[i].castling = cast (Castling) (((stack[i].castling & 3) << 2) | ((stack[i].castling & 12) >> 2));
			if (stack[i].enpassant != Square.none) stack[i].enpassant ^= 56;
		}
		stack[ply].key.set(this);

		return this;
	}

	/* set the board from a FEN string */
	Board set(in string fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") {
		Square x;
		CPiece p;
		int r = 7, f;
		string [] s = fen.split();

		void error(in string msg) {
			stderr.writeln("Parsing FEN error: ", msg);
			stderr.writeln('\"', fen, '\"');
			throw new Error("Bad FEN");
		}

		clear();

		if (s.length < 4) error("missing fields");

		foreach (c; s[0]) {
			if (c== '/') {
				if (r <= 0) error("rank overflow");
				if (f != 8) error("missing square");
				f = 0; --r;
			} else if (isDigit(c)) {
				f += c - '0';
				if (f > 8) error("file overflow");
			} else {
				if (f > 8) error("file overflow");
				x = toSquare(f, r);
				cpiece[x] = p = toCPiece(c);
				if (cpiece[x] == CPiece.size) error("bad piece");
				piece[toPiece(p)] |= mask[x].bit;
				color[toColor(p)] |= mask[x].bit;
				++f;
			}
		}
		if (r > 0 || f != 8) error("missing squares");

		player = toColor(s[1][0]);
		if (player == Color.size) error("bad player's turn");

		if (s.length > 5 && isNumeric(s[4])) {
			plyOffset = 2 * (std.conv.to!int(s[5]) - 1) + player;
			stack[ply].fifty = std.conv.to!ubyte(s[4]);
		}

		if (s[2] != "-") {
			foreach (c; s[2]) stack[ply].castling |= toCastling(c);
		}
		if (cpiece[Square.e1] == CPiece.wking) {
			if (cpiece[Square.h1] != CPiece.wrook) stack[ply].castling &= ~1;
			if (cpiece[Square.a1] != CPiece.wrook) stack[ply].castling &= ~2;
		} else stack[ply].castling &= ~3;
		if (cpiece[Square.e8] == CPiece.bking) {
			if (cpiece[Square.h8] != CPiece.brook) stack[ply].castling &= ~4;
			if (cpiece[Square.a8] != CPiece.brook) stack[ply].castling &= ~8;
		} else stack[ply].castling &= ~12;

		if (s[3] != "-") {
			stack[ply].enpassant = toSquare(s[3]);
			if (stack[ply].enpassant == Square.none) error("bad enpassant");
		}

		piece[Piece.none] = ~(color[Color.white] | color[Color.black]);
		setPinsCheckers(stack[ply].checkers, stack[ply].pins);
		stack[ply].key.set(this);
		stack[ply].pawnKey.set(this);

		if (!verify()) error("bad board");

		return this;
	}

	/* constructor */
	this() {
		set();
	}

	/* copy constructor */
	this (in Board b) pure {
		foreach (p; Piece.none .. Piece.size) piece[p] = b.piece[p];
		foreach (c; Color.white .. Color.size) color[c] = b.color[c];
		foreach (x; Square.a1 .. Square.size) cpiece[x] = b.cpiece[x];
		foreach (i; 0 .. cast (int) Limits.gameSize) stack[i] = b.stack[i];
		player = b.player;
		ply = b.ply; plyOffset = b.plyOffset;
	}

	/* duplicate the board */
	Board dup() pure const @property {
		return new Board(this);
	}

	/* Convert to a printable string */
	override string toString() pure const {
		Square x;
		int f, r;
		string p = ".PpNnBbRrQqKk#", c = "wb", s;

		s ~= "  a b c d e f g h\n";
		for (r = 7; r >= 0; --r)
		for (f = 0; f <= 7; ++f) {
			x = toSquare(f, r);
			if (f == 0) s ~= format("%1d ", r + 1);
			s ~= p[cpiece[x]] ~ " ";
			if (f == 7) s ~= format("%1d\n", r + 1);
		}
		s ~= "  a b c d e f g h\n";
		s ~= c[player] ~ " ";
		if (stack[ply].castling & Castling.K) s ~= "K";
		if (stack[ply].castling & Castling.Q) s ~= "Q";
		if (stack[ply].castling & Castling.k) s ~= "k";
		if (stack[ply].castling & Castling.q) s ~= "q";
		if (stack[ply].enpassant != Square.none) s ~= format(" ep: %s", stack[ply].enpassant);
		s ~= format(" move %d, fifty %d\n", (ply + plyOffset) / 2 + 1, stack[ply].fifty);

		return s;
	}

	/* chess  board content */
	ref CPiece opIndex(in Square x) pure {
		 return cpiece[x];
	}

	/* chess  board content */
	CPiece opIndex(in Square x) pure const {
		 return cpiece[x];
	}

	/* king is in check */
	bool inCheck() const pure @property {
		 return stack[ply].checkers > 0;
	}

	/* 50-move rule counter */
	int fifty() const pure @property {
		 return stack[ply].fifty;
	}

	/* zobrist key */
	Key key() const pure @property {
		 return stack[ply].key;
	}

	/* zobrist pawn key */
	ulong pawnKey() const pure @property {
		 return stack[ply].pawnKey.code;
	}

	/* return true if a position is a draw */
	Result isDraw() pure const @property {
		// repetition
		int n_repetition = 0;
		immutable end = max(0, ply - stack[ply].fifty);
		for (auto i = ply - 4; i >= end; i -= 2) {
			if (stack[i].key.code == stack[ply].key.code && ++n_repetition >= 2) return Result.repetitionDraw;
		}

		// fifty move rule
		if (stack[ply].fifty > 100) return Result.fiftyDraw;

		// lack of mating material
		if (piece[Piece.pawn] + piece[Piece.rook] + piece[Piece.queen] == 0) {
			// a single minor: KNK or KBK
			immutable  n_minor = countBits(piece[Piece.knight] + piece[Piece.bishop]);
			if (n_minor <= 1) return Result.insufficientMaterialDraw;
			// only bishops on same square color: KBBK
			immutable  diff = abs(countBits(color[Color.white]) - countBits(color[Color.black]));
			immutable blackSquares = 0x55aa55aa55aa55aa;
			immutable whiteSquares = ~blackSquares;
			if (diff == n_minor && piece[Piece.knight] == 0
				&& ((piece[Piece.bishop] & blackSquares) == piece[Piece.bishop] || (piece[Piece.bishop] & whiteSquares) == piece[Piece.bishop])) return Result.insufficientMaterialDraw;
		}

		return Result.none;
	}

	/* verify if the game is over and return the game result */
	Result isGameOver() pure @property {
		Moves moves = void;
		Result [Color.size] wins = [Result.blackWin, Result.whiteWin];

		moves.generate(this);
		if (moves.empty) {
			if (inCheck) return wins[player];
			else return Result.stalemateDraw;
		}

		return isDraw;
	}

	/* verify if a move gives check */
	int giveCheck(in Move m) pure const {
		immutable ulong K = piece[Piece.king] & color[opponent(player)];
		immutable Square from = m.from, to = m.to, k = firstSquare(K);
		immutable Piece p = m.promotion ? m.promotion : toPiece(cpiece[from]);
		immutable Square ep = (p == Piece.pawn && m.to == stack[ply].enpassant) ? toSquare(file(to), rank(from)) : from;
		immutable ulong O = (~piece[Piece.none]) ^ (mask[from].bit | mask[ep].bit | mask[to].bit);
		immutable ulong P = color[player] ^ mask[from].bit;
		immutable int dir = mask[from].direction[k];
		int check = 0;
	
		// direct check...
		final switch(p) {
		case Piece.pawn:
			if (attack!(Piece.pawn)(m.to, K, O, player)) ++check;
			break;
		case Piece.knight:
			if (attack!(Piece.knight)(m.to, K, O, player)) ++check;
			break;
		case Piece.bishop:
			if (attack!(Piece.bishop)(m.to, K, O, player)) ++check;
			break;
		case Piece.rook:
			if (attack!(Piece.rook)(m.to, K, O, player)) ++check;
			break;
		case Piece.queen:
			if (attack!(Piece.queen)(m.to, K, O, player)) ++check;
			break;
		case Piece.king: // castling
			if (m.to == m.from + 2 && attack!(Piece.rook)(cast (Square) (m.from + 1), K, O, player)) ++check;
			else if (m.to == m.from + 2 && attack!(Piece.rook)(cast (Square) (m.from - 1), K, O, player)) ++check;
			break;
		case Piece.none, Piece.size:
			break;
		}

		//discovered check
		if ((dir == 7 || dir == 9) && attack!(Piece.bishop)(k, P & (piece[Piece.bishop] | piece[Piece.queen]), O)) ++check;
		else if ((dir == 1 || dir == 8) && attack!(Piece.rook)(k, P & (piece[Piece.rook] | piece[Piece.queen]), O)) ++check;

		return check;
	}		

	/* is a move a Capture or a promotion */
	bool isTactical(in Move m) const pure {
		return (cpiece[m.to] != CPiece.none || m.promotion);
	}

	/* Count the number of a piece */
	int count(in Piece p, in Color c) pure const {
		return countBits(piece[p] & color[c]);
	}

	/* Count the number of a colored piece */
	int count(in CPiece p) pure const {
		return count(toPiece(p), toColor(p));
	}

	/* Play a move on the board */
	void update(in Move move) pure {
		immutable to = mask[move.to].bit;
		immutable enemy = opponent(player);
		immutable p = toPiece(cpiece[move.from]);
		const Stack *u = &stack[ply];
		Stack *n = &stack[ply + 1];

		n.key.update(this, move);
		n.pawnKey.update(this, move);
		n.castling = u.castling;
		n.enpassant = Square.none;
		n.fifty = cast (byte) (u.fifty + 1);

		if (move != 0) {
			n.victim = toPiece(cpiece[move.to]);
			deplace(move.from, move.to, p);
			if (n.victim) {
				n.fifty = 0;
				piece[Piece.none] ^= to;
				piece[n.victim] ^= to;
				color[enemy] ^= to;
			}
			if (p == Piece.pawn) {
				n.fifty = 0;
				if (move.promotion) {
					piece[Piece.pawn] ^= to;
					piece[move.promotion] ^= to;
					cpiece[move.to] = toCPiece(move.promotion, player);
				} else if (u.enpassant == move.to) {
					immutable x = toSquare(file(move.to), rank(move.from));
					immutable b = mask[x].bit;
					piece[Piece.none] ^= b;
					piece[Piece.pawn] ^= b;
					color[enemy] ^= b;
					cpiece[x] = CPiece.none;
				} else if (abs(move.to - move.from) == 16 && (mask[move.to].enpassant & (color[enemy] & piece[Piece.pawn]))) {
					n.enpassant = cast (Square) ((move.from + move.to) / 2);
				}
			} else if (p == Piece.king) {
				if (move.to == move.from + 2) deplace(move.from + 3, move.from + 1, Piece.rook);
				else if (move.to == move.from - 2) deplace(move.from - 4, move.from - 1, Piece.rook);
			}
			n.castling &= (mask[move.from].castling & mask[move.to].castling);
		}

		player = enemy;
		setPinsCheckers(n.checkers, n.pins);
		++ply;

		debug assert(verify());
	}

	/* Undo a move on the board */
	void restore(in Move move) pure {
		immutable ulong to = mask[move.to].bit;
		immutable Color enemy = player;
		immutable p = move.promotion ? Piece.pawn : toPiece(cpiece[move.to]);
		const Stack *n = &stack[ply];
		const Stack *u = &stack[--ply];

		player = opponent(enemy);
		if (move != 0) {
			deplace(move.to, move.from, p);
			if (n.victim) {
				piece[Piece.none] ^= to;
				piece[n.victim] ^= to;
				color[enemy] ^= to;
				cpiece[move.to] = toCPiece(n.victim, enemy);
			}
			if (p == Piece.pawn) {
				if (move.promotion) {
					piece[Piece.pawn] ^= to;
					piece[move.promotion] ^= to;
					cpiece[move.from] = toCPiece(Piece.pawn, player);
				} else if (u.enpassant == move.to) {
					immutable x = toSquare(file(move.to), rank(move.from));
					immutable b = mask[x].bit;
					piece[Piece.none] ^= b;
					piece[Piece.pawn] ^= b;
					color[enemy] ^= b;
					cpiece[x] = toCPiece(Piece.pawn, enemy);
				}
			} else if (p == Piece.king) {
				if (move.to == move.from + 2) deplace(move.from + 1, move.from + 3, Piece.rook);
				else if (move.to == move.from - 2) deplace(move.from - 1, move.from - 4, Piece.rook);
			}
		}

		debug assert(verify());
	}

	/* play a sequence of moves */
	void update(in Move [] moves) pure {
		foreach (m; moves) update(m);
	}

	/* generate evasions */
	void generateEvasions (ref Moves moves) pure {
		immutable Color enemy = opponent(player);
		immutable ulong occupancy = ~piece[Piece.none];
		immutable ulong bq = piece[Piece.bishop] | piece[Piece.queen];
		immutable ulong rq = piece[Piece.rook] | piece[Piece.queen];
		immutable ulong pinfree = color[player] & ~stack[ply].pins;
		immutable ulong K = piece[Piece.king] & color[player];
		immutable Square k = firstSquare(K);
		immutable int[2] push = [8, -8];
		immutable int pawnPush = push[player];
		ulong attacker, target, occ;
		Square from, to, x;

		// king evades
		piece[Piece.none] ^= K;
		target = attack!(Piece.king)(k, ~color[player]);
		while (target) {
			to = popSquare(target);
			if (!isSquareAttacked(to, enemy)) moves.push(k, to);
		}
		piece[Piece.none] ^= K;

		// capture or bloc the (single) checker;
		if (hasSingleBit(stack[ply].checkers)) {
			x = firstSquare(stack[ply].checkers);
			target = mask[k].between[x];

			//enpassant
			to = stack[ply].enpassant;
			if (x == to - pawnPush && to != Square.none) {
				from = cast (Square) (x - 1);
				if (file(to) > 0 && cpiece[from] == toCPiece(Piece.pawn, player)) {
					occ = occupancy ^ mask[from].bit ^ mask[x].bit ^ mask[to].bit;
					if (!attack!(Piece.rook)(k, rq & color[enemy], occ)) moves.push(from, to);
				}
				from = cast (Square) (x + 1);
				if (file(to) < 7 && cpiece[from] == toCPiece(Piece.pawn, player)) {
					occ = occupancy ^ mask[from].bit ^ mask[x].bit ^ mask[to].bit;
					if (!attack!(Piece.rook)(k, rq & color[enemy], occ)) moves.push(from, to);
				}
			}

			// pawn
			attacker = piece[Piece.pawn] & pinfree;
			if (player == Color.white) generateWhitePawnMoves!(Generate.all)(moves, attacker, stack[ply].checkers, target);
			else generateBlackPawnMoves!(Generate.all)(moves, attacker, stack[ply].checkers, target);

			// knight
			target |= stack[ply].checkers;
			attacker = piece[Piece.knight] & pinfree;
			while (attacker) {
				from = popSquare(attacker);
				generateMoves(moves, attack!(Piece.knight)(from, target), from);
			}

			// bishop or queen
			attacker = bq & pinfree;
			while (attacker) {
				from = popSquare(attacker);
				generateMoves(moves, attack!(Piece.bishop)(from, target, occupancy), from);
			}

			// rook or queen
			attacker = rq & pinfree;
			while (attacker) {
				from = popSquare(attacker);
				generateMoves(moves, attack!(Piece.rook)(from, target, occupancy), from);
			}
		}
	}

	/* generate moves */
	void generateMoves (Generate type = Generate.all) (ref Moves moves) pure {
		immutable Color enemy = opponent(player);
		immutable ulong occupancy = ~piece[Piece.none];
		immutable ulong target = type == Generate.all ? ~color[player] : type == Generate.capture ? color[enemy] : piece[Piece.none];
		immutable ulong pinfree = color[player] & ~stack[ply].pins;
		immutable ulong bq = piece[Piece.bishop] | piece[Piece.queen];
		immutable ulong rq = piece[Piece.rook] | piece[Piece.queen];
		immutable ulong K = piece[Piece.king] & color[player];
		immutable Square k = firstSquare(K);
		immutable int[2] push = [8, -8];
		immutable int pawnLeft = push[player] - 1;
		immutable int pawnRight = push[player] + 1;
		immutable int pawnPush = push[player];
		ulong attacker, occ, attacked;
		Square from, to, x;
		int d;

		// castling
		if (type != Generate.capture) {
			if (can_castle_kingside()
				&& (~piece[Piece.none] & mask[k].between[k + 3]) == 0
				&& !isSquareAttacked(cast (Square) (k + 1), enemy)
				&& !isSquareAttacked(cast (Square) (k + 2), enemy)) moves.push(k, cast (Square) (k + 2));
			if (can_castle_queenside()
				&& (~piece[Piece.none] & mask[k].between[k - 4]) == 0
				&& !isSquareAttacked(cast (Square) (k - 1), enemy)
				&& !isSquareAttacked(cast (Square) (k - 2), enemy)) moves.push(k, cast (Square) (k - 2));
		}

		// pawn (enpassant)
		if (type != Generate.quiet && stack[ply].enpassant != Square.none) {
			to = stack[ply].enpassant;
			x = cast (Square) (to - pawnPush);
			from = cast (Square) (x - 1);
			if (file(to) > 0 && cpiece[from] == toCPiece(Piece.pawn, player)) {
				occ = occupancy ^ (mask[from].bit | mask[x].bit | mask[to].bit);
				if (!attack!(Piece.bishop)(k, bq & color[enemy], occ) && !attack!(Piece.rook)(k, rq & color[enemy], occ)) moves.push(from, to);
			}
			from = cast (Square) (x + 1);
			if (file(to) < 7 && cpiece[from] == toCPiece(Piece.pawn, player)) {
				occ = occupancy ^ (mask[from].bit | mask[x].bit | mask[to].bit);
				if (!attack!(Piece.bishop)(k, bq & color[enemy], occ) && !attack!(Piece.rook)(k, rq & color[enemy], occ)) moves.push(from, to);
			}
		}

		// pawn (pins)
		attacker = piece[Piece.pawn] & stack[ply].pins;
		while (attacker) {
			from = popSquare(attacker);
			d = mask[k].direction[from];
			if (type != Generate.quiet && d == abs(pawnLeft)) {
				to = cast (Square) (from + pawnLeft);
				if (toColor(cpiece[to]) == enemy) {
					if (rank(forward(from, player)) == 6) moves.pushPromotions(from, to);
					else moves.push(from, to);
				}
			} else if (type != Generate.quiet && d == abs(pawnRight)) {
				to = cast (Square) (from + pawnRight);
				if (toColor(cpiece[to]) == enemy) {
					if (rank(forward(from, player)) == 6) moves.pushPromotions(from, to);
					else moves.push(from, to);
				}
			} else if (d == abs(pawnPush)) {
				to = cast (Square) (from + pawnPush);
			 	if (cpiece[to] == CPiece.none) {
					moves.push(from, to);
					to = cast (Square) (to + pawnPush);
					if (rank(forward(from, player)) == 1 && cpiece[to] == CPiece.none) moves.push(from, to);
				}
			}
		}

		// pawn (no pins)
		attacker = piece[Piece.pawn] & pinfree;
		if (player == Color.white) generateWhitePawnMoves!type(moves, attacker, color[enemy], piece[Piece.none]);
		else generateBlackPawnMoves!type(moves, attacker, color[enemy], piece[Piece.none]);

		// knight (no pins)
		attacker = piece[Piece.knight] & pinfree;
		while (attacker) {
			from = popSquare(attacker);
			generateMoves(moves, attack!(Piece.knight)(from, target), from);
		}

		// bishop or queen (pins)
		attacker = bq & stack[ply].pins;
		while (attacker) {
			from = popSquare(attacker);
			d = mask[k].direction[from];
			if (d == 9) generateMoves(moves, diagonalAttack(occupancy, from) & target, from);
			else if (d == 7) generateMoves(moves, antidiagonalAttack(occupancy, from) & target, from);
		}

		// bishop or queen (no pins)
		attacker = bq & pinfree;
		while (attacker) {
			from = popSquare(attacker);
			generateMoves(moves, attack!(Piece.bishop)(from, target, occupancy), from);
		}

		// rook or queen (pins)
		attacker = rq & stack[ply].pins;
		while (attacker) {
			from = popSquare(attacker);
			d = mask[k].direction[from];
			if (d == 1) generateMoves(moves, rankAttack(occupancy, from) & target, from);
			else if (d == 8) generateMoves(moves, fileAttack(occupancy, from) & target, from);
		}

		// rook or queen (no pins)
		attacker = rq & pinfree;
		while (attacker) {
			from = popSquare(attacker);
			generateMoves(moves, attack!(Piece.rook)(from, target, occupancy), from);
		}

		// king
		attacked = attack!(Piece.king)(k, target);
		while (attacked) {
			to = popSquare(attacked);
			if (!isSquareAttacked(to, enemy)) moves.push(k, to);
		}
	}

	/* verify board integrity */
	bool verify() const {
		immutable ulong o = ~piece[Piece.none];
		immutable int[2] push = [8, -8];
		ulong b;

		bool error(string msg) const {
			stderr.writeln(this.toString());
			stderr.writeln(msg);
			return false;
		}

		// player to move
		if (player != Color.white && player != Color.black) return error(format("bad player", player));

		// bitboard: 1 color per square
		if (color[Color.white] & color[Color.black]) {
				writeln(Color.white); writeBitboard(color[Color.white]);
				writeln(Color.black); writeBitboard(color[Color.black]);

			return error("colors intersect");
		}

		// bitboard: 1 piece per square
		foreach (p; Piece.none .. Piece.size)
		foreach (q; Piece.none .. Piece.size) {
			if (p != q && piece[p] & piece[q]) {
				writeln(p); writeBitboard(piece[p]);
				writeln(q); writeBitboard(piece[q]);
				return error(format("pieces  %s & %s intersect", p, q));
			}
		}

		// bitboard: each piece with 1 color
		b = 0;
		foreach(p; Piece.pawn .. Piece.size) b |= piece[p];
		if (b != o) {
			writeln("colors :"); writeBitboard(o);
			writeln("pieces :"); writeBitboard(b);
			return error("pieces / colors mismatch");
		}

		// mailbox matches bitboards
		foreach(Piece p; Piece.pawn .. Piece.size)
		foreach(c; Color.white .. Color.black) {
			b = piece[p] & color[c];
			while (b) {
				Square x = popSquare(b);
				if (cpiece[x] != toCPiece(p, c)) {
					writeln(c, " ", p, ":"); writeBitboard(piece[p] & color[c]);
					return error(format("board array differs from %s %s bitboard at %s", c, p, x));
				}
			}
		}

		// bitboards match mailbox
		foreach(x; Square.a1 .. Square.size) {
			b = mask[x].bit;
			Piece p = toPiece(cpiece[x]);
			Color c = toColor(cpiece[x]);
			if ((b & piece[p]) == 0) {
				writeln(p); writeBitboard(piece[p]);
				return error(format("%s bitboard different to board array at %s", p, x));
			}
			if (p != Piece.none && (b & piece[p] & color[c]) == 0) {
				writeln(c); writeBitboard(color[c]);
				return error(format("%s colors different to board array at %s", c, x));
			}
		}

		// valid en passant Square
		if (stack[ply].enpassant != Square.none) {
			Square x = cast (Square) (stack[ply].enpassant - push[player]);
			if (cpiece[x] != toCPiece(Piece.pawn, opponent(player))) return error(format("bad enpassant Square: %s", stack[ply].enpassant));
		}

		// 2 kings
		foreach(c; Color.white .. Color.black) {
			b = piece[Piece.king] & color[c];
			if (!hasSingleBit(b)) error(format("bad %s king", c));
			Square x = popSquare(b);
			if (c != player && isSquareAttacked(x, opponent(c))) error(format("king %s going to be captures", x));
		}

		// valid castling
		int castling = 15;
		if (cpiece[Square.e1] == CPiece.wking) {
			if (cpiece[Square.h1] != CPiece.wrook) castling &= ~1;
			if (cpiece[Square.a1] != CPiece.wrook) castling &= ~2;
		} else castling &= ~3;
		if (cpiece[Square.e8] == CPiece.bking) {
			if (cpiece[Square.h8] != CPiece.brook) castling &= ~4;
			if (cpiece[Square.a8] != CPiece.brook) castling &= ~8;
		} else castling &= ~12;
		if ((castling & stack[ply].castling) != stack[ply].castling) return error(format("bad castling status: %s", stack[ply].castling));

		// valid key
		Key k;
		k.set(this);
		if (k.code != stack[ply].key.code) return error(format("bad key %s vs %s", k.code, stack[ply].key));

		return true;
	}

	/* is a move legal */
	bool isLegal(bool verbose = false) (in Move move) pure {
		immutable ulong occupancy = ~piece[Piece.none];
		Piece p = toPiece(cpiece[move.from]);
		Color enemy = opponent(player);
		CPiece victim = cpiece[move.to];
		bool inCheck;
		enum Reason { none, wrongDeplacement, wrongColor, obstacle, blockedSquare, wrongCapture, missingPromotion, wrongPromotion, 
			wrongVictim, illegalCastling, kingInCheck, illegalMissed }

		bool fail(in Reason r) {		
			debug if (verbose) {
				writeln(toString);
				writeln(move.toString);
				writeln(r);
				assert(0);
			}
			return false;	
		}

		// legal deplacement ?
		if (!legal[p][(move ^ (player * 3640)) & Limits.moveMask]) return fail(Reason.wrongDeplacement);

		// bad piece color ?
		if (toColor(cpiece[move.from]) != player) return fail(Reason.wrongColor);

		// obstacle on a slider's trajectory ?
		if (mask[move.from].between[move.to] & occupancy) return fail(Reason.obstacle);

		// bad pawn move ?
		if (p == Piece.pawn) {
			if (mask[move.from].direction[move.to] == 8 || mask[move.from].direction[move.to] == 16) {
				// push to an empty square ?
				if (victim) return fail(Reason.blockedSquare);
			} else {
				if (!victim && move.to != stack[ply].enpassant) return fail(Reason.wrongCapture); // capture ?
			}
			if ((rank(move.to) == 0 || rank(move.to) == 7) && !move.promotion) return fail(Reason.missingPromotion); // promotion ?
			if (rank(move.to) > 0 && rank(move.to) < 7 && move.promotion) return fail(Reason.wrongPromotion);
		} else {
			if (move.promotion) return fail(Reason.wrongPromotion);
		}

		// bad victim ?
		if (victim && (toColor(victim) == player || toPiece(victim) == Piece.king)) return fail(Reason.wrongVictim);

		// illegal castling ?	
		if (p == Piece.king) {
			Square k = move.from;
			if ((k == move.to - 2) && 
				   (!can_castle_kingside 
				|| (~piece[Piece.none] & mask[k].between[k + 3])
				|| isSquareAttacked(cast (Square) (k + 1), enemy) 
				|| isSquareAttacked(cast (Square) (k + 2), enemy))) return fail(Reason.illegalCastling);
			if ((k == move.to + 2) &&
				   (!can_castle_queenside()
				|| (~piece[Piece.none] & mask[k].between[k - 4])
				|| isSquareAttacked(cast (Square) (k - 1), enemy)
				|| isSquareAttacked(cast (Square) (k - 2), enemy))) return fail(Reason.illegalCastling);
		}

		// king in check after the move ?
		piece[Piece.none] ^= mask[move.to].bit | mask[move.from].bit;
		if (victim) {
			color[enemy] ^= mask[move.to].bit;
			piece[Piece.none] ^= mask[move.to].bit;
		} else if (p == Piece.pawn && move.to == stack[ply].enpassant) {
			immutable x = toSquare(file(move.to), rank(move.from));
			color[enemy] ^= mask[x].bit;
			piece[Piece.none] ^= mask[x].bit;
		}
		Square k = (p == Piece.king ? move.to : firstSquare(piece[Piece.king] & color[player]));
		inCheck = isSquareAttacked(k, enemy);
		if (victim) {
			color[enemy] ^= mask[move.to].bit;
			piece[Piece.none] ^= mask[move.to].bit;
		} else if (p == Piece.pawn && move.to == stack[ply].enpassant) {
			immutable x = toSquare(file(move.to), rank(move.from));
			color[enemy] ^= mask[x].bit;
			piece[Piece.none] ^= mask[x].bit;
		}
		piece[Piece.none] ^= mask[move.to].bit | mask[move.from].bit;
	
		if (inCheck) return fail(Reason.kingInCheck);

		debug {
			Moves moves = void;
			moves.generate(this);
			foreach (m; moves) if (move == m) return true;
			return fail(Reason.illegalMissed);
		}

		return true;
	}

	/* get next Attacker to compute SEE */
	Piece nextVictim(ref ulong [Color.size] board, in Square to, in Color c, ref Piece [Color.size] last) pure const {
		immutable Color enemy = opponent(c);
		immutable ulong P = board[c];
		immutable ulong occupancy = board[c] | board[enemy];
		ulong attacker;
		static immutable Piece [Piece.size] next = [Piece.none, Piece.pawn, Piece.knight, Piece.bishop, Piece.rook, Piece.bishop, Piece.size];

		// remove the last attacker and return the victim
		Piece victim(in Piece p) {
			board[c] ^= (attacker & -attacker);
			last[c] = p;
			return p;
		}

		// loop...
		switch (next[last[c]]) {
		case Piece.pawn:
			attacker = attack!(Piece.pawn)(to, piece[Piece.pawn] & P, occupancy, enemy);
			if (attacker) return victim(Piece.pawn);
			goto case;
		case Piece.knight:
			attacker = attack!(Piece.knight)(to, piece[Piece.knight] & P);
			if (attacker) return victim(Piece.knight);
			goto case;
		case Piece.bishop:
			attacker = attack!(Piece.bishop)(to, piece[Piece.bishop] & P, occupancy);
			if (attacker) return victim(Piece.bishop);
			goto case;
		case Piece.rook:
			attacker = attack!(Piece.rook)(to, piece[Piece.rook] & P, occupancy);
			if (attacker) return victim(Piece.rook);
		// queen
			attacker = attack!(Piece.queen)(to, piece[Piece.queen] & P, occupancy);
			if (attacker) return victim(Piece.queen);
		// king
			attacker = attack!(Piece.king)(to, piece[Piece.king] & P);
			if (attacker) return victim(Piece.king);
			goto default;
		default:
			return Piece.none;
		}
	}

	/* SEE of a move (before playing it) */
	int see(in Move move) const pure {
		immutable Square to = move.to;
		immutable Color enemy = opponent(player);
		immutable Piece p = toPiece(cpiece[move.from]);
		ulong [Color.size] board = color;
		Piece [Color.size] last = [Piece.pawn, Piece.pawn];
		Piece victim = toPiece(cpiece[to]);
		int score = seeValue[victim], α = score - seeValue[p], β = score;

		if (α <= 0) {
			board[player] ^= mask[move.from].bit;
			score -= seeValue[p];
			if ((victim = nextVictim(board, to, enemy, last)) == Piece.none) return β;
			while (true) {
				score += seeValue[victim];
				if (score <= α || (victim = nextVictim(board, to, player, last)) == Piece.none) return α;
				if (score < β) β = score;

				score -= seeValue[victim];
				if (score >= β || (victim = nextVictim(board, to, enemy, last)) == Piece.none) return β;
				if (score > α) α = score;
			}
		}

		return score;
	}

	/* run perft: test correctness & speed of the move generator */
	ulong perft (in int depth, in bool div = false) {
		Moves moves = void;
		ulong count, total;

		moves.generate(this);

		if (!div && depth == 1) return moves.length;

		foreach (Move move; moves) {
			update(move);
				if (div && depth == 1) count = 1;
				else count = perft(depth - 1);
				total += count;
				if (div) writefln("%5s %16d", move.toString(), count);
			restore(move);
		}

		return total;
	}
}

/* perft with arguments */
void perft(string[] arg, Board init) {
	Board board = init ? init : new Board;
	int depth = 6;
	bool div, help;
	ulong total;
	Chrono t;
	string fen;

	if (init) {
		getopt(arg, "div", &div, "depth|d", &depth, "help|h", &help);
		if (help) writeln("perft [--depth <depth>] [--div] [--help]");
	} else {
		getopt(arg, "fen|f", &fen, "div", &div, "depth|d", &depth, "help|h", &help);
		if (help) writeln("perft [--depth <depth>] [--div] [--fen <fen>] [--help]");
		if (fen.length > 0) board.set(fen);
		else board.set();
	}
	writeln(board);

	t.start();
	total = board.perft(depth, div);
	writefln("perft %2d : %15d leaves in %10.3f s %12.0f leaves/s", depth, total, t.time(), total / t.time());
}

/* Test the correctness of the move generator */
unittest {
	struct TestBoard {
		string comments;
		string fen;
		int depth;
		ulong result;
	}

	TestBoard [] tests = [
		{"1. Initial position ", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 6, 119060324},
		{"2.", "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -", 5, 193690690},
		{"3.", "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -", 7, 178633661},
		{"4.", "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 6, 706045033},
		{"5.", "rnbqkb1r/pp1p1ppp/2p5/4P3/2B5/8/PPP1NnPP/RNBQK2R w KQkq - 0 6", 3, 53392},
		{"6.", "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 6, 6923051137},
		{"7.", "8/5bk1/8/2Pp4/8/1K6/8/8 w - d6 0 1", 6, 824064},
		{"8. Enpassant capture gives check", "8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1", 6, 1440467},
		{"9. Short castling gives check", "5k2/8/8/8/8/8/8/4K2R w K - 0 1", 6, 661072},
		{"10. Long castling gives checs", "3k4/8/8/8/8/8/8/R3K3 w Q - 0 1", 6, 803711},
		{"11. Castling", "r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1", 4, 1274206},
		{"12. Castling prevented", "r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1", 4, 1720476},
		{"13. Promote out of check", "2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1", 6, 3821001},
		{"14. Discovered check", "8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1", 5, 1004658},
		{"15. Promotion gives check", "4k3/1P6/8/8/8/8/K7/8 w - - 0 1", 6, 217342},
		{"16. Underpromotion gives check", "8/P1k5/K7/8/8/8/8/8 w - - 0 1", 6, 92683},
		{"17. Self stalemate", "K1k5/8/P7/8/8/8/8/8 w - - 0 1", 6, 2217},
		{"18. Stalemate/Checkmate", "8/k1P5/8/1K6/8/8/8/8 w - - 0 1", 7, 567584},
		{"19. Double check", "8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1", 4, 23527},
	];
	Board b = new Board;

	foreach (test; tests) {
		write("Test ", test.comments);
		b.set(test.fen); assert(b.perft(test.depth) == test.result);
		b.mirror(); assert(b.perft(test.depth) == test.result);
		writeln(" passed"); stdout.flush();
	}
}

