/*
 * file kpk.d
 * KPK base
 * © 2016-2017 Richard Delorme
 */

/* TODO: pack it onto a bitbase ? */

module kpk;

import board, eval, util;
import std.algorithm, std.stdio;

private:
enum {illegal = 0, unknown = 1, draw = 2, win = 4};
enum size = 24 * 64 * 64 * 2;
shared byte [size] result;

/* compute the index to the result table from the square coordinates */
int getIndex(in Square p, in Square wk, in Square bk, in Color c) {
	int x = 4 * rank(p) + file(p) - 4;
	return (x << 13) | (wk << 7) | (bk << 1) | c;
}

/* from an index, compute the position */
void getSquares(in int i, ref Square p, ref Square wk, ref Square bk, ref Color player) {
	int x = i >> 13;
	p  = toSquare(x & 3, x / 4 + 1);
	wk = cast (Square) ((i >> 7) & 63);
	bk = cast (Square) ((i >> 1) & 63);
	player  = cast (Color) (i & 1);
}

/* bit x */
ulong bit(in int x) {
	return 1UL << x;
}

public:
/* initialise the result[] array */
void init () {
	struct Mask { ulong king, pawn; }
	Mask [Square.size] attacks;
	ulong attack;
	int done;
	Square p, wk, bk;
	Color player, enemy;

	// init attacks mask
	foreach(x; Square.a1 .. Square.size) {
		if (0 < rank(x) && rank(x) < 7) {
			if (file(x) > 0) attacks[x].pawn |= bit(x + 7);
			if (file(x) < 7) attacks[x].pawn |= bit(x + 9);			
		}
		for (int r = rank(x) - 1; r <= rank(x) + 1; ++r)
		for (int f = file(x) - 1; f <= file(x) + 1; ++f) {
			if (r == rank(x) && f == file(x)) continue;
			if (0 <= r && r < 8 && 0 <= f && f < 8) attacks[x].king |= bit(8 * r + f);
		}		
	}
	// initial setup: score obvious positions
	foreach (int i; 0 .. size) {
		getSquares(i, p, wk, bk, player);
		result[i] = unknown;
		if (wk == p || bk == p || wk == bk) result[i] = illegal; // square confusions
		else if (bit(wk) & attacks[bk].king) result[i] = illegal; // illegal touching kings
		else if (player == Color.white) {
			if ((bit(bk) & attacks[p].pawn)) result[i] = illegal; // illegal king captures
			else if (rank(p) == 6 && p + 8 != bk && p + 8 != wk) { // promotions
				if ((attacks[bk].king & bit(p + 8) & ~attacks[wk].king) == 0) result[i] = win; // safe
			}
		} else { // black king 
			if ((attacks[bk].king & bit(p) & ~attacks[wk].king) != 0) result[i] = draw; // pawn capture
			if ((attacks[bk].king & ~(attacks[p].pawn | attacks[wk].king)) == 0) result[i] = draw; // stalemate
		}
	}

	// score other positions iteratively by playing from them and looking at known results
	do {
		done = 0;
		foreach (int i; 0 .. size) {
			if (result[i] == unknown) {
				getSquares(i, p, wk, bk, player);
				enemy = opponent(player);
				byte r = illegal;
				if (player == Color.white) {
					// white king moves
					attack = (attacks[wk].king & ~attacks[bk].king & ~bit(p));
					while (attack) r |= result[getIndex(p, popSquare(attack), bk, enemy)];
					// white pawn moves
					if (rank(p) < 6) {
						r |= result[getIndex(cast (Square) (p + 8), wk, bk, enemy)];
						if (rank(p) == 1 && p + 8 != bk && p + 8 != wk) {
							r |= result[getIndex(cast (Square) (p + 16), wk, bk, enemy)];
						}
					}
					result[i] = r & win ? win : (r & unknown ? unknown : draw);
				} else {
					// black king moves
					attack = (attacks[bk].king & ~(attacks[p].pawn | attacks[wk].king));
					while (attack) r |= result[getIndex(p, wk, popSquare(attack), enemy)];
					result[i] = r & draw ? draw : (r & unknown ? unknown : win);
				}
				done += (result[i] != unknown);
			}
		}
	} while (done > 0);

	// remaining unknown results are draws by repetition or 50-moves rule
	foreach (i; 0 .. size) if (result[i] == unknown) result[i] = draw;
}

/* evaluate a kpk position, rescale the score according to win/draw/loss */
Value rescale(in Board b, in Value score) {
	const ulong P = b.piece[Piece.pawn];
	const ulong V = b.piece[Piece.none];
	
	if (countBits(P) == 1 && countBits(V) == 61) {
		const ulong W = b.color[Color.white];
		const ulong B = b.color[Color.black];
		const ulong K = b.piece[Piece.king];
		Square p = firstSquare(P);
		Square wk = firstSquare(K & W);
		Square bk = firstSquare(K & B);
		Color player = b.player;
	
		if ((P & B)) {
			p ^= 56; bk ^= 56; wk ^= 56;
			swap(bk, wk); player = opponent(player);
		}
		if (file(p) > 3) {
			p ^= 7; bk ^= 7; wk ^= 7;
		}

		int i = getIndex(p, wk, bk, player);
		
		if (result[i] == win) return score * 4;
		else return score / 16;
	}

	return score;
}

/* check correctness of algorithm */
unittest {
	int [5] count;

	init();

	write("Testing kpk..."); stdout.flush();
	foreach (r; result) count[r]++;
	claim(count[win] == 111282);
	claim(count[draw] == 54394);
	claim(count[unknown] == 0);
	claim(count[illegal] == 30932);
	writeln("ok"); stdout.flush();
}

