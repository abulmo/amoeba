/*
 * File eval.d
 * Evaluation function
 * © 2016 Richard Delorme
 */

module eval;

import board, kpk, move, util, weight;
import std.stdio, std.conv, std.algorithm;

/* 
 * Value
 */
struct Value {
	int opening;
	int endgame;

	/* operator overloading: a + b; c * d; etc. apply the operator to each member data */
	Value opBinary(string op)(in Value s) const {
		Value r = { mixin("opening " ~ op ~ " s.opening"), mixin("endgame " ~ op ~ " s.endgame") };
		return r;
	}

	/* operator overloading: a + v, b * v; apply the operator to each member data */
	Value opBinary(string op)(in int v) const {
		Value r = {mixin("opening " ~ op ~ " v"), mixin("endgame " ~ op ~ " v")};
		return r;
	}

	/* assignment operator overloading: apply the operator to each member data */
	void opOpAssign(string op)(in Value s) {
		mixin("opening "~op~"= s.opening;");
		mixin("endgame "~op~"= s.endgame;");
	}

	/* assignment operator overloading: apply the operator to each member data */
	void opOpAssign(string op)(in int v) {
		mixin("opening "~op~"= v;");
		mixin("endgame "~op~"= v;");
	}
}


/*
 * Evaluation function
 */
final class Eval {
private:
	struct PawnStructure {
		Value material;
		Value positional;
	}
	struct Weight {
		Value [Piece.size] material;
		Value [Square.size][Piece.size] positional;
		Value [Piece.size] mobility;
		Value [Piece.size] attack;
		Value [Piece.size] defense;
		Value [Piece.size] kingAttack;
		Value [Piece.size] kingDefense;
		PawnStructure passedPawn, candidatePawn, isolatedPawn, doublePawn;
		Value bishopPair;
		Value materialImbalance;
		Value tempo;
	}
	struct Stack {
		ulong [Color.size] kingZone;
		Value [Color.size] value;
		int [Color.size] nPiece;
		int stage;
	}
	struct PawnEntry {
		ulong code;
		Value [Color.size] value;
	}

	static immutable int [Piece.size] stageValue = [0, 0, 3, 3, 5, 10, 0];

	enum ε = 170;
	enum centipawn = 1024;
	enum halfcentipawn = 512;
	Weight coeff;
	Stack [Limits.plyMax + 1] stack;
	PawnEntry [] pawnTable;
	int ply;

	/* compute the attractive force of a target square x to a distant square y */
	static double attraction(in Square x, in Square y) {
		int r = rank(x) - rank(y);
		int f = file(x) - file(y);
		int d = (r * r + f * f);
		return d == 0 ? 2.0 : 1.0 / d;
	}

	/* scale a floating point coeff & round it to an integer n so that n * 64 = 1 centipawn (1024) */
	static int scale(in double w, in double f = 1600) {
		return cast (int) (f * w + (w > 0 ? 0.5 : w < 0 ? -0.5 : 0.0));
	}

	/* adjust pawn positional coeff, so that min positional = 0 */
	static void adjustPawn(string phase) (ref Value [Square.size] p) {
		foreach(x; Square.a1 .. Square.a2) mixin("p[x]."~phase) = 0;
		foreach(x; Square.a8 .. Square.size) mixin("p[x]."~phase) = 0;
		int m = int.max;
		foreach(x; Square.a2 .. Square.a8) m = min(m, mixin("p[x]."~phase));
		foreach(x; Square.a2 .. Square.a8) mixin("p[x]."~phase) -= m;
	}

	/* Build positional array coeffs from an array of attractive squares */
	static void buildPositional(string phase)(ref Value [Square.size] positional, in Square [] y, in double a, in bool isPawn) {
		double w;	
		double [Square.size] p;
	
		foreach (Square x; Square.a1 .. Square.size) {
			w = attraction(x, y[0]);
			foreach (i; 1 .. y.length) w = max(attraction(x, y[i]), w);
			p[x] = a * w;
		}

		if (!isPawn) {
			double m = 0.0; foreach (x; Square.a1 .. Square.size) m += p[x]; m /= Square.size;
			foreach (x; Square.a1 .. Square.size) p[x] -= m;
		}

		foreach(x; Square.a1 .. Square.size) mixin("positional[x]."~phase) += scale(p[x]);
	}

	/* remove a piece */
	void remove(in Piece p, in Color c, in Square x) {
		Stack *s = &stack[ply];
		s.value[c] -= coeff.positional[p][forward(x, c)] + coeff.material[p];
		if (p > Piece.pawn) --s.nPiece[c];
		s.stage -= stageValue[p];
	}

	/* set a piece */
	void set(in Piece p, in Color c, in Square x) {
		Stack *s = &stack[ply];
		s.value[c] += coeff.positional[p][forward(x, c)] + coeff.material[p];
		if (p > Piece.pawn) ++s.nPiece[c];
		s.stage += stageValue[p];
	}

	/* move a piece */
	void deplace(in Piece p, in Color c, in Square from, in Square to) {
		Stack *s = &stack[ply];
		s.value[c] -= coeff.positional[p][forward(from, c)];
		s.value[c] += coeff.positional[p][forward(to, c)];
	}

	/* update material imbalance */
	void updateImbalance(in Color player, in Color enemy) {
		Stack *s = &stack[ply];
		if (s.nPiece[player] == s.nPiece[enemy] + 1) {
			s.value[player] += coeff.materialImbalance;
		} else if (s.nPiece[player] == s.nPiece[enemy]) {
			s.value[enemy] -= coeff.materialImbalance;
		}
	}

	/* mobility / attack / defense evaluation components */
	Value influence(Piece p)(in Board b, in Color player) const {
		immutable Color enemy = opponent(player);
		immutable ulong P = b.color[player];
		immutable ulong E = b.color[enemy];
		immutable ulong V = b.piece[Piece.none];
		immutable ulong O = ~V;
		ulong attacker = b.piece[p] & P, a;
		const Stack *s = &stack[ply];
		int f;
		Value v;
		Square x;

		static if (p != Piece.king) {
			f = countBits(attacker & s.kingZone[player]);
			v = coeff.kingDefense[p] * f;
		}

		while (attacker) {
			x = popSquare(attacker);
			a = Board.coverage!p(x, O, player);
			f = countBits(a & V);
			v += coeff.mobility[p] * f;
			f = countBits(a & E);
			v += coeff.attack[p] * f;
			f = countBits(a & P);
			v += coeff.defense[p] * f;
			static if (p != Piece.king) {
				f = countBits(a & s.kingZone[enemy]);
				v += coeff.kingAttack[p] * f;
			}
		}

		return v;
	}

	/* pawn structure */
	Value pawnStructure(in Board b, in Color player) const {
		immutable Color enemy = opponent(player);
		immutable ulong pawns = b.piece[Piece.pawn];
		immutable ulong [Color.size] pawn = [pawns & b.color[0], pawns & b.color[1]];
		ulong attacker = pawn[player];
		Value v, mat, pos; 
		
		while (attacker) {
			auto x = popSquare(attacker);
			mat = pos = Value.init;
			// open file ?
			if ((pawns & b.mask[x].openFile[player]) == 0) {
				// passed pawn ?
				if ((pawn[enemy] & b.mask[x].passedPawn[player]) == 0) {
					mat += coeff.passedPawn.material;
					pos += coeff.passedPawn.positional;
				// candidate pawn
				} else {
					mat += coeff.candidatePawn.material;
					pos += coeff.candidatePawn.positional;
				}
			}
			// isolated pawn ?
			if ((pawn[player] & b.mask[x].isolatedPawn) == 0) {
				mat += coeff.isolatedPawn.material;
				pos += coeff.isolatedPawn.positional;
			}
			// double pawn ?
			if (attacker & b.mask[x].file) {
				mat += coeff.doublePawn.material;
				pos += coeff.doublePawn.positional;
			}				
			v += mat + pos * coeff.positional[player][forward(x, player)] / centipawn;
		}
		return v;
	}


	/* pawn structure with cache */
	Value pawnStructure(in Board b) const {
		immutable Color player = b.player;
		immutable Color enemy = opponent(player);
		PawnEntry h = pawnTable[b.pawnKey & (pawnTable.length - 1)];
		if (h.code != b.pawnKey) {
			h.code = b.pawnKey;
			h.value[player] = pawnStructure(b, player);
			h.value[enemy]  = pawnStructure(b, enemy);
		}
		return h.value[player] - h.value[enemy];
	}

	/* convert value to centipawns */
	int toCentipawns(in Value value) const {
		const Stack *s = &stack[ply];
		int v = value.opening * s.stage + value.endgame * (64 - s.stage);
		if (v < 0) v -= halfcentipawn; else if (v > 0) v += halfcentipawn;
		return v / centipawn;
	}


public:
	/* clear */
	void clear() {
		pawnTable[] = PawnEntry.init;
	}

	/* Constructor */
	this(const ref double [] w = weight.initialWeights, in size_t size = 65536) {
		size_t i;
		immutable Square [] pawnCenter = [Square.d4, Square.e4];
		immutable Square [] pawnAdvance = [Square.b8, Square.c8, Square.d8, Square.e8, Square.f8, Square.g8];
		immutable Square [] knightOutpost = [Square.c6, Square.d6, Square.e6, Square.f6];
		immutable Square [] knightCenter = [Square.d4, Square.e4, Square.d5, Square.e5];
		immutable Square [] bishopCenter = [Square.c3, Square.f3, Square.c6, Square.f6];
		immutable Square [] rook7thRank = [Square.b7, Square.c7, Square.d7, Square.e7, Square.f7, Square.g7];
		immutable Square [] rookCenter = [Square.a4, Square.b4, Square.c4, Square.d4, Square.e4, Square.f4, Square.g4, Square.h4, 
		                                  Square.a5, Square.b5, Square.c5, Square.d5, Square.e5, Square.f5, Square.g5, Square.h5];
		immutable Square [] queenCenter = [Square.d4, Square.e4, Square.d5, Square.e5];
		immutable Square [] kingCastle = [Square.b1, Square.g1];
		immutable Square [] kingCenter = [Square.d4, Square.e4, Square.d5, Square.e5];

		// allocate the pawn hash table.
		pawnTable.length = size;

		// opening
		// material
		foreach(p; Piece.pawn .. Piece.king) coeff.material[p].opening = scale(w[i++]);
		coeff.material[Piece.king].opening = 0;
		coeff.bishopPair.opening = scale(w[i++]);
		coeff.materialImbalance.opening = scale(w[i++]);

		// mobility
		foreach(p; Piece.pawn .. Piece.size) coeff.mobility[p].opening    = scale(w[i++]);
		foreach(p; Piece.pawn .. Piece.size) coeff.attack[p].opening      = scale(w[i++]);
		foreach(p; Piece.pawn .. Piece.size) coeff.defense[p].opening     = scale(w[i++]);
		foreach(p; Piece.pawn .. Piece.king) coeff.kingAttack[p].opening  = scale(w[i++]);
		foreach(p; Piece.pawn .. Piece.king) coeff.kingDefense[p].opening = scale(w[i++]);

		// positional
		buildPositional!"opening"(coeff.positional[Piece.pawn],   pawnCenter,    w[i++], true);
		buildPositional!"opening"(coeff.positional[Piece.pawn],   pawnAdvance,   w[i++], true);
		buildPositional!"opening"(coeff.positional[Piece.knight], knightOutpost, w[i++], false);
		buildPositional!"opening"(coeff.positional[Piece.bishop], bishopCenter,  w[i++], false);
		buildPositional!"opening"(coeff.positional[Piece.rook],   rook7thRank,   w[i++], false);
		buildPositional!"opening"(coeff.positional[Piece.rook],   rookCenter,    w[i++], false);
		buildPositional!"opening"(coeff.positional[Piece.queen],  queenCenter,   w[i++], false);
		buildPositional!"opening"(coeff.positional[Piece.king],   kingCastle,    w[i++], false);
		adjustPawn!"opening"(coeff.positional[Piece.pawn]);

		// starting square bonus/malus
		coeff.positional[Piece.pawn][Square.d2].opening   += scale(w[i]) ;
		coeff.positional[Piece.pawn][Square.e2].opening   += scale(w[i++]);
		coeff.positional[Piece.knight][Square.b1].opening += scale(w[i]);
		coeff.positional[Piece.knight][Square.g1].opening += scale(w[i++]);
		coeff.positional[Piece.bishop][Square.c1].opening += scale(w[i]);
		coeff.positional[Piece.bishop][Square.f1].opening += scale(w[i++]);
		coeff.positional[Piece.rook][Square.a1].opening   += scale(w[i]);
		coeff.positional[Piece.rook][Square.h1].opening   += scale(w[i++]);
		coeff.positional[Piece.queen][Square.d1].opening  += scale(w[i++]);
		coeff.positional[Piece.king][Square.e1].opening   += scale(w[i++]);

		// pawn structure
		coeff.passedPawn.material.opening      = scale(w[i++]);
		coeff.candidatePawn.material.opening   = scale(w[i++]);
		coeff.isolatedPawn.material.opening    = scale(w[i++]);
		coeff.doublePawn.material.opening      = scale(w[i++]);
		coeff.passedPawn.positional.opening    = scale(w[i++], centipawn);
		coeff.candidatePawn.positional.opening = scale(w[i++], centipawn);
		coeff.isolatedPawn.positional.opening  = scale(w[i++], centipawn);
		coeff.doublePawn.positional.opening    = scale(w[i++], centipawn);

		coeff.tempo.opening = scale(w[i++]);

		// endgame
		// material
		foreach(p; Piece.pawn .. Piece.king) coeff.material[p].endgame = scale(w[i++]);
		coeff.material[Piece.king].endgame = 0;
		coeff.bishopPair.endgame = scale(w[i++]);
		coeff.materialImbalance.endgame = scale(w[i++]);

		// mobility
		foreach(p; Piece.pawn .. Piece.size) coeff.mobility[p].endgame    = scale(w[i++]);
		foreach(p; Piece.pawn .. Piece.size) coeff.attack[p].endgame      = scale(w[i++]);
		foreach(p; Piece.pawn .. Piece.size) coeff.defense[p].endgame     = scale(w[i++]);
		foreach(p; Piece.pawn .. Piece.king) coeff.kingAttack[p].endgame  = scale(w[i++]);
		foreach(p; Piece.pawn .. Piece.king) coeff.kingDefense[p].endgame = scale(w[i++]);

		// positional
		buildPositional!"endgame"(coeff.positional[Piece.pawn],   pawnAdvance,  w[i++], true);
		buildPositional!"endgame"(coeff.positional[Piece.knight], knightCenter, w[i++], false);
		buildPositional!"endgame"(coeff.positional[Piece.bishop], bishopCenter, w[i++], false);
		buildPositional!"endgame"(coeff.positional[Piece.king],   kingCenter,   w[i++], false);
		buildPositional!"endgame"(coeff.positional[Piece.queen],  queenCenter,  w[i++], false);
		adjustPawn!"endgame"(coeff.positional[Piece.pawn]);
	
		// pawn structure
		coeff.passedPawn.material.endgame      = scale(w[i++]);
		coeff.candidatePawn.material.endgame   = scale(w[i++]);
		coeff.isolatedPawn.material.endgame    = scale(w[i++]);
		coeff.doublePawn.material.endgame      = scale(w[i++]);
		coeff.passedPawn.positional.endgame    = scale(w[i++], centipawn);
		coeff.candidatePawn.positional.endgame = scale(w[i++], centipawn);
		coeff.isolatedPawn.positional.endgame  = scale(w[i++], centipawn);
		coeff.doublePawn.positional.endgame    = scale(w[i++], centipawn);

		coeff.tempo.endgame = scale(w[i++]);

		debug writeln(coeff);

	}

	/* start a new Eval */
	void set(in Board board) {
		Stack *s = &stack[0];

		ply = 0;
		s.value[Color.white] = s.value[Color.black] = Value.init;
		s.stage = 0;

		foreach(Color c; Color.white .. Color.size)
		foreach(Piece p; Piece.pawn .. Piece.size) {
			ulong b = board.color[c] & board.piece[p];
			immutable n = countBits(b);
			s.stage += stageValue[p] * n;
			s.value[c] += coeff.material[p] * n;
			if (p == Piece.bishop && n >= 2) {
				s.value[c] += coeff.bishopPair;
			}
			if (p == Piece.king) {
				Square x = firstSquare(b);
				s.kingZone[c] = Board.coverage!(Piece.king)(x);
			}
			while (b) {
				Square x = popSquare(b);
				s.value[c] += coeff.positional[p][forward(x, c)];
			}
		}
		s.nPiece[Color.white] = countBits(board.color[Color.white] & ~board.piece[Piece.pawn]);
		s.nPiece[Color.black] = countBits(board.color[Color.black] & ~board.piece[Piece.pawn]);

		if (s.nPiece[Color.white] > s.nPiece[Color.black]) {
			s.value[Color.white] += coeff.materialImbalance;
		} else if (s.nPiece[Color.white] < s.nPiece[Color.black]) {
			s.value[Color.black] += coeff.materialImbalance;
		}
	}

	/* update the eval after a move */
	void update(in Board b, in Move m) {
		immutable Color enemy = b.player;
		immutable Color player = opponent(enemy);
		immutable Piece p = m.promotion ? Piece.pawn : toPiece(b[m.to]);
		immutable Piece v = b.stack[b.ply].victim;
		Stack *s = &stack[ply + 1];

		++ply; stack[ply] = stack[ply - 1];

		deplace(p, player, m.from, m.to);
		if (v) {
			remove(v, enemy, m.to);
			if (v > Piece.pawn) updateImbalance(player, enemy);
			if (v == Piece.bishop && b.count(Piece.bishop, enemy) == 1) {
				s.value[enemy] -= coeff.bishopPair;
			}
		}

		if (p == Piece.pawn) {
			if (m.promotion) {
				remove(p, player, m.to);
				set(m.promotion, player, m.to);
				updateImbalance(player, enemy);
				if (m.promotion == Piece.bishop && b.count(Piece.bishop, player) == 2) {
					s.value[player] += coeff.bishopPair;
				}
			} else if (b.stack[b.ply - 1].enpassant == m.to) {
				remove(Piece.pawn, enemy, toSquare(file(m.to), rank(m.from)));
			}
		}
		if (p == Piece.king) {
			s.kingZone[player] = Board.coverage!(Piece.king)(m.to);
			if (m.to == m.from + 2) deplace(Piece.rook, player, cast (Square) (m.from + 3), cast (Square) (m.from + 1));
			if (m.to == m.from - 2) deplace(Piece.rook, player, cast (Square) (m.from - 4), cast (Square) (m.from - 1));
		}
	}

	/* restore the evaluation */
	void restore() {
		--ply;
	}

	/* stage */
	int stage() const @property {
		return stack[ply].stage;
	}

	/* functor: lazy evaluation */
	int opCall(in Board b) const {
		immutable Color player = b.player;
		immutable Color enemy = opponent(player);
		const Stack *s = &stack[ply];
		immutable Value value = s.value[player] - s.value[enemy] + coeff.tempo;

		return toCentipawns(value);
	}

	/* functor: complete evaluation if the lazy evaluation is αβ ± ε-bounded */
	int opCall(in Board b, in int α, in int β) const {
		immutable Color player = b.player;
		immutable Color enemy = opponent(player);
		const Stack *s = &stack[ply];
		Value value = s.value[player] - s.value[enemy] + coeff.tempo;
		immutable lazyValue = toCentipawns(value);

		if (α - ε <= lazyValue && lazyValue <= β + ε) {
			// pieces inluence (mobility / attack / defense
			value += influence!(Piece.pawn)(b, player)   - influence!(Piece.pawn)(b, enemy);
			value += influence!(Piece.knight)(b, player) - influence!(Piece.knight)(b, enemy);
			value += influence!(Piece.bishop)(b, player) - influence!(Piece.bishop)(b, enemy);
			value += influence!(Piece.rook)(b, player)   - influence!(Piece.rook)(b, enemy);
			value += influence!(Piece.queen)(b, player)  - influence!(Piece.queen)(b, enemy);
			value += influence!(Piece.king)(b, player)   - influence!(Piece.king)(b, enemy);
			// pawnStructure
			value += pawnStructure(b);
		}

		// some value corrections
		value = kpk.rescale(b, value); // kpk table

		if (b.fifty > 50) { // diminish value when fifty move rule approach
			if (b.fifty >= 100) value = Value.init;
			else value *= (100 - b.fifty) / 50;
		}

		return toCentipawns(value);
	}
}

