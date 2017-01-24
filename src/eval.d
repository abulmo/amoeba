/*
 * File eval.d
 * Evaluation function
 * © 2016-2017 Richard Delorme
 */

module eval;

import board, kpk, move, util, weight;
import std.stdio, std.conv, std.algorithm;

/* Score limits */
enum Score {mate = 30000, low = -29000, high = 29000, big = 3000}

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
		Value safePawnAdvance, unsafePawnAdvance;
		Value safePawnBlock, unsafePawnBlock;
		Value safePawnDouble, unsafePawnDouble;
		Value [Piece.size] safeMobility, unsafeMobility;
		Value [Piece.size] safeAttack, unsafeAttack;
		Value [Piece.size] safeDefense, unsafeDefense;
		Value [Piece.size] kingAttack;
		Value [Piece.size] kingDefense;
		Value kingShield, kingStorm;
		PawnStructure passedPawn, candidatePawn, isolatedPawn, doublePawn, backwardPawn, chainedPawn;
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
	ulong [Square.size] coverage;
	ulong [Color.size] attack;
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

	/* update material imbalance after capturing an enemy's piece or promoting to a player's piece */
	void updateImbalance(in Color player, in Color enemy) {
		Stack *s = &stack[ply];
		if (s.nPiece[player] == s.nPiece[enemy] + 1) {
			s.value[player] += coeff.materialImbalance;
		} else if (s.nPiece[player] == s.nPiece[enemy]) {
			s.value[enemy] -= coeff.materialImbalance;
		}
	}

	/* compute a bitboard of all squares attacked by a type of piece */
	void setCoverage(Piece p)(in Board b, in Color player) {
		static if (p == Piece.pawn) {
			const ulong O = ~b.piece[Piece.none];
			ulong attacker = b.piece[p] & b.color[player];

			while (attacker) {
				Square x = popSquare(attacker);
				coverage[x] = Board.coverage!p(x, O, player);
				attack[player] |= coverage[x];
			}

		}
	}
	
	/* compute a bitboard of all squares attacked by a type of piece */
	void setCoverage(Piece p)(in Board b) {
		const ulong O = ~b.piece[Piece.none];
		ulong attacker = b.piece[p];

		while (attacker) {
			const Square x = popSquare(attacker);
			coverage[x] = Board.coverage!p(x, O);
			const Color player = toColor(b[x]);
			attack[player] |= coverage[x];
		}
	}

	/* init the coverage[] & attack[] arrays */
	void initAttack(in Board b) {
		attack[] = 0;
		setCoverage!(Piece.pawn)(b, Color.white);
		setCoverage!(Piece.pawn)(b, Color.black);
		setCoverage!(Piece.knight)(b);
		setCoverage!(Piece.bishop)(b);
		setCoverage!(Piece.rook)(b);
		setCoverage!(Piece.queen)(b);
		setCoverage!(Piece.king)(b);
	}

	/* mobility / attack / defense evaluation components */
	Value influence(Piece p)(in Board b, in Color player) const {
		const Color enemy = opponent(player);
		const ulong P = b.color[player];
		const ulong E = b.color[enemy];
		const ulong V = b.piece[Piece.none];
		const ulong O = ~V;
		const ulong A = attack[enemy];
		ulong attacker = b.piece[p] & P, a;
		const Stack *s = &stack[ply];
		Value v;
		Square x;

		static if (p != Piece.king) {
			v = coeff.kingDefense[p] * countBits(attacker & s.kingZone[player]);
		}

		static if (p == Piece.pawn) {
			a = player == Color.white ? attacker << 8 : attacker >> 8;
			v += coeff.safePawnAdvance * countBits(a & V & ~A);
			v += coeff.unsafePawnAdvance * countBits(a & V & A);
		}

		while (attacker) {
			x = popSquare(attacker);
			a = coverage[x];
			v += coeff.safeMobility[p] * countBits(a & V & ~A);
			v += coeff.unsafeMobility[p] * countBits(a & V & A);
			v += coeff.safeAttack[p] * countBits(a & E & ~A);
			v += coeff.unsafeAttack[p] * countBits(a & E & A);
			v += coeff.safeDefense[p] * countBits(a & P & ~A);
			v += coeff.unsafeDefense[p] * countBits(a & P & A);
			static if (p != Piece.king) {
				v += coeff.kingAttack[p] * countBits(a & s.kingZone[enemy]);
			}
		}

		return v;
	}

	/* per square influence (for debugging purpose) */
	Value influence(Piece p)(in Board b, in Square x, in Color player) const {
		const Color enemy = opponent(player);
		const ulong P = b.color[player];
		const ulong E = b.color[enemy];
		const ulong V = b.piece[Piece.none];
		const ulong O = ~V;
		const ulong A = attack[enemy];
		ulong attacker = b.mask[x].bit, a;
		const Stack *s = &stack[ply];
		Value v;

		static if (p != Piece.king) {
			v = coeff.kingDefense[p] * countBits(attacker & s.kingZone[player]);
		}

		static if (p == Piece.pawn) {
			a = player == Color.white ? attacker << 8 : attacker >> 8;
			v += coeff.safePawnAdvance * countBits(a & V & ~A);
			v += coeff.unsafePawnAdvance * countBits(a & V & A);
			v += coeff.safePawnBlock * countBits(a & E & ~A);
			v += coeff.unsafePawnBlock * countBits(a & E & A);
			v += coeff.safePawnDouble * countBits(a & P & ~A);
			v += coeff.unsafePawnDouble * countBits(a & P & A);
		}

		a = coverage[x];
		v += coeff.safeMobility[p] * countBits(a & V & ~A);
		v += coeff.unsafeMobility[p] * countBits(a & V & A);
		v += coeff.safeAttack[p] * countBits(a & E & ~A);
		v += coeff.unsafeAttack[p] * countBits(a & E & A);
		v += coeff.safeDefense[p] * countBits(a & P & ~A);
		v += coeff.unsafeDefense[p] * countBits(a & P & A);
		static if (p != Piece.king) {
			v += coeff.kingAttack[p] * countBits(a & s.kingZone[enemy]);
		}

		return v;
	}

	/* pawn structure */
	Value pawnStructure(in Board b, in Color player) const {
		const Color enemy = opponent(player);
		const ulong pawns = b.piece[Piece.pawn];
		const ulong [Color.size] pawn = [pawns & b.color[0], pawns & b.color[1]];
		ulong attacker = pawn[player];
		Value v;

		if (attacker) {
			const Square *k = &b.xKing[0];
			const ulong shield = (b.mask[k[player]].openFile[player] | b.mask[k[player]].passedPawn[player]);
			const ulong storm  = (b.mask[k[enemy]].openFile[enemy] | b.mask[k[enemy]].passedPawn[enemy]);
			double vShield = 0.0, vStorm = 0.0;
		
			do {
				auto x = popSquare(attacker);
				Value mat, pos;

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
				// double pawn ?
				if (attacker & b.mask[x].file) {
					mat += coeff.doublePawn.material;
					pos += coeff.doublePawn.positional;
				}
				// isolated pawn ?
				if ((pawn[player] & b.mask[x].isolatedPawn) == 0) {
					mat += coeff.isolatedPawn.material;
					pos += coeff.isolatedPawn.positional;
				// backward pawn ? (no pawns behind on nearby files)
				} else if ((pawn[player] & b.mask[x].backwardPawn[player]) == 0) {
					mat += coeff.backwardPawn.material;
					pos += coeff.backwardPawn.positional;
				// chained pawn ? (protected by own pawn(s))
				} else if (pawn[player] & b.mask[x].pawnAttack[enemy]) {
					mat += coeff.chainedPawn.material;
					pos += coeff.chainedPawn.positional;
				}
				v += mat + (pos * coeff.positional[player][forward(x, player)]) / centipawn;

				if (b.mask[x].bit & shield) vShield += attraction(x, k[player]);
				if (b.mask[x].bit & storm)  vStorm  += attraction(x, k[enemy]);
			} while (attacker);

			v.opening += to!int(coeff.kingShield.opening * vShield + coeff.kingStorm.opening * vStorm);
		}

		return v;
	}

	/* pawn structure */
	void showPawnStructure(in Board b, in Color player) const {
		const Color enemy = opponent(player);
		const ulong pawns = b.piece[Piece.pawn];
		const ulong [Color.size] pawn = [pawns & b.color[0], pawns & b.color[1]];
		const Square [Color.size] k = b.xKing;
		const ulong shield = (b.mask[k[player]].openFile[player] | b.mask[k[player]].passedPawn[player]);
		const ulong storm  = (b.mask[k[enemy]].openFile[enemy] | b.mask[k[enemy]].passedPawn[enemy]);
		ulong attacker = pawn[player];
		
		while (attacker) {
			auto x = popSquare(attacker);

			write(x, ": ");

			if ((pawns & b.mask[x].openFile[player]) == 0) {
				if ((pawn[enemy] & b.mask[x].passedPawn[player]) == 0) write("passed, ");
				else write("candidate, ");
			}
			if (attacker & b.mask[x].file) write("doubled, ");
			if ((pawn[player] & b.mask[x].isolatedPawn) == 0) write("isolated, ");
			else if ((pawn[player] & b.mask[x].backwardPawn[player]) == 0) write("backward, ");
			else if (pawn[player] & b.mask[x].pawnAttack[enemy]) write("chained, ");
			if (b.mask[x].bit & shield) write("in shield, ");
			if (b.mask[x].bit & shield) write("on storm, ");
			writeln();
		}
	}

	/* pawn structure with cache */
	Value pawnStructure(in Board b) const {
		const Color player = b.player;
		const Color enemy = opponent(player);
		PawnEntry h = pawnTable[b.pawnKey & (pawnTable.length - 1)];
		if (h.code != b.pawnKey) {
			h.code = b.pawnKey;
			h.value[player] = pawnStructure(b, player);
			h.value[enemy]  = pawnStructure(b, enemy);
		}
		return h.value[player] - h.value[enemy];
	}

	/* per square pawn structure */
	Value pawnStructure(in Board b, in Square x, in Color player) const {
		const Color enemy = opponent(player);
		const ulong pawns = b.piece[Piece.pawn];
		const ulong [Color.size] pawn = [pawns & b.color[0], pawns & b.color[1]];
		ulong attacker = pawn[player];
		Value v;
		
		Value mat, pos;

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
		// double pawn ?
		if (attacker & b.mask[x].file) {
			mat += coeff.doublePawn.material;
			pos += coeff.doublePawn.positional;
		}
		// isolated pawn ?
		if ((pawn[player] & b.mask[x].isolatedPawn) == 0) {
			mat += coeff.isolatedPawn.material;
			pos += coeff.isolatedPawn.positional;
		// backward pawn ? (no pawns behind on nearby files)
		} else if ((pawn[player] & b.mask[x].backwardPawn[player]) == 0) {
			mat += coeff.backwardPawn.material;
			pos += coeff.backwardPawn.positional;
		// chained pawn ? (protected by own pawn(s))
		} else if (pawn[player] & b.mask[x].pawnAttack[enemy]) {
			mat += coeff.chainedPawn.material;
			pos += coeff.chainedPawn.positional;
		}
		v += mat + pos * coeff.positional[player][forward(x, player)] / centipawn;

		return v;
	}

	/* convert value to centipawns */
	int toCentipawns(in Value value) const {
		const Stack *s = &stack[ply];
		int v = value.opening * s.stage + value.endgame * (64 - s.stage);
		if (v < 0) v -= halfcentipawn; else if (v > 0) v += halfcentipawn;
		return v / centipawn;
	}

	/* eval a single square */
	Value evalSquare(in Board b, in Square x) {
		const Piece p = toPiece(b.cpiece[x]);
		const Color c = toColor(b.cpiece[x]);
		Value v;

		if (p > Piece.none) {
			v = coeff.material[p];
			v += coeff.positional[p][forward(x, c)];
			switch(p) {
				case Piece.pawn:   v += influence!(Piece.pawn)(b, x, c) + pawnStructure(b, x, c); break;
				case Piece.knight: v += influence!(Piece.knight)(b, x, c); break;
				case Piece.bishop: v += influence!(Piece.bishop)(b, x, c); break;
				case Piece.rook:   v += influence!(Piece.rook)(b, x, c); break;
				case Piece.queen:  v += influence!(Piece.queen)(b, x, c); break;
				case Piece.king:   v += influence!(Piece.king)(b, x, c); break;
				default: break;
			}
		}
		return v;
	}

	/* value of a piece */
	Value pieceValue(in Piece p, in Color c, in Square x) const {
		return coeff.material[p] + coeff.positional[p][forward(x, c)];
	}

	/* display eval weights for a single stage */
	void showWeight(string phase)() const {
		write(phase ~ ":\nmaterial: ");
		foreach(p; Piece.pawn .. Piece.king) write(mixin("coeff.material[p]." ~ phase) / 16, ", ");
		writefln("bishop pair: %d, imbalance: %d", mixin("coeff.bishopPair." ~ phase) / 16, mixin("coeff.materialImbalance." ~ phase) / 16);
		writeln("positional:");
		foreach(p; Piece.pawn .. Piece.size) {
			write(p, ":");
			foreach(Square x; Square.a1 .. Square.size) {
				if (file(x) == 0) write("\n\t");
				writef("%+4d, ", mixin("coeff.positional[p][x]." ~ phase) / 16);
			}
			writeln("");
		}
		write("safe mobility: ");
		foreach(p; Piece.pawn .. Piece.size) writef("%+4d, ", mixin("coeff.safeMobility[p]." ~ phase) / 16);
		writeln("");
		write("unsafe mobility: ");
		foreach(p; Piece.pawn .. Piece.size) writef("%+4d, ", mixin("coeff.unsafeMobility[p]." ~ phase) / 16);
		writeln("");
		write("safe attack: ");
		foreach(p; Piece.pawn .. Piece.size) writef("%+4d, ", mixin("coeff.safeAttack[p]." ~ phase) / 16);
		writeln("");
		write("unsafe attack: ");
		foreach(p; Piece.pawn .. Piece.size) writef("%+4d, ", mixin("coeff.unsafeAttack[p]." ~ phase) / 16);
		writeln("");
		write("safe defense: ");
		foreach(p; Piece.pawn .. Piece.size) writef("%+4d, ", mixin("coeff.safeDefense[p]." ~ phase) / 16);
 		writeln("");
		write("unsafe defense: ");
		foreach(p; Piece.pawn .. Piece.size) writef("%+4d, ", mixin("coeff.unsafeDefense[p]." ~ phase) / 16);
		writeln("");
		write("King attack: ");
		foreach(p; Piece.pawn .. Piece.king) writef("%+4d, ", mixin("coeff.kingAttack[p]." ~ phase) / 16);
		writeln("");
		write("King defense: ");
		foreach(p; Piece.pawn .. Piece.king) writef("%+4d, ", mixin("coeff.kingDefense[p]." ~ phase) / 16);
		writeln("");
		writefln("King shield: %+4d", mixin("coeff.kingShield." ~ phase) / 16);
		writefln("King storm: %+4d", mixin("coeff.kingStorm." ~ phase) / 16);
		writeln("Pawn structure : material - positional");
		writeln("passed pawn: ", mixin("coeff.passedPawn.material." ~ phase) / 16,  ", ", mixin("coeff.passedPawn.positional." ~ phase) / 10, "%");
		writeln("candidate pawn: ", mixin("coeff.candidatePawn.material." ~ phase) / 16,  ", ", mixin("coeff.candidatePawn.positional." ~ phase) / 10, "%");
		writeln("isolated pawn: ", mixin("coeff.isolatedPawn.material." ~ phase) / 16,  ", ", mixin("coeff.isolatedPawn.positional." ~ phase) / 10, "%");
		writeln("double pawn: ", mixin("coeff.doublePawn.material." ~ phase) / 16,  ", ", mixin("coeff.doublePawn.positional." ~ phase) / 10, "%");
		writeln("backward pawn: ", mixin("coeff.backwardPawn.material." ~ phase) / 16,  ", ", mixin("coeff.backwardPawn.positional." ~ phase) / 10, "%");
		writeln("chained pawn: ", mixin("coeff.chainedPawn.material." ~ phase) / 16, ", ", mixin("coeff.chainedPawn.positional." ~ phase) / 10, "%");
		writeln("tempo: ", mixin("coeff.tempo." ~ phase));
	}

public:
	/* clear (the pawn hashtable) */
	void clear() {
		pawnTable[] = PawnEntry.init;
	}

	/* set evaluation weights */
	void setWeight(const ref double [] w) {
		size_t i;
		static immutable Square [] pawnCenter = [Square.d4, Square.e4];
		static immutable Square [] pawnAdvance = [Square.b8, Square.c8, Square.d8, Square.e8, Square.f8, Square.g8];
		static immutable Square [] knightOutpost = [Square.c6, Square.d6, Square.e6, Square.f6];
		static immutable Square [] knightCenter = [Square.d4, Square.e4, Square.d5, Square.e5];
		static immutable Square [] bishopCenter = [Square.c3, Square.f3, Square.c6, Square.f6];
		static immutable Square [] rook7thRank = [Square.b7, Square.c7, Square.d7, Square.e7, Square.f7, Square.g7];
		static immutable Square [] rookCenter = [Square.a4, Square.b4, Square.c4, Square.d4, Square.e4, Square.f4, Square.g4, Square.h4, 
		                                  Square.a5, Square.b5, Square.c5, Square.d5, Square.e5, Square.f5, Square.g5, Square.h5];
		static immutable Square [] queenCenter = [Square.d4, Square.e4, Square.d5, Square.e5];
		static immutable Square [] kingCastle = [Square.b1, Square.g1];
		static immutable Square [] kingCenter = [Square.d4, Square.e4, Square.d5, Square.e5];

		coeff = Weight.init;

		// opening
		// material
		foreach(p; Piece.pawn .. Piece.king) coeff.material[p].opening = scale(w[i++]);
		coeff.material[Piece.king].opening = 0;
		coeff.bishopPair.opening = scale(w[i++]);
		coeff.materialImbalance.opening = scale(w[i++]);

		// mobility
		coeff.safePawnAdvance.opening   = scale(w[i++]); foreach(p; Piece.pawn .. Piece.size) coeff.safeMobility[p].opening    = scale(w[i++]);
		coeff.unsafePawnAdvance.opening = scale(w[i++]); foreach(p; Piece.pawn .. Piece.size) coeff.unsafeMobility[p].opening  = scale(w[i++]);
		coeff.safePawnBlock.opening     = scale(w[i++]); foreach(p; Piece.pawn .. Piece.size) coeff.safeAttack[p].opening      = scale(w[i++]);
		coeff.unsafePawnBlock.opening   = scale(w[i++]); foreach(p; Piece.pawn .. Piece.size) coeff.unsafeAttack[p].opening    = scale(w[i++]);
		coeff.safePawnDouble.opening    = scale(w[i++]); foreach(p; Piece.pawn .. Piece.size) coeff.safeDefense[p].opening     = scale(w[i++]);
		coeff.unsafePawnDouble.opening  = scale(w[i++]); foreach(p; Piece.pawn .. Piece.size) coeff.unsafeDefense[p].opening   = scale(w[i++]);
		foreach(p; Piece.pawn .. Piece.king) coeff.kingAttack[p].opening      = scale(w[i++]);
		foreach(p; Piece.pawn .. Piece.king) coeff.kingDefense[p].opening     = scale(w[i++]);

		// king shield / storm
		coeff.kingShield.opening = scale(w[i++]);
		coeff.kingStorm.opening  = scale(w[i++]);

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
		coeff.backwardPawn.material.opening    = scale(w[i++]);
		coeff.chainedPawn.material.opening     = scale(w[i++]);
		coeff.passedPawn.positional.opening    = scale(w[i++], centipawn);
		coeff.candidatePawn.positional.opening = scale(w[i++], centipawn);
		coeff.isolatedPawn.positional.opening  = scale(w[i++], centipawn);
		coeff.doublePawn.positional.opening    = scale(w[i++], centipawn);
		coeff.backwardPawn.positional.opening  = scale(w[i++], centipawn);
		coeff.chainedPawn.positional.opening   = scale(w[i++], centipawn);

		coeff.tempo.opening = scale(w[i++]);

		// endgame
		// material
		foreach(p; Piece.pawn .. Piece.king) coeff.material[p].endgame = scale(w[i++]);
		coeff.material[Piece.king].endgame = 0;
		coeff.bishopPair.endgame = scale(w[i++]);
		coeff.materialImbalance.endgame = scale(w[i++]);

		// mobility
		coeff.safePawnAdvance.endgame   = scale(w[i++]); foreach(p; Piece.pawn .. Piece.size) coeff.safeMobility[p].endgame    = scale(w[i++]);
		coeff.unsafePawnAdvance.endgame = scale(w[i++]); foreach(p; Piece.pawn .. Piece.size) coeff.unsafeMobility[p].endgame  = scale(w[i++]);
		coeff.safePawnBlock.endgame     = scale(w[i++]); foreach(p; Piece.pawn .. Piece.size) coeff.safeAttack[p].endgame      = scale(w[i++]);
		coeff.unsafePawnBlock.endgame   = scale(w[i++]); foreach(p; Piece.pawn .. Piece.size) coeff.unsafeAttack[p].endgame    = scale(w[i++]);
		coeff.safePawnDouble.endgame    = scale(w[i++]); foreach(p; Piece.pawn .. Piece.size) coeff.safeDefense[p].endgame     = scale(w[i++]);
		coeff.unsafePawnDouble.endgame  = scale(w[i++]); foreach(p; Piece.pawn .. Piece.size) coeff.unsafeDefense[p].endgame   = scale(w[i++]);
		foreach(p; Piece.pawn .. Piece.king) coeff.kingAttack[p].endgame      = scale(w[i++]);
		foreach(p; Piece.pawn .. Piece.king) coeff.kingDefense[p].endgame     = scale(w[i++]);

		// positional
		buildPositional!"endgame"(coeff.positional[Piece.pawn],   pawnAdvance,  w[i++], true);
		buildPositional!"endgame"(coeff.positional[Piece.knight], knightCenter, w[i++], false);
		buildPositional!"endgame"(coeff.positional[Piece.bishop], bishopCenter, w[i++], false);
		buildPositional!"endgame"(coeff.positional[Piece.queen],  queenCenter,  w[i++], false);
		buildPositional!"endgame"(coeff.positional[Piece.king],   kingCenter,   w[i++], false);
		adjustPawn!"endgame"(coeff.positional[Piece.pawn]);
	
		// pawn structure
		coeff.passedPawn.material.endgame      = scale(w[i++]);
		coeff.candidatePawn.material.endgame   = scale(w[i++]);
		coeff.isolatedPawn.material.endgame    = scale(w[i++]);
		coeff.doublePawn.material.endgame      = scale(w[i++]);
		coeff.backwardPawn.material.endgame    = scale(w[i++]);
		coeff.chainedPawn.material.endgame     = scale(w[i++]);
		coeff.passedPawn.positional.endgame    = scale(w[i++], centipawn);
		coeff.candidatePawn.positional.endgame = scale(w[i++], centipawn);
		coeff.isolatedPawn.positional.endgame  = scale(w[i++], centipawn);
		coeff.doublePawn.positional.endgame    = scale(w[i++], centipawn);
		coeff.backwardPawn.positional.endgame  = scale(w[i++], centipawn);
		coeff.chainedPawn.positional.endgame   = scale(w[i++], centipawn);

		coeff.tempo.endgame = scale(w[i++]);
	}

	/* resize the pawn hash table */
	void resize(size_t size) {
		const lMax = size / PawnEntry.sizeof;
		size_t l;
		
		for (l = 1; l < lMax; l <<= 1) {}
		if (l > lMax) l >>= 1;
		pawnTable.length = l;
		debug writefln("pawnTT size: %s -> lMax: %s, l: %s entry.length: %s -> size: %s", size, lMax, l, pawnTable.length, pawnTable.length * PawnEntry.sizeof);
	}

	/* Constructor (initialize evaluation weights & allocate pawnhash table) */
	this(const ref double [] w = weight.initialWeights, in size_t size = 2 * 1024 * 1024) {
		// allocate the pawn hash table.
		resize(size);

		// set the weights
		setWeight(w);
	}

	/* display eval weights */
	void showWeight() const {
		showWeight!"opening"();
		showWeight!"endgame"();
	}

	/* display detailed evaluation */
	void show(in Board board) {
		Value [Piece.size][Color.size] material, positional, mobility, king;
		Value [Color.size] value;
		int [Color.size] m;

		initAttack(board);
		foreach(Color c; Color.white .. Color.size) {
			foreach(Piece p; Piece.pawn .. Piece.size) {
				ulong b = board.color[c] & board.piece[p];
				immutable n = countBits(b);
				material[c][p] = coeff.material[p] * n / 16;
				if (p == Piece.bishop && n >= 2) material[c][p] += coeff.bishopPair;
				while (b) {
					Square x = popSquare(b);
					positional[c][p] += coeff.positional[p][forward(x, c)];
				}
				positional[c][p] /= 16;
			}
			mobility[c][Piece.pawn] = influence!(Piece.pawn)(board, c) / 16;
			mobility[c][Piece.knight] = influence!(Piece.knight)(board, c) / 16;
			mobility[c][Piece.bishop] = influence!(Piece.bishop)(board, c) / 16;
			mobility[c][Piece.rook] = influence!(Piece.rook)(board, c) / 16;
			mobility[c][Piece.queen] = influence!(Piece.queen)(board, c) / 16;
			mobility[c][Piece.king] = influence!(Piece.king)(board, c) / 16;
		}

		writeln("               Material                Positional                Mobility");
		writeln("          Opening     Endgame      Opening     Endgame      Opening     Endgame");
		writeln("Piece   White Black White Black  White Black White Black  White Black White Black");
		foreach(Piece p; Piece.pawn .. Piece.size) {
			writefln("%6s  %+5d %+5d %+5d %+5d  %+5d %+5d %+5d %+5d  %+5d %+5d %+5d %+5d", p,
				material[0][p].opening, material[1][p].opening, material[0][p].endgame, material[1][p].endgame,
				positional[0][p].opening, positional[1][p].opening, positional[0][p].endgame, positional[1][p].endgame,
				mobility[0][p].opening, mobility[1][p].opening, mobility[0][p].endgame, mobility[1][p].endgame,
			);
		}
		

		foreach(Color c; Color.white .. Color.size) m[c] = countBits(board.color[c] & ~board.piece[Piece.pawn]);
		if (m[0] > m[1]) {
			writefln("imbal. %+5d        %+5d", coeff.materialImbalance.opening/16, coeff.materialImbalance.endgame / 16);
		} else if (m[1] > m[0]) {
			writefln("imbal.       %+5d        %+5d", coeff.materialImbalance.opening/16, coeff.materialImbalance.endgame / 16);
		}
		foreach(Color c; Color.white .. Color.size) value[c] = pawnStructure(board, c);
		writefln("pawn s. %+5d %+5d %+5d %+5d", value[0].opening/16, value[1].opening/16, value[0].endgame/16, value[1].endgame/16);
		if (board.player == Color.white) writefln(" tempo  %+5d      %+5d", coeff.tempo.opening/16, coeff.tempo.endgame/16);
		else writefln(" tempo  %+5d      %+5d", coeff.tempo.opening/16, coeff.tempo.endgame/16);

		writefln(" stage  %+5d", stack[ply].stage);
		writefln("  lazy  %+5d", opCall(board));
		writefln("  full  %+5d", opCall(board, Score.low, Score.high));
		writeln("Pawn structure:"); foreach(Color c; Color.white .. Color.size)  showPawnStructure(board, c);

		writeln("\nEval per square");
		writeln("      a     b     c     d     e     f     g     h");
		for (int i = 7; i >= 0; --i) {
			write(i + 1, ". ");
			for (int j = 0; j < 8; ++j) {
				Square x = cast (Square) (i * 8 + j);
				int score = toCentipawns(evalSquare(board, x));
				if (toColor(board[x]) == Color.black) score = -score;
				writef("%+5d ", score) ;
			}
			writeln(" .", i + 1);
		}
		writeln("      a     b     c     d     e     f     g     h\n");
	}

	/* display setting */
	void showSetting() {
		writefln("PawnTT size: %s entries %s Mb", pawnTable.length, pawnTable.length * PawnEntry.sizeof);
	}	
	

	/* start a new eval (material + positional) from a new position */
	void set(in Board board) {
		Stack *s = &stack[0];

		ply = 0;
		s.value[Color.white] = s.value[Color.black] = Value.init;
		s.stage = 0;

		foreach(Color c; Color.white .. Color.size) {
			s.nPiece[c] = countBits(board.color[c] & ~board.piece[Piece.pawn]);
			foreach(Piece p; Piece.pawn .. Piece.size) {
				ulong b = board.color[c] & board.piece[p];
				const n = countBits(b);
				s.stage += stageValue[p] * n;
				s.value[c] += coeff.material[p] * n;
				if (p == Piece.bishop && n >= 2) {
					s.value[c] += coeff.bishopPair;
				}
				if (p == Piece.king) {
					Square x = firstSquare(b);
					s.kingZone[c] = Board.mask[x].king;
				}
				while (b) {
					Square x = popSquare(b);
					s.value[c] += coeff.positional[p][forward(x, c)];
				}
			}
		}
		if (s.nPiece[Color.white] > s.nPiece[Color.black]) {
			s.value[Color.white] += coeff.materialImbalance;
		} else if (s.nPiece[Color.white] < s.nPiece[Color.black]) {
			s.value[Color.black] += coeff.materialImbalance;
		}
	}

	/* update the evaluation (material & positional) after a move */
	void update(in Board b, in Move m) {
		const Color enemy = b.player;
		const Color player = opponent(enemy);
		const Piece p = m.promotion ? Piece.pawn : toPiece(b[m.to]);
		const Piece v = b.stack[b.ply].victim;
		Stack *s = &stack[++ply];

		stack[ply] = stack[ply - 1];

		// move
		deplace(p, player, m.from, m.to);
		// capture
		if (v) {
			remove(v, enemy, m.to);
			if (v > Piece.pawn) updateImbalance(player, enemy);
			if (v == Piece.bishop && b.count(Piece.bishop, enemy) == 1) {
				s.value[enemy] -= coeff.bishopPair;
			}
		}
		// pawn move
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
		// king move
		if (p == Piece.king) {
			s.kingZone[player] = Board.mask[m.to].king;
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
		const Color player = b.player;
		const Color enemy = opponent(player);
		const Stack *s = &stack[ply];
		const Value value = s.value[player] - s.value[enemy] + coeff.tempo;

		return toCentipawns(value);
	}

	/* functor: lazy evaluation */
	int opCall(in Board b, in Move m) const {
		const Color player = b.player;
		const Color enemy = opponent(player);
		const Stack *s = &stack[ply];
		Value value = coeff.tempo * 2;

		if (m.promotion) value += pieceValue(m.promotion, player, m.to);
		else value += pieceValue(toPiece(b[m.from]), player, m.to);
		if (b[m.to]) value += pieceValue(toPiece(b[m.to]), enemy, m.to);

		return toCentipawns(value);
	}

	/* functor: complete evaluation if the lazy evaluation is αβ ± ε-bounded */
	int opCall(in Board b, in int α, in int β) {
		const Color player = b.player;
		const Color enemy = opponent(player);
		const Stack *s = &stack[ply];
		Value value = s.value[player] - s.value[enemy] + coeff.tempo;
		const lazyValue = toCentipawns(value);

		if (α - ε <= lazyValue && lazyValue <= β + ε) {
			// pieces influence (mobility / attack / defense)
			initAttack(b);
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

