/*
 * File eval.d
 * Evaluation function
 * © 2016-2021 Richard Delorme
 */

module eval;

import board, kpk, move, util, weight;
import std.algorithm, std.conv, std.math, std.stdio;


/*
 * Value
 */
struct Value {
	int opening;
	int endgame;

	/* operator overloading: a + b; c * d; etc. apply the operator to each member data */
	Value opBinary(string op)(const Value s) const {
		Value r = { mixin("opening " ~ op ~ " s.opening"), mixin("endgame " ~ op ~ " s.endgame") };
		return r;
	}

	/* operator overloading: a + v, b * v; apply the operator to each member data */
	Value opBinary(string op)(const int v) const {
		Value r = {mixin("opening " ~ op ~ " v"), mixin("endgame " ~ op ~ " v")};
		return r;
	}

	/* operator overloading: +/-a , +/-b; apply the unary operator to each member data */
	Value opUnary(string op)() const {
		Value r = {mixin(op ~ "opening"), mixin(op ~ "endgame")};
		return r;
	}

	/* operator overloading: +/-v; apply the operator to each member data */
	Value opBinary(string op)(const double v) const {
		Value r = {cast (int) mixin("(opening " ~ op ~ " v)"), cast (int) mixin("(endgame " ~ op ~ " v)")};
		return r;
	}

	/* assignment operator overloading: apply the operator to each member data */
	void opOpAssign(string op)(const Value s) {
		mixin("opening "~op~"= s.opening;");
		mixin("endgame "~op~"= s.endgame;");
	}

	/* assignment operator overloading: apply the operator to each member data */
	void opOpAssign(string op)(const int v) {
		mixin("opening "~op~"= v;");
		mixin("endgame "~op~"= v;");
	}

	/* write */
	void print(std.stdio.File f = stdout) {
		f.writef("{%+5d, %+5d}, ", opening, endgame);
	}
}


/*
 * Evaluation function
 */
final class Eval {
public:
	/* each possible pawn state */
	enum PawnState { none = 0, isolated, backward, chained,
		candidate, candidateIsolated, candidateBackward, candidateChained,
		passed, passedIsolated, passedBackward, passedChained,
		doubled, doubledIsolated, doubledBackward, doubledChained,
		doubledCandidate, doubledCandidateIsolated, doubledCandidateBackward, doubledCandidateChained,
		doubledPassed, doubledPassedIsolated, doubledPassedBackward, doubledPassedChained, size
	}

	/* pawn structure as material & positional percentages */
	struct PawnStructure {
		Value material;
		Value positional;

		void print(std.stdio.File f, PawnState s) {
			f.write("\t\t{");
			material.print(f); positional.print(f);
			f.writeln("}, // ", s);
		}
	}

	/* mobility */
	struct Mobility {
		Value kingAttack, kingDefense;
		Value safeMobility, unsafeMobility;
		Value safeAttack, unsafeAttack;
		Value safeDefense, unsafeDefense;
		Value centerControl;
		Value hanging, trapped, enclosed;

		void print(std.stdio.File f, const Piece p) {
			f.writeln("\t\t// ", p);
			f.writeln("\t\t{");
			f.write("\t\t\t"); kingAttack.print(); kingDefense.print(); f.writeln("// king neighborhood");
			f.write("\t\t\t"); safeMobility.print(f); unsafeMobility.print(f); f.writeln("// mobility");
			f.write("\t\t\t"); safeAttack.print(f); unsafeAttack.print(f); f.writeln("// attack");
			f.write("\t\t\t"); safeDefense.print(f); unsafeDefense.print(f); f.writeln("// defense");
			f.write("\t\t\t"); centerControl.print(f); f.writeln("// center control");
			f.write("\t\t\t"); hanging.print(f); f.writeln("// hanging");
			f.write("\t\t\t"); trapped.print(f); f.writeln("// trapped");
			f.write("\t\t\t"); enclosed.print(f); f.writeln("// enclosed");
			f.writeln("\t\t},");
		}
	}

	/* weights */
	struct Weight {
		Value [Piece.size] material;
		Value bishopPair;
		Value materialImbalance;
		Value [Square.size][Piece.size] positional;
		Value safePawnAdvance, unsafePawnAdvance;
		Value safePawnBlock, unsafePawnBlock;
		Value safePawnDouble, unsafePawnDouble;
		Mobility [Piece.size] mobility;
		PawnStructure [PawnState.size] pawn;
		Value kingShield, kingStorm, kingCenter;
		Value rookOnOpenFile, rookOnSemiOpenFile, rookSustainPawn, rookBlockPawn;
		Value bishopDefendPawn, bishopAttackPawn, bishopDefendPromotion, bishopAttackPromotion;
		Value tempo;

		void print(std.stdio.File f = stdout) {
			f.writeln("/*");
			f.writeln(" * File weight.d");
			f.writeln(" * Evaluation weight - automatically generated");
			f.writeln(" * © 2016-2020 Richard Delorme");
			f.writeln(" */");
			f.writeln("\nimport eval;");
			f.writeln("static immutable Weight initialWeights = {");

			f.writeln("\t// Material");
			f.write("\t[ ");
			foreach (p; Piece.none .. Piece.king) material[p].print(f);
			f.writeln("], ");

			f.write("\t"); bishopPair.print(f); f.writeln("// Bishop pair");
			f.write("\t"); materialImbalance.print(f); f.writeln("// material Imbalance");

			f.writeln("\n\t// Positional");
			f.writeln("\t[ ");
			foreach (p; Piece.none .. Piece.size) {
				f.writeln("\t\t// ", p);
				f.write("\t\t[ ");
				if (p > Piece.none) foreach (x; Square.a1 .. Square.size) {
					if (file(x) == 0 && x > 0) f.write("\n\t\t  ");
					positional[p][x].print(f);
				}
				f.writeln("],");
			}
			f.writeln("\t], ");
		
			f.writeln("\n\t// Mobility");
			f.write("\t"); safePawnAdvance.print(f); unsafePawnAdvance.print(f); f.writeln("// pawn can advance");
			f.write("\t"); safePawnBlock.print(f); unsafePawnBlock.print(f); f.writeln("// pawn block ennemy");
			f.write("\t"); safePawnDouble.print(f); unsafePawnDouble.print(f); f.writeln("// double pawn");
			f.writeln("\t[ ");
			f.writeln("\t\t// none\n\t\tEval.Mobility.init,");
			foreach (p; Piece.pawn .. Piece.size) mobility[p].print(f, p);
			f.writeln("\t], ");

			f.writeln("\n\t// Pawn structure");	
			f.writeln("\t[ ");
			foreach (s; PawnState.none .. PawnState.size) pawn[s].print(f, s);
			f.writeln("\t], ");
			
			f.writeln("\t// King structure");	
			f.write("\t"); kingShield.print(f); f.writeln("// shield");
			f.write("\t"); kingStorm.print(f); f.writeln("// storm");
			f.write("\t"); kingCenter.print(f); f.writeln("// center");

			f.writeln("\t// Rook structure");	
			f.write("\t"); rookOnOpenFile.print(f); f.writeln("// open file");
			f.write("\t"); rookOnSemiOpenFile.print(f); f.writeln("// semi open file");
			f.write("\t"); rookSustainPawn.print(f); f.writeln("// sustain pawn");
			f.write("\t"); rookBlockPawn.print(f); f.writeln("// block pawn");

			f.writeln("\t// Bishop structure");	
			f.write("\t"); bishopDefendPawn.print(f); f.writeln("// defend pawn");
			f.write("\t"); bishopAttackPawn.print(f); f.writeln("// attack pawn");
			f.write("\t"); bishopDefendPromotion.print(f); f.writeln("// defend promotion");
			f.write("\t"); bishopAttackPromotion.print(f); f.writeln("// attack promotion");

			f.writeln("\t// Tempo");	
			f.write("\t"); tempo.print(f); f.writeln("// tempo");
			f.writeln("};");
		}
	}
private:
	/* compute the attractive force of a target square x to a distant square y */
	static double attraction(const Square x, const Square y) {
		const int r = rank(x) - rank(y);
		const int f = file(x) - file(y);
		const int d = (r * r + f * f);
		return 1.0 / (d + 1.0);
	}

	/* Barycenter = average position of pieces (usually pawns) */
	struct Barycenter {
		int r, f, n;
		Square square() @property const {
			return n ? toSquare(f / n, r / n) : Square.none;
		}
		void set(const Square x) {
			r += x.rank;
			f += x.file;
			++n;
		}
		void remove(const Square x) {
			r -= x.rank;
			f -= x.file;
			--n;
		}

		void deplace(const Square from, const Square to) {
			r += to.rank - from.rank;
			f += to.file - from.file;
		}
	}

	/* stack for fast computation of lazy evaluation */
	struct Stack {
		ulong [Color.size] kingZone;
		Value [Color.size] value;
		uint [Color.size] nPiece;
		uint [Color.size] materialIndex;
		Barycenter pawnCenter;
		int stage;
	}

	/* Pawn Entry of the pawn hash */
	struct Entry {
		uint code;
		Value value;
	}

	static immutable int [Piece.size] stageValue = [0, 0, 3, 3, 5, 10, 0];
	static immutable uint [Color.size][20] drawishTable = [
		// QRBNP-    qrbnp-
		[0x000200, 0x000000], // 2 knights
		[0x001100, 0x000100], // bishop + knight vs knight
		[0x001100, 0x001000], // bishop + knight vs bishop
		[0x002000, 0x000100], // 2 bishops vs knight
		[0x002000, 0x001000], // 2 bishops vs bishop
		[0x000200, 0x000100], // 2 knights vs knight
		[0x000200, 0x001000], // 2 knights vs bishop
		[0x010000, 0x010000], // rook vs rook
		[0x010000, 0x001000], // rook vs bishop
		[0x010000, 0x000100], // rook vs knight
		[0x011000, 0x010000], // rook + bishop vs rook
		[0x010100, 0x010000], // rook + knight vs rook
		[0x100000, 0x100000], // queen vs queen
		[0x100000, 0x020000], // queen vs 2 rooks
		[0x100000, 0x011000], // queen vs rook & bishop
		[0x100000, 0x010100], // queen vs rook & knight
		[0x100000, 0x002000], // queen vs 2 bishops
		[0x100000, 0x000200], // queen vs 2 knights
		[0x100100, 0x100000], // queen + knight vs queen
		[0x101000, 0x100000]  // queen + bishop vs queen
	];
	static immutable uint [Color.size][9] pawnDrawishTable = [
		[0x000010, 0x000100], // pawn vs knight
		[0x000010, 0x001000], // pawn vs bishop
		[0x000010, 0x000200], // pawn vs 2 knights
		[0x000110, 0x000100], // pawn + knight vs knight
		[0x001010, 0x000100], // pawn + bishop vs knight
		[0x000210, 0x000100], // pawn + 2 knights vs knight
		[0x000110, 0x001000], // pawn + knight vs bishop
		[0x001010, 0x001000], // pawn + bishop vs bishop
		[0x000210, 0x001000]  // pawn + 2 knights vs bishop
	];

	static immutable uint [Color.size] KBPk = [0x001010, 0x000000]; // bishop + pawn vs king


	enum ε = 170;
	enum centipawn = 1024;
	enum halfcentipawn = 512;
	enum ulong center = 0x0000001818000000;
	public Weight coeff;
	Stack [Limits.ply.max + 1] stack;
	ulong [Square.size] attackFromSquare;
	ulong [CPiece.size] attackByPiece, attackByMinor;
	ulong [Piece.size] major;
	ulong [Color.size] attackByPlayer;
	ulong [Color.size] promotable;
	ulong pins;
	Entry [] pawnTable;
	int ply;

	/* scale a floating point coeff & round it to an integer n so that n * 64 = 1 centipawn (1024) */
	static int scale(const double w, const double f = 1600) {
		return cast (int) (f * w + (w > 0 ? 0.5 : w < 0 ? -0.5 : 0.0));
	}

	/* remove a piece */
	void remove(const Piece p, const Color c, const Square x) {
		Stack *s = &stack[ply];
		s.value[c] -= coeff.positional[p][forward(x, c)] + coeff.material[p];
		if (p > Piece.pawn) --s.nPiece[c];
		else s.pawnCenter.remove(x);
		s.stage -= stageValue[p];
		s.materialIndex[c] -= 1u << (4 * p);
	}

	/* set a piece */
	void set(const Piece p, const Color c, const Square x) {
		Stack *s = &stack[ply];
		s.value[c] += coeff.positional[p][forward(x, c)] + coeff.material[p];
		if (p > Piece.pawn) ++s.nPiece[c];
		else s.pawnCenter.set(x);
		s.stage += stageValue[p];
		s.materialIndex[c] += 1u << (4 * p);
	}

	/* move a piece */
	void deplace(const Piece p, const Color c, const Square from, const Square to) {
		Stack *s = &stack[ply];
		s.value[c] -= coeff.positional[p][forward(from, c)];
		s.value[c] += coeff.positional[p][forward(to, c)];
		if (p == Piece.pawn) s.pawnCenter.deplace(from, to);
	}

	/* update material imbalance after capturing an enemy's piece or promoting to a player's piece */
	void updateImbalance(const Color player, const Color enemy) {
		Stack *s = &stack[ply];
		if (s.nPiece[player] == s.nPiece[enemy] + 1) {
			s.value[player] += coeff.materialImbalance;
		} else if (s.nPiece[player] == s.nPiece[enemy]) {
			s.value[enemy] -= coeff.materialImbalance;
		}
	}

	/* compute a bitboard of all squares attacked by a type of piece */
	void setCoverage(Piece p : Piece.pawn)(const Board b, const Color player, const ulong pins) {
		const ulong O = ~b.piece[Piece.none];
		const int [2] diag = [9, -9];
		const int [2] antidiag = [7, -7];
		ulong attacker = b.piece[p] & b.color[player] & ~pins;
		CPiece cp = toCPiece(p, player);

		while (attacker) {
			Square x = popSquare(attacker);
			attackFromSquare[x] = Board.coverage(p, x, O, player);
			attackByPiece[cp] |= attackFromSquare[x];
			attackByPlayer[player] |= attackFromSquare[x];
		}

		attacker = b.piece[p] & b.color[player] & pins;
		while (attacker) {
			const Square x = popSquare(attacker);
			const Color c = toColor(b[x]);
			const int d = Board.mask[b.xKing[c]].direction[x];
			if (d == 9) attackFromSquare[x] = x.shift(diag[c]).toBit;
			else if (d == 7) attackFromSquare[x] = x.shift(antidiag[c]).toBit;
			else attackFromSquare[x] = 0;
			attackByPiece[cp] |= attackFromSquare[x];
			attackByPlayer[player] |= attackFromSquare[x];
		}
	}

	/* compute a bitboard of all squares attacked by a type of piece */
	void setCoverage(Piece p)(const Board b, const ulong pins) {
		static assert (p != Piece.pawn);
		ulong O = ~b.piece[Piece.none];
		ulong attacker = b.piece[p] & ~pins;

		while (attacker) {
			const Square x = popSquare(attacker);
			const Color c = toColor(b[x]);
			CPiece cp = toCPiece(p, c);

			attackFromSquare[x] = Board.coverage(p, x, O);
			attackByPiece[cp] |= attackFromSquare[x];
			attackByPlayer[c] |= attackFromSquare[x];
		}

		attacker = b.piece[p] & pins;
		while (attacker) {
			const Square x = popSquare(attacker);
			const Color c = toColor(b[x]);
			CPiece cp = toCPiece(p, c);
			const int d = Board.mask[b.xKing[c]].direction[x];

			attackFromSquare[x] = 0;
			static if (p == Piece.bishop || p == Piece.queen) {
				if (d == 9)  attackFromSquare[x] = Board.diagonalAttack(O, x);
				else if (d == 7)  attackFromSquare[x] = Board.antidiagonalAttack(O, x);
			}
			static if (p == Piece.rook || p == Piece.queen) {
				if (d == 1) attackFromSquare[x] = Board.rankAttack(O, x);
				else if (d == 8) attackFromSquare[x] = Board.fileAttack(O, x);
			}
			attackByPiece[cp] |= attackFromSquare[x];
			attackByPlayer[c] |= attackFromSquare[x];
		}
	}

	/* Initialize various arrays */
	void initAttack(const Board b) {
		const ulong pawns = b.piece[Piece.pawn];
		const ulong [Color.size] pawn = [pawns & b.color[0], pawns & b.color[1]];

		attackByPlayer[] = 0;
		attackByPiece[] = 0;

		pins = b.pins | b.pins(opponent(b.player));

		setCoverage!(Piece.pawn)(b, Color.white, pins);
		setCoverage!(Piece.pawn)(b, Color.black, pins);
		setCoverage!(Piece.knight)(b, pins);
		setCoverage!(Piece.bishop)(b, pins);
		setCoverage!(Piece.rook)(b, pins);
		setCoverage!(Piece.queen)(b, pins);
		setCoverage!(Piece.king)(b, pins);

		attackByMinor[CPiece.wpawn] = 0;
		attackByMinor[CPiece.wknight] = attackByMinor[CPiece.wbishop] = attackByPiece[CPiece.wpawn];
		attackByMinor[CPiece.wrook] = attackByMinor[CPiece.wbishop] | attackByPiece[CPiece.wknight] | attackByPiece[CPiece.wbishop];
		attackByMinor[CPiece.wqueen] = attackByMinor[CPiece.wrook] | attackByPiece[CPiece.wrook];

		attackByMinor[CPiece.bpawn] = 0;
		attackByMinor[CPiece.bknight] = attackByMinor[CPiece.bbishop] = attackByPiece[CPiece.bpawn];
		attackByMinor[CPiece.brook] = attackByMinor[CPiece.bbishop] | attackByPiece[CPiece.bknight] | attackByPiece[CPiece.bbishop];
		attackByMinor[CPiece.bqueen] = attackByMinor[CPiece.brook] | attackByPiece[CPiece.brook];

		major[Piece.pawn] = ~b.piece[Piece.none];
		major[Piece.bishop] = major[Piece.knight] = major[Piece.pawn] & ~b.piece[Piece.pawn];
		major[Piece.queen] = b.piece[Piece.queen] | b.piece[Piece.king];
		major[Piece.rook] = b.piece[Piece.rook] | major[Piece.queen];
	}

	void initPromotion(const Board b) {
		const ulong pawns = b.piece[Piece.pawn];
		const ulong [Color.size] pawn = [pawns & b.color[0], pawns & b.color[1]];

		promotable[Color.white] = ((pawn[Color.white] & Rank.r2) << 48)
		                        | ((pawn[Color.white] & Rank.r3) << 40)
		                        | ((pawn[Color.white] & Rank.r4) << 32)
		                        | ((pawn[Color.white] & Rank.r5) << 24)
		                        | ((pawn[Color.white] & Rank.r6) << 16)
		                        | ((pawn[Color.white] & Rank.r7) << 8);

		promotable[Color.black] = ((pawn[Color.black] & Rank.r7) >> 48)
		                        | ((pawn[Color.black] & Rank.r6) >> 40)
		                        | ((pawn[Color.black] & Rank.r5) >> 32)
                                | ((pawn[Color.black] & Rank.r4) >> 24)
		                        | ((pawn[Color.black] & Rank.r3) >> 16)
		                        | ((pawn[Color.black] & Rank.r2) >> 8);
	}

	/* mobility / attack / defense evaluation components */
	Value influence(Piece p)(const Board b, const Color player) const {
		const Color enemy = opponent(player);
		const ulong P = b.color[player];
		const ulong E = b.color[enemy];
		const ulong V = b.piece[Piece.none];
		const ulong A = attackByPlayer[enemy];
		const ulong D = attackByPlayer[player];
		const CPiece ep = toCPiece(p, enemy);
		ulong attacker = b.piece[p] & P, a, pawns, threat, safe;
		const Stack *s = &stack[ply];
		const Mobility *mobility = &coeff.mobility[p];
		Value v;
		Square x;

		if (attacker) {
			static if (p != Piece.king) {
				v = mobility.kingDefense * countBits(attacker & s.kingZone[player]); // pieces near own king
				threat =  (A & ~D) | attackByMinor[ep];   // squares undefended or attacked by a minor
				safe = (E & major[p]) | ~A | (A & D & ~attackByMinor[ep]);  // squares with enemy major or not attacked or defended & not attacked by a minor
			}

			static if (p == Piece.pawn) { // pawns' pushes
				a = attacker & pins;
				pawns = attacker & ~pins;
				while (a) {
					x = popSquare(a);
					if (Board.mask[b.xKing[player]].direction[x] == 8) pawns |= x.toBit;
				}
				a = player == Color.white ? pawns << 8 : pawns >> 8;
				if (a) {
					v += coeff.safePawnAdvance * countBits(a & V & ~A); //  on empty squares
					v += coeff.unsafePawnAdvance * countBits(a & V & A);
					v += coeff.safePawnBlock * countBits(a & E & ~A);   // blocked by enemy
					v += coeff.unsafePawnBlock * countBits(a & E & A);
					v += coeff.safePawnDouble * countBits(a & P & ~A);  // blocked by own piece
					v += coeff.unsafePawnDouble * countBits(a & P & A);
				}
			}

			do {
				x = popSquare(attacker);
				a = attackFromSquare[x];
				v += mobility.safeMobility   * countBits(a & V & ~A); // moves on empty squares
				v += mobility.unsafeMobility * countBits(a & V & A);
				v += mobility.safeAttack     * countBits(a & E & ~A); // attacks opponent pieces
				v += mobility.unsafeAttack   * countBits(a & E & A);
				v += mobility.safeDefense    * countBits(a & P & ~A); // defends own pieces.
				v += mobility.unsafeDefense  * countBits(a & P & A);
				v += mobility.centerControl  * countBits(a & center); // center control.
				static if (p != Piece.king) {
					v += mobility.kingAttack * countBits(a & s.kingZone[enemy]); // pieces attacking opponent king neighbourhood
					static if (p == Piece.pawn) a = (a & E) | (b.mask[x].pawnPush[player] & V);
					else a &= ~P;
					if (threat & x.toBit) {
						v += mobility.hanging; // piece under a serious attack
						if ((a & safe) == 0) v += mobility.trapped; // & without escape
					} else {
						if ((a & safe) == 0) v += mobility.enclosed; // a piece that cannot move
					}
				}
			} while (attacker);
		}

		return v;
	}

	/* pawn structure */
	Value pawnStructure(const Board b, const Color player) const {
		const ulong pawns = b.piece[Piece.pawn];
		const ulong [Color.size] p = [pawns & b.color[0], pawns & b.color[1]];
		const Color enemy = opponent(player);
		const Square *k = &b.xKing[0];
		const Stack *s = &stack[ply];
		ulong attacker = p[player];
		Value v;

		if (attacker) {
			const ulong shield = (b.mask[k[player]].openFile[player] | b.mask[k[player]].passedPawn[player]);
			const ulong storm  = (b.mask[k[enemy]].openFile[enemy] | b.mask[k[enemy]].passedPawn[enemy]);
			double vShield = 0.0, vStorm = 0.0;

			do {
				const Square x = popSquare(attacker);
				PawnState state = PawnState.none;

				if ((p[enemy] & b.mask[x].openFile[player]) == 0) {
					if ((p[enemy] & b.mask[x].passedPawn[player]) == 0) state += PawnState.passed;
					else state += PawnState.candidate;
				}
				if (attacker & b.mask[x].file) state += PawnState.doubled;
				if ((p[player] & b.mask[x].isolatedPawn) == 0) state += PawnState.isolated;
				else if ((p[player] & b.mask[x].backwardPawn[player]) == 0) state += PawnState.backward;
				else if (p[player] & b.mask[x].pawnAttack[enemy]) state += PawnState.chained;
				v += coeff.pawn[state].material + (coeff.pawn[state].positional * coeff.positional[Piece.pawn][forward(x, player)]) / centipawn;

				if (x.toBit & shield) vShield += attraction(x, k[player]);
				if (x.toBit & storm)  vStorm  += attraction(x, k[enemy]);
			} while (attacker);

			v += coeff.kingShield * vShield + coeff.kingStorm * vStorm;
		}

		if (pawns) v += coeff.kingCenter * attraction(s.pawnCenter.square, k[player]);

		return v;
	}

	/* pawn structure with cache */
	Value pawnStructure(const Board b) {
		Entry *h = &pawnTable[b.pawnKey.index(pawnTable.length - 1)];
		if (h.code != b.pawnKey.code) {
			h.code = b.pawnKey.code;
			h.value = pawnStructure(b, Color.white) - pawnStructure(b, Color.black);
		}
		return b.player == Color.white ? h.value : -h.value;
	}

	/* bishop structure */
	Value bishopStructure(const Board b, const Color player) const {
		const ulong bishops = b.piece[Piece.bishop];
		const ulong [Color.size] bishop = [bishops & b.color[0], bishops & b.color[1]];
		Value v;

		if (bishop[player]) {
			immutable ulong [Color.size] promotionRank = [Rank.r1, Rank.r8];
			const Color enemy = opponent(player);
			const ulong pawns = b.piece[Piece.pawn];
			const ulong [Color.size] pawn = [pawns & b.color[0], pawns & b.color[1]];

			if (bishop[player] & b.blackSquares) {
				v += coeff.bishopDefendPromotion * countBits(promotable[player] & b.blackSquares);
				v += coeff.bishopAttackPromotion * countBits(promotable[enemy ] & b.blackSquares);
				v += coeff.bishopDefendPawn * countBits(pawn[player] & b.blackSquares);
				v += coeff.bishopAttackPawn * countBits(pawn[enemy] & b.blackSquares);
			}
			if (bishop[player] & b.whiteSquares) {
				v += coeff.bishopDefendPromotion * countBits(promotable[player] & b.whiteSquares);
				v += coeff.bishopAttackPromotion * countBits(promotable[enemy ] & b.whiteSquares);
				v += coeff.bishopDefendPawn * countBits(pawn[player] & b.whiteSquares);
				v += coeff.bishopAttackPawn * countBits(pawn[enemy] & b.whiteSquares);
			}
		}

		return v;
	}

	/* rook structure */
	Value rookStructure(const Board b, const Square x, const Color player) const {
		const Color enemy = opponent(player);
		const ulong pawns = b.piece[Piece.pawn];
		const ulong [Color.size] pawn = [pawns & b.color[0], pawns & b.color[1]];
		Value v;

		// open file ?
		if ((pawns & b.mask[x].file) == 0) v += coeff.rookOnOpenFile;
		else {
			// semi open file ?
			if ((pawn[player] & b.mask[x].file) == 0) v += coeff.rookOnSemiOpenFile;
			// behind player's pawn ?
			else if ((pawn[player] & b.mask[x].openFile[player]) != 0) v += coeff.rookSustainPawn;
			// blocking opponent's pawn ?
			if ((pawn[enemy] & b.mask[x].openFile[player]) != 0) v += coeff.rookBlockPawn;
		}

		return v;
	}

	/* rook structure */
	Value rookStructure(const Board b, const Color player) const {
		ulong rooks = b.piece[Piece.rook] & b.color[player];
		Value v;

		while (rooks) {
			const Square x = popSquare(rooks);
			v += rookStructure(b, x, player);
		}

		return v;
	}

	/* drawish position */
	int bound(const Board b, const int value) const {
		const Stack *s = &stack[ply];
		const int draw = sign(value);

		// some drawish positions
		if (s.stage <= 23) {
			foreach (d; drawishTable[0 .. $]) {
				if ((d[0] == s.materialIndex[0] && d[1] == s.materialIndex[1])
				 || (d[1] == s.materialIndex[0] && d[0] == s.materialIndex[1])) return draw;
			}

			foreach (d; pawnDrawishTable[0 .. $]) {
				if ((d[0] == s.materialIndex[0] && d[1] == s.materialIndex[1])
				 || (d[1] == s.materialIndex[0] && d[0] == s.materialIndex[1])) return value / 16; // TODO; tune
			}

			// bad bishop
			if ((KBPk[0] == s.materialIndex[0] && KBPk[1] == s.materialIndex[1])
			 || (KBPk[1] == s.materialIndex[0] && KBPk[0] == s.materialIndex[1])) {
				if (b.piece[Piece.pawn] & (board.File.A | board.File.H)) {
					if ((b.whiteSquares & b.piece[Piece.bishop]) && ((promotable[0]|promotable[1]) & b.blackSquares)) return value / 16;
					if ((b.blackSquares & b.piece[Piece.bishop]) && ((promotable[0]|promotable[1]) & b.whiteSquares)) return value / 16;
				}
			}


			if (s.stage == 0) return kpk.rescale(b, value); // king vs king + pawn
		}

		// fifty-move rule
		if (b.fifty > 50) { // diminish value when fifty-move rule approaches
			if (b.fifty >= 100) return draw;
			return value * (100 - b.fifty) / 50;
		}

		return value;
	}

	/* convert value to centipawns */
	int toCentipawns(const Value value) const {
		const Stack *s = &stack[ply];
		int v = value.opening * s.stage + value.endgame * (64 - s.stage);
		if (v < 0) v -= halfcentipawn; else if (v > 0) v += halfcentipawn;
		return v / centipawn;
	}

	/* get sign of a value */
	int sign(const int value) const {
		return value < 0 ? -1 : (value > 0 ? +1 : 0);
	}

	/* value of a piece */
	Value pieceValue(const Piece p, const Color c, const Square x) const {
		return coeff.material[p] + coeff.positional[p][forward(x, c)];
	}

public:
	/* clear (the pawn hashtable) */
	void clear() {
		pawnTable[] = Entry.init;
	}

	/* set evaluation weights */
	void setWeight(const Weight w) {
		coeff = w;
	}

	/* resize the pawn hash table */
	void resize(const size_t size) {
		size_t s = clamp(size, 65_536, 67_108_864);
		pawnTable.length = 1UL << lastBit(s / Entry.sizeof);
		clear();
	}

	/* get the size of the pawn hash table */
	size_t size() const {
		return pawnTable.length * Entry.sizeof;
	}

	/* Constructor (initialize evaluation weights & allocate pawnhash table) */
	this(const size_t size = 2 * 1048 * 1048) {
		// allocate the pawn hash table.
		resize(size);

		// set the weights
		setWeight(initialWeights);
	}

	/* start a new eval (material + positional) from a new position */
	void set(const Board board) {
		Stack *s = &stack[0];

		ply = 0;
		s.value[Color.white] = s.value[Color.black] = Value.init;
		s.stage = 0;
		s.pawnCenter = Barycenter.init;

		foreach (Color c; Color.white .. Color.size) {
			s.nPiece[c] = countBits(board.color[c] & ~board.piece[Piece.pawn]);
			s.materialIndex[c] = 0;
			foreach (Piece p; Piece.pawn .. Piece.size) {
				ulong b = board.color[c] & board.piece[p];
				const n = countBits(b);
				s.stage += stageValue[p] * n;
				s.value[c] += coeff.material[p] * n;
				if (p < Piece.king) s.materialIndex[c] += n << (4 * p);
				if (p == Piece.bishop && n >= 2) s.value[c] += coeff.bishopPair;
				if (p == Piece.king) {
					Square x = firstSquare(b);
					s.kingZone[c] = Board.mask[x].king;
				}
				while (b) {
					const Square x = popSquare(b);
					s.value[c] += coeff.positional[p][forward(x, c)];
					if (p == Piece.pawn) s.pawnCenter.set(x);
				}
			}
		}
		if (s.nPiece[Color.white] > s.nPiece[Color.black]) s.value[Color.white] += coeff.materialImbalance;
		else if (s.nPiece[Color.white] < s.nPiece[Color.black]) s.value[Color.black] += coeff.materialImbalance;
	}

	/* update the evaluation (material & positional) after a move */
	void update(const Board b, const Move m) {
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

	/* functor: lazy evaluation change by a move */
	int opCall(const Board b, const Move m) const {
		const Color player = b.player;
		const Color enemy = opponent(player);
		Value value = coeff.tempo * -2 - pieceValue(toPiece(b[m.from]), player, m.from);
		const Piece victim = toPiece(b[m.to]);
		const Stack *s = &stack[ply];

		if (m.promotion) {
			value += pieceValue(m.promotion, player, m.to);
			if (m.promotion == Piece.bishop && b.count(Piece.bishop, player) == 1) value += coeff.bishopPair;
			if (s.nPiece[player] == s.nPiece[enemy] || s.nPiece[player] == s.nPiece[enemy] - 1) value += coeff.materialImbalance;
		} else value += pieceValue(toPiece(b[m.from]), player, m.to);

		if (victim) {
			value += pieceValue(victim, enemy, m.to);
			if (victim == Piece.bishop && b.count(Piece.bishop, enemy) == 2) value += coeff.bishopPair;
			if (victim > Piece.pawn && (s.nPiece[player] == s.nPiece[enemy] || s.nPiece[player] == s.nPiece[enemy] - 1)) value += coeff.materialImbalance;
		}

		return toCentipawns(value);
	}

	/* functor: complete evaluation if the lazy evaluation is αβ ± ε-bounded */
	int opCall(const Board b, const int α, const int β) {
		const Color player = b.player;
		const Color enemy = opponent(player);
		const Stack *s = &stack[ply];
		Value value = s.value[player] - s.value[enemy] + coeff.tempo;
		const lazyValue = toCentipawns(value);

		initPromotion(b);
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
			// rookStructure
			value += rookStructure(b, player) - rookStructure(b, enemy);
			// bishopStructure
			value += bishopStructure(b, player) - bishopStructure(b, enemy);
		}

		// return score in centipawns with some corrections for drawish positions
		return bound(b, toCentipawns(value));
	}
}

