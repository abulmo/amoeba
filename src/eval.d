/*
 * File eval.d
 * Evaluation function
 * © 2016 Richard Delorme
 */

module eval;

import board, kpk, move, util, weight;
import std.stdio, std.conv, std.algorithm;

/*
 * Evaluation function
 */
final class Eval {
private:
	struct PawnStructure {
		int material;
		int positional;
	}
	struct Weight {
		int [Piece.size] material;
		int [Square.size][Piece.size] positional;
		int [Piece.size] mobility;
		int [Piece.size] attack;
		int [Piece.size] defense;
		int [Piece.size] kingAttack;
		int [Piece.size] kingDefense;
		PawnStructure passedPawn, candidatePawn, isolatedPawn, doublePawn;
		int bishopPair;
		int materialImbalance;
		int tempo;
	}
	struct Stack {
		ulong [Color.size] kingZone;
		int [Color.size] opening, endgame;
		int [Color.size] nPiece;
		int stage;
	}
	struct Entry {
		ulong code;
		int [Color.size] score;
	}

	static immutable int [Piece.size] stageValue = [0, 0, 3, 3, 5, 10];
	static immutable int ε = 200;
	Weight opening, endgame;
	Stack [Limits.plyMax + 1] stack;
	Entry [] pawnTable;
	int ply;

	/* compute the attractive force of a target square x to a distant square y */
	static real attraction(in Square x, in Square y) pure {
		int r = rank(x) - rank(y);
		int f = file(x) - file(y);
		int d = (r * r + f * f);
		return d == 0 ? 2.0 : 1.0 / d;
	}

	static int scale(in real w, in real f = 1600) pure {
		return cast (int) (f * w + (w > 0 ? 0.5 : w < 0 ? -0.5 : 0.0));
	}

	static void adjustPawn(ref int [Square.size] p) pure {
		foreach(x; Square.a1 .. Square.a2) p[x] = 0;
		foreach(x; Square.a8 .. Square.size) p[x] = 0;
		int m = int.max;
		foreach(x; Square.a2 .. Square.a8) m = min(m, p[x]);
		foreach(x; Square.a2 .. Square.a8) p[x] -= m;
	}

	/* Build positional array weights from an array of attractive squares */
	static void buildPositional(ref int [Square.size] positional, in Square [] y, in real a, in bool isPawn) pure {
		real w;	
		real [Square.size] p;
	
		foreach (Square x; Square.a1 .. Square.size) {
			w = attraction(x, y[0]);
			foreach (i; 1 .. y.length) {
				w = max(attraction(x, y[i]), w);
			}
			p[x] = a * w;
		}

		if (!isPawn) {
			real m = 0.0; foreach (x; Square.a1 .. Square.size) m += p[x]; m /= Square.size;
			foreach (x; Square.a1 .. Square.size) p[x] -= w;
		}

		foreach(x; Square.a1 .. Square.size) positional[x] += scale(p[x]);
	}

	/* remove a piece */
	void remove(in Piece p, in Color c, in Square x) pure {
		Stack *s = &stack[ply];
		s.opening[c] -= opening.positional[p][forward(x, c)] + opening.material[p];
		s.endgame[c] -= endgame.positional[p][forward(x, c)] + endgame.material[p];
		if (p > Piece.pawn) --s.nPiece[c];
		s.stage -= stageValue[p];
	}

	/* set a piece */
	void set(in Piece p, in Color c, in Square x) pure {
		Stack *s = &stack[ply];
		s.opening[c] += opening.positional[p][forward(x, c)] + opening.material[p];
		s.endgame[c] += endgame.positional[p][forward(x, c)] + endgame.material[p];
		if (p > Piece.pawn) ++s.nPiece[c];
		s.stage += stageValue[p];
	}

	/* move a piece */
	void deplace(in Piece p, in Color c, in Square from, in Square to) pure {
		Stack *s = &stack[ply];
		s.opening[c] -= opening.positional[p][forward(from, c)];
		s.opening[c] += opening.positional[p][forward(to, c)];
		s.endgame[c] -= endgame.positional[p][forward(from, c)];
		s.endgame[c] += endgame.positional[p][forward(to, c)];
	}

	/* update material imbalance */
	void updateImbalance(in Color player, in Color enemy) pure {
		Stack *s = &stack[ply];
		if (s.nPiece[player] == s.nPiece[enemy] + 1) {
			s.opening[player] += opening.materialImbalance;
			s.endgame[player] += endgame.materialImbalance;
		} else if (s.nPiece[player] == s.nPiece[enemy]) {
			s.opening[enemy] -= opening.materialImbalance;
			s.endgame[enemy] -= endgame.materialImbalance;
		}
	}

	/* mobility / attack / defense evaluation components */
	int influence(Piece p)(in Board b, in Color player) pure const {
		immutable Color enemy = opponent(player);
		immutable ulong P = b.color[player];
		immutable ulong E = b.color[enemy];
		immutable ulong V = b.piece[Piece.none];
		immutable ulong O = ~V;
		ulong attacker = b.piece[p] & P;
		const Stack *s = &stack[ply];
		ulong a;
		int o, e, f;
		Square x;

		static if (p != Piece.king) {
			f = countBits(attacker & s.kingZone[player]);
			o = opening.kingDefense[p] * f; e = endgame.kingDefense[p] * f; 
		}

		while (attacker) {
			x = popSquare(attacker);
			a = Board.coverage!p(x, O, player);
			f = countBits(a & V);
			o += opening.mobility[p] * f; e += endgame.mobility[p] * f;
			f = countBits(a & E);
			o += opening.attack[p] * f; e += endgame.attack[p] * f;
			f = countBits(a & P);
			o += opening.defense[p] * f; e += endgame.defense[p] * f;
			static if (p != Piece.king) {
				f = countBits(a & s.kingZone[enemy]);
				o += opening.kingAttack[p] * f; e += endgame.kingAttack[p] * f;
			}
		}

		return o * s.stage + e * (64 - s.stage);
	}

	/* pawn structure */
	int pawnStructure(in Board b, in Color player) pure const {
		immutable Color enemy = opponent(player);
		immutable ulong pawns = b.piece[Piece.pawn];
		immutable ulong [Color.size] pawn = [pawns & b.color[0], pawns & b.color[1]];
		const Stack *s = &stack[ply];
		ulong attacker = pawn[player];
		int o, e, oMat, oPos, eMat, ePos;
		
		while (attacker) {
			auto x = popSquare(attacker);
			oMat = oPos = eMat = ePos = 0;
			// open file ?
			if ((pawns & b.mask[x].openFile[player]) == 0) {
				// passed pawn ?
				if ((pawn[enemy] & b.mask[x].passedPawn[player]) == 0) {
					oMat += opening.passedPawn.material;
					oPos += opening.passedPawn.positional;
					eMat += endgame.passedPawn.material;
					ePos += endgame.passedPawn.positional;
				// candidate pawn
				} else {
					oMat += opening.candidatePawn.material;
					oPos += opening.candidatePawn.positional;
					eMat += endgame.candidatePawn.material;
					ePos += endgame.candidatePawn.positional;
				}
			}
			// isolated pawn ?
			if ((pawn[player] & b.mask[x].isolatedPawn) == 0) {
				oMat += opening.isolatedPawn.material;
				oPos += opening.isolatedPawn.positional;
				eMat += endgame.isolatedPawn.material;
				ePos += endgame.isolatedPawn.positional;
			}
			// double pawn ?
			if (attacker & b.mask[x].file) {
				oMat += opening.doublePawn.material;
				oPos += opening.doublePawn.positional;
				eMat += endgame.doublePawn.material;
				ePos += endgame.doublePawn.positional;
			}				
			o += oMat + oPos * opening.positional[player][forward(x, player)] / 1024;
			e += eMat + ePos * endgame.positional[player][forward(x, player)] / 1024;
		}
		return o * s.stage + e * (64 - s.stage);
	}


	/* pawn structure with cache */
	int pawnStructure(in Board b) pure const {
		immutable Color player = b.player;
		immutable Color enemy = opponent(player);
		Entry h = pawnTable[b.pawnKey & (pawnTable.length - 1)];
		if (h.code != b.pawnKey) {
			h.code = b.pawnKey;
			h.score[player] = pawnStructure(b, player);
			h.score[enemy]  = pawnStructure(b, enemy);
		}
		return h.score[player] - h.score[enemy];
	}

	/* convert score to centipawns */
	int toCentipawns(int score) pure const {
		if (score < 0) score -= 512; else if (score > 0) score += 512;
		return score / 1024;
	}

public:
	/* clear */
	void clear() pure {
		pawnTable[] = Entry.init;
	}

	/* Constructor */
	this(const ref real [] weight = weight.initialWeights, in size_t size = 65536) pure {
		size_t i;
		immutable Square [] pawnCenter = [Square.d4, Square.e4];
		immutable Square [] pawnAdvance = [Square.a8, Square.b8, Square.c8, Square.d8, Square.e8, Square.f8, Square.g8, Square.h8]; //TODO: remove a8 & h8
		immutable Square [] knightOutpost = [Square.c6, Square.d6, Square.e6, Square.f6];
		immutable Square [] knightCenter = [Square.d4, Square.e4, Square.d5, Square.e5];
		immutable Square [] bishopCenter = [Square.c3, Square.f3, Square.c6, Square.f6];
		immutable Square [] rook7thRank = [Square.a7, Square.b7, Square.c7, Square.d7, Square.e7, Square.f7, Square.g7, Square.h7];
		immutable Square [] kingCastle = [Square.b1, Square.g1];
		immutable Square [] kingCenter = [Square.d4, Square.e4, Square.d5, Square.e5];

		pawnTable.length = size;

	// opening
		// material
		foreach(p; Piece.pawn .. Piece.king) opening.material[p] = scale(weight[i++]);
		opening.material[Piece.king] = 0;
		opening.bishopPair = scale(weight[i++]);
		opening.materialImbalance = scale(weight[i++]);

		// mobility
		foreach(p; Piece.pawn .. Piece.size) opening.mobility[p]    = scale(weight[i++]);
		foreach(p; Piece.pawn .. Piece.size) opening.attack[p]      = scale(weight[i++]);
		foreach(p; Piece.pawn .. Piece.size) opening.defense[p]     = scale(weight[i++]);
		foreach(p; Piece.pawn .. Piece.king) opening.kingAttack[p]  = scale(weight[i++]);
		foreach(p; Piece.pawn .. Piece.king) opening.kingDefense[p] = scale(weight[i++]);

		// positional
		buildPositional(opening.positional[Piece.pawn],   pawnCenter,    weight[i++], true);
		buildPositional(opening.positional[Piece.pawn],   pawnAdvance,   weight[i++], true);
		buildPositional(opening.positional[Piece.knight], knightOutpost, weight[i++], false);
		buildPositional(opening.positional[Piece.bishop], bishopCenter,  weight[i++], false);
		buildPositional(opening.positional[Piece.rook],   rook7thRank,   weight[i++], false);
		buildPositional(opening.positional[Piece.king],   kingCastle,    weight[i++], false);
		adjustPawn(opening.positional[Piece.pawn]);

		// starting square bonus/malus
		opening.positional[Piece.pawn][Square.d2]   += scale(weight[i]) ;
		opening.positional[Piece.pawn][Square.e2]   += scale(weight[i++]);
		opening.positional[Piece.knight][Square.b1] += scale(weight[i]);
		opening.positional[Piece.knight][Square.g1] += scale(weight[i++]);
		opening.positional[Piece.bishop][Square.c1] += scale(weight[i]);
		opening.positional[Piece.bishop][Square.f1] += scale(weight[i++]);
		opening.positional[Piece.rook][Square.a1]   += scale(weight[i]);
		opening.positional[Piece.rook][Square.h1]   += scale(weight[i++]);
		opening.positional[Piece.queen][Square.d1]  += scale(weight[i++]);
		opening.positional[Piece.king][Square.e1]   += scale(weight[i++]);

		// pawn structure
		opening.passedPawn.material      = scale(weight[i++]);
		opening.candidatePawn.material   = scale(weight[i++]);
		opening.isolatedPawn.material    = scale(weight[i++]);
		opening.doublePawn.material      = scale(weight[i++]);
		opening.passedPawn.positional    = scale(weight[i++], 1024);
		opening.candidatePawn.positional = scale(weight[i++], 1024);
		opening.isolatedPawn.positional  = scale(weight[i++], 1024);
		opening.doublePawn.positional    = scale(weight[i++], 1024);

		opening.tempo = scale(weight[i++]);

	// endgame
		// material
		foreach(p; Piece.pawn .. Piece.king) endgame.material[p] = scale(weight[i++]);
		endgame.material[Piece.king] = 0;
		endgame.bishopPair = scale(weight[i++]);
		endgame.materialImbalance = scale(weight[i++]);

		// mobility
		foreach(p; Piece.pawn .. Piece.size) endgame.mobility[p]    = scale(weight[i++]);
		foreach(p; Piece.pawn .. Piece.size) endgame.attack[p]      = scale(weight[i++]);
		foreach(p; Piece.pawn .. Piece.size) endgame.defense[p]     = scale(weight[i++]);
		foreach(p; Piece.pawn .. Piece.king) endgame.kingAttack[p]  = scale(weight[i++]);
		foreach(p; Piece.pawn .. Piece.king) endgame.kingDefense[p] = scale(weight[i++]);

		// positional
		buildPositional(endgame.positional[Piece.pawn],   pawnAdvance,  weight[i++], true);
		buildPositional(endgame.positional[Piece.knight], knightCenter, weight[i++], false);
		buildPositional(endgame.positional[Piece.bishop], bishopCenter, weight[i++], false);
		buildPositional(endgame.positional[Piece.king],   kingCenter,   weight[i++], false);
		adjustPawn(endgame.positional[Piece.pawn]);
	
		// pawn structure
		endgame.passedPawn.material      = scale(weight[i++]);
		endgame.candidatePawn.material   = scale(weight[i++]);
		endgame.isolatedPawn.material    = scale(weight[i++]);
		endgame.doublePawn.material      = scale(weight[i++]);
		endgame.passedPawn.positional    = scale(weight[i++]);
		endgame.candidatePawn.positional = scale(weight[i++]);
		endgame.isolatedPawn.positional  = scale(weight[i++]);
		endgame.doublePawn.positional    = scale(weight[i++]);

		endgame.tempo = scale(weight[i++]);

	}

	/* start a new Eval */
	void set(in Board board) pure {
		Stack *s = &stack[0];

		ply = 0;
		s.opening[Color.white] = s.opening[Color.black] = 0;
		s.endgame[Color.white] = s.endgame[Color.black] = 0;
		s.stage = 0;

		foreach(Color c; Color.white .. Color.size)
		foreach(Piece p; Piece.pawn .. Piece.size) {
			ulong b = board.color[c] & board.piece[p];
			immutable n = countBits(b);
			s.stage += stageValue[p] * n;
			s.opening[c] += opening.material[p] * n;
			s.endgame[c] += endgame.material[p] * n;
			if (p == Piece.bishop && n >= 2) {
				s.opening[c] += opening.bishopPair;
				s.endgame[c] += endgame.bishopPair;
			}
			if (p == Piece.king) {
				Square x = firstSquare(b);
				s.kingZone[c] = Board.coverage!(Piece.king)(x);
			}
			while (b) {
				Square x = popSquare(b);
				s.opening[c] += opening.positional[p][forward(x, c)];
				s.endgame[c] += endgame.positional[p][forward(x, c)];
			}
		}
		s.nPiece[Color.white] = countBits(board.color[Color.white] & ~board.piece[Piece.pawn]);
		s.nPiece[Color.black] = countBits(board.color[Color.black] & ~board.piece[Piece.pawn]);

		if (s.nPiece[Color.white] > s.nPiece[Color.black]) {
			s.opening[Color.white] += opening.materialImbalance;
			s.endgame[Color.white] += endgame.materialImbalance;
		} else if (s.nPiece[Color.white] < s.nPiece[Color.black]) {
			s.opening[Color.black] += opening.materialImbalance;
			s.endgame[Color.black] += endgame.materialImbalance;
		}
	}

	/* update the eval after a move */
	void update(in Board b, in Move m) pure {
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
				s.opening[enemy] -= opening.bishopPair;
				s.endgame[enemy] -= endgame.bishopPair;
			}
		}

		if (p == Piece.pawn) {
			if (m.promotion) {
				remove(p, player, m.to);
				set(m.promotion, player, m.to);
				updateImbalance(player, enemy);
				if (m.promotion == Piece.bishop && b.count(Piece.bishop, player) == 2) {
					s.opening[player] += opening.bishopPair;
					s.endgame[player] += endgame.bishopPair;
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

		debug {
			auto e = new Eval;
			e.set(b);
			foreach(c; Color.white..Color.size) {
				assert (e.stack[0].opening[c] == s.opening[c]);
				assert (e.stack[0].endgame[c] == s.endgame[c]);
			}
		}
	}

	/* restore the evaluation */
	void restore() pure {
		--ply;
	}

	/* functor: lazy evaluation */
	int opCall(in Board b) pure const {
		immutable Color player = b.player;
		immutable Color enemy = opponent(player);
		const Stack *s = &stack[ply];
		immutable int o = s.opening[player] - s.opening[enemy] + opening.tempo;
		immutable int e = s.endgame[player] - s.endgame[enemy] + endgame.tempo;

		return toCentipawns(o * s.stage + e * (64 - s.stage));
	}

	/* functor: complete evaluation if the lazy evaluation is αβ ± ε-bounded */
	int opCall(in Board b, in int α, in int β) const {
		immutable Color player = b.player;
		immutable Color enemy = opponent(player);
		const Stack *s = &stack[ply];
		immutable int o = s.opening[player] - s.opening[enemy] + opening.tempo;
		immutable int e = s.endgame[player] - s.endgame[enemy] + endgame.tempo;
		int score = o * s.stage + e * (64 - s.stage);
		immutable lazyScore = toCentipawns(score);

		if (α - ε <= lazyScore && lazyScore <= β + ε) {
			score += influence!(Piece.pawn)(b, player)   - influence!(Piece.pawn)(b, enemy);
			score += influence!(Piece.knight)(b, player) - influence!(Piece.knight)(b, enemy);
			score += influence!(Piece.bishop)(b, player) - influence!(Piece.bishop)(b, enemy);
			score += influence!(Piece.rook)(b, player)   - influence!(Piece.rook)(b, enemy);
			score += influence!(Piece.queen)(b, player)  - influence!(Piece.queen)(b, enemy);
			score += influence!(Piece.king)(b, player)   - influence!(Piece.king)(b, enemy);
			score += pawnStructure(b);
		}

		if (b.fifty > 50) {
			if (b.fifty >= 100) score = 0;
			else score = score * (100 - b.fifty) / 50;
		}

		score = kpk.rescale(b, score);

		return toCentipawns(score);
	}

	/* Write weights */
	static printWeights(const ref real [] weight = initialWeights, std.stdio.File f = stdout) {
		f.writeln("real [] initialWeights = [");
		f.writeln("\t// Opening");
		f.write("\t"); foreach(i;  0 ..   7) f.writef("%+7.4f, ", weight[i]); f.writeln(" // material    [ 0- 6]");
		f.write("\t"); foreach(i;  7 ..  13) f.writef("%+7.4f, ", weight[i]); f.writeln("          // mobility    [ 7-12]");
		f.write("\t"); foreach(i; 13 ..  19) f.writef("%+7.4f, ", weight[i]); f.writeln("          // attack      [13-18]");
		f.write("\t"); foreach(i; 19 ..  25) f.writef("%+7.4f, ", weight[i]); f.writeln("          // defense     [19-24]");
		f.write("\t"); foreach(i; 25 ..  30) f.writef("%+7.4f, ", weight[i]); f.writeln("                   // K attack    [25-29]");
		f.write("\t"); foreach(i; 30 ..  35) f.writef("%+7.4f, ", weight[i]); f.writeln("                   // K defense   [30-34]");
		f.write("\t"); foreach(i; 35 ..  41) f.writef("%+7.4f, ", weight[i]); f.writeln("          // positional  [35-46]");
		f.write("\t"); foreach(i; 41 ..  47) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach(i; 47 ..  51) f.writef("%+7.4f, ", weight[i]); f.writeln("                            // P structure [47-54]");
		f.write("\t"); foreach(i; 51 ..  55) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach(i; 55 ..  56) f.writef("%+7.4f, ", weight[i]); f.writeln("                                                       // tempo [55]");
		f.writeln("\t// Endgame");
		f.write("\t"); foreach(i; 56 ..  63) f.writef("%+7.4f, ", weight[i]); f.writeln(" // material    [56-62]");
		f.write("\t"); foreach(i; 63 ..  69) f.writef("%+7.4f, ", weight[i]); f.writeln("          // mobility    [63-68]");
		f.write("\t"); foreach(i; 69 ..  75) f.writef("%+7.4f, ", weight[i]); f.writeln("          // attack      [69-74]");
		f.write("\t"); foreach(i; 75 ..  81) f.writef("%+7.4f, ", weight[i]); f.writeln("          // defense     [75-80]");
		f.write("\t"); foreach(i; 81 ..  86) f.writef("%+7.4f, ", weight[i]); f.writeln("                   // K attack    [81-85]");
		f.write("\t"); foreach(i; 86 ..  91) f.writef("%+7.4f, ", weight[i]); f.writeln("                   // K defense   [86-90]");
		f.write("\t"); foreach(i; 91 ..  95) f.writef("%+7.4f, ", weight[i]); f.writeln("                            // positional  [91-94]");
		f.write("\t"); foreach(i; 95 ..  99) f.writef("%+7.4f, ", weight[i]); f.writeln("                            // P structure [95-102]");
		f.write("\t"); foreach(i; 99 .. 103) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach(i;103 .. 104) f.writef("%+7.4f, ", weight[i]); f.writeln("                                                       // tempo [103]");
		f.writeln("];"); 
		f.flush();
	}

}

