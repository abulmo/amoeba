/*
 * File search.d
 * Best move search.
 * © 2016-2020 Richard Delorme
 */

module search;

import board, eval, kpk, move, util;
import std.algorithm, std.concurrency, std.parallelism, std.conv, std.format, std.getopt, std.math, std.stdio, std.string, std.traits;
import core.atomic, core.thread;

/* Hash table score bound */
enum Bound {none, upper, lower, exact}

/*
 * Entry Table Entry
 */
struct Entry {
	uint code;    // zobrist key
	union {
		struct {
			ushort info;  // date (6 bits) / depth (7 bits) / bound (2 bits) / singular (1 bit)
			short score;  // from search
		}
		uint blur; // lockless hashing by bluring the code with some data
	}
	short value;  // static eval
	Move[2] move; // 2 best moves

	/* depth */
	int depth() const @property {
		return ((info >> 3) & 127);
	}

	/* bound type */
	Bound bound() const @property {
		return cast (Bound) ((info >> 1) & 3);
	}

	/* aging date */
	int date() const @property {
		return (info >> 10);
	}

	/* aging date */
	bool singular() const @property {
		return (info & 1);
	}

	/* refresh the aging date */
	void refresh(const int date) {
		code ^= blur;
		info = cast (ushort) ((info & 1023) | (date << 10));
		code ^= blur;
	}

	/* store common data to update & set into this entry */
	void store(const Bound b, const int d, const int date, const bool singular, const int s, const int v) {
		info = cast (ushort) (singular | (b << 1) | (d << 3) | (date << 10));
		score = cast (short) s;
		value = cast (short) v;
	}

	/* update an existing entry */
	void update(const int d, const int date, const Bound b, const bool singular, const int s, const int v, const Move m) {
		code ^= blur;
		store(b, d, date, singular, s, v);
		code ^= blur;
		if (m != move[0]) { move[1] = move[0]; move[0] = m; }
	}

	/* set a new entry */
	void set(const Key k, const int d, const int date, const Bound b, const bool singular, const int s, const int v, const Move m) {
		store(b, d, date, singular, s, v);
		code = (k.code ^ blur);
		move = [m, 0];
	}

	/* toString */
	string toString() const {
		return format("{ key = %016x, singular %s, depth = %s, bound = %s, age = %s, score = %s, move = %s }", code, singular, depth, bound, date, value, move);
	}
}

/*
 * Transposition table
 */
final class TranspositionTable {
	ulong nUsed;
	enum size_t bucketSize = 3;
	Entry [] entry;
	size_t mask;
	ubyte date;

	/* constructor */
	this(size_t size) {
		resize(size);
		clear();
	}

	/* resize */
	void resize(size_t size) {
		const size_t l = 1 << lastBit(size / Entry.sizeof);
		if (l >= 1) {
			mask = l - 1;
			entry.length = mask + bucketSize;
		} else {
			entry.length = mask = 0;
		}
	}

	/* clear the table */
	void clear(const bool cleaner = true) {
		if (cleaner || date == 63) {
			date = 0;
			if (cleaner) nUsed = 0;
			foreach (ref h; entry) {
				if (cleaner) h = Entry.init;
				else {
					h.code ^= h.blur;
					h.info = h.info & 1023; // reset date to 0;
					h.code ^= h.blur;
				}
			}
		}
		date = cast (ubyte) (date + 1);
	}

	/* look for an entry matching the zobrist key */
	bool probe(const Key k, ref Entry found) {
		const size_t i = k.index(mask);
		foreach (ref h; entry[i .. i + bucketSize]) {
			if ((h.code ^ h.blur) == k.code) {
				h.refresh(date);
				found = h;
				return (found.code ^ found.blur) == k.code;
			}
		}
		return false;
	}

	/* store search data */
	void store(const Key k, const int depth, const Bound b, const bool singular, const int v, const int e, const Move m) {
		const size_t i = k.index(mask);
		Entry *w = &entry[i];
		foreach (ref h; entry[i .. i + bucketSize]) {
			if ((h.code ^ h.blur) == k.code) {
				h.update(depth, date, b, singular, v, e, m);
				return;
			} else if (w.info > h.info) w = &h;
		}
		if (w.info == 0) nUsed = nUsed + 1;
		w.set(k, depth, date, b, singular, v, e, m);
	}

	/* speed up further access */
	void prefetch(const Key k) {
		const size_t i = k.index(mask);
		util.prefetch(cast (void*) &entry[i]);
	}

	/* choose between lower & exact bound */
	Bound bound(const int v, const int β) const {
		return v >= β ? Bound.lower : Bound.exact;
	}

	/* return the size of s the transposition table */
	size_t size() const @property {
		return entry.length * Entry.sizeof;
	}


	/* return how full is the transposition table */
	int full() const @property {
		return cast (int) ((1000.0 * nUsed) / entry.length);
	}

}

/*
 * Search option
 */
struct Option {
	struct Time {
		double max;
		double extra;
	}
	struct Nodes {
		ulong max;
	}
	struct Depth {
		int begin;
		int end;
	}
	struct Score {
		int begin;
	}
	struct CPU {
		int max;
		CPUAffinity affinity;
	}

	Time time;
	Nodes nodes;
	Depth depth;
	Score score;
	CPU cpu;
	int multiPv;
	bool easy;
	bool isPondering;
	bool verbose;
	bool doPrune;
}

/* search Info */
struct Info {
	Line [Limits.moves.max] pv;
	ulong nNodes;
	double time;
	int [Limits.moves.max] score, depth, selDepth;
	int multiPv;

	/* save current search infos (except pv / score) */
	void update(const ulong n, const int d, const double t) {
		nNodes = n; depth = d; time = t;
	}

	/* save current search infos (pv + score) */
	void store(const int iPv, const int s, const int d, const int sd, const ref Line p) {
		const Move m = p.move[0];
		foreach (i; iPv + 1 .. multiPv) {
			if (m == pv[i].move[0]) {
				foreach_reverse (j; iPv .. i) {
					depth[j + 1] = depth[j];
					selDepth[iPv + 1] = selDepth[j];
					score[j + 1] = score[j];
					pv[j + 1].set(pv[j]);
				}
				break;
			}
		}
		depth[iPv] = d;
		selDepth[iPv] = sd;
		score[iPv] = s;
		pv[iPv].set(p);
	}

	/* sort the pv & score */
	void sort(const int iPv) {
		for (int j = iPv - 1; j >= 0 && score[j + 1] > score[j]; --j) {
			swap(depth[j + 1], depth[j]);
			swap(selDepth[j + 1], selDepth[j]);
			swap(score[j + 1], score[j]);
			pv[j].swap(pv[j + 1]);
		}
	}

	/* clear results */
	void clear(const int n, const int scoreInit) {
		nNodes = 0; time = 0.0;
		multiPv = n;
		depth[] = 0; selDepth[] = 0; score[] = 0;
		foreach (i; 0 .. multiPv) pv[i].clear();
		score[0] = scoreInit;
	}

	/* write the search results found so far using UCI protocol */
	string toUCI(const int iPv, TranspositionTable tt, const ulong nodes) const {
		string s;

		foreach (i; 0 .. multiPv) {
			s ~= "info depth " ~ to!string(depth[i]);
			s ~= " seldepth " ~ to!string(selDepth[i]);
			if (multiPv > 1) s ~= " multipv " ~ to!string(i + 1);
			s ~= " score ";
			if (score[i] > Score.high) s ~= "mate " ~ to!string((Score.mate + 1 - score[i]) / 2);
			else if (score[i] < Score.low) s ~= "mate " ~ to!string(-(Score.mate + score[i]) / 2);
			else if (score[i] < 0) s ~= "cp " ~ to!string(score[i] / 2);
			else s ~= "cp " ~ to!string(score[i]);
			s ~=  " nps " ~ to!string(cast (ulong) (nodes / time));
			s ~= " time " ~ to!string(cast (ulong) (1000 * time));
			s ~= " nodes " ~ to!string(nodes);
			s ~= " hashfull " ~ to!string(tt.full);
			s ~= " pv " ~ pv[i].toString();
			if (i + 1 < multiPv) s ~= '\n';
		}

		return s;
	}
}


/*
 * Task : unit task for parallel search
 */
struct Task {
private:
	Search *search;
	Board board;
	Eval eval;
	TranspositionTable tt;
	Option *option;
	Info info;
	Moves rootMoves;
	Line line;
	Line [Limits.ply.max + 1] pv;

	History history;
	ubyte[32][32] reduction;
	Move [2][Limits.ply.max + 2] killer;
	Move [Limits.move.size] refutation;
	ulong nNodes;
	int ply, iPv, selDepth, id;

	Thread thread;
	shared bool stop;

	/* is the main task */
	bool isMaster() {
		return id == 0;
	}

	/* update heuristics */
	void heuristicsUpdate(const Move m, const int d) {
		if (m != killer[ply][0]) {
			killer[ply][1] = killer[ply][0];
			killer[ply][0] = m;
		}

		if (ply > 0) refutation[line.top & Limits.move.mask] = m;

		history.updateGood(board, m, d * d);
	}

	/* update a move */
	void update(const Move m) {
		board.update(m);
		tt.prefetch(board.key);
		if (m) eval.update(board, m);
		line.push(m);
		++ply;
		++nNodes;
	}

	/* restore a move */
	void restore(const Move m) {
		--ply;
		line.pop();
		board.restore(m);
		if (m) eval.restore();
	}

	/* convert a score for storage a score */
	int toHash(const int v) const {
		return (v < Score.low ? v - ply : (v > Score.high ? v + ply : v));
	}

	/* convert a stored score to a searched score */
	int fromHash(const int v) const {
		return (v < Score.low ? v + ply : (v > Score.high ? v - ply : v));
	}

	/* get a reduction value */
	int reduce(const int d, const int m) const {
		return reduction[min(d, 31)][min(m, 31)];
	}

	/* get an eval margin to prune */
	static int margin(const int d) {
		return d ? 200 * d - 100 : 50;
	}


	/* error in score prediction by depth r for depth d on quiet position */
	static int error(const int d, const int r) {
		claim(r < d);

		double a = 0.09 * r + 1.94;
		double b = -5.74 * r + 48.52;

		return cast (int) (a * d + b);
	}

	/* quiescence search */
	int qs(int α, int β) {
		enum δ = margin(0);
		const bool isPv = (α + 1 < β);
		int s, bs, v = Score.mate;
		Moves moves = void;
		Move m;
		Entry h;

		// stop
		if (stop) return α;

		// drawn position ?
		if (board.isDraw) return 0;

		// distance to mate pruning
		bs = ply - Score.mate;
		/+ if (bs > α && (α = bs) >= β) return bs; // never happens +/
		s = Score.mate - ply - 1;
		if (s < β && (β = s) <= α) return s;

		// transposition table probe
		if (tt.probe(board.key, h)) {
			s = fromHash(h.score);
			if (h.bound == Bound.lower && (α = s) >= β) return s;
			else if (h.bound == Bound.upper && (β = s) <= α) return s;
			else if (h.bound == Bound.exact) return s;
			v = fromHash(h.value);
			if ((h.bound != Bound.upper && s > v) || (h.bound != Bound.lower && s < v)) v = s;
		} else v = eval(board, α, β);

		// standpat
		const αOld = α;
		if (!board.inCheck && v > bs && (bs = v) > α) {
			tt.store(board.key, 0, tt.bound(bs, β), false, toHash(bs), toHash(v), h.move[0]);
			if ((α = bs) >= β) return bs;
		}

		//max depth reached
		if (ply == Limits.ply.max) return v;

		if (ply > selDepth) selDepth = ply;

		// move generation: good captures & promotions if not in check
		moves.setup(board.inCheck, h.move);

		while ((m = moves.selectMove(board).move) != 0) {
			if (!board.inCheck && board.see(m) < 0) continue;
			s = eval(board, m) + v + δ;
			if (s > α || isPv || board.inCheck || board.giveCheck(m)) {
				update(m);
					s = -qs(-β, -α);
				restore(m);
			}
			if (stop) return α;
			if (s > bs && (bs = s) > α) {
				tt.store(board.key, board.inCheck, tt.bound(bs, β), false, toHash(bs), toHash(v), m);
				if ((α = bs) >= β) return bs;
			}
		}

		if (bs <= αOld) tt.store(board.key, board.inCheck, Bound.upper, false, toHash(bs), toHash(v), h.move[0]);

		return bs;
	}

	/* alpha-beta search (PVS/negascout variant) */
	int αβ(int α, int β, const int d, const bool doPrune, const Move excludeMove = 0) {
		const bool isPv = (α + 1 < β);
		int v, s, bs, e, r, iQuiet;
		Moves moves = void;
		MoveItem i = void;
		Move m;
		Entry h;
		bool isSingular = false;
		const Key key = board.key.exclude(excludeMove);

		// qs search
		if (d <= 0) return qs(α, β);

		// stop
		if (stop) return α;

		// draw
		if (board.isDraw) return 0;

		// distance to mate
		bs = ply - Score.mate;
		/+ if (bs > α && (α = bs) >= β) return bs; // never happens +/
		s = Score.mate - ply - 1;
		if (s < β && (β = s) <= α) return s;

		// transposition table probe
		if (tt.probe(key, h) && !isPv) {
			s = fromHash(h.score);
			if (h.depth >= d || s <= ply - Score.mate || s >= Score.mate - ply - 1) {
				if (h.bound == Bound.lower && s >= β) return s;
				else if (h.bound == Bound.upper && s <= α) return s;
				else if (h.bound == Bound.exact) return s;
				if (h.bound != Bound.upper && s > bs) bs = s;
			}
			v = fromHash(h.value);
			if ((h.bound != Bound.upper && s > v) || (h.bound != Bound.lower && s < v)) v = s;
		} else v = eval(board, α, β);

		// max depth reached
		if (ply == Limits.ply.max) return v;

		// selective search: "frontier" node pruning & null move
		bool isSafe = !(isPv || board.inCheck || α >= Score.big || β <= -Score.big);
		if (doPrune && isSafe) {
			// pruning
			const int  δ = margin(d);
			const int sα = α - δ;

			// razoring (our position is so bad, no need to search further)
			if (v <= sα && (s = qs(sα, sα + 1)) <= sα) return α;
			// TODO if (d <= 2 && v <= α - δ) return qs(α, β);

			if (board.color[board.player] & ~(board.piece[Piece.pawn] | board.piece[Piece.king])) {
				// eval pruning (our position is very good, no need to search further)
				if (v >= β + δ && v < Score.high) return v;

				// null move
				if (d >= 2 && v >= β) {
					r = 3 + d / 4 + min((v - β) / 128, 3);
					update(0);
						s = -αβ(-β, -β + 1, d - r, true);
					restore(0);
					if (stop) return α;
					if (s >= β) {
						if (s >= Score.high) s = β;
						if (d < 10 ||  (s = αβ(β - 1, β, d - r, false)) >= β) return s;
					}
					isSafe = (s > -Score.big);
				}
			}

			// probcut : look for a good capture that cannot be recovered by the opponent
			if (d >= 8 && v >= β) {
				moves.setup(board.inCheck, h.move); // only captures and promotions
				r = 4;
				const int λ = β + error(d, d - r) * 2;
				while ((m = moves.selectMove(board).move) != 0) {
					if (m == excludeMove || board.see(m) < 0) continue;
					update(m);
						s = -qs(-λ, -λ + 1);
						if (s >= λ) s = -αβ(-λ, -λ + 1, d - r, true);
					restore(m);
					if (stop) return α;
					if (s >= λ) return β;
				}
			}
		}

		// IID
		if (!h.move[0] && !excludeMove) {
			r = isPv ? 2 : max(4, 2 + d / 4);
			if (d > r) {
				s = αβ(α, β, d - r, false, excludeMove);
				tt.probe(key, h);
				isSafe = isSafe && (-Score.big < s && s < Score.big);
			}
		}

		// prepare move generation
		moves.setup(board.inCheck, h.move, killer[ply], refutation[line.top & Limits.move.mask], history);

		const int αOld = α;

		// generate moves in order & loop through them
		while ((m = (i = moves.selectMove(board)).move) != 0) {

			if (m == excludeMove) continue;

			const bool isQuiet = !((i.value > 0) || (isPv && (board.isTactical(m) || board.giveCheck(m) || board.inCheck)));
			iQuiet += isQuiet;
			// late move pruning
			if (isSafe && isQuiet && iQuiet > 4 + d * d && !board.giveCheck(m)) continue;
			// see pruning
			if (isSafe && d < 4 && iQuiet > 4 && board.see(m) < 0) continue;

			// singular move
			if (d >= 8 && m == h.move[0] && !excludeMove && h.depth >= d - 4 && h.bound != Bound.upper) {
				e = (h.singular && h.depth >= d);
				if (e == 0) {
					const int λ = h.score - 2 * d - 1;
					r = max(4, 2 + d / 4);
					if (λ >= -Score.big) {
						s = αβ(λ, λ + 1, d - r, false, m);
						e = (s <= λ);
					}
				}
				isSingular = (e > 0);
			} else {
				// check extension (if move is not losing)
				e = (board.giveCheck(m) && board.see(m) >= 0);
			}

			if (isPv) pv[ply + 1].clear();
			killer[ply + 2] = [0, 0];

			update(m);
				// principal variation search (pvs)
				if (moves.isFirst(m)) {
					s = -αβ(-β, -α, d + e - 1, option.doPrune);
				} else {
					// late move reduction (lmr)
					r = isQuiet ? reduce(d, iQuiet) : 0;
					if (r && isPv) --r;
					// null window search (nws)
					s = -αβ(-α - 1, -α, d + e - r - 1, option.doPrune);
					// new pv found or new bestscore (bs) at reduced depth ?
					if ((α < s && s < β) || (s > bs && r > 0)) {
						s = -αβ(-β, -α, d + e - 1, option.doPrune);
					}
				}
			restore(m);

			if (stop) return α;

			// best move ?
			if (s <= α && !board.isTactical(m) && !board.inCheck && !excludeMove) history.updateBad(board, m, d * d);
			if (s > bs && (bs = s) > α) {
				tt.store(key, d, tt.bound(bs, β), isSingular, toHash(bs), toHash(v), m);
				if (isPv) pv[ply].set(m, pv[ply + 1]);
				if ((α = bs) >= β) {
					if (!board.isTactical(m) && !board.inCheck && !excludeMove) heuristicsUpdate(m, d);
					return bs;
				}
			}
		}

		// no move: mate or stalemate.
		if (moves.length == 0) {
			if (board.inCheck) return bs;
			else return 0;
		}

		if (bs <= αOld) tt.store(key, d, Bound.upper, isSingular, toHash(bs), toHash(v), moves[0]);

		return bs;
	}

	/* alpha-beta search at root level */
	void αβRoot(int α, const int β, const int d) {
		const αOld = α;
		int s, bs = -Score.mate, e, r, iQuiet;
		const int v = eval(board, α, β);

		pv[0].clear();
		selDepth = 0;

		// loop thru all moves (and order them)
		for (int i = iPv; i < rootMoves.length; ++i) {
			Move m = rootMoves[i];
			const bool isQuiet = !(rootMoves.item[i].value > 0 || board.isTactical(m) || board.giveCheck(m) || board.inCheck);
			// check extension (if move is not losing)
			e = (board.giveCheck(m) && board.see(m) >= 0);

			pv[1].clear();
			update(m);
				// principal variation search (pvs)
				if (i == iPv) {
					s = -αβ(-β, -α, d + e - 1, option.doPrune);
				} else {
					// late move reduction (lmr)
					r = isQuiet ? reduce(d, iQuiet) : 0;
					// null window search (nws)
					s = - αβ(-α - 1, -α, d + e - r - 1, option.doPrune);
					// new pv ?
					if ((α < s && s < β) || (s > bs && r > 0)) {
						s = -αβ(-β, -α, d + e - 1, option.doPrune);
					}
				}
			restore(m);
			info.update(nNodes, d, search.time);
			if (stop) return;
			if (s <= α && !board.isTactical(m) && !board.inCheck) history.updateBad(board, m, d * d);
			if (s > bs && (bs = s) > α) {
				rootMoves.setBest(m, iPv);
				pv[0].set(m, pv[1]);
				info.store(iPv, s, d, selDepth, pv[0]);
				if (iPv == 0) tt.store(board.key, d, tt.bound(bs, β), false, toHash(bs), toHash(v), m);
				if ((α = bs) >= β) {
					if (!board.isTactical(m) && !board.inCheck) heuristicsUpdate(m, d);
					break;
				}
			} else if (i == iPv) {
				pv[0].set(m, pv[1]);
				info.store(iPv, s, d, selDepth, pv[0]);
			}
		}

		if (iPv == 0 && bs <= αOld) tt.store(board.key, d, Bound.upper, false, toHash(bs), toHash(v), rootMoves[0]);
	}

	/* aspiration window */
	void aspiration(const int α, const int β, const int d) {
		int λ, υ, δ = +10;

		if (d <= 4) {
			αβRoot(α, β, d);
		} else {
			λ = info.score[iPv] - δ; υ = info.score[iPv] + δ;
			for (; !stop; δ *= 2) {
				λ = max(α, λ); υ = min(β, υ);
				αβRoot(λ, υ, d);
				if (info.score[iPv] <= λ && λ > α) {
					if (isMaster && !option.isPondering && search.time > 0.3 * option.time.max) option.time.max = option.time.extra;
					υ = (λ + υ) / 2; λ = info.score[iPv] - δ;
				} else if (info.score[iPv] >= υ && υ < β) {
					λ = (λ + υ) / 2; υ = info.score[iPv] + δ;
				} else {
					break;
				}
			}
		}
	}

	/* multiPv : search n best moves */
	void multiPv(const int n, const int d) {
		int β = Score.mate - 1;

		// search the ith best move
		for (iPv = 0; iPv < n && !stop; ++iPv) {
			aspiration(2 - Score.mate, β, d);
			if (info.score[iPv] >= β) aspiration(2 - Score.mate, Score.mate - 1, d);
			info.sort(iPv);
			β = info.score[iPv];
		}
		// sort rootmoves to match 'info' ordering
		foreach (i; 0 .. n) rootMoves.setBest(info.pv[i].move[0], i);

		if (isMaster) search.toUCI();
	}

	/* check if search must continue */
	bool persist(const int d) const {
		return (option.isPondering || (id > 0 || search.time < 0.7 * option.time.max))
		    && !stop && d <= option.depth.end
		    && (option.multiPv > 1 || (info.score[0] <= Score.mate - (d - 4) && info.score[0] >= (d - 4) - Score.mate));
	}

	// is this position's bestmove obvious (when a single move is legal) ?
	bool isBestmoveObvious() {
		return option.easy && rootMoves.length == 1;
	}

	// iterative deepening
	void iterate() {
		int d;
		if (search.message) search.message.log("smp > task[", id, "] option ", option);
		for (d = option.depth.begin; persist(d); ++d) {
			if (id > 0 && d < option.depth.end && (id + d) % 4 == 0) continue;
			multiPv(option.multiPv, d);
		}
		stop = true;
		if (search.message) search.message.log("smp> task[", id, "] finished: ", info.toUCI(option.multiPv, tt, nNodes));
	}

	/* clear search setting before searching */
	void setup() {
		line.clear();
		pv[0].clear();
		ply = 0;
		nNodes = 0;
		stop = false;
		info.clear(option.multiPv, option.score.begin);
		history.rescale(8);
		iPv = 0;
	}

	/* create a new task */
	void clone(ref Search s, uint i) {
		stop = true;
		search = &s;
		tt = s.tt;
		option = &s.option;
		eval = new Eval(tt.size / 32);
		clear();
		setReduction(1.1, 0.7);
		id = i;
	}

	/* start the search */
	void go(Moves moves) {
		setup();
		if (moves.length > 0) rootMoves = moves;
		if (search.message) search.message.log("smp> task[", id, "] launched");
		thread = new Thread((){iterate();});
		thread.start();

		if (option.cpu.affinity.step) {
			int cpu = id * option.cpu.affinity.step + option.cpu.affinity.offset;
			if (cpu >= option.cpu.max) cpu = cpu % option.cpu.max + (cpu / option.cpu.max) % option.cpu.affinity.step;
			if (cpu < option.cpu.max) thread.setAffinity(cpu);
			if (search.message) search.message.log("smp> thread ", id, " associated to cpu ", cpu);
		}
	}

	/* stop the search */
	void abort() {
		if (search.message) search.message.log("smp> task[", id, "] aborted");
		stop = true;
	}

	/* clear task caches */
	void clear() {
		eval.clear();
		foreach (ref k; killer) k = [0, 0];
		foreach (ref r; refutation) r = 0;
		history.clear();

	}

	/* Initialize late move reduction table */
	void setReduction(const double lmrDepth, const double lmrMove) {
		foreach (d; 0 .. 32)
		foreach (m; 0 .. 32) {
			reduction[d][m] = cast (ubyte) ((d ? lmrDepth * std.math.log(d) : 0) + (m ? lmrMove * std.math.log(m) : 0));
		}
	}

	/* set board */
	void position(Copy copy = Copy.on)(const Board b) {
		Entry h;
		Move m;
		static if (copy) board = b.dup; else board = cast (Board) b;
		tt.probe(board.key, h);
		rootMoves.setup(board.inCheck, h.move, killer[2], refutation[2], history);
		while ((m = rootMoves.selectMove(board).move) != 0) {}
		eval.set(board);
	}
}

/*
 * Search interface
 * Manage a set of tasks
 */
struct Search {
	Task [] tasks;
	Task *master;
	TranspositionTable tt;
	Option option;
	Chrono timer;
	Message message;
	Eval eval;

	this(const size_t s, const uint n, Message msg) {
		message = msg;
		tt = new TranspositionTable(s);
		threads(n);
	}

	ulong countNodes() {
		ulong n = 0;
		foreach(ref t; tasks) n += t.nNodes;
		return n;
	}

	/* return the spent time */
	double time() const @property {
		return timer.time();
	}

	/* check if search can continue or must stop */
	bool keepSearching() {
		if (message) {
			if (option.isPondering && message.has("ponderhit")) {
				option.isPondering = false;
				message.log(format("ponderhit> time: %.3f ; timeMax: %.3f ⭢ %.3f", time, option.time.max, time + option.time.max));
				option.time.max += time;
				option.time.extra += time;
			} else if (message.has("stop")) {
				return false;
			} else if (message.has("isready")) {
				message.send("readyok");
				message.peek();
			}
		}
		return (option.isPondering || time < option.time.max)
			&& countNodes < option.nodes.max
			&& !master.stop;
	}

	/* set the number of threads */
	void threads(const uint n = 1) {
		uint i = 0;
		tasks.length = n;
		foreach(ref t; tasks) t.clone(this, i++);
		master = &tasks[0];
		eval = master.eval;
	}

	/* resize the tt */
	void resize(const size_t size) {
		tt.resize(size);
		foreach(ref t; tasks) t.eval.resize(size / 32);
	}

	/* set eval weights */
	void setWeight(const double [] weights) {
		foreach(ref t; tasks) t.eval.setWeight(weights);

	}

	/* set a new board position */
	void position(Copy copy = Copy.on)(Board board) {
		foreach(ref t; tasks) t.position!copy(board);
	}

	/* set a new board position */
	void clear(bool cleaner = true) {
		tt.clear(cleaner);
		foreach(ref t; tasks) t.clear();
	}

	/* search according to some option settings */
	void go(Option o, Moves moves) {
		Entry h;

		timer.start();

			// set up the search
			clear(false);
			option = o;
			if (moves.length == 0 && tt.probe(master.board.key, h)) option.depth.begin = h.depth - (h.bound != Bound.exact);
			else option.depth.begin = 1;
			if (master.isBestmoveObvious()) option.depth.end = option.depth.begin;

			// make the tasks search and wait for its termination
			foreach (ref t; tasks) t.go(moves);
			while (keepSearching()) Thread.sleep(1.msecs);

			// terminate the tasks
			foreach (ref t; tasks) {
				atomicStore(t.stop, true);
				t.thread.join();
				destroy(t.thread);
				if (message) message.log("smp> task[", t.id, "] terminated");
			}

		timer.stop();
	}

	/* go search without aspiration window nor iterative deepening */
	void go(const int d) {
		timer.start();
			master.setup();
			master.option.doPrune = true;
			master.info.score[0] = master.αβ(2 - Score.mate, Score.mate - 1, d, master.option.doPrune);
		timer.stop();
	}

	/* get the best move */
	Move bestMove() const @property {
		return master.rootMoves[0];
	}

	/* get the opponent expected move */
	Move hint() @property {
		Entry h;
		Move m;
		// seek opponent move from the pv
		if (master.info.pv[0].n > 1) m = master.info.pv[0].move[1];
		// else from the tt
		else if (!timer.on) { // only if search is not running
			Board b = master.board;
			if (b.isLegal(bestMove)) {
				b.update(bestMove);
					tt.probe(b.key, h);
					if (b.isLegal(h.move[0])) m = h.move[0];
				b.restore(bestMove);
			}
		}

		return m;
	}

	/* get the last evaluated score */
	int score() const @property {
		return master.info.score[0];
	}

	/* get the last evaluated score */
	Line pv() const @property {
		return master.info.pv[0];
	}

	/* toUCI */
	void toUCI() {
		if (option.verbose) {
			if (message) message.send(master.info.toUCI(master.iPv, tt, countNodes()));
			else writeln(master.info.toUCI(master.iPv, tt, countNodes()));
		} else if (message) message.log!'>'(master.info.toUCI(master.iPv, tt, countNodes()));
	}
}

