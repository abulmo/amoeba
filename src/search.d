/*
 * File search.d
 * Best move search.
 * © 2016-2017 Richard Delorme
 */

module search;

import board, eval, kpk, move, util;
import std.algorithm, std.conv, std.format, std.getopt, std.math, std.stdio, std.string;

/* Hash table score bound */
enum Bound {none, upper, lower, exact}

/*
 * Entry Table Entry
 */
struct Entry {
	uint code;    // zobrist key
	ushort info;  // date / depth / bound
	Move[2] move; // 2 best moves
	short score;  // from search
	short value;  // static eval

	/* depth */
	int depth() const @property {
		return ((info >> 2) & 127);
	}

	/* bound type */
	Bound bound() const @property {
		return cast (Bound) (info & 3);
	}

	/* aging date */
	int date() const @property {
		return (info >> 9);
	}

	/* refresh the aging date */
	void refresh(const int date) {
		info = cast (ushort) ((info & 511) | (date << 9));
	}

	/* store common data to update & set into this entry */
	void store(const Bound b, const int d, const int date, const int s, const int v) {
		info = cast (ushort) (b | (d << 2) | (date << 9));
		score = cast (short) s;
		value = cast (short) v;
	}

	/* update an existing entry */
	void update(const int d, const int date, const Bound b, const int s, const int v, const Move m) {
		store(b, d, date, s, v);
		if (m != move[0]) { move[1] = move[0]; move[0] = m; }
	}

	/* set a new entry */
	void set(const Key k, const int d, const int date, const Bound b, const int s, const int v, const Move m) {
		code = k.code;
		store(b, d, date, s, v);
		move = [m, 0];
	}

	/* toString */
	string toString() const {
		return format("{ key = %016x, depth = %s, bound = %s, age = %s, score = %s, move = %s }", code, depth, bound, date, value, move);
	}
}

/*
 * Transposition table
 */
final class TranspositionTable {
	ulong nUsed;
	enum size_t bucketSize = 4;
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
		entry.length = mask = 0;
		const size_t l = 1 << lastBit(size / Entry.sizeof);
		if (l >= 1) {
			mask = l - 1;
			entry.length = mask + bucketSize;
		}
	}

	/* clear the table */
	void clear(const bool cleaner = true) {
		if (cleaner || date == 127) {
			date = 0;
			if (cleaner) nUsed = 0;
			foreach (ref h; entry) {
				if (cleaner) h = Entry.init;
				else h.info &= 511; // reset date to 0;
			}
		}
		++date;
	}

	/* look for an entry matching the zobrist key */
	bool probe(const Key k, ref Entry found) {
		const size_t i = k.index(mask);
		foreach (ref h; entry[i .. i + bucketSize]) {
			if (h.code == k.code) {
				h.refresh(date);
				found = h;
				return true;
			}
		}
		return false;
	}

	/* store search data */
	void store(const Key k, const int depth, const Bound b, const int v, const int e, const Move m) {
		const size_t i = k.index(mask);
		Entry *w = &entry[i];
		foreach (ref h; entry[i .. i + bucketSize]) {
			if (h.code == k.code) {
				h.update(depth, date, b, v, e, m);
				return;
			} else if (w.info > h.info) w = &h;
		}
		if (w.info == 0) ++nUsed;
		w.set(k, depth, date, b, v, e, m);
	}

	/* speed up further access */
	void prefetch(const Key k) {
		const size_t i = k.index(mask);
		util.prefetch(&entry[i]);
	}

	/* choose between lower & exact bound */
	Bound bound(const int v, const int β) const {
		return v >= β ? Bound.lower : Bound.exact;
	}

	/* return how full is the transposition table */
	int full() const @property {
		return cast (int) ((1000.0 * nUsed) / entry.length);
	}
}


/*
 * Search termination
 */
struct Termination {
	struct Time {
		double max;
		double extra;
	}
	struct Nodes {
		ulong max;
	}
	struct Depth {
		int max;
	}
	Time time;
	Nodes nodes;
	Depth depth;
}


/*
 * Search
 */
final class Search {
private:
	/* search option */
	struct Option {
		Termination termination;
		int depthInit;
		int scoreInit;
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
					foreach_reverse (j; iPv .. i + 1) {
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

		/* sort the pvs & score */
		void sort(const int iPv) {
			for (int j = iPv - 1; j >= 0 && score[j + 1] > score[j]; --j) {
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
		string toUCI(const int iPv, TranspositionTable tt) const {
			string s;

			foreach (i; 0 .. multiPv) {
				s ~= "info depth " ~ to!string(depth[i]);
				s ~= " selDepth " ~ to!string(selDepth[i]);
				if (multiPv > 1) s ~= " multipv " ~ to!string(i + 1);
				s ~= " score ";
				if (score[i] > Score.high) s ~= "mate " ~ to!string((Score.mate + 1 - score[i]) / 2);
				else if (score[i] < Score.low) s ~= "mate " ~ to!string(-(Score.mate + score[i]) / 2);
				else if (score[i] < 0) s ~= "cp " ~ to!string(score[i] / 2);
				else s ~= "cp " ~ to!string(score[i]);
				s ~=  " nps " ~ to!string(cast (ulong) (nNodes / time));
				s ~= " time " ~ to!string(cast (ulong) (1000 * time));
				s ~= " nodes " ~ to!string(nNodes);
				s ~= " hashfull " ~ to!string(tt.full);
				s ~= " pv " ~ pv[i].toString();
				if (i + 1 < multiPv) s ~= '\n';
			}

			return s;
		}
	}
	Board board;
	TranspositionTable tt;
	History history;
	ubyte[32][32] reduction;
	Info info;
	Moves rootMoves;
	Line line;
	Line [Limits.ply.max + 1] pv;
	Move [2][Limits.ply.max + 1] killer;
	Move [Limits.move.size] refutation;
	ulong nNodes;
	int ply, iPv, selDepth;
	Chrono timer;
	bool stop;
	
public:
	Eval eval;
	shared Message message;
	Option option;

private:
	/* check if enough time is available */
	bool checkTime(const double timeMax) const {
		return option.isPondering || time < timeMax;
	}

	/* check if the search should abort or continue */
	bool abort() {
		if ((nNodes & 0x3ff) == 0) {
			if (message) {
				if (option.isPondering && message.has("ponderhit")) {
					option.isPondering = false;
					message.log(format("ponderhit> time: %.3f ; timeMax: %.3f ⭢ %.3f", time, option.termination.time.max, time + option.termination.time.max));
					option.termination.time.max += time;
					option.termination.time.extra += time;
				} else if (message.has("stop")) {
					stop = true;
				} else if (message.has("isready")) {
					message.send("readyok");
					message.peek();
				}
			}
			if (!checkTime(option.termination.time.max)) stop = true;
		}
		if (nNodes >= option.termination.nodes.max) stop = true;

		return stop;
	}

	/* return the spent time */
	double time() const @property {
		return timer.time();
	}

	/* clear heuristics */
	void heuristicsClear() {
		foreach (ref k; killer) k = [0, 0];
		foreach (ref r; refutation) r = 0;
		history.clear();
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

	/* quiescence search */
	int qs(int α, int β) {
		enum δ = margin(0);
		const bool isPv = (α + 1 < β);
		int s, bs, v = Score.mate;
		Moves moves = void;
		Move m;
		Entry h;

		// search abort
		if (abort()) return α;

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
			if ((h.bound == Bound.lower && s > v) || (h.bound == Bound.upper && s < v)) v = s;
		} else v = eval(board, α, β);

		// standpat
		const αOld = α;
		if (!board.inCheck && v > bs && (bs = v) > α) {
			tt.store(board.key, 0, tt.bound(bs, β), toHash(bs), toHash(v), h.move[0]);
			if ((α = bs) >= β) return bs;
		}

		//max depth reached
		if (ply == Limits.ply.max) return eval(board, α, β);
		
		if (ply > selDepth) selDepth = ply;

		// move generation: good captures & promotions if not in check
		moves.setup(board.inCheck, h.move);

		while ((m = moves.selectMove(board).move) != 0) {
			if (board.see(m) < 0) continue;
			s = eval(board, m) + v + δ;
			if (s > α || isPv || board.inCheck || board.giveCheck(m)) {
				update(m);
					s = -qs(-β, -α);
				restore(m);
			}
			if (stop) break;
			if (s > bs && (bs = s) > α) {
				tt.store(board.key, board.inCheck, tt.bound(bs, β), toHash(bs), toHash(v), m);
				if ((α = bs) >= β) return bs;
			}
		}

		if (!stop && bs <= αOld) tt.store(board.key, board.inCheck, Bound.upper, toHash(bs), toHash(v), h.move[0]);

		return bs;
	}

	/* alpha-beta search (PVS/negascout variant) */
	int αβ(int α, int β, const int d, const bool doPrune) {
		const bool isPv = (α + 1 < β);
		int v, s, bs, e, r, iQuiet;
		Moves moves = void;
		MoveItem i = void;
		Move m;
		Entry h;

		// qs search
		if (d <= 0) return qs(α, β);

		// search abort
		if (abort()) return α;

		// draw
		if (board.isDraw) return 0;

		// distance to mate
		bs = ply - Score.mate;
		/+ if (bs > α && (α = bs) >= β) return bs; // never happens +/
		s = Score.mate - ply - 1;
		if (s < β && (β = s) <= α) return s;

		// transposition table probe
		if (tt.probe(board.key, h) && !isPv) {
			s = fromHash(h.score);
			if (h.depth >= d || s <= ply - Score.mate || s >= Score.mate - ply - 1) {
				if (h.bound == Bound.lower && s >= β) return s;
				else if (h.bound == Bound.upper && s <= α) return s;
				else if (h.bound == Bound.exact) return s;
				if (h.bound != Bound.upper && s > bs) bs = s;
			}
			v = fromHash(h.value);
			if ((h.bound == Bound.lower && s > v) || (h.bound == Bound.upper && s < v)) v = s;
		} else v = eval(board);

		//max depth reached
		if (ply == Limits.ply.max) return eval(board, α, β);

		// selective search: "frontier" node pruning & null move
		bool isSafe = !(isPv || board.inCheck || α >= Score.big || β <= -Score.big);
		if (doPrune && isSafe) {
			// pruning
			const  δ = margin(d);
			const sα = α - δ;
			const sβ = β + δ;

			// eval pruning (our position is very good, no need to search further)
			if (v >= sβ) return β;
			// razoring (our position is so bad, no need to search further)
			else if (v <= sα && (s = qs(sα, sα + 1)) <= sα) return α;

			// null move
			if (d >= 2 && v >= β && (board.color[board.player] & ~(board.piece[Piece.pawn] | board.piece[Piece.king]))) {
				r = 3 + d / 4 + min((v - β) / 128, 3);
				update(0);
					s = -αβ(-β, -β + 1, d - r, true);
				restore(0);
				if (!stop && s >= β) {
					if (s >= Score.high) s = β;
					if (d < 12 ||  (s = αβ(β - 1, β, d - r, false)) >= β) {
						tt.store(board.key, d, Bound.lower, toHash(s), toHash(v), h.move[0]);
						return s;
					}
				}
				isSafe = (s > -Score.big);
			}
		}

		// IID
		if (!h.move[0]) {
			r = isPv ? 2 : max(4, 2 + d / 4);
			if (d > r) {
				s = αβ(α, β, d - r, false);
				tt.probe(board.key, h);
				isSafe &= (-Score.big < s && s < Score.big);
			}
		}

		// prepare move generation
		moves.setup(board.inCheck, h.move, killer[ply], refutation[line.top & Limits.move.mask], history);

		const αOld = α;

		// generate moves in order & loop through them
		while ((m = (i = moves.selectMove(board)).move) != 0) {

			const bool isTactical = (i.value > 0) || (isPv && (board.isTactical(m) || board.giveCheck(m) || board.inCheck));
			iQuiet += !isTactical;
			// late move pruning
			if (isSafe && !isTactical && iQuiet > 4 + d * d && !board.giveCheck(m)) continue;
			// see pruning
			if (isSafe && d < 4 && iQuiet > 4 && board.see(m) < 0) continue;
			// check extension (if move is not losing)
			e = (board.giveCheck(m) && board.see(m) >= 0);

			if (isPv) pv[ply + 1].clear();

			update(m);
				// principal variation search (pvs)
				if (moves.isFirst(m)) {
					s = -αβ(-β, -α, d + e - 1, option.doPrune);
				} else {
					// late move reduction (lmr)
					r = isTactical ? 0 : reduce(d, iQuiet);
					// null window search (nws)
					s = -αβ(-α - 1, -α, d + e - r - 1, option.doPrune);
					// new pv found or new bestscore (bs) at reduced depth ?
					if ((α < s && s < β) || (s > bs && r > 0)) {
						s = -αβ(-β, -α, d + e - 1, option.doPrune);
					}
				}
			restore(m);
			if (stop) break;

			// best move ?
			if (s > bs && (bs = s) > α) {
				tt.store(board.key, d, tt.bound(bs, β), toHash(bs), toHash(v), m);
				if (isPv) pv[ply].set(m, pv[ply + 1]);
				if ((α = bs) >= β) {
					if (!board.isTactical(m) && !board.inCheck) heuristicsUpdate(m, d);
					return bs;
				}
			}
			if (!board.isTactical(m) && !board.inCheck) history.updateBad(board, m, d * d);
		}

		// no moves: mate or stalemate.
		if (moves.length == 0) {
			if (board.inCheck) return bs;
			else return 0;
		}

		if (!stop && bs <= αOld) tt.store(board.key, d, Bound.upper, toHash(bs), toHash(v), moves[0]);

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
			const bool isTactical = rootMoves.item[i].value > 0 || board.isTactical(m) || board.giveCheck(m) || board.inCheck;
			// check extension (if move is not losing)
			e = (board.giveCheck(m) && board.see(m) >= 0);

			pv[1].clear();
			update(m);
				// principal variation search (pvs)
				if (i == iPv) {
					s = -αβ(-β, -α, d + e - 1, option.doPrune);
				} else {
					// late move reduction (lmr)
					r = isTactical ? 0 : reduce(d, iQuiet);
					// null window search (nws)
					s = - αβ(-α - 1, -α, d + e - r - 1, option.doPrune);
					// new pv ?
					if ((α < s && s < β) || (s > bs && r > 0)) {
						s = -αβ(-β, -α, d + e - 1, option.doPrune);
					}
				}
			restore(m);
			info.update(nNodes, d, time);
			if (stop) break;
			if (s > bs && (bs = s) > α) {
				rootMoves.setBest(m, iPv);
				pv[0].set(m, pv[1]);
				info.store(iPv, s, d, selDepth, pv[0]);
				if (iPv == 0) tt.store(board.key, d, tt.bound(bs, β), toHash(bs), toHash(v), m);
				if ((α = bs) >= β) {
					if (!board.isTactical(m) && !board.inCheck) heuristicsUpdate(m, d);
					break;
				}
			} else if (i == iPv) {
				pv[0].set(m, pv[1]);
				info.store(iPv, s, d, selDepth, pv[0]);
			}
			if (!board.isTactical(m) && !board.inCheck) history.updateBad(board, m, d * d);
		}

		if (!stop && iPv == 0 && bs <= αOld) tt.store(board.key, d, Bound.upper, toHash(bs), toHash(v), rootMoves[0]);
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
					if (!checkTime(0.381966 * option.termination.time.max)) option.termination.time.max = option.termination.time.extra;
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

		if (option.verbose) {
			if (message) message.send(info.toUCI(n, tt));
			else writeln(info.toUCI(n, tt));
		} else if (message) message.log!'>'(info.toUCI(n, tt));
	}

	/* clear search setting before searching */
	void setup() {
		tt.clear(false);
		line.clear();
		pv[0].clear();
		ply = 0;
		nNodes = 0;
		stop = false;
		info.clear(option.multiPv, option.scoreInit);
		history.rescale(8);
		iPv = 0;
	}

	/* continue iterative deepening */
	bool persist(const int d) const {
		return checkTime(0.618034 * option.termination.time.max)
		    && !stop && d <= option.termination.depth.max
		    && (option.multiPv > 1 || (info.score[0] <= Score.mate - (d - 4) && info.score[0] >= (d - 4) - Score.mate));
	}

	/* search limited on some moves */
	void keepMoves(const ref Moves moves) {
		rootMoves = moves;
	}

	/* check position easyness and diminish thinking time if a position look easy */
	void adjustTime(const int d) {
		if (option.easy && d > 4) {
			Option o = option;
			option.verbose = false;
			multiPv(option.multiPv = 2, 4);
			option = o;
			if (info.score[0] > info.score[1] + 128) {
				option.termination.time.max *= 0.5;
			}
		}
	}

public:
	/* constructor: allocate the transposition table & set some options */
	this(size_t size = 64 * 1024 * 1024) {
		board = null;
		eval = new Eval(size / 32);
		tt = new TranspositionTable(size);
		message = null;
		option.verbose = true;
		option.doPrune = true;
		clear();
		setReduction(1.1, 0.7);
	}

	/* clear search caches */
	void clear() {
		tt.clear(true);
		heuristicsClear();
		eval.clear();
	}

	/* resize the tt */
	void resize(const size_t size) {
		tt.resize(size);
		eval.resize(size / 32);
	}

	/* init late move reduction table */
	void setReduction(const double lmrDepth, const double lmrMove) {
		foreach (d; 0 .. 32)
		foreach (m; 0 .. 32) {
			reduction[d][m] = cast (ubyte) ((d ? lmrDepth * std.math.log(d) : 0) + (m ? lmrMove * std.math.log(m) : 0));
		}
	}

	/* set board */
	void set(bool copy = true)(Board b) {
		Entry h;
		Move m;
		static if (copy) board = b.dup; else board = b;
		tt.probe(board.key, h);
		rootMoves.setup(board.inCheck, h.move, killer[2], refutation[2], history);
		while ((m = rootMoves.selectMove(board).move) != 0) {}
		if (h.bound == Bound.exact && h.move[0] == rootMoves[0]) {
			option.depthInit = max(1, h.depth);
			option.scoreInit = fromHash(h.score);
			if (abs(option.scoreInit) >= Score.mate - option.depthInit) option.depthInit = max(0, Score.mate - abs(option.scoreInit)); // to force entering the search
		} else {
			option.depthInit = 1;
			option.scoreInit = 0;
		}
		eval.set(board);
	}

	/* go search */
	void go(const ref Termination t, const ref Moves moves, const bool easy = false, const int doMultiPv = 1, const bool ponder = false, const bool doPrune = true) {
		timer.start();
			option.termination = t;
			option.isPondering = ponder;
			option.multiPv = max(1, min(doMultiPv, rootMoves.length));
			option.easy = easy;
			option.doPrune = doPrune;
			setup();
			if (moves.length > 0) keepMoves(moves);
			if (message) {
				if (tt.date == 1) message.log("search> date in hashtable cleared");
				message.log("search> go: ", option);
				message.log("search> moves: ", rootMoves);
			}
			if (rootMoves.length == 0) {
				rootMoves.push(0);
			} else if (option.easy && rootMoves.length == 1) {
				multiPv(option.multiPv, option.depthInit);
			} else  {
				adjustTime(option.depthInit);
				for (int d = option.depthInit; persist(d); ++d) {
					multiPv(option.multiPv, d);
				}
			}
		timer.stop();
	}

	/* go search without aspiration window nor iterating deepening */
	void go(const int d) {
		timer.start();
			setup();
			option.termination.time.max = option.termination.time.extra = double.infinity;
			option.termination.nodes.max = ulong.max;
			option.termination.depth.max = min(d, Limits.ply.max - 1);
			option.isPondering = false;
			option.multiPv = 1;
			option.easy = false;
			option.doPrune = true;
			info.score = αβ(2 - Score.mate, Score.mate - 1, d, option.doPrune);
		timer.stop();
	}

	/* get the best move */
	Move bestMove() @property {
		if (rootMoves[0] != info.pv[0].move[0] && message) message.log("bug> best move mismatch: rm: ", rootMoves[0].toPan(), ", pv[0]: ", info.pv[0].move[0].toPan());
		return rootMoves[0];
	}

	/* get the opponent expected move */
	Move hint() const @property {
		return info.pv[0].n > 1 ? info.pv[0].move[1] : 0;
	}

	/* get the last evaluated score */
	int score() const @property {
		return info.score[0];
	}
}

