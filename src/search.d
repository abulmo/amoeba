/*
 * File search.d
 * Best move search.
 * © 2016-2017 Richard Delorme
 */

module search;

import board, eval, kpk, move, util;
import std.algorithm, std.conv, std.format, std.getopt, std.math, std.stdio, std.string;

/* Hash table score bound */
enum Bound {upper, lower, exact}

/*
 * Entry Table Entry
 */
struct Entry {
	ulong code;
	ushort info;
	Move[2] move;
	short value;

	/* depth */
	int depth() const @property {
		return (info >> 2) & 127;
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

	void store(const Bound b, const int d, const int date, const int v, const int ply) {
		info = cast (ushort) (b | (d << 2) | (date << 9));
		value = cast (short) (v < Score.low ? v - ply : (v > Score.high ? v + ply : v));
	}

	/* score (with Mate score rescaled) */
	int score(const int ply) const {
		return value < Score.low ? value + ply : (value > Score.high ? value - ply : value);
	}

	/* update an existing entry */
	void update(const int d, const int ply, const int date, const Bound b, const int v, const Move m) {
		if (d >= depth) store(b, d, date, v, ply);
		if (m != move[0]) { move[1] = move[0]; move[0] = m;	}
	}

	/* set a new entry */
	void set(const Key k, const int d, const int ply, const int date, const Bound b, const int v, const Move m) {
		code = k.code;
		store(b, d, date, v, ply);
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
			foreach (ref h; entry) {
				if (cleaner) h = Entry.init;
				else h.info &= 511; // reset date to 0;
			}
		}
		++date;
	}

	/* look for an entry matching the zobrist key */
	bool probe(const Key k, ref Entry found) {
		const size_t i = cast (size_t) (k.code & mask);
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
	void store(const Key k, const int depth, const int ply, const Bound b, const int v, const Move m) {
		const size_t i = cast (size_t) (k.code & mask);
		Entry *w = &entry[i];
		foreach (ref h; entry[i .. i + bucketSize]) {
			if (h.code == k.code) {
				h.update(depth, ply, date, b, v, m);
				break;
			} else if (w.info > h.info) w = &h;
		}
		w.set(k, depth, ply, date, b, v, m);
	}

	/* speed up further access */
	void prefetch(const Key k) {
		const size_t i = cast (size_t) (k.code & mask);
		util.prefetch(&entry[i]);
	}

	/* choose between lower & exact bound */
	Bound bound(const int v, const int β) const {
		return v >= β ? Bound.lower : Bound.exact;
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
		size_t multiPv;
		bool easy;
		bool isPondering;
		bool verbose;

		string toString() const {
			string s;

			s = "Search setting:";
			s ~= "\n - Time:  max: " ~ to!string(termination.time.max) ~ " s, extra:" ~ to!string(termination.time.extra);
			s ~= " s\n - Nodes: max: " ~ to!string(termination.nodes.max);
			s ~= "\n - Depth: max: " ~ to!string(termination.depth.max) ~ " init: " ~ to!string(depthInit);
			s ~= "\n - Score: init: " ~ to!string(scoreInit);
			s ~= "\n - MultiPv: " ~ to!string(multiPv);
			s ~= "\n - easy: " ~ to!string(easy);
			s ~= "\n - isPondering: " ~ to!string(isPondering);
			s ~= "\n - verbose: " ~ to!string(verbose);

			return s;
		}
	}

	/* search Info */
	struct Info {
		ulong nNodes;
		int [Limits.moves.max] score;
		int depth;
		double time;
		Line [Limits.moves.max] pv;
		size_t multiPv;

		/* save current search infos (except pv / score) */
		void update(const ulong n, const int d, const double t) {
			nNodes = n; depth = d; time = t;
		}

		/* save current search infos (pv + score) */
		void store(const int iPv, int s, ref Line p) {
			const Move m = p.move[0];
			foreach (i; iPv + 1 .. multiPv) {
				if (m == pv[i].move[0]) {
					foreach_reverse (j; iPv .. i + 1) {
						score[j + 1] = score[j];
						pv[j + 1].set(pv[j]);
					}
					break;
				}
			}
			score[iPv] = s;	pv[iPv].set(p);
		}

		/* sort the pvs & score */
		void sort(const int iPv) {
			for (int j = iPv - 1; j >= 0 && score[j + 1] > score[j]; --j) {
				swap(score[j + 1], score[j]);
				pv[j].swap(pv[j + 1]);
			}
		}

		/* clear results */
		void clear(const size_t n, const int scoreInit) {
			nNodes = 0; depth = 0; time = 0.0;
			multiPv = n;
			foreach (i; 0 .. multiPv) {
				score[i] = 0;
				pv[i].clear();
			}
			score[0] = scoreInit;
		}

		/* write the search results found so far using UCI protocol */
		string toUCI(const int iPv) const {
			ulong t = cast (ulong) (1000 * time);
			ulong speed = cast (ulong) (nNodes / time);
			string s;

			foreach (i; 0 .. multiPv) {
				auto d = depth - (i > iPv);
				s ~= "info depth " ~ to!string(d);
				if (multiPv > 1) s ~= " multipv " ~ to!string(i + 1);
				s ~= " score ";
				if (score[i] > Score.high) s ~= "mate " ~ to!string((Score.mate + 1 - score[i]) / 2);
				else if (score[i] < Score.low) s ~= "mate " ~ to!string(-(Score.mate + score[i]) / 2);
				else s ~= "cp " ~ to!string(score[i]);
				s ~=  " nps " ~ to!string(cast (ulong) (nNodes / time));
				s ~= " time " ~ to!string(cast (ulong) (1000 * time));
				s ~= " nodes " ~ to!string(nNodes);
				s ~= " pv " ~ pv[i].toString();
				if (i + 1 < multiPv) s ~= '\n';
			}

			return s;
		}
	}
	Board board;
	TranspositionTable tt;
	History history;
	Info info;
	Moves rootMoves;
	Line line;
	Line [Limits.ply.max + 1] pv;
	Move [2][Limits.ply.max + 1] killer;
	Move [Limits.move.size] refutation;
	ulong nNodes;
	int ply, iPv;
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
					message.log("ponderhit> time: %.3f ; timeMax: %.3f ⭢ %.3f", time, option.termination.time.max, time + option.termination.time.max);
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

	/* quiescence search */
	int qs(int α, int β) {
		enum δ = 200;
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
		if (bs > α && (α = bs) >= β) return bs;
		s = Score.mate - ply - 1;
		if (s < β && (β = s) <= α) return s;

		// transposition table probe
		if (tt.probe(board.key, h)) {
			s = h.score(ply);
			if (h.bound == Bound.exact) return s;
			else if (h.bound == Bound.lower && (α = s) >= β) return s;
			else if (h.bound == Bound.upper && (β = s) <= α) return s;
		}

		// standpat
		const αOld = α;
		if (!board.inCheck) {
			v = eval(board, α, β);
			if (h.info > 0 && ((h.bound == Bound.lower && s > v) || (h.bound == Bound.upper && s < v))) v = s;
			if (v > bs && (bs = v) > α) {
				tt.store(board.key, 0, ply, tt.bound(bs, β), bs, h.move[0]);
				if ((α = bs) >= β) return bs;
			}
			v += δ;
		}

		//max depth reached
		if (ply == Limits.ply.max) return eval(board, α, β);

		// move generation: good captures & promotions if not in check
		moves.setup(board.inCheck, h.move);

		while ((m = moves.selectMove(board).move) != 0) {
			s = eval(board, m) + v;
			if (s > α || isPv || board.inCheck || board.giveCheck(m)) {
				update(m);
					s = -qs(-β, -α);
				restore(m);
			}
			if (stop) break;
			if (s > bs && (bs = s) > α) {
				tt.store(board.key, board.inCheck, ply, tt.bound(bs, β), bs, m);
				if ((α = bs) >= β) break;
			}
		}

		if (!stop && bs <= αOld) tt.store(board.key, board.inCheck, ply, Bound.upper, bs, h.move[0]);

		return bs;
	}

	/* alpha-beta search (PVS/negascout variant) */
	int αβ(int α, int β, const int d, const bool doPrune = true) {
		const bool isPv = (α + 1 < β);
		int v, s, bs, e, r, iQuiet;
		Moves moves = void;
		Move m;
		MoveItem i;
		Entry h;

		// qs search
		if (d <= 0) return qs(α, β);

		// search abort
		if (abort()) return α;

		// draw
		if (board.isDraw) return 0;

		// distance to mate
		bs = ply - Score.mate;
		if (bs > α && (α = bs) >= β) return bs;
		s = Score.mate - ply - 1;
		if (s < β && (β = s) <= α) return s;

		// transposition table probe
		if (tt.probe(board.key, h) && !isPv) {
			s = h.score(ply);
			if (h.depth >= d || s <= ply - Score.mate || s >= Score.mate - ply - 1) {
				if (h.bound == Bound.exact) return s;
				else if (h.bound == Bound.lower && s >= β) return s;
				else if (h.bound == Bound.upper && s <= α) return s;
				if (h.bound != Bound.upper && s > bs) bs = s;
			}
		}

		//max depth reached
		if (ply == Limits.ply.max) return eval(board, α, β);

		// selective search: "frontier" node pruning & null move
		bool hasThreats = (board.inCheck || α >= Score.big || β <= -Score.big);
		if (doPrune && !isPv && !hasThreats) {
			// pruning
			const  δ = 200 * d - 100;
			const sα = α - δ;
			const sβ = β + δ;

			v = eval(board);
			if (h.info > 0 && ((h.bound == Bound.lower && s > v) || (h.bound == Bound.upper && s < v))) v = s;
			
			// eval pruning (our position is very good, no need to search further)
			if (v >= sβ) return β;
			// razoring (our position is so bad, no need to search further)
			else if (v <= sα && (s = qs(sα, sα + 1)) <= sα) return α;

			// null move
			if (d >= 2 && line.top && (board.color[board.player] & ~(board.piece[Piece.pawn] | board.piece[Piece.king]))) {
				r = 3 + d / 4;
				update(0);
					s = -αβ(-β, -β + 1, d - r);
				restore(0);
				if (!stop && s >= β) {
					if (s >= Score.high) s = β;
					tt.store(board.key, d, ply, Bound.lower, s, h.move[0]);
					return s;
				}
				hasThreats = (s < Score.low);
			}
		}

		// IID
		if (!h.move[0]) {
			r = isPv ? 2 : max(4, 2 + d / 4);
			if (d > r) {
				αβ(α, β, d - r, false);
				tt.probe(board.key, h);
			}
		}

		// prepare move generation
		moves.setup(board.inCheck, h.move, killer[ply], refutation[line.top & Limits.move.mask], history);

		const αOld = α;

		// generate moves in order & loop through them
		while ((m = (i = moves.selectMove(board)).move) != 0) {
			if (isPv) pv[ply + 1].clear();
			const bool isTactical = (i.value > 0);
			const bool isQuiet = !(isTactical || hasThreats || board.giveCheck(m));
			iQuiet += isQuiet;

			// late move pruning
			if (isQuiet && !isPv && iQuiet > 4 + d * d) continue;

			// see pruning
			if (!hasThreats && !isPv && d < 2 && iQuiet > 4 && board.see(m) < 0) continue;

			// check extension (if move is not losing)
			e = (board.inCheck && board.see(m) >= 0);

			update(m);
				// principal variation search (pvs)
				if (moves.isFirst(m)) {
					s = -αβ(-β, -α, d + e - 1);
				} else {
					// late move reduction (lmr)
					if (!isQuiet) r = 0;
					else if (iQuiet <= 4) r = 1;
					else r = 1 + d / 4;
					// null window search (nws)
					s = -αβ(-α - 1, -α, d + e - r - 1);
					// new pv found or new bestscore (bs) at reduced depth ?
					if ((α < s && s < β) || (s > bs && r > 0)) {
						s = -αβ(-β, -α, d + e - 1);
					}
				}
			restore(m);
			if (stop) break;

			// best move ?
			if (s > bs && (bs = s) > α) {
				tt.store(board.key, d, ply, tt.bound(bs, β), bs, m);
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

		if (!stop && bs <= αOld) tt.store(board.key, d, ply, Bound.upper, bs, moves[0]);

		return bs;
	}

	/* alpha-beta search at root level */
	void αβRoot(int α, const int β, const int d) {
		const αOld = α;
		int s, bs = -Score.mate, e, r, iQuiet;

		pv[0].clear();

		// loop thru all moves (and order them)
		const bool hasThreats = (board.inCheck || α >= Score.big || β <= -Score.big);
		for (int i = iPv; i < rootMoves.length; ++i) {
			Move m = rootMoves[i];
			const bool isTactical = rootMoves.item[i].value > 0;
			pv[1].clear();
			// check extension (if move is not losing)
			e = (board.inCheck && board.see(m) >= 0);
			update(m);
				// principal variation search (pvs)
				if (rootMoves.isFirst(m)) {
					s = -αβ(-β, -α, d + e - 1);
				} else {
					// late move reduction (lmr)
					if (hasThreats || isTactical || board.inCheck) r = 0;
					else if (iQuiet++ <= 3) r = 1;
					else r = 1 + d / 4;
					// null window search (nws)
					s = - αβ(-α - 1, -α, d + e - r - 1);
					// new pv ?
					if ((α < s && s < β) || (s > bs && r > 0)) {
						s = -αβ(-β, -α, d + e - 1);
					}
				}
			restore(m);
			if (stop) break;
			if (s > bs && (bs = s) > α) {
				rootMoves.setBest(m, iPv);
				pv[0].set(m, pv[1]);
				info.update(nNodes, d, time);
				info.store(iPv, bs, pv[0]);
				if (iPv == 0) tt.store(board.key, d, 0, tt.bound(bs, β), bs, m);
				if ((α = bs) >= β) {
					if (!board.isTactical(m) && !board.inCheck) heuristicsUpdate(m, d);
					break;
				}
			} else if (i == iPv) {
				pv[0].set(m, pv[1]);
				info.store(iPv, s, pv[0]);
			}
			if (!board.isTactical(m) && !board.inCheck) history.updateBad(board, m, d * d);
		}

		if (!stop && iPv == 0 && bs <= αOld) tt.store(board.key, d, ply, Bound.upper, bs, rootMoves[0]);

		// store results
		info.update(nNodes, d, time);
	}

	/* aspiration window */
	void aspiration(const int α, const int β, const int d) {
		int λ, υ, up = +10, down = -10;

		if (d <= 4) {
			αβRoot(α, β, d);
		} else do {
				λ = max(α, info.score[iPv] + down);
				υ = min(β, info.score[iPv] + up);
				αβRoot(λ, υ, d);
				if (info.score[iPv] <= λ && down < -1) {
					if (!checkTime(0.381966 * option.termination.time.max)) option.termination.time.max = option.termination.time.extra;
					down *= 2 ; up = 1;
				} else if (info.score[iPv] >= υ && up > 1) {
					down = -1; up *= 2;
				} else {
					down = α - info.score[iPv];
					up = β - info.score[iPv];
				}
		} while (!stop && ((info.score[iPv] <= λ && λ > α) || (info.score[iPv] >= υ && υ < β)));
	}

	/* multiPv : search n best moves */
	void multiPv(const size_t n, const int d) {
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
			if (message) message.send(info.toUCI(iPv - 1));
			else writeln(info.toUCI(iPv - 1));
		} else if (message) message.log!'>'(info.toUCI(iPv - 1));
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
		heuristicsClear();
		iPv = 0;
	}

	/* continue iterative deepening */
	bool persist(const int d) const {
		return checkTime(0.618034 * option.termination.time.max)
		    && !stop && d <= option.termination.depth.max
		    && (option.multiPv > 1 || (info.score[0] <= Score.mate - d && info.score[0] >= d - Score.mate));
	}

	/* search limited on some moves */
	void keepMoves(const ref Moves moves) {
		rootMoves = moves;
	}

public:
	/* constructor: allocate the transposition table & set some options */
	this(size_t size = 64 * 1024 * 1024) {
		board = null;
		eval = new Eval(size / 32);
		tt = new TranspositionTable(size);
		message = null;
		option.verbose = true;
		clear();
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
			option.scoreInit = h.score(0);
		} else {
			option.depthInit = 1;
			option.scoreInit = 0;
		}
		eval.set(board);
	}

	/* go search */
	void go(const ref Termination t, const ref Moves moves, const bool easy = false, const int doMultiPv = 1, bool ponder = false) {
		timer.start();
			option.termination = t;
			option.isPondering = ponder;
			option.multiPv = max(1, min(doMultiPv, rootMoves.length));
			option.easy = easy;
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
			info.score = αβ(2 - Score.mate, Score.mate - 1, d);
		timer.stop();
	}

	/* show settings */
	void showSetting() {
		if (message) {
			message.send(" setting> ", option.toString());
			message.send(" setting> TT size: ", tt.entry.length, " entries, ", tt.entry.length * Entry.sizeof, " B");
			message.send(" setting> ", eval.setting());
		}
	}

	/* show settings */
	void showMoves() {
		rootMoves.dump();
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


/*
 * Test the search quality using an epd file
 */

/* Look if epd best moves match the best move found. */
bool epdMatch(string epd, Board b, const Move found) {
	write("found ", found.toSan(b));
	string [] solutions = epd.findBetween("bm", ";").split();
	if (solutions.length > 0) {
		write("; expected = ", solutions);
		foreach (s; solutions) {
			const Move m = fromSan(s, b);
			if (m == found) return true;
		}
		return false;
	} else {
		string [] mistakes = epd.findBetween("am", ";").split();
		if (mistakes.length > 0) {
			write("; unexpected = ", mistakes);
			foreach (s; mistakes) {
				const Move m = fromSan(s, b);
				if (m == found) return false;
			}
		}
	}
	return true;
}

/* Test the search using an epd file */
int epdTest(string [] arg, const bool checkSolution = true) {
	double t = double.infinity, T = 0, D = 0;
	int d = Limits.ply.max, n, nGoods;
	size_t ttSize = 256;
	ulong N;
	string epdFile;
	bool verbose, help, dbg;
	Termination termination;
	Moves moves;

	getopt(arg, std.getopt.config.caseSensitive, "movetime|t", &t, "depth|d", &d, "hash|H", &ttSize,
		"verbose|v", &verbose, "file|f", &epdFile, "debug|g", &dbg, "help|h", &help);
	if (help) {
		stderr.writeln(arg[0] ~ " [--movetime|-t <time>] [--depth|-d <depth>] [--hash|-H] [--verbose|-v <bool>] --file|-f <epd file> [--help|-h]");
		if (arg[0] == "bench") stderr.writefln("Test search speed on a set of positions.");
		else if (arg[0] == "epd") stderr.writefln("Test search performance on a set of positions.");
		stderr.writefln("\t--movetime|-t <time>  Maximum time per move in seconds (default: %s)", t);
		stderr.writefln("\t--depth|-d <depth>    Maximum depth time per move in seconds (default: %s)", d);
		stderr.writefln("\t--hash|-H <size>      Hash size in Mb (default %s MB)", ttSize);
		stderr.writefln("\t--verbose|-v          More verbose output (default: %s)", verbose);
		stderr.writefln("\t--debug|-g            Log on to a debug file (default: %s)", dbg);
		stderr.writeln("\t--file|-f <epd file>   EPD file to test");
		stderr.writeln("\t--help|-h              Display this help\n");
	}

	termination.time.max = termination.time.extra = t;
	termination.depth.max = min(d, Limits.ply.max);
	termination.nodes.max = ulong.max;
	moves.clear();
	ttSize = min(65536, max(1, ttSize));
	Search s = new Search(ttSize * 1024 * 1024);
	s.message = new shared Message("Amoeba-epdtest");
	if (dbg) s.message.logOn();
	s.option.verbose = verbose;
	Board b = new Board;
	writefln("*** epdTest  depth: %d, time: %.3fs, memory: %d MB ***", d, t, ttSize);
	auto f = std.stdio.File(epdFile);

	foreach (line; f.byLine()) {
		string epd = to!string(line).chomp();
		if (epd == "" || epd[0] == '#') continue;
		b.set(epd);
		if (verbose) {
			writeln();
			writeln(b);
		}
		s.clear();
		s.set(b);
		s.go(termination, moves);
		if (checkSolution && epdMatch(epd, b, s.bestMove)) ++nGoods;
		T += s.info.time;
		N += s.info.nNodes;
		D += s.info.depth;
		++n;
		if (checkSolution) writefln(": %d founds / %d problems ", nGoods, n);
	}

	if (checkSolution) writef("epd: %d founds / %d problems ", nGoods, n);
	else writef("bench: %d positions ", n);
	writefln(" %d nodes in %.3fs : %.0f nps, depth = %.2f", N, T, N / T, D / n);
	stdout.flush();

	return (100 * nGoods) / n;
}

/* Unittest */
unittest {
	writeln("");
	claim(epdTest(["epd", "-t", "1", "-f", "bk.epd"]) >= 40);
}

