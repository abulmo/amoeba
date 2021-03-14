/*
 * File search.d
 * Best move search.
 * © 2016-2020 Richard Delorme
 */

module search;

import board, eval, kpk, move, tt, util;
import std.algorithm, std.concurrency, std.parallelism, std.conv, std.format, std.getopt, std.math, std.stdio, std.string, std.traits;
import core.atomic, core.thread;

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
}

/*
 * Search Info
 */
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
	string toUCI(const int iPv, const TranspositionTable* tt, const ulong nodes) const {
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
			s ~= " time " ~ to!string(cast (ulong) (1000 * time));
			s ~= " nodes " ~ to!string(nodes);
			if (time > 0.0) s ~=  " nps " ~ to!string(cast (ulong) (nodes / time));
			if (time > 1.0) s ~= " hashfull " ~ to!string(tt.full);
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
	TranspositionTable *tt;
	Info info;
	Moves rootMoves;
	Line line;
	Line [Limits.ply.max + 1] pv;

	History history;
	ubyte[32][32] reduction;
	Move [2][Limits.ply.max + 2] killer;
	Move [Limits.move.size] refutation;
	int [Limits.ply.max] value;
	ulong nNodes;
	int ply, depth, selDepth, iPv, id;

	Thread thread;
	shared bool stop;

	/* is the main task */
	bool isMaster() const{
		return id == 0;
	}

	/* update heuristics */
	void heuristicsUpdate(const Moves moves, const Move m, const int d) {
		if (m != killer[ply][0]) {
			killer[ply][1] = killer[ply][0];
			killer[ply][0] = m;
		}

		if (ply > 0) refutation[line.top & Limits.move.mask] = m;

		history.updateGood(board, m, d * d);
		for (int i = 0; moves[i] != m; ++i) history.updateBad(board, moves[i], d * d);
	}

	/* update a move */
	void update(const Move m) {
		board.update(m, tt);
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
		return 200 * d - 100;
	}


	/* statistical error in score prediction by depth r for depth d on quiet position */
	static int error(const int d, const int r) {
		double a = 0.09 * r + 1.94;
		double b = -5.74 * r + 48.52;

		return cast (int) (a * d + b);
	}

	/* quiescence search */
	int qs(int α, int β) {
		enum δ = 50;
		const bool isPv = (α + 1 < β);
		int s, bs, v = Score.mate;
		Moves moves = void;
		Move m;
		Entry h;

		// drawn position ?
		if (board.isDraw) return 0;

		// distance to mate pruning
		bs = ply - Score.mate;
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
		value[ply] = v;

		// standpat
		const αOld = α;
		if (!board.inCheck && v > bs && (bs = v) > α) {
			tt.store(board.key, 0, tt.bound(bs, β), false, toHash(bs), toHash(v), h.move[0]);
			if ((α = bs) >= β) return bs;
		}

		//max depth reached
		if (ply == Limits.ply.max) return v;

		if (isPv && ply > selDepth) selDepth = ply;

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
	int pvs(int α, int β, const int d, const Move excludeMove = 0) {
		const bool isPv = (α + 1 < β);
		int v, s, bs, e, r, iQuiet;
		Moves moves = void;
		MoveItem i = void;
		Move m;
		Entry h;
		bool isextended = false;
		const Key key = board.key.exclude(excludeMove);

		if (stop) return α;

		// qs search
		if (d <= 0) return qs(α, β);

		// draw
		if (board.isDraw) return 0;

		// distance to mate
		bs = ply - Score.mate;
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
		value[ply] = v;

		// max depth reached
		if (ply == Limits.ply.max) return v;

		// selective search: "frontier" node pruning & null move & probcut
		const bool tactical = (board.inCheck || abs(v) >= Score.big || α >= Score.big || β <= -Score.big);
		const bool suspicious = (isPv || (ply >= 2 && value[ply] > value[ply - 2]));

		if (!tactical && !isPv) {
			// pruning
			const int  δ = margin(d);
			const int sα = α - δ;

			// razoring (our position is so bad, no need to search further)
			if (v <= sα) {
				if (d <= 2) return qs(α, β);
				else if (qs(sα, sα + 1) <= sα) return α;
			}

			if (board.nonPawnPiece(board.player)) {
				// eval pruning (our position is very good, no need to search further)
				if (v >= β + δ && v < Score.high) return v;

				// null move
				if (d >= 2 && v >= β) {
					r = 3 + d / 4 + min((v - β) / 128, 3);
					update(0);
						s = -pvs(-β, -β + 1, d - r);
					restore(0);
					if (stop) return α;
					if (s >= β) {
						if (s >= Score.high) s = β;
						if (d < 8 ||  (s = pvs(β - 1, β, d - r)) >= β) return s;
					}
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
						if (s >= λ) s = -pvs(-λ, -λ + 1, d - r);
					restore(m);
					if (stop) return α;
					if (s >= λ) return β;
				}
			}
		}

		// prepare move generation
		moves.setup(board.inCheck, h.move, killer[ply], refutation[line.top & Limits.move.mask], history);

		const int αOld = α;

		// generate moves in order & loop through them
		while ((m = (i = moves.selectMove(board)).move) != 0) {

			// skip the excluded move
			if (m == excludeMove) continue;

			const bool isQuiet = !((i.value > 0) || (isPv && (board.isTactical(m) || board.giveCheck(m) || board.inCheck)));
			iQuiet += isQuiet;

			// late move pruning
			if (!tactical && isQuiet && iQuiet > (4 + d * d) / (2 - suspicious) && !board.giveCheck(m)) continue;
			// see pruning
			if (!tactical && !suspicious && d < 4 && iQuiet > 4 && board.see(m) < 0) continue;

			// depth extension
			e = (h.extended && h.depth >= d);
			// singular move extension
			if (!e && d >= 8 && !excludeMove && m == h.move[0] && h.depth >= d - 4 && h.bound != Bound.upper) {
				const int λ = h.score - 2 * d - 1;
				r = 2 + d / 4;
				if (λ >= Score.low) {
					s = pvs(λ, λ + 1, d - r, m);
					e = (s <= λ);
				}
			} else {
				// check extension (if move is not losing or turbulent)
				e = (board.giveCheck(m) && (board.see(m) >= 0 || !tactical));
			}

			pv[ply + 1].clear();
			killer[ply + 2] = [0, 0];

			update(m);
				// principal variation search (pvs)
				if (moves.isFirst(m)) {
					s = -pvs(-β, -α, d + e - 1);
				} else {
					// late move reduction (lmr)
					r = isQuiet ? reduce(d, iQuiet) : 0;
					if (r && (suspicious || history.isGood(board[m.to], m.to))) --r;
					// null window search (nws)
					s = -pvs(-α - 1, -α, d + e - r - 1);
					// new pv found or new bestscore (bs) at reduced depth ?
					if ((α < s && s < β) || (s > bs && r > 0)) {
						s = -pvs(-β, -α, d + e - 1);
					}
				}
			restore(m);

			if (stop) return α;

			// best move ?
			if (s > bs && (bs = s) > α) {
				tt.store(key, d, tt.bound(bs, β), (e > 0), toHash(bs), toHash(v), m);
				if (isPv) pv[ply].set(m, pv[ply + 1]);
				if ((α = bs) >= β) {
					if (!board.isTactical(m) && !board.inCheck && !excludeMove) heuristicsUpdate(moves, m, d);
					return bs;
				}
			}
		}

		// no move: mate or stalemate.
		if (moves.length == 0) {
			if (board.inCheck) return bs;
			else return 0;
		}

		if (bs <= αOld) tt.store(key, d, Bound.upper, isextended, toHash(bs), toHash(v), moves[0]);

		return bs;
	}

	/* alpha-beta search at root level */
	void pvsRoot(int α, const int β, const int d) {
		const αOld = α;
		int s, bs = -Score.mate, e, r, iQuiet;
		const int v = eval(board, α, β);

		pv[0].clear();
		selDepth = 0;

		// loop thru all moves (and order them)
		for (int i = iPv; i < rootMoves.length; ++i) {
			Move m = rootMoves[i];
			const bool isQuiet = !(rootMoves.item[i].value > 0 || board.isTactical(m) || board.giveCheck(m) || board.inCheck);

			// check extension
			e = board.giveCheck(m);

			pv[1].clear();
			update(m);
				// principal variation search (pvs)
				if (i == iPv) {
					s = -pvs(-β, -α, d + e - 1);
				} else {
					// late move reduction (lmr)
					r = isQuiet ? reduce(d, iQuiet) : 0;
					// null window search (nws)
					s = - pvs(-α - 1, -α, d + e - r - 1);
					// new pv ?
					if ((α < s && s < β) || (s > bs && r > 0)) {
						s = -pvs(-β, -α, d + e - 1);
					}
				}
			restore(m);
			info.update(nNodes, d, search.time);
			if (stop) return;
			if (s > bs && (bs = s) > α) {
				rootMoves.setBest(m, iPv);
				pv[0].set(m, pv[1]);
				info.store(iPv, s, d, selDepth, pv[0]);
				if (iPv == 0) tt.store(board.key, d, tt.bound(bs, β), false, toHash(bs), toHash(v), m);
				if ((α = bs) >= β) break;
			} else if (i == iPv) {
				pv[0].set(m, pv[1]);
				info.store(iPv, s, d, selDepth, pv[0]);
			}
		}

		if (iPv == 0 && bs <= αOld) tt.store(board.key, d, Bound.upper, false, toHash(bs), toHash(v), rootMoves[0]);
	}

	/* aspiration window */
	void aspiration(const int α, const int β, const int d) {
		int λ, υ, δ = +10; // todo width depend on score ?

		if (d <= 4) {
			pvsRoot(α, β, d);
		} else {
			λ = info.score[iPv] - δ; υ = info.score[iPv] + δ;
			for (; !stop; δ *= 2) {
				λ = max(α, λ); υ = min(β, υ);
				pvsRoot(λ, υ, d);
				if (info.score[iPv] <= λ && λ > α) {
					if (isMaster && !search.option.isPondering && search.time > 0.3 * search.option.time.max) search.option.time.max = search.option.time.extra;
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
		return (search.option.isPondering || (id > 0 || search.time < 0.7 * search.option.time.max))
		    && !stop && d <= search.option.depth.end
		    && (search.option.multiPv > 1 || (info.score[0] <= Score.mate - (d - 4) && info.score[0] >= (d - 4) - Score.mate));
	}

	// is this position's bestmove obvious (when a single move is legal) ?
	bool isBestmoveObvious() {
		return search.option.easy && rootMoves.length == 1;
	}

	// iterative deepening
	void iterate() {
		if (search.message) search.message.log("smp > task[", id, "] iterate");
		for (depth = search.option.depth.begin; persist(depth); ++depth) {
			if (id > 0 && depth < search.option.depth.end && (id + depth) % 4 == 0) continue;
			multiPv(search.option.multiPv, depth);
		}
		stop = true;
		if (search.message) search.message.log("smp> task[", id, "] finished: ", info.toUCI(search.option.multiPv, tt, nNodes));
	}

	/* clear search setting before searching */
	void setup() {
		line.clear();
		pv[0].clear();
		ply = 0;
		nNodes = 0;
		stop = false;
		info.clear(search.option.multiPv, search.option.score.begin);
		history.rescale(8);
		iPv = 0;
	}

	/* create a new task */
	void clone(Search *s, uint i) {
		stop = true;
		search = s;
		tt = s.tt;
		eval = new Eval(tt.size / 32);
		clear();
		setReduction(1.1, 0.7);
		id = i;
	}

	/* start the search */
	void go(Moves moves, const int score) {
		setup();
		if (moves.length > 0) rootMoves = moves;
		info.score[0] = score;
		if (search.message) search.message.log("smp> task[", id, "] launched");
		thread = new Thread((){iterate();});
		thread.start();

		if (search.option.cpu.affinity.step) {
			int cpu = id * search.option.cpu.affinity.step + search.option.cpu.affinity.offset;
			if (cpu >= search.option.cpu.max) cpu = cpu % search.option.cpu.max + (cpu / search.option.cpu.max) % search.option.cpu.affinity.step;
			if (cpu < search.option.cpu.max) thread.setAffinity(cpu);
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
	TranspositionTable *tt;
	Option option;
	Chrono timer;
	Message message;
	Eval eval;

	/* constructor */
	this(const size_t s, const uint n, Message msg) {
		message = msg;
		tt = new TranspositionTable(s);
		threads(n);
		if (message) message.log("Transposition: size = ", tt.size, ", entries = ", tt.entry.length);
	}

	/* return the node count sum */
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

		foreach(ref t; tasks) t.clone(&this, i++);
		master = &tasks[0];
		eval = master.eval;
	}

	/* resize the tt */
	void resize(const size_t size) {
		tt.resize(size);
		foreach(ref t; tasks) t.eval.resize(size / 32);
		if (message) message.log("Transposition: size = ", tt.size, ", entries = ", tt.entry.length);
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
	void go(const ref Option o, Moves moves) {
		Entry h;

		timer.start();

			// set up the search
			clear(false);
			option = o;
			if (moves.length == 0 && tt.probe(master.board.key, h)) option.depth.begin = h.depth - (h.bound != Bound.exact);
			else option.depth.begin = 1;
			if (master.isBestmoveObvious()) option.depth.end = option.depth.begin;

			// make the tasks search and wait for its termination
			foreach (ref t; tasks) t.go(moves, h.score);
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
			master.info.score[0] = master.pvs(2 - Score.mate, Score.mate - 1, d);
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

	/* get the last pv */
	Line pv() const @property {
		return master.info.pv[0];
	}

	/* return search info */
	Info info() const @property {
		return master.info;
	}

	/* toUCI */
	void toUCI() {
		if (option.verbose) {
			if (message) message.send(master.info.toUCI(master.iPv, tt, countNodes()));
			else writeln(master.info.toUCI(master.iPv, tt, countNodes()));
		} else if (message) message.log!'>'(master.info.toUCI(master.iPv, tt, countNodes()));
	}

	//* test the speed of the search up to depth d */
	void bench(const int depth, const CPUAffinity affinity, const int loop) {
		// bratko - kopeck position set
		string [24] fens = [
			"1k1r4/pp1b1R2/3q2pp/4p3/2B5/4Q3/PPP2B2/2K5 b - -",
			"3r1k2/4npp1/1ppr3p/p6P/P2PPPP1/1NR5/5K2/2R5 w - - ",
			"2q1rr1k/3bbnnp/p2p1pp1/2pPp3/PpP1P1P1/1P2BNNP/2BQ1PRK/7R b - -",
			"rnbqkb1r/p3pppp/1p6/2ppP3/3N4/2P5/PPP1QPPP/R1B1KB1R w KQkq -",
			"r1b2rk1/2q1b1pp/p2ppn2/1p6/3QP3/1BN1B3/PPP3PP/R4RK1 w - -",
			"2r3k1/pppR1pp1/4p3/4P1P1/5P2/1P4K1/P1P5/8 w - -",
			"1nk1r1r1/pp2n1pp/4p3/q2pPp1N/b1pP1P2/B1P2R2/2P1B1PP/R2Q2K1 w - -",
			"4b3/p3kp2/6p1/3pP2p/2pP1P2/4K1P1/P3N2P/8 w - -",
			"2kr1bnr/pbpq4/2n1pp2/3p3p/3P1P1B/2N2N1Q/PPP3PP/2KR1B1R w - -",
			"3rr1k1/pp3pp1/1qn2np1/8/3p4/PP1R1P2/2P1NQPP/R1B3K1 b - -",
			"2r1nrk1/p2q1ppp/bp1p4/n1pPp3/P1P1P3/2PBB1N1/4QPPP/R4RK1 w - -",
			"r3r1k1/ppqb1ppp/8/4p1NQ/8/2P5/PP3PPP/R3R1K1 b - -",
			"r2q1rk1/4bppp/p2p4/2pP4/3pP3/3Q4/PP1B1PPP/R3R1K1 w - -",
			"rnb2r1k/pp2p2p/2pp2p1/q2P1p2/8/1Pb2NP1/PB2PPBP/R2Q1RK1 w - -",
			"2r3k1/1p2q1pp/2b1pr2/p1pp4/6Q1/1P1PP1R1/P1PN2PP/5RK1 w - -",
			"r1bqkb1r/4npp1/p1p4p/1p1pP1B1/8/1B6/PPPN1PPP/R2Q1RK1 w kq -",
			"r2q1rk1/1ppnbppp/p2p1nb1/3Pp3/2P1P1P1/2N2N1P/PPB1QP2/R1B2RK1 b - -",
			"r1bq1rk1/pp2ppbp/2np2p1/2n5/P3PP2/N1P2N2/1PB3PP/R1B1QRK1 b - -",
			"3rr3/2pq2pk/p2p1pnp/8/2QBPP2/1P6/P5PP/4RRK1 b - -",
			"r4k2/pb2bp1r/1p1qp2p/3pNp2/3P1P2/2N3P1/PPP1Q2P/2KRR3 w - -",
			"3rn2k/ppb2rpp/2ppqp2/5N2/2P1P3/1P5Q/PB3PPP/3RR1K1 w - -",
			"2r2rk1/1bqnbpp1/1p1ppn1p/pP6/N1P1P3/P2B1N1P/1B2QPP1/R2R2K1 b - -",
			"r1bqk2r/pp2bppp/2p5/3pP3/P2Q1P2/2N1B3/1PP3PP/R4RK1 b kq -",
			"r2qnrnk/p2b2b1/1p1p2pp/2pPpp2/1PP1P3/PRNBB3/3QNPPP/5RK1 w - -"
		];
		Option o = { {double.infinity, double.infinity}, {ulong.max}, {1, depth}, {0},  {totalCPUs, affinity}, 1, false, false, false };
		Board board = new Board;
		Moves moves = void;
		double N, N2, T, T2, V, V2;

		moves.clear();

		N = N2 = T = T2 = V = V2 = 0.0;
		foreach (l; 0 .. loop) {
			ulong n;
			double t = 0.0;
			foreach (fen; fens) {
				board.set(fen);
				clear();
				position(board);
				go(o, moves);
				n += countNodes;
				t += time;
			}
			if (loop > 1) write("loop ", l, " "); else write("bench: ");
			writeln("depth: ", depth, "; ", n, " nodes in ", t, " s, ", cast (int) (n / t), " nps.");
			N += n; N2 += n * n;
			T += t; T2 += t * t;
			V += n / t; V2 += (n * n) / (t * t);
		}
		if (loop > 1) {
			N /= loop; N2 /= loop; 
			T /= loop; T2 /= loop;
			V /= loop; V2 /= loop;
			writeln("bench: depth: ", depth, "; ", cast (long) N, " +/-", sqrt(N2 - N * N), " nodes in ", T, " +/-", sqrt(T2 - T * T), " s, ", cast (int) (N / T), " nps +/-", sqrt(V2 - V * V), ".");
		}
	}
}

/* bench command */
void bench(string [] arg, ref Search search) {
	int depth = 18;
	size_t hashSize = 256;
	int cpu = 1, loop = 1;
	bool help = false;
	CPUAffinity affinity;
	string a = "";

	getopt(arg, "depth|d", &depth, "hash|H", &hashSize, "cpu|c", &cpu, "affinity|a", &a, "loop|l", &loop, "help|h", &help);
	if (help) {
		writeln("bench  [--depth|-d <depth>] [--hash|-H <hashsize>] [--cpu|-c] <threads>] [--affinity|-a <[o:]s>] | [--loop|-l <n>] | [--help|-h]");
		writeln("Test the speed of the search on the bratko-kopeck test. Options");
		writeln("    --depth|-d <depth>     Search at depth d (default: 18)");
		writeln("    --hash|-H <Mb>         Set default HashSize");
		writeln("    --cpu|-c <threads>     Set default number of threads\n");
		writeln("    --loop|-l <n>          repeat the bench <n> times\n");
		writeln("    --affinity|-a <[o:]s>  Set cpu affinity as 'offset:step'");
		writeln("    --help|-h              Display this help");
	}

	if (hashSize.MBytes != search.tt.mask + 1) search.resize(hashSize.MBytes);
	if (search.tasks.length != cpu) search.threads(cpu);
	if (a != "") affinity.set(a);
	search.bench(depth, affinity, loop);
}

