/*
 * File search.d
 * Best move search.
 * © 2016 Richard Delorme
 */

module search;

import board, eval, kpk, move, util;
import std.stdio, std.conv, std.string, std.format, std.algorithm, std.math, std.getopt;

enum Score {mate = 30000, low = -10000, high = 10000, big = 1200}

/*
 * Node type
 */
enum Node {pv, cut, all}

/* check if node is in the PV */
bool isPV(in Node node) pure @property {
	return node == Node.pv;
}

/* check if node is a cut node */
bool isCut(in Node node) pure @property {
	return node == Node.cut;
}

/* check if node is an all node */
bool isAll(in Node node) pure @property {
	return node == Node.all;
}

/* guess Node type */
Node next(in Node node, in bool isBest) pure {
	static immutable Node [3][2] nextNode = [[Node.cut, Node.all, Node.cut], [Node.pv, Node.all, Node.cut]];
	return nextNode[isBest][node];
}


/* Hash table score bound (without upper bound) */
enum Bound {lower, exact}

/*
 * Entry Table Entry
 */
final struct Entry {
	ulong code;
	ushort info;
	Move[2] move;
	short value;

	/* depth */
	int depth() pure const @property {
		return (info >> 1) & 127;
	}

	/* bound type */
	Bound bound() pure const @property {
		return cast (Bound) (info & 1);
	}

	/* aging date */
	int date() pure const @property {
		return (info >> 8);
	}

	/* refresh the aging date */
	void refresh(in int date) pure {
		info = cast (ushort) ((info & 255) | (date << 8));
	}

	/* score (with Mate score rescaled) */
	int score(in int ply) pure const {
		return value < Score.low ? value + ply : (value > Score.high ? value - ply : value);
	}

	/* update an existing entry */
	void update(int d, in int ply, in int date, in int α, in int β, in int v, in Move m) pure {
		info = cast (ushort) ((v < β) | (d << 1) | (date << 8));
		value = cast (short) (v < Score.low ? v - ply : (v > Score.high ? v + ply : v));
		if (m != move[0]) {
			move[1] = move[0]; move[0] = m;
		}
	}

	/* set a new entry */
	void set(in Key k, in int d, in int ply, in int date, in int α, in int β, in int v, in Move m) pure {
		code = k.code;
		info = cast (ushort) ((v < β) | (d << 1) | (date << 8));
		value = cast (short) (v < Score.low ? v - ply : (v > Score.high ? v + ply : v));
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
	static immutable bucketSize = 4;
	Entry [] entry;
	ulong mask;
	ubyte date;

	/* constructor */
	this(size_t size) pure {
		size_t s;
		for (s = Entry.sizeof; s < size; s <<= 1) {}
		if (s > size) s >>= 1;
		if (s < Entry.sizeof) entry.length = mask = 0;
		else {
			mask = s / Entry.sizeof - 1;
			entry.length = mask + bucketSize;
		}
		clear();
	}

	/* clear the table */
	void clear(in bool cleaner = true) pure {
		if (cleaner || date == 255) {
			date = 0;
			foreach (ref h; entry) {
				if (cleaner) h = Entry.init;
				else h.info &= 255; // reset date to 0;
			}
		}
		++date;
	}

	/* look for an entry matching the zobrist key */
	bool probe(in Key k, out Entry found) pure {
		immutable i = k.code & mask;
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
	void store(in Key k, in int depth, in int ply, in int α, in int β, in int v, in Move m) pure {
		immutable i = k.code & mask;
		Entry *w = &entry[i];
		foreach (ref h; entry[i .. i + bucketSize]) {
			if (h.code == k.code) {
				h.update(depth, ply, date, α, β, v, m);
				break;
			} else if (w.info > h.info) w = &h;
		}
		w.set(k, depth, ply, date, α, β, v, m);
	}
}

/*
 * Search
 */
final class Search {
	/* search option */
	struct Option {
		struct Time {
			real max;
			real extra;
		}
		size_t ttSize;
		Time time;
		int depthMax;
		int depthInit;
		bool isPondering;
		bool verbose;
	}

	/* search Info */
	struct Info {
		ulong nNodes;
		int score;
		int depth;
		real time;
		Line pv;

		/* save curent results (except pv) */
		void save(in int s, in ulong n, in int d, in real t) pure {
			nNodes = n; depth = d; time = t;
			if (s > -Score.mate) score = s;
		}

		/* clear results */
		void clear() pure {
			nNodes = 0; score = 0; depth = 0; time = 0.0;
			pv.clear();
		}
	}
	Board board;
	Eval eval;
	TranspositionTable tt;
	shared Event event;
	Option option;
	Info info;
	int ply;
	Moves rootMoves;
	Line line;
	Line [Limits.plyMax + 1] pv;
	Move [2][Limits.plyMax + 1] killer;
	Move [Limits.moveSize] refutation;
	ushort [CPiece.size * Square.size] history;
	bool stop;
	std.stdio.File logFile;

	ulong nNodes;
	Chrono timer;

	/* constructor: allocate the transposition table & set some options */
	this(size_t size = 64 * 1024 * 1024) {
		board = null;
		eval = new Eval;
		tt = new TranspositionTable(size);
		event = null;
		option.ttSize = size;
		option.verbose = true;
	}

	/* (simple) logging */
	void log(T...) (in string fmt, T args) {
		if (logFile.isOpen) logFile.writefln(fmt, args);
	}

	/* check if enough time is available */
	bool checkTime(in real timeMax) const {
		return option.isPondering || time < timeMax;
	}

	/* check if the search should abort or continue */
	bool abort() {
		if ((nNodes & 0x3ff) == 0) {
			if (event) {
				if (option.isPondering && event.has("ponderhit")) {
					option.isPondering = false;
					log("ponderhit> time: %.3f ; timeMax: %.3f ⭢ %.3f", time, option.time.max, time + option.time.max);
					option.time.max += time;
					option.time.extra += time;
				}
				if (event.has("stop")) stop = true;
			}
			if (!checkTime(option.time.max)) stop = true;
			if ((nNodes & 0xfffff) == 0) log("time> %.4 s", time);
		}
		return stop;
	}

	/* get the best Move */
	Move bestMove() pure const @property {
		return rootMoves[0];
	}

	/* get the best Move */
	Move hint() pure const @property {
		return info.pv.n > 1 ? info.pv.move[1] : 0;
	}

	/* return the spent time */
	real time() const @property {
		return timer.time();
	}

	/* clear heuristics */
	void heuristicsClear() pure {
		foreach (ref k; killer) k = 0;
		foreach (ref r; refutation) r = 0;
		foreach (ref h; history) h = 0;
	}

	/* update heuristics */
	void heuristicsUpdate(in Move m, in int d) pure {
		if (m != killer[ply][0]) {
			killer[ply][1] = killer[ply][0];
			killer[ply][0] = m;
		}
		if (ply > 0) refutation[line.top & Limits.moveMask] = m;

		auto i = board[m.from] * Square.size + m.to;
		if ((history[i] += d * d) > Limits.historyMax) {
			foreach (ref h; history) h /= 2;
		}
	}

	/* write the search result so far using UCI protocol */
	void writeUCI(std.stdio.File f=stdout) const {
		f.write("info depth ", info.depth, " score ");
		if (info.score > Score.high) f.write("mate ", (Score.mate + 1 - info.score) / 2);
		else if (info.score < -Score.high) f.write("mate ", -(Score.mate + info.score) / 2);
		else f.write("cp ", info.score);
		f.writefln(" nps %.0f time %.0f nodes %s pv %s", toad(info.nNodes / info.time), toad(1000 * info.time), info.nNodes, info.pv);
		f.flush();
	}

	/* update a move */
	void update(in Move m) pure {
		board.update(m);
		if (m) eval.update(board, m);
		line.push(m);
		++ply;
		++nNodes;
	}

	/* restore a move */
	void restore(in Move m) pure {
		--ply;
		line.pop();
		board.restore(m);
		if (m) eval.restore();
	}

	/* quiescence search */
	int qs(int α, int β) {
		int s, bs; 
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
			if (s > bs) bs = s;
		}

		// standpat
		immutable αOld = α;
		if (!board.inCheck) {
			bs = eval(board, α, β);
			if (bs > α && (α = bs) >= β) return bs;
		}
	
		//max depth reached
		if (ply == Limits.plyMax) return eval(board, α, β);

		// move generation: good capture & promotion if not in check
		moves.init(board.inCheck, h.move);

		while ((m = moves.selectMove(board)) != 0) {
			update(m);
				s = -qs(-β, -α);
			restore(m);
			if (s > bs && (bs = s) > α) {
				tt.store(board.key, board.inCheck, ply, αOld, β, bs, m);
				if ((α = bs) >= β) break;
			}
		}

		return bs;
	}

	/* alpha-beta search (PVS/negascout variant) */
	int αβ(int α, int β, in int d, in Node node = Node.pv, in bool doPrune = true) {
		int s, bs, e, r, iQuiet;
		Moves moves = void;
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
		if (bs > α && (α = bs) >= β) return bs;
		s = Score.mate - ply - 1;
		if (s < β && (β = s) <= α) return s;		

		// transposition table probe
		if (tt.probe(board.key, h) && h.depth >= d && !node.isPV) {
			s = h.score(ply);
			if (h.bound == Bound.exact) return s;
			else if (h.bound == Bound.lower && s >= β) return s;
		}

		// ply max reached
		if (ply == Limits.plyMax) return eval(board, α, β);

		// selective search: "frontier" node pruning & null move
		bool hasThreats = (board.inCheck || α >= Score.big || β <= -Score.big);
		if (doPrune && !node.isPV && !hasThreats) {
			immutable int [5] δ = [0, 100, 300, 500, 900];
			// pruning
			if (d < δ.length) {
				immutable sα = α - δ[d];
				immutable sβ = β + δ[d];
				s = eval(board);
				// eval pruning (our position is very good, no need to search further)
				if (s >= sβ) return β;
				// razoring (our position is so bad, no need to search further)
				else if (s <= sα && (s = qs(sα, sα + 1)) <= sα) return α;
			}
			// null move
			if (line.top && (board.color[board.player] & ~(board.piece[Piece.pawn] | board.piece[Piece.king]))) {
				r = 4;
				update(0);
					s = -αβ(-β, -β + 1, d - r, Node.cut, true);
				restore(0);
				if (s >= β) {
					if (s >= Score.big) s = β;
					if (d < 8 ||  αβ(β - 1, β, d - r, Node.cut, false) >= β) {
						tt.store(board.key, d, ply, α, β, s, h.move[0]);
						return s;
					}
				}
				hasThreats |= (s < -Score.big);
			}
		}

		// IID
		if (!h.move[0]) {
			r = node.isPV ? 2 : max(4, 2 + d / 4);
			if (d > r) {
				αβ(α, β, d - r, node, false);
				tt.probe(board.key, h);
			}
		}

		// generate all moves & score them
		moves.init(board.inCheck, h.move, killer[ply], refutation[line.top & Limits.moveMask], history);

		// loop thru all moves (and order them)
		immutable αOld = α;
		while ((m = moves.selectMove(board)) != 0) {
			if (node.isPV) pv[ply + 1].clear();
			immutable bool isTactical = moves.value[moves.index - 1] > 0;
			// check extension (if move is not loosing)
			e = (board.inCheck && board.see(m) >= 0)  ? 1 : 0;
			update(m);
				// principal variation search (pvs)
				if (moves.isFirst(m)) {
					s = -αβ(-β, -α, d + e - 1, node.next(true));
				} else {
					// late move reduction (lmr)
					if (hasThreats || isTactical || board.inCheck || d <= 1) r = 0;
					else if (iQuiet++ <= 4) r = 1;
					else r = 1 + d / 4;
					// null window search (nws) 
					s = -αβ(-α - 1, -α, d + e - r - 1, node.next(false));
					// new pv ?
					if ((α < s && s < β) || (s > bs && r > 0)) {
						s = -αβ(-β, -α, d + e - 1, node.next(s > bs));
					}
				}
			restore(m);
			// best move ?
			if (s > bs && (bs = s) > α) {
				tt.store(board.key, d, ply, αOld, β, bs, m);
				if (board[m.to] == CPiece.none && !board.inCheck) heuristicsUpdate(m, d);
				if (node.isPV) pv[ply].set(m, pv[ply + 1]);
				if ((α = bs) >= β) return bs;
			}
		}

		if (moves.length == 0) {
			if (board.inCheck) return bs;
			else return 0;
		}

		return bs;
	}

	/* alpha-beta search at root level */
	void αβRoot(int α, in int β, in int d) {
		int s, bs = -Score.mate, e, r, iQuiet;
		Result draw;
		Entry h;

		pv[0].clear();
		rootMoves.reset();	

		// loop thru all moves (and order them)
		immutable bool hasThreats = (board.inCheck || α >= Score.big || β <= -Score.big);
		immutable αOld = α;
		foreach (ref m; rootMoves) {
			immutable bool isTactical = rootMoves.value[rootMoves.index] > 0;
			pv[1].clear();
			// check extension (if move is not loosing)
			e = (board.inCheck && board.see(m) >= 0);
			update(m);
				// principal variation search (pvs)
				if (rootMoves.isFirst(m)) {
					s = -αβ(-β, -α, d + e - 1, Node.pv);
				} else {
					// late move reduction (lmr)
					if (hasThreats || isTactical || board.inCheck || d <= 1) r = 0;
					else if (iQuiet++ <= 4) r = 1;
					else r = 1 + d / 4;
					// null window search (nws) 
					s = - αβ(-α - 1, -α, d + e - r - 1, Node.cut);
					// new pv ?
					if ((α < s && s < β) || (s > bs && r > 0)) {
						s = -αβ(-β, -α, d + e - 1, Node.pv.next(s > bs));
					}
				}
			restore(m);
			if (!stop && s > bs && (bs = s) > α) {
				rootMoves.setBest(m);
				pv[0].set(m, pv[1]);
				info.pv.set(pv[0]);
				tt.store(board.key, d, 0, αOld, β, bs, m);
				α = bs;
				if (α >= β) break;
			}
		}

		// store results
		info.save(bs, nNodes, d, time);
	}

	/* aspiration window */
	void aspiration(in int α, in int β, in int d) {
		int λ, υ, up = +50, down = -50;

		if (d <= 4) {
			αβRoot(α, β, d);
		} else do {
				λ = max(α, info.score + down);
				υ = min(β, info.score + up);
				αβRoot(λ, υ, d);
				if (info.score <= λ && down < -1) {
					if (checkTime(0.381966 * option.time.max)) option.time.max = option.time.extra;
					down *= 2 ; up = 1;
				} else if (info.score >= υ && up > 1) {
					down = -1; up *= 2;
				} else {
					down = α - info.score;
					up = β - info.score;
				}
		} while (!stop && ((info.score <= λ) || (info.score >= υ)));

		if (option.verbose) {
			writeUCI();
			if (logFile.isOpen) {
				logFile.write("search> ");
				writeUCI(logFile);
			}
		}			
	}

	/* clear search setting before searching */
	void setup() pure {
		tt.clear(false);
		line.clear();
		pv[0].clear();
		ply = 0;
		nNodes = 0;
		stop = false;
		info.clear();
		heuristicsClear();
		eval.set(board);
	}

	/* continue id */
	bool persist(in int d) const {
		return checkTime(0.618034 * option.time.max) && !stop && d <= option.depthMax && abs(info.score) <= Score.mate - d;
	}

	/* clear search setting */
	void clear() pure {
		tt.clear(true);
		heuristicsClear();
	}


	/* resize the tt */
	void resize(in size_t size) {
		tt = new TranspositionTable(size);
	}

	/* set board */
	void set(in Board b) pure {
		Entry h;
		Move m;
		board = b.dup;
		tt.probe(board.key, h);
		rootMoves.init(board.inCheck, h.move, killer[0], refutation[0], history);
		while ((m = rootMoves.selectMove(board)) != 0) {}
		if (h.bound == Bound.exact && h.move[0] == rootMoves[0]) {
			option.depthInit = max(1, h.depth);
		} else {
			option.depthInit = 1;
		}
	}

	/* go search */
	void go(in int depthMax, in real timeMax, in real timeExtra, bool ponder = false) {
		timer.start();
			setup();
			if (tt.date == 1) log("search> date in hashtable cleared");
			option.time.max = timeMax;
			option.time.extra = max(timeMax, timeExtra);
			option.depthMax = depthMax;
			option.isPondering = ponder;
			log("search> go: %s", option);
			log("search> moves: %s", rootMoves);
			if (rootMoves.length == 0) {
				rootMoves.push(0);
			} else if (rootMoves.length == 1) {
				aspiration(2 - Score.mate, Score.mate - 1, option.depthInit);			
			} else  {
				for (int d = option.depthInit; persist(d); ++d) {
					aspiration(2 - Score.mate, Score.mate - 1, d);
				}				
			}
		timer.stop();
	}


	/* go search without aspiration window nor iterating deepening */
	void go(in int d) {
		timer.start();
			setup();
			option.time.max = option.time.extra = real.infinity;
			option.depthMax = d;
			option.isPondering = false;
			info.score = αβ(2 - Score.mate, Score.mate - 1, d);
		timer.stop();
	}
}


/*
 * Test the search quality using an epd file
 */

/* Look if epd best moves match the best move found. */
bool epdMatch(string epd, Board b, in Move found) {
	write("found ", found.toString());
	string [] solutions = epd.findBetween("bm", ";").split();
	if (solutions.length > 0) {
		writeln("; expected = ", solutions);
		foreach (s; solutions) {
			immutable Move m = fromSan(s, b);
			if (m == found) return true;
		}
		return false;
	} else {
		string [] mistakes = epd.findBetween("am", ";").split();
		if (mistakes.length > 0) {
			writeln("; unexpected = ", mistakes);
			foreach (s; mistakes) {
				immutable Move m = fromSan(s, b);
				if (m == found) return false;
			}
		} else {
			writeln();
		}
	}
	return true;
}

/* Test the search using an epd file */
void epdTest(string [] args, in bool checkSolution = true) {
	real t = real.infinity, T = 0, D = 0;
	int d = Limits.plyMax, n, nGoods;
	size_t ttSize = 256;
	ulong N;
	string epdFile;
	bool verbose, help;

	getopt(args, "movetime|t", &t, "depth|d", &d, "hash", &ttSize, "verbose|v", &verbose, "file|f", &epdFile, "help|h", &help);
	if (help) writeln("epd [--movetime <time>] [--depth <depth>] [--verbose <bool>] --file <epd file> [--help]");
	
	ttSize = min(4096, max(1, ttSize));
	Search s = new Search(ttSize * 1024 * 1024);
	s.option.verbose = verbose;
	Board b = new Board;
	writefln("*** epdTest  depth: %d, time: %.3fs, memory: %d MB ***", d, toad(t), ttSize);

	auto f = std.stdio.File(epdFile);

	foreach(line; f.byLine()) {
		string epd = to!string(line).chomp();
		if (epd == "") continue;
		b.set(epd);
		if (verbose) {
			writeln();
			writeln(b);
		}
		s.set(b);
		s.go(d, t, t);
		if (checkSolution && epdMatch(epd, b, s.bestMove)) ++nGoods;
		T += s.info.time;
		N += s.info.nNodes;
		D += s.info.depth;
		++n;
	}

	if (checkSolution) writef("epd: %d founds / %d problems ", nGoods, n);
	else writef("bench: %d positions ", n);
	writefln(" %d nodes in %.3fs : %.0f nps, depth = %.2f", N, toad(T), toad(N / T), toad(D / n));
	stdout.flush();
}

