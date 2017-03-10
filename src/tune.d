/*
 * Tune the evaluation function
 * © 2016-2017 Richard Delorme
 */

module tune;
import board, eval, game, move, search, util, weight;
import std.algorithm, std.concurrency, std.getopt, std.math, std.stdio;
import core.atomic, core.thread;


/*
 * Vector  (as in Mathematics)
 */
struct Vector {
	double [] data;

	/* construct a vector of size n */
	this (const size_t n) {
		data.length = n;
	}

	/* length property */
	size_t length() @property const {
		return data.length;
	}

	/* opUnary */
	Vector opUnary(string op) () const {
		if (op == "-") {
		 	Vector R = Vector(length);
			foreach (i; 0 .. length) R[i] = -data[i];
			return R;
		}
	}

	/* operator overloading: V + W; V * W; etc. apply the operator to each member data */
	Vector opBinary(string op)(const Vector V) const {
		debug claim(V.length == length);
		Vector R = Vector(length);
		foreach (i; 0 .. length) mixin("R[i] = data[i] "~op~" V[i];");
		return R;
	}

	/* operator overloading: V + s; V * s; etc. apply the operator to each member data */
	Vector opBinary(string op)(const double scalar) const {
		Vector R = Vector(length);
		foreach (i; 0 .. length) mixin("R[i] = data[i] "~op~" scalar;");
		return R;
	}

	/* operator overloading: V + s; V * s; etc. apply the operator to each member data */
	Vector opBinaryRight(string op)(const double scalar) const {
		return opBinary!op(scalar);
	}

	/* assignment overloading: set all data member to a value */
	void opAssign(const double scalar) {
		foreach (ref x; data) x = scalar;
	}

	/* assignment overloading: copy a vector */
	void opAssign(const Vector V) {
		data.length = V.length;
		foreach (i; 0 .. length) data[i] = V[i];
	}

	/* assignment operator overloading: apply the operator to each member data */
	void opOpAssign(string op)(const double scalar) {
		foreach (i; 0 .. length) mixin("data[i] "~op~"= scalar;");
	}

	/* assignment operator overloading: apply the operator to each member data */
	void opOpAssign(string op)(const Vector V) {
		debug claim(V.length == length);
		foreach (i; 0 .. length) mixin("data[i] "~op~"= V[i];");
	}

	/* indexed access */
	double opIndex(const size_t i) const {
		debug claim(i < length);
		return data[i];
	}
	/* indexed access */
	ref double opIndex(const size_t i) {
		debug claim(i < length);
		return data[i];
	}

	/* set all value to 0.0 */
	void clear() {
		foreach (ref x; data) x = 0;
	}

	/* compute the norm of the vector */
	double norm() const {
		double n = 0;
		foreach (x; data) n += x * x;
		return sqrt(n);
	}

	/* compute a normalized diff between two vectors */
	double diff(const Vector v) const {
		return (this - v).norm;
	}

	/* set a vector from evaluation weights */
	void set(const double [] weight, const bool [] isTunable) {
		int j;
		foreach (i; 0..weight.length) {
			if (isTunable[i]) data[j++] = weight[i];
		}
	}

	/* get evaluation weights from a vector */
	void get(ref double [] weight, const bool [] isTunable) const {
		int j;
		foreach (i; 0..weight.length) {
			if (isTunable[i]) weight[i] = data[j++];
		}
	}
}

struct Sum {
	double error = 0.0;
	ulong n;

	void opOpAssign(string op)(const double x) {
		static if (op == "+") {
			error += x;
			++n;
		} else claim(0);
	}

	void opOpAssign(string op)(const ref Sum v) shared {
		static if (op == "+") {
			atomicOp!"+="(this.error, v.error);
			atomicOp!"+="(this.n, v.n);
		} else claim(0);
	}
}

/* compute the sigmoid */
double sigmoid(const double score) {
	return 1.0 / (1.0 + 10.0 ^^ (-0.0025 * score));
}

/* translate a game result into [-1, 0, 1] for the player to move */
double result(const Color c, const shared Game game) {
	if (c == Color.white && game.result == Result.whiteWin) return 1;
	else if (c == Color.black && game.result == Result.blackWin) return 1;
	else if (game.result == Result.draw) return 0.5;
	else return 0;
}

/* compute the error for a single thread */
void getPartialError(const int id, shared GameBase games, const double [] weights, const double K, shared Sum *sum) {
	double r, s;
	shared Game game;
	Sum part;

	Board board = new Board;
	Search search = new Search(256);
	search.eval.setWeight(weights);

	while ((game = games.next()) !is null) {
		board.set();
		search.clear();
		foreach (m; game.moves[0 .. 10]) board.update(m);
		foreach (m; game.moves[10 .. $ - 10]) {
			search.set!false(board);
			search.go(0);
			s = sigmoid(search.score * K);
			r = result(board.player, game);
			part += ((r - s) ^^ 2);
			board.update(m);
		}
	}
	synchronized {
		*sum += part;
	}
	debug write("id:", id, " ->", part, "; ");
}

/*
 * Optimize a set of eval weights, from played game
 * using the Nelder-Mead simplex method.
 */
class Amoeba {
	Vector [] P;
	double [] y;
	bool [] isTunable;
	double [] weights;
	shared GameBase games;
	size_t best, secondWorst, worst;
	double K = 1.0;
	ulong nBoard;
	int iter;
	int nCpu;

	/* constructor */
	this (ref const double [] w, string gameFile, const int cpu) {
		isTunable.length = w.length;
		setTunable(1, isTunable.length);
		weights = w.dup;
		games = new shared GameBase;
		nCpu = cpu;
		if (gameFile !is null) games.read(gameFile, 20);
		writeln("Amoeba running ", nCpu, " tasks in parallel");
	}

	/* count the number of Tunable weights */
	size_t countTunable() const {
		size_t n;
		foreach (b; isTunable) n += b;
		return n;
	}

	/* set a range of tunable weights */
	void setTunable(const size_t from, const size_t to) {
		foreach (ref b; isTunable[from .. to]) b = true;
	}

	/* set a weight tunable */
	void setTunable(const size_t i) {
		isTunable[i] = true;
	}

	/* unset a range of tunable weights */
	void clearTunable(const size_t from, const size_t to) {
		foreach (ref b; isTunable[from .. to]) b = false;
	}

	/* compute the error from a vector */
	double getError(const ref Vector v) {
		v.get(weights, isTunable);
		shared Sum sum;
		immutable double [] w = weights.idup;
		games.clear();

		foreach (i; 0 .. nCpu) spawn(&getPartialError, i, games, w, K, &sum);
		thread_joinAll();

		debug writeln(" =>", sum);
		++iter;
		nBoard = sum.n;
		return sum.error / sum.n;
	}

	/* eval stats */
	void stats() {
		double r, s;
		int iGame, iMove, phase;

		Board board = new Board;
		Search search = new Search(65536);
		search.eval.setWeight(weights);
		games.clear();

		while (true) {
			auto g = games.next();
			if (g.moves.length == 0) break;
			++iGame; iMove = 10;
			board.set();
			search.clear();
			foreach (m; g.moves[0 .. 10]) board.update(m);
			foreach (m; g.moves[10 .. $ - 10]) {
				r = result(board.player, g);
				write(iGame, ", ", iMove, ", ", r, ", ");
				search.set!false(board);
				write(search.eval.stage, ", ");
				write(search.eval(board), ", ");
				write(search.eval(board, -Score.mate, Score.mate), ", ");
				foreach (depth; 0 .. 3) {
					search.go(depth);
					s = sigmoid(search.score);
					write(search.score, ", ", s, ", ");
					board.update(m);
				}
				writeln();
			}
		}
	}

	/* set the best, worst & second worst vector indexes */
	void setExtrema() {
		if (y[1] > y[0]) {
			best = secondWorst = 0;
			worst = 1;
		} else {
			best = secondWorst = 1;
			worst = 0;
		}
		foreach (i; 2 .. y.length) {
			if (y[i] > y[worst]) {
				secondWorst = worst;
				worst = i;
			} else if (y[i] > y[secondWorst] && i != worst) {
				secondWorst = i;
			} else if (y[i] < y[best]) {
				best = i;
			}
		}
	}

	/* Write weights */
	static void printWeights(const ref double [] weight, std.stdio.File f = stdout) {
		f.writeln("/*");
		f.writeln(" * File weight.d");
		f.writeln(" * Evaluation weight - automatically generated");
		f.writeln(" * © 2016-2017 Richard Delorme");
		f.writeln(" */");
		f.writeln("");
		f.writeln("static immutable double [] initialWeights = [");
		f.writeln("\t// Opening");
		f.write("\t"); foreach (i;  0 ..   7) f.writef("%+7.4f, ", weight[i]); f.writeln("// material        [ 0- 6]");
		f.write("\t"); foreach (i;  7 ..  14) f.writef("%+7.4f, ", weight[i]); f.writeln(" // safe mobility   [ 7-13]");
		f.write("\t"); foreach (i; 14 ..  21) f.writef("%+7.4f, ", weight[i]); f.writeln(" // unsafe mobility [14-20]");
		f.write("\t"); foreach (i; 21 ..  28) f.writef("%+7.4f, ", weight[i]); f.writeln(" // safe attack     [21-27]");
		f.write("\t"); foreach (i; 28 ..  35) f.writef("%+7.4f, ", weight[i]); f.writeln(" // unsafe attack   [28-34]");
		f.write("\t"); foreach (i; 35 ..  42) f.writef("%+7.4f, ", weight[i]); f.writeln(" // safe defense    [35-41]");
		f.write("\t"); foreach (i; 42 ..  49) f.writef("%+7.4f, ", weight[i]); f.writeln(" // unsafe defense  [42-48]");
		f.write("\t"); foreach (i; 49 ..  54) f.writef("%+7.4f, ", weight[i]); f.writeln("                   // K attack        [49-53]");
		f.write("\t"); foreach (i; 54 ..  59) f.writef("%+7.4f, ", weight[i]); f.writeln("                   // K defense       [54-58]");
		f.write("\t"); foreach (i; 59 ..  61) f.writef("%+7.4f, ", weight[i]); f.writeln("                                              // K shield/storm  [59-60]");
		f.write("\t"); foreach (i; 61 ..  68) f.writef("%+7.4f, ", weight[i]); f.writeln(" // positional      [61-74]");
		f.write("\t"); foreach (i; 68 ..  75) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i; 75 ..  81) f.writef("%+7.4f, ", weight[i]); f.writeln("          // P structure [75-86]");
		f.write("\t"); foreach (i; 81 ..  87) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i; 87 ..  88) f.writef("%+7.4f, ", weight[i]); f.writeln("                                                       // tempo [87]");
		f.writeln("\t// Endgame");
		f.write("\t"); foreach (i; 88 ..  95) f.writef("%+7.4f, ", weight[i]); f.writeln("// material        [88-94]");
		f.write("\t"); foreach (i; 95 .. 102) f.writef("%+7.4f, ", weight[i]); f.writeln(" // safe mobility   [95-101]");
		f.write("\t"); foreach (i;102 .. 109) f.writef("%+7.4f, ", weight[i]); f.writeln(" // unsafe mobility [102-108]");
		f.write("\t"); foreach (i;109 .. 116) f.writef("%+7.4f, ", weight[i]); f.writeln(" // safe attack     [109-115]");
		f.write("\t"); foreach (i;116 .. 123) f.writef("%+7.4f, ", weight[i]); f.writeln(" // unsafe attack   [116-122]");
		f.write("\t"); foreach (i;123 .. 130) f.writef("%+7.4f, ", weight[i]); f.writeln(" // safe defense    [123-129]");
		f.write("\t"); foreach (i;130 .. 137) f.writef("%+7.4f, ", weight[i]); f.writeln(" // unsafe defense  [130-136]");
		f.write("\t"); foreach (i;137 .. 142) f.writef("%+7.4f, ", weight[i]); f.writeln("                   // K attack        [137-141]");
		f.write("\t"); foreach (i;142 .. 147) f.writef("%+7.4f, ", weight[i]); f.writeln("                   // K defense       [142-146]");
		f.write("\t"); foreach (i;147 .. 152) f.writef("%+7.4f, ", weight[i]); f.writeln("                   // positional      [147-151]");
		f.write("\t"); foreach (i;152 .. 158) f.writef("%+7.4f, ", weight[i]); f.writeln("          // P structure     [152-163]");
		f.write("\t"); foreach (i;158 .. 164) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i;164 .. 165) f.writef("%+7.4f, ", weight[i]); f.writeln("                                                       // tempo [164]");
		f.writeln("];");
		f.flush();
	}

	/* Simplex relative size */
	double getVolume() {
		double s = 0.0;
		foreach (ref p; P) if (p != P[best]) s += p.diff(P[best]);
		double n = P[best].norm();
		if (n < 1.0) n = 1.0; // ?
		return s / n;
	}

	/* compute the centroid */
	Vector centroid() {
		Vector c = -P[worst];
		foreach (ref p; P) c += p;
		c /= c.length;
		return c;
	}

	/* optimize K using the golden section search. */
	void optimizeK(const double tolerance, const int maxIter) {
		double a, b, c, d, fc, fd;
		Vector v = Vector(countTunable());
		const double gr = (sqrt(5.0) - 1.0) / 2.0;
		int iter;

		v.set(weights, isTunable);

		a = 0.50; b = 2.00;
		c = b - gr * (b - a);
		d = a + gr * (b - a);
		writefln("optimize sigmoid parameter K: tolerance = %.8f, maxIter = %d", tolerance, maxIter); stdout.flush();
		K = c; fc = getError(v);
		writeln("using ", nBoard, " positions");
		writefln("%4d c :  %.8f ⭢  %.8f  [%.8f, %.8f]", 1, c, fc, a, b);
		K = d; fd = getError(v); stdout.flush();
		writefln("%4d c :  %.8f ⭢  %.8f  [%.8f, %.8f]", 2, d, fd, a, b); stdout.flush();

		for (iter = 3; abs(fc - fd) > tolerance * abs(fc) && iter < maxIter; ++iter) {
			if (fc < fd) {
				b = d; d = c; fd = fc;
				c = b - gr * (b - a);
				K = c; fc = getError(v);
				writefln("%4d c :  %.8f ⭢  %.8f  [%.8f, %.8f]", iter, c, fc, a, b); stdout.flush();
			} else {
				a = c; c = d; fc = fd;
				d = a + gr * (b - a);
				K = d; fd = getError(v);
				writefln("%4d d :  %.8f ⭢  %.8f  [%.8f, %.8f]", iter, d, fd, a, b); stdout.flush();
			}
		}

		K = (a + b) * 0.5;
		writefln("%.8f < %.8f ⭢ K = %.8f\n", fabs(fc - fd), tolerance, K); stdout.flush();
	}

	/* optimize 1 parameter using the golden section search. */
	void optimizeWeight(const size_t i, const double tolerance, const int maxIter, const double [2] limits = [-2.0, 2.0]) {
		double a, b, c, d, fc, fd;
		Vector v = Vector(countTunable());
		const double gr = (sqrt(5.0) - 1.0) / 2.0; // Golden ration
		int iter;

		v.set(weights, isTunable);

		a = v[i] + limits[0]; b = v[i] + limits[1];
		c = b - gr * (b - a);
		d = a + gr * (b - a);
		writefln("optimize weight %d: %.8f ; tolerance = %.8f, maxIter = %d", i, v[i], tolerance, maxIter); stdout.flush();
		v[i] = c; fc = getError(v);
		writeln("using ", nBoard, " positions");
		writefln("%4d c :  %.8f ⭢  %.8f  [%.8f, %.8f]", 1, c, fc, a, b);
		v[i] = d; fd = getError(v); stdout.flush();
		writefln("%4d c :  %.8f ⭢  %.8f  [%.8f, %.8f]", 2, d, fd, a, b); stdout.flush();

		for (iter = 3; abs(c - d) > tolerance && iter < maxIter; ++iter) {	// absolute precision targeted
			if (fc < fd) {
				b = d; d = c; fd = fc;
				c = b - gr * (b - a);
				v[i] = c; fc = getError(v);
				writefln("%4d c :  %.8f ⭢  %.8f  [%.8f, %.8f]", iter, c, fc, a, b); stdout.flush();
			} else {
				a = c; c = d; fc = fd;
				d = a + gr * (b - a);
				v[i] = d; fd = getError(v);
				writefln("%4d d :  %.8f ⭢  %.8f  [%.8f, %.8f]", iter, d, fd, a, b); stdout.flush();
			}
		}

		v[i] = (a + b) * 0.5;
		writefln("%.8f < %.8f ⭢ weight = %.8f\n", fabs(c - d), tolerance, v[i]); stdout.flush();

		v.get(weights, isTunable);
	}

	/* init the Simplex */
	void init(const double volume) {
		const size_t size = countTunable() + 1;
		const double a = volume / size * sqrt(2.0);
		const double δi = a * (sqrt(size - 1.0) + size - 1.0);
		const double δj = a * (sqrt(size - 1.0) - 1.0);

		P.length = size;
		foreach (ref p; P) p = Vector(size - 1);
		P[0].set(weights, isTunable);
		foreach (i; 1 .. size) {
			P[i] = P[0] + δj;
			if (i < size - 1) P[i][i] = P[0][i] + δi;
		}
		y.length = size;
		foreach (i; 0 .. size) {
			y[i] = getError(P[i]);
			writef("\r %d/%d", i + 1, size); stdout.flush();
		}
		writeln();
	}

	/* Amoeba or simplex optimization algorithm. */
	void tune(const double tolerance, const int maxIter, const bool adaptative = false) 	{
		Vector C, Pr, Pe, Pc;
		double size, flat, υ, yBest = +double.max, yr, ye, yc;
		static immutable int minIter = 10;
		const double tiny = tolerance * tolerance;
		const double  ν = adaptative ? P.length - 1 : 2.0;
		const double α = 1.0, β = 1.0 + 2.0 / ν, γ = 0.75 - 1.0 / (2.0 * ν), δ = 1.0 - 1.0 / ν;
		enum Stage {init, reflection, expansion, contraction, reduction}
		Stage stage;

		iter = 0;
		writefln("stopping condition: size < %10.8f flat < %10.8f iter > %d", tolerance, tiny, maxIter);
		// loop
		while (true) {
			setExtrema();
			if (yBest > y[best]) {
				yBest = y[best];
				P[best].get(weights, isTunable);
				printWeights(weights);
			}

			flat = (y[worst] - y[best]) / (y[best] + y[worst]);
			size = getVolume();
			writefln("%12s: %3d; best = %10.8f worst = %10.8f, 2nd_worst = %10.8f : size = %10.8f flat = %10.8f", stage, iter, y[best], y[worst], y[secondWorst], size, flat);
			stdout.flush();
			// quit when some targets are reached
			if (((size < tolerance  || flat < tiny) && iter > (minIter + P.length)) || iter >= maxIter) break;

			C = centroid();
			stage = Stage.reflection;
			Pr = C + α * (C  - P[worst]); // reflection
			yr = getError(Pr);
			if (y[best] <= yr &&  yr < y[secondWorst]) {
				P[worst] = Pr;
				y[worst] = yr;
			} else if (yr < y[best]) {
				stage = Stage.expansion;
				Pe = C + β * (Pr - C);  // expansion
				ye = getError(Pe);
				if (ye < yr) {
					P[worst] = Pe;
					y[worst] = ye;
				} else {
					P[worst] = Pr;
					y[worst] = yr;
				}
			} else if (y[secondWorst] <= yr) {
				stage = Stage.contraction;
				if (yr < y[worst]) Pc = C + γ * (Pr - C); // outside contraction
				else Pc = C - γ * (Pr - C); // inside contraction
				yc = getError(Pc);
				if (yc < yr) {
					P[worst] = Pc;
					y[worst] = yc;
				} else {
					stage = Stage.reduction;
					foreach (i; 0 .. P.length) if (i != best) { // reduction
						P[i] = P[best] + δ * (P[i] - P[best]);
						y[i] =  getError(P[i]);
					}
				}
			}
		}

		// restore weights
		P[best].get(weights, isTunable);

	}

	void roundWeights(const real unit) {
		foreach(ref w; weights) w = rint(w / unit) * unit;
	}
}

/* tune a range of evaluation weights */
void tune(Amoeba amoeba, const int [] openingRange, const int [] endgameRange, const double size, const double tolerance, const int maxIter, const bool adaptative) {
	amoeba.clearTunable(0, amoeba.isTunable.length);
	amoeba.setTunable(openingRange[0], openingRange[1]);
	amoeba.setTunable(endgameRange[0], endgameRange[1]);

	amoeba.init(size); amoeba.tune(tolerance, maxIter, adaptative);
}


/* tune by piece type */
void tuneByPiece(Amoeba amoeba, const double volume, const double tolerance, const int maxIter, const bool adaptative) {
	with (Piece) {
		Piece [] pieces = [
			// Opening
			size, knight, bishop, rook, queen, bishop, none, // material
			pawn, pawn, knight, bishop, rook, queen, king, // mobility
			pawn, pawn, knight, bishop, rook, queen, king,
			pawn, pawn, knight, bishop, rook, queen, king, // attack
			pawn, pawn, knight, bishop, rook, queen, king,
			pawn, pawn, knight, bishop, rook, queen, king, // defense
			pawn, pawn, knight, bishop, rook, queen, king,
			pawn, knight, bishop, rook, queen,       // king
			pawn, knight, bishop, rook, queen,
			king, king,
			pawn, pawn, knight, bishop, rook, rook, queen, // positional
			king, pawn, knight, bishop, rook, queen, king,
			pawn, pawn, pawn, pawn, pawn, pawn, // pawn structure
			pawn, pawn, pawn, pawn, pawn, pawn,
			none,
			// Endgame
			pawn, knight, bishop, rook, queen, bishop, none, // material
			pawn, pawn, knight, bishop, rook, queen, king, // mobility
			pawn, pawn, knight, bishop, rook, queen, king,
			pawn, pawn, knight, bishop, rook, queen, king, // attack
			pawn, pawn, knight, bishop, rook, queen, king,
			pawn, pawn, knight, bishop, rook, queen, king, // defense
			pawn, pawn, knight, bishop, rook, queen, king,
			pawn, knight, bishop, rook, queen,       // king
			pawn, knight, bishop, rook, queen,
			pawn, knight, bishop, queen, king, // positional
			pawn, pawn, pawn, pawn, pawn, pawn, // pawn structure
			pawn, pawn, pawn, pawn, pawn, pawn,
			none,
		];

		debug claim(pieces.length == amoeba.isTunable.length);
		debug claim(pieces.length == amoeba.weights.length);
		writeln("size: ", volume, "; tolerance: ", tolerance, "; maxIter: ", maxIter, "; adaptative: ", adaptative);

		foreach (p; none .. size) {
			amoeba.clearTunable(0, amoeba.isTunable.length);
			writeln(p);
			foreach (i; 0 .. pieces.length) if (p == pieces[i]) amoeba.setTunable(i);
			amoeba.init(volume); amoeba.tune(tolerance, maxIter, adaptative);
		}
	}
}

/* tune the evaluation function weights */
void main(string [] args) {
	int [] range;
	int maxIter = 10000, nCpu = 1;
	double size = 0.1, tolerance = 0.001, round = 0.00001;
	string weightFile, gameFile;
	bool fromScratch, byConcept, byPiece, optimizeK, adaptative, help, bench, individualy, stats;

	getopt(args, "iter|i", &maxIter , "size|s", &size, "tolerance|t", &tolerance, "range|r", &range, "stats", &stats,
		"weight|w", &weightFile, "game|g", &gameFile, "cpu|n", &nCpu, "scratch|z", &fromScratch, "concept|c", &byConcept,
		"piece|p", &byPiece, "optimizeK|k", &optimizeK, "adaptative|d", &adaptative, "bench|b", &bench, "optimize|o", &individualy,
		"round", &round, "help|h", &help);
	if (help) {
		writeln("tune [--iter|-i <integer>] [--size|-s <real>] [--tolerance|-t <real>] [--weight|-w <file>] [--game|-g <file>] [--range|-r <integer>/--scracth|-z/--all|-a/--piece|-p] [--adaptative|-d] [--help|-h]");
		writeln("\t--iter|-i <integer>   Set the maximal number of iteration");
		writeln("\t--size|-s <real>      Set the size of the simplex");
		writeln("\t--tolerance|-t <real> Set the accuracy of the solution");
		writeln("\t--weight|-w <file>    Set the file to write the weights too, e.g. weight.d");
		writeln("\t--game|-g <file>      Set the game file to learn from");
		writeln("\t--cpu|-n <# of cpu>   Set the number of CPU to use for parallel computation");
		writeln("\t--adaptative|-d       Turn on adaptative mode (better convergence for big set of weights)");
		writeln("\t--range|-r <integer>  Set the ranges of weights to tune : you need to repeat it by pair: -r 1 -r 7 -r 88 -r 95");
		writeln("\t--scratch|-z          Compute the whole set of weights from scratch");
		writeln("\t--concept|-c          compute the set of weights by concepts (material, positional, mobility, ...)");
		writeln("\t--piece|-p            compute the set of weights by pieces (pawn, knight, ...)");
		writeln("\t--optimizeK|-k        compute the optimal value for k");
		writeln("\t--optimize|-o         compute the set of weights using golden ratio");
		writeln("\t--bench|-b            bench");
		writeln("\t--round <unit>        quantize the weights to unit, f.ex '--round 0.01' rounds the weights to centipawns");
		writeln("\t--stats               compute some statistics about current weights");
		writeln("\t--help|-h             Display this help");
		writeln("(*) the range/scratch/concept/piece/help arguments are exclusive");
		writeln("");
		return;
	}

	Amoeba amoeba = new Amoeba(weight.initialWeights, gameFile, nCpu);

	if (stats) {
		amoeba.stats();
	}

	// bench: just optimize K (to test/debug parallelism, ...)
	else if (bench) {
		amoeba.optimizeK(0.0000001, 100);
	}

	// determine weights from scratch
	else if (fromScratch) {
		amoeba.weights[] = 0;
		amoeba.weights[0..5] = amoeba.weights[89..94] = [1, 3, 3, 5, 9];
	}

	// tune weight sets as material, positional, mobility, pawn structures & tempo concepts.
	else if (byConcept) {
		if (optimizeK) amoeba.optimizeK(0.0000001, 100);
		// material
		writeln("Tuning material");
	 	tune(amoeba, [1, 7], [88, 95], size, tolerance, maxIter, adaptative);
		// positional
		writeln("Tuning positional");
	 	tune(amoeba, [61, 75], [147, 152], size, tolerance, maxIter, adaptative);
		// mobility
		writeln("Tuning mobility");
		tune(amoeba, [7, 21], [95, 109], size, tolerance, maxIter, adaptative);
		writeln("Tuning attack");
		tune(amoeba, [21, 35], [109, 123], size, tolerance, maxIter, adaptative);
		writeln("Tuning defense");
		tune(amoeba, [35, 49], [123, 137], size, tolerance, maxIter, adaptative);
		// king
		writeln("Tuning King attack");
		tune(amoeba, [49, 54], [137, 142], size, tolerance, maxIter, adaptative);
		writeln("Tuning King defense"); // +king shield/storm
		tune(amoeba, [54, 61], [142, 147], size, tolerance, maxIter, adaptative);
		// pawn structure
		writeln("Tuning pawn structure");
		tune(amoeba, [75, 87], [152, 164], size, tolerance, maxIter, adaptative);
		// tempo
		writeln("Tuning tempo");
	 	tune(amoeba, [87, 88], [164, 165], size, tolerance, maxIter, adaptative);

	// tune weight sets by piece involved.
	} else if (byPiece) {
		if (optimizeK) amoeba.optimizeK(0.0000001, 100);
		tuneByPiece(amoeba, size, tolerance, maxIter, adaptative);

	// tune each weight individually
	} else if (individualy) {
		if (optimizeK) amoeba.optimizeK(0.0000001, 100);
		foreach (i; 0 .. amoeba.isTunable.length) amoeba.optimizeWeight(i, tolerance, maxIter);

	// rounding
	} else if (round > 0.0001) {
		amoeba.roundWeights(round);

	// manual choice: tune by range of weights.
	} else {
		writeln("range", range);
		if (range.length) {
			amoeba.clearTunable(0, amoeba.isTunable.length);
			for (auto i = 0; i + 1 < range.length; i += 2) amoeba.setTunable(range[i], range[i + 1]);
		}
		amoeba.clearTunable(0, 1);      // pawn opening material = 1 pawn by definition.

		if (optimizeK) amoeba.optimizeK(0.0000001, 100);
		amoeba.init(size);
		amoeba.tune(tolerance, maxIter, adaptative);
	}

	// save the result to weightFile
	if (weightFile.length > 0) {
		std.stdio.File file = std.stdio.File(weightFile, "w");
		Amoeba.printWeights(amoeba.weights, file);
		file.close();	
	}
}

