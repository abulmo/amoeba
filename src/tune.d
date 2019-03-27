/*
 * Tune the evaluation function
 * © 2016-2019 Richard Delorme
 */

module tune;
import board, eval, game, move, search, util, weight;
import std.algorithm, std.concurrency, std.getopt, std.math, std.stdio;
import core.atomic, core.thread;

version (GNU) {
} else {
	pragma(msg, "Please, compile me with GDC");
	static assert(0);
}

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
		assert(0);
	}

	/* operator overloading: V + W; V * W; etc. apply the operator to each member data */
	Vector opBinary(string op)(const Vector V) const {
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
		foreach (i; 0 .. length) mixin("data[i] "~op~"= V[i];");
	}

	/* indexed access */
	double opIndex(const size_t i) const {
		return data[i];
	}
	/* indexed access */
	ref double opIndex(const size_t i) {
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
		} else assert(0);
	}

	void opOpAssign(string op)(const ref Sum v) shared {
		static if (op == "+") {
			atomicOp!"+="(this.error, v.error);
			atomicOp!"+="(this.n, v.n);
		} else assert(0);
	}
}

/* compute the sigmoid */
double sigmoid(const double score) {
	return 1.0 / (1.0 + 10.0 ^^ (-0.0025 * score));
}

/* translate a game result into [1, 0.5, 0] for the player to move */
double result(const Color c, const shared Game game) {
	if (c == Color.white && game.result == Result.whiteWin) return 1;
	else if (c == Color.black && game.result == Result.blackWin) return 1;
	else if (c == Color.white && game.result == Result.blackWin) return 0;
	else if (c == Color.black && game.result == Result.whiteWin) return 0;
	else return 0.5;
}

/* error type */
enum ErrorType {absolute, square, logLikelihood}

/* compute the error for a single thread */
void getPartialError(const int id, shared GameBase games, const double [] weights, const double K, shared Sum *sum, const ErrorType errorType) {
	double r, s;
	shared Game game;
	Sum part = Sum.init;
	size_t end;

	Board board = new Board;
	Search search = Search(1_048_576, 1, null); //TODO: with tasks ?
	search.setWeight(weights);

	while ((game = games.next()) !is null) {
		board.set();
		foreach (m; game.moves) board.update(m);
		end = game.moves.length;
		if (game.result == Result.draw) end -= board.fifty;
		else end -= 10;

		if (end > 15) {
			search.clear();
			board.set();
			foreach (m; game.moves[0 .. 10])  board.update(m);
			foreach (m; game.moves[10 .. end]) {
				search.position!(Copy.off)(board);
				search.go(0);
				s = sigmoid(search.score * K);
				r = result(board.player, game);
				if (errorType == ErrorType.logLikelihood) {
					if (r == 0.0) part += -log(1.0 - s);
					else if (r == 1.0) part += -log(s);
					else part += -2.0 * (log(1.0 - s) + log(s));
		  		} else if (errorType == ErrorType.square) {
					part += ((r - s) ^^ 2);
				} else if (errorType == ErrorType.absolute) {
					part += abs(r - s);
				} else {
					unreachable();
				}
				board.update(m);
			}
		}
	}
	synchronized {
		*sum += part;
	}
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
	ErrorType errorType;

	/* constructor */
	this (ref const double [] w, string gameFile, const int cpu) {
		isTunable.length = w.length;
		setTunable(1, isTunable.length);
		weights = w.dup;
		games = new shared GameBase;
		nCpu = cpu;
		errorType = ErrorType.square;
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
		shared Sum sum = Sum.init;
		immutable double [] w = weights.idup;
		games.clear();

		foreach (i; 0 .. nCpu) spawn(&getPartialError, i, games, w, K, &sum, errorType);
		thread_joinAll();

		++iter;
		nBoard = sum.n;
		return sum.error / sum.n;
	}

	/* eval stats */
	void stats() {
		double r, s;
		int iGame, iMove;
		shared Game g;

		Board board = new Board;
		Search search = Search(1_048_576, 1, null);
		search.setWeight(weights);
		games.clear();

		writeln("game,move,result, stage, lazy, S(lazy), eval, S(eval), search_0, S(search_0), search_1, S(search_1), search_2, S(search_2)");
		while ((g = games.next()) !is null) {
			board.set(); foreach (m; g.moves) board.update(m);
			auto end = g.moves.length;
			if (g.result == Result.draw) end -= board.fifty;
			else end -= 10;
			if (end > 15) {
				++iGame; iMove = 10;
				board.set();
				search.clear();
				foreach (m; g.moves[0 .. 10]) board.update(m);
				foreach (m; g.moves[10 .. end]) {
					r = result(board.player, g);
					write(iGame, ", ", iMove++, ", ", r, ", ");
					search.position!(Copy.off)(board);
					write(search.eval.stage, ", ");
					s = search.eval(board); write(s, ", ", sigmoid(s), ", ");
					s = search.eval(board, -Score.mate, Score.mate); write(s, ", ", sigmoid(s), ", ");
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
		f.writeln(" * © 2016-2019 Richard Delorme");
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
		f.write("\t"); foreach (i; 49 ..  55) f.writef("%+7.4f, ", weight[i]); f.writeln("          // hanging/trapped [49-54]");
		f.write("\t"); foreach (i; 55 ..  61) f.writef("%+7.4f, ", weight[i]); f.writeln("          // Center control  [55-60]");
		f.write("\t"); foreach (i; 61 ..  66) f.writef("%+7.4f, ", weight[i]); f.writeln("                   // K attack        [61-65]");
		f.write("\t"); foreach (i; 66 ..  71) f.writef("%+7.4f, ", weight[i]); f.writeln("                   // K defense       [66-70]");
		f.write("\t"); foreach (i; 71 ..  73) f.writef("%+7.4f, ", weight[i]); f.writeln("                                              // K shield/storm  [71-72]");
		f.write("\t"); foreach (i; 73 ..  80) f.writef("%+7.4f, ", weight[i]); f.writeln(" // positional      [73-86]");
		f.write("\t"); foreach (i; 80 ..  87) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i; 87 ..  94) f.writef("%+7.4f, ", weight[i]); f.writeln(" // P structure     [87-132]");
		f.write("\t"); foreach (i; 94 .. 101) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i;101 .. 108) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i;108 .. 115) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i;115 .. 122) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i;122 .. 129) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i;129 .. 133) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i;133 .. 137) f.writef("%+7.4f, ", weight[i]); f.writeln("                            // B structure     [133-136]");
		f.write("\t"); foreach (i;137 .. 141) f.writef("%+7.4f, ", weight[i]); f.writeln("                            // R structure     [137-140]");
		f.write("\t"); foreach (i;141 .. 142) f.writef("%+7.4f, ", weight[i]); f.writeln("                                                       // tempo [141]");
		f.writeln("\t// Endgame");
		f.write("\t"); foreach (i;142 .. 149) f.writef("%+7.4f, ", weight[i]); f.writeln("// material        [142-148]");
		f.write("\t"); foreach (i;149 .. 156) f.writef("%+7.4f, ", weight[i]); f.writeln(" // safe mobility   [149-155]");
		f.write("\t"); foreach (i;156 .. 163) f.writef("%+7.4f, ", weight[i]); f.writeln(" // unsafe mobility [156-162]");
		f.write("\t"); foreach (i;163 .. 170) f.writef("%+7.4f, ", weight[i]); f.writeln(" // safe attack     [163-169]");
		f.write("\t"); foreach (i;170 .. 177) f.writef("%+7.4f, ", weight[i]); f.writeln(" // unsafe attack   [170-176]");
		f.write("\t"); foreach (i;177 .. 184) f.writef("%+7.4f, ", weight[i]); f.writeln(" // safe defense    [177-173]");
		f.write("\t"); foreach (i;184 .. 191) f.writef("%+7.4f, ", weight[i]); f.writeln(" // unsafe defense  [184-190]");
		f.write("\t"); foreach (i;191 .. 197) f.writef("%+7.4f, ", weight[i]); f.writeln("          // hanging/trapped [191-196]");
		f.write("\t"); foreach (i;197 .. 203) f.writef("%+7.4f, ", weight[i]); f.writeln("          // Center control  [197-202]");
		f.write("\t"); foreach (i;203 .. 208) f.writef("%+7.4f, ", weight[i]); f.writeln("                   // K attack        [203-207]");
		f.write("\t"); foreach (i;208 .. 213) f.writef("%+7.4f, ", weight[i]); f.writeln("                   // K defense       [208-212]");
		f.write("\t"); foreach (i;213 .. 216) f.writef("%+7.4f, ", weight[i]); f.writeln("                                     // K shield/storm  [213-215]");
		f.write("\t"); foreach (i;216 .. 221) f.writef("%+7.4f, ", weight[i]); f.writeln("                   // positional      [216-220]");
		f.write("\t"); foreach (i;221 .. 228) f.writef("%+7.4f, ", weight[i]); f.writeln(" // P structure     [221-266]");
		f.write("\t"); foreach (i;228 .. 235) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i;235 .. 242) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i;242 .. 249) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i;249 .. 256) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i;256 .. 263) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i;263 .. 267) f.writef("%+7.4f, ", weight[i]); f.writeln("");
		f.write("\t"); foreach (i;267 .. 271) f.writef("%+7.4f, ", weight[i]); f.writeln("                            // B structure     [267-270]");
		f.write("\t"); foreach (i;271 .. 275) f.writef("%+7.4f, ", weight[i]); f.writeln("                            // R structure     [271-274]");
		f.write("\t"); foreach (i;275 .. 276) f.writef("%+7.4f, ", weight[i]); f.writeln("                                                       // tempo [275]");
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

	/* initialize the Simplex */
	void start(const double volume) {
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
		double size, flat, yBest = +double.max, yr, ye, yc;
		static immutable int minIter = 10;
		const double tiny = tolerance * tolerance;
		const double  ν = adaptative ? P.length - 1 : 2.0;
		const double α = 1.0, β = 1.0 + 2.0 / ν, γ = 0.75 - 1.0 / (2.0 * ν), δ = 1.0 - 1.0 / ν;
		enum Stage {start, reflection, expansion, contraction, reduction}
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

	/* round all weights */
	void roundWeights(const real unit) {
		foreach(ref w; weights) w = rint(w / unit) * unit;
	}

	/* rescale all weights */
	void roundWeights(const double scale) {
		foreach(ref w; weights) w *= scale;
	}

	/* display a table of error for a variable */
	void graph(int i, const double width, const double step = 0.01) {
		Vector v = Vector(countTunable());
		v.set(weights, isTunable);

		const f = v[i] - width, t = v[i] + width;
		writeln(i + 1, " error");
		for (v[i] = f; v[i] <= t; v[i] += step) {
			writeln(v[i], ", ", getError(v));
		}
	}
}

/* tune a range of evaluation weights */
void tune(Amoeba amoeba, const int [] openingRange, const int [] endgameRange, const double size, const double tolerance, const int maxIter, const bool adaptative) {
	amoeba.clearTunable(0, amoeba.isTunable.length);
	amoeba.setTunable(openingRange[0], openingRange[1]);
	amoeba.setTunable(endgameRange[0], endgameRange[1]);

	amoeba.start(size);
	amoeba.tune(tolerance, maxIter, adaptative);
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
			none, none, none,   none,   none, none,
			pawn, knight, bishop, rook, queen, king,       // center control
			pawn, knight, bishop, rook, queen,       // king
			pawn, knight, bishop, rook, queen,
			king, king,
			pawn, pawn, knight, bishop, rook, rook, queen, // positional
			king, pawn, knight, bishop, rook, queen, king,
			pawn, pawn, pawn, pawn, pawn, pawn, pawn, // pawn structure
			pawn, pawn, pawn, pawn, pawn, pawn, pawn,
			pawn, pawn, pawn, pawn, pawn, pawn, pawn,
			pawn, pawn, pawn, pawn, pawn, pawn, pawn,
			pawn, pawn, pawn, pawn, pawn, pawn, pawn,
			pawn, pawn, pawn, pawn, pawn, pawn, pawn,
			pawn, pawn, pawn, pawn,
			bishop, bishop, bishop, bishop,     // bishop structure
			rook, rook, rook, rook,             // rook structure
			none,
			// Endgame
			pawn, knight, bishop, rook, queen, bishop, none, // material
			pawn, pawn, knight, bishop, rook, queen, king, // mobility
			pawn, pawn, knight, bishop, rook, queen, king,
			pawn, pawn, knight, bishop, rook, queen, king, // attack
			pawn, pawn, knight, bishop, rook, queen, king,
			pawn, pawn, knight, bishop, rook, queen, king, // defense
			pawn, pawn, knight, bishop, rook, queen, king,
			none, none, none,   none,   none, none,
			pawn, knight, bishop, rook, queen, king,       // center control
			pawn, knight, bishop, rook, queen,       // king
			pawn, knight, bishop, rook, queen,
			king, king, king,
			pawn, knight, bishop, queen, king, // positional
			pawn, pawn, pawn, pawn, pawn, pawn, pawn, // pawn structure
			pawn, pawn, pawn, pawn, pawn, pawn, pawn,
			pawn, pawn, pawn, pawn, pawn, pawn, pawn,
			pawn, pawn, pawn, pawn, pawn, pawn, pawn,
			pawn, pawn, pawn, pawn, pawn, pawn, pawn,
			pawn, pawn, pawn, pawn, pawn, pawn, pawn,
			pawn, pawn, pawn, pawn,
			bishop, bishop, bishop, bishop,     // bishop structure
			rook, rook, rook, rook,             // rook structure
			none,
		];

		writeln("size: ", volume, "; tolerance: ", tolerance, "; maxIter: ", maxIter, "; adaptative: ", adaptative);

		foreach (p; none .. size) {
			amoeba.clearTunable(0, amoeba.isTunable.length);
			writeln(p);
			foreach (i; 0 .. pieces.length) if (p == pieces[i]) amoeba.setTunable(i);
			amoeba.start(volume); amoeba.tune(tolerance, maxIter, adaptative);
		}
	}
}


/* tune the evaluation function weights */
void main(string [] args) {
	int [] range;
	int maxIter = 10_000, nCpu = 1, graph = 0;
	double size = 0.1, tolerance = 0.001, round = 0.00001;
	string weightFile, gameFile;
	bool fromScratch, byConcept, byPiece, optimizeK, adaptative, help, bench, individualy, stats, useLL, useAbs;

	getopt(args, "iter|i", &maxIter , "size|s", &size, "tolerance|t", &tolerance, "range|r", &range, "stats", &stats,
		"weight|w", &weightFile, "game|g", &gameFile, "cpu|n", &nCpu, "scratch|z", &fromScratch, "concept|c", &byConcept,
		"piece|p", &byPiece, "optimizeK|k", &optimizeK, "adaptative|d", &adaptative, "bench|b", &bench, "optimize|o", &individualy,
		"likelihood|l", &useLL, "absolute|a", &useAbs, "graph", &graph, "round", &round, "help|h", &help);
	if (help) {
		writeln("tune [options]");
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
		writeln("\t--likelihood|-l       optimize the log-likelihood of the game outcome (instead of least square error)");
		writeln("\t--absolute            optimize the least absolute error of the game outcome (instead of least square error)");
		writeln("\t--bench|-b            bench");
		writeln("\t--round <unit>        quantize the weights to unit, f.ex '--round 0.01' rounds the weights to centipawns");
		writeln("\t--stats               compute some statistics about current weights");
		writeln("\t--help|-h             Display this help");
		writeln("(*) the range/scratch/concept/piece/help arguments are exclusive");
		writeln("");
		return;
	}

	Amoeba amoeba = new Amoeba(weight.initialWeights, gameFile, nCpu);

	if (useAbs && useLL) {
		writeln("likelihood or least absolute error ? Please use a single method");
		return;
	} else if (useAbs) amoeba.errorType = ErrorType.absolute;
	else if (useLL) amoeba.errorType = ErrorType.logLikelihood;

	if (stats) {
		amoeba.stats();
	}

	else if (graph) {
		amoeba.graph(graph - 1, 2.0, 0.01);
	}

	// bench: just optimize K (to test/debug parallelism, ...)
	else if (bench) {
		amoeba.optimizeK(0.0000001, 100);
	}

	// determine weights from scratch
	else if (fromScratch) {
		amoeba.weights[] = 0;
		amoeba.weights[0..5] = amoeba.weights[142..147] = [1, 3, 3, 5, 9];
	}

	// tune weight sets as material, positional, mobility, pawn structures & tempo concepts.
	else if (byConcept) {
		if (optimizeK) amoeba.optimizeK(0.0000001, 100);
		// material
		writeln("Tuning material");
	 	tune(amoeba, [1, 7], [142, 149], size, tolerance, maxIter, adaptative);
		// positional
		writeln("Tuning positional");
	 	tune(amoeba, [73, 87], [216, 221], size, tolerance, maxIter, adaptative);
		// mobility
		writeln("Tuning mobility");
		tune(amoeba, [7, 21], [149, 163], size, tolerance, maxIter, adaptative);
		writeln("Tuning attack");
		tune(amoeba, [21, 35], [163, 177], size, tolerance, maxIter, adaptative);
		writeln("Tuning defense");
		tune(amoeba, [35, 49], [177, 191], size, tolerance, maxIter, adaptative);
		writeln("Hanging/trapped/enclosed");
		tune(amoeba, [49, 54], [191, 197], size, tolerance, maxIter, adaptative);
		writeln("Center control");
		tune(amoeba, [55, 61], [197, 203], size, tolerance, maxIter, adaptative);
		// king
		writeln("Tuning King attack");
		tune(amoeba, [61, 66], [203, 208], size, tolerance, maxIter, adaptative);
		writeln("Tuning King defense"); // +king shield/storm
		tune(amoeba, [66, 73], [208, 216], size, tolerance, maxIter, adaptative);
		// pawn structure
		writeln("Tuning pawn structure");
		tune(amoeba, [87, 133], [221, 267], size, tolerance, maxIter, adaptative);
		// bishop structure
		writeln("Tuning bishop structure");
		tune(amoeba, [133, 137], [267, 270], size, tolerance, maxIter, adaptative);
		// rook structure
		writeln("Tuning rook structure");
		tune(amoeba, [137, 140], [271, 275], size, tolerance, maxIter, adaptative);
		// tempo
		writeln("Tuning tempo");
	 	tune(amoeba, [141, 142], [275, 276], size, tolerance, maxIter, adaptative);

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
		amoeba.start(size);
		amoeba.tune(tolerance, maxIter, adaptative);
	}

	// save the result to weightFile
	if (weightFile.length > 0) {
		std.stdio.File file = std.stdio.File(weightFile, "w");
		Amoeba.printWeights(amoeba.weights, file);
		file.close();
	}
}

