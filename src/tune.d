/*
 * Tune the evaluation function
 * © 2016 Richard Delorme
 */

import board, eval, game, move, search, weight;
import std.math, std.stdio, std.getopt;

/*
 * Vector  (as in Mathematics) 
 */
struct Vector {
	real [] data;

	/* construct a vector of size n */
	this (in size_t n) {
		data.length = n;
	}

	/* length property */
	size_t length() pure @property const {
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
	Vector opBinary(string op)(in Vector V) const {
		assert(V.length == length);
		Vector R = Vector(length);
		foreach (i; 0 .. length) mixin("R[i] = data[i] "~op~" V[i];");
		return R;
	}

	/* operator overloading: V + s; V * s; etc. apply the operator to each member data */
	Vector opBinary(string op)(in real scalar) const {
		Vector R = Vector(length);
		foreach (i; 0 .. length) mixin("R[i] = data[i] "~op~" scalar;");
		return R;
	}

	/* operator overloading: V + s; V * s; etc. apply the operator to each member data */
	Vector opBinaryRight(string op)(in real scalar) const {
		return opBinary!op(scalar);
	}

	/* assignment overloading: set all data member to a value */
	void opAssign(in real scalar) {
		foreach (ref x; data) x = scalar;
	}

	/* assignment overloading: copy a vector */
	void opAssign(in Vector V) {
		data.length = V.length;
		foreach (i; 0 .. length) data[i] = V[i];
	}

	/* assignment operator overloading: apply the operator to each member data */
	void opOpAssign(string op)(in real scalar) {
		foreach (i; 0 .. length) mixin("data[i] "~op~"= scalar;");
	}

	/* assignment operator overloading: apply the operator to each member data */
	void opOpAssign(string op)(in Vector V) {
		assert(V.length == length);
		foreach (i; 0 .. length) mixin("data[i] "~op~"= V[i];");
	}

	/* indexed access */
	real opIndex(in size_t i) const {
		assert(i < length);
		return data[i];
	}
	/* indexed access */
	ref real opIndex(in size_t i) {
		assert(i < length);
		return data[i];
	}

	/* set all value to 0.0 */
	void clear() {
		foreach (ref x; data) x = 0;
	} 
	
	/* compute the norm of the vector */
	real norm() const {
		real n = 0;
		foreach (x; data) n += x * x; 
		return sqrt(n);
	}

	/* compute a normalized diff between two vectors */
	real diff(in Vector v) const {
		return (this - v).norm;
	}

	/* set a vector from evaluation weights */
	void set(in real [] weight, in bool [] isTunable) {		
		int j;
		foreach (i; 0..weight.length) {
			if (isTunable[i]) data[j++] = 0.01 * weight[i];
		}
	}
		
	/* get evaluation weights from a vector */
	void get(ref real [] weight, in bool [] isTunable) const {
		int j;
		foreach (i; 0..weight.length) {
			if (isTunable[i]) weight[i] = 100.0 * data[j++];
		}
	}
}


/*
 * Optimize a set of eval weights, from played game
 * using the Nelder-Mead simplex method.
 */
class Amoeba {
	Vector [] P;
	real [] y;
	bool [] isTunable;
	real [] weights;
	Game [] games;
	size_t best, secondWorst, worst;
	real K = 1.0;
	Search search;
	immutable depth = 0;
	ulong nBoard;
	int iter;
	
	/* read all games */
	void readGames(in string file) {
		auto f = std.stdio.File(file, "r");	
		while (true) {
			Game g = new Game;
			g.read(f);
			if (g.moves.length == 0) break;
			games ~= g;
		}
		writeln("read ", games.length, " games"); stdout.flush();
	}
	/* constructor */
	this (ref const real [] w, in string gameFile) {
		isTunable.length = w.length;
		setTunable(1, isTunable.length);
		weights = w.dup;
		search = new Search(65536);
		search.board = new Board;
		readGames(gameFile);
	}

	/* count the number of Tunable weights */
	size_t countTunable() {
		size_t n;
		foreach(b; isTunable) n += b;
		return n;
	}

	/* set a range of tunable weights */
	void setTunable(in size_t from, in size_t to) {
		foreach(ref b; isTunable[from .. to]) b = true;
	}

	/* unset a range of tunable weights */
	void clearTunable(in size_t from, in size_t to) {
		foreach(ref b; isTunable[from .. to]) b = false;
	}

	/* compute the sigmoid */
	real sigmoid(const real score) const {
		return 1.0 / (1.0 + 10.0 ^^ (-0.0025 * K * score));
	}

	/* translate a game result into [-1, 0, 1] for the player to move */
	static real result(in Color c, in Game game) {
		if (c == Color.white && game.result == Result.whiteWin) return 1;
		else if (c == Color.black && game.result == Result.blackWin) return 1;
		else if (game.result == Result.draw) return 0.5;
		else return 0;
	}

	/* compute the error from a vector */
	// TODO: make the computation parallel (via Taskpool?)
	real getError(in Vector v)  {
		real error, n, r, s;

		++iter;
		v.get(weights, isTunable);
		search.eval = new Eval(weights);

		error = n = 0.0;
		foreach (g; games) {
			if (g.moves.length <= 20) continue;
			search.board.set();
			search.clear();
			foreach (m; g.moves[0 .. 10]) search.board.update(m);
			foreach (m; g.moves[10 .. $ - 10]) {
				search.go(depth);
				s = sigmoid(search.info.score);
				r = result(search.board.player, g);
				error += (r - s) ^^ 2;
				n += 1.0;
				search.board.update(m);
			}
		}
		nBoard = cast (ulong) n;
		return error / n;
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

	/* Simplex relative size */
	real getVolume() {
		real s = 0.0;
		foreach (ref p; P) if (p != P[best]) s += p.diff(P[best]);
		real n = P[best].norm();
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

	/* optimize K in the using the golden section search. */
	void optimizeK(const real tolerance, const int maxIter) {
		real a, b, c, d, fc, fd;
		Vector v = Vector(countTunable());
		const real gr = (sqrt(5.0) - 1.0) / 2.0;
		int iter;

		v.set(weights, isTunable);

		a = 0.00; b = 10.00;
		c = b - gr * (b - a);
		d = a + gr * (b - a);
		writefln("optimize sigmoid parameter K: tolerance = %.8f, maxIter = %d", tolerance, maxIter); stdout.flush();
		K = c; fc = getError(v);
		writeln("using ", nBoard, " positions");
		K = d; fd = getError(v); stdout.flush();
		writefln("%4d c :  %.6f ⭢  %.6f  [%.6f, %.6f]", 1, c, fc, a, b);
		writefln("%4d c :  %.6f ⭢  %.6f  [%.6f, %.6f]", 2, d, fd, a, b); stdout.flush();

		for (iter = 3; fabs(c - d) > tolerance * abs(c) && iter < maxIter; ++iter) {	
			if (fc < fd) {
				b = d; d = c; fd = fc;
				c = b - gr * (b - a);
				K = c; fc = getError(v);
				writefln("%4d c :  %.6f ⭢  %.6f  [%.6f, %.6f]", iter, c, fc, a, b); stdout.flush();
			} else {
				a = c; c = d; fc = fd;
				d = a + gr * (b - a);
				K = d; fd = getError(v);
				writefln("%4d d :  %.6f ⭢  %.6f  [%.6f, %.6f]", iter, d, fd, a, b); stdout.flush();
			}
		}

		K = (a + b) * 0.5;
		writefln("%.8f < %.8f ⭢ K = %.8f\n", fabs(c - d), tolerance, K); stdout.flush();
	}

	/* init the Simplex */
	void init(in real volume) {
		const size_t size = countTunable() + 1;
		const real a = volume / size * sqrt(2.0);
		const real δi = a * (sqrt(size - 1.0) + size - 1.0);
		const real δj = a * (sqrt(size - 1.0) - 1.0);

		P.length = size;
		foreach(ref p; P) p = Vector(size - 1);
		P[0].set(weights, isTunable);
		foreach (i; 1 .. size) {
			P[i] = P[0] + δj;
			P[i][i] = P[0][i] + δi;
		}
		y.length = size;
		foreach (i; 0 .. size) {
			y[i] = getError(P[i]);
			writef("\r %d/%d", i + 1, size); stdout.flush();
		}
		writeln();
	}

	/* Amoeba or simplex optimization algorithm. */
	void tune(in real tolerance, in int maxIter, in bool adaptative = false)	{
		Vector C, Pr, Pe, Pc;
		real size, flat, υ, yBest = +real.max, yr, ye, yc;
		immutable int minIter = 10;
		immutable real tiny = tolerance * tolerance;
		immutable real  ν = adaptative ? P.length - 1 : 2.0;
		immutable real α = 1.0, β = 1.0 + 2.0 / ν, γ = 0.75 - 1.0 / (2.0 * ν), δ = 1.0 - 1.0 / ν;
		enum Stage {init, reflection, expansion, contraction, reduction}
		Stage stage;

		writefln("stopping condition: size < %10.8f flat < %10.8f iter > %d", tolerance, tiny, maxIter);
		while (true) {
			setExtrema();
			if (yBest > y[best]) {
				yBest = y[best];
				P[best].get(weights, isTunable);
				Eval.printWeights(weights);
			}
		
			flat = (y[worst] - y[best]) / (y[best] + y[worst]);
			size = getVolume();
			writefln("%12s: %3d; best = %10.8f worst = %10.8f, 2nd_worst = %10.8f : size = %10.8f flat = %10.8f", stage, iter, y[best], y[worst], y[secondWorst], size, flat);
			stdout.flush();
			if (((size < tolerance  || flat < tiny) && iter > minIter) || iter >= maxIter) return;

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
	}
}

/* tune */
void main(string [] args) {
	int [] range;
	int maxIter = 3000;
	real size = 0.1, tolerance = 0.001;
	string weightFile, gameFile;

	getopt(args, "iter|i", &maxIter , "size|s", &size, "tolerance|t", &tolerance, "range|r", &range,
		"weight|w", &weightFile, "game|g", &gameFile);

	Amoeba amoeba = new Amoeba(weight.initialWeights, gameFile);
	
	writeln("range", range);
	if (range.length) {
		amoeba.clearTunable(0, amoeba.isTunable.length);
		for (auto i = 0; i + 1 < range.length; i += 2) amoeba.setTunable(range[i], range[i + 1]);
	}
	amoeba.clearTunable(0, 1);      // pawn opening material = 1 pawn by definition.

	amoeba.optimizeK(0.0001, 100);
	amoeba.init(size);
	amoeba.tune(tolerance, maxIter);
	
	if (weightFile.length > 0) {
		std.stdio.File file = std.stdio.File(weightFile, "w");
		Eval.printWeights(amoeba.weights, file);
		file.close();				
	}
}

