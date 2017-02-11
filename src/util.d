/*
 * File util.d
 * Fast implementation on X86_64, portable algorithm for other platform
 * of some bit functions.
 * © 2016-2017 Richard Delorme
 */

module util;
import std.array, std.datetime, std.format, std.stdio, std.string;
import core.bitop, core.simd, core.time, core.thread, core.stdc.stdlib;

version (LDC) import ldc.intrinsics;
else version (GNU) import gcc.builtins;

/*
 * bit utilities
 */
/* Swap the bytes of a bitboard (vertical mirror of a Chess board) */
version (LDC) alias swapBytes = llvm_bswap;
else version(GNU) alias swapBytes = __builtin_bswap64;
else alias swapBytes = bswap;

/* Check if a single bit is set */
bool hasSingleBit(const ulong b) {
	return (b & (b - 1)) == 0;
}

/* Get the first bit set */
version (LDC) int firstBit(ulong b) {return cast (int) llvm_cttz(b, true);}
else version (GDC) alias firstBit = __builtin_ctz;
else alias firstBit = bsf;

/* Get the last bit set */
version (GDC) alias firstBit = __builtin_clz;
else alias lastBit = bsr;

/* Extract a bit */
int popBit(ref ulong b) {
	const int i = firstBit(b);
	b &= b - 1;
	return i;
}

/* Count the number of bits */
version (withPopCount) {
	version (GNU) alias countBits = __builtin_popcountll;
	else version (LDC) int countBits(const ulong b) {return cast (int) llvm_ctpop(b);}
	else alias countBits = _popcnt;
} else {
	int countBits(const ulong b) {
		ulong c = b
			- ((b >> 1) & 0x7777777777777777)
			- ((b >> 2) & 0x3333333333333333)
			- ((b >> 3) & 0x1111111111111111);
		c = ((c + (c >> 4)) & 0x0F0F0F0F0F0F0F0FU) * 0x0101010101010101;

		return  cast (int) (c >> 56);
	}
}

/* Print bits */
void writeBitboard(const ulong b, File f = stdout) {
	int i, j, x;
	const char[2] bit = ".X";
	const char[8] file = "12345678";

	f.writeln("  a b c d e f g h");
	for (i = 7; i >= 0; --i) {
		f.write(file[i], " ");
		for (j = 0; j < 8; ++j) {
			x = i * 8 + j;
			f.write(bit[((b >> x) & 1)], " ");
		}
		f.writeln(file[i], " ");
	}
	f.writeln("  a b c d e f g h");
}


/*
 * prefetch
 */
void prefetch(void *v) {
	version (GNU) __builtin_prefetch(v);
	else version (LDC) llvm_prefetch(v, 0, 3, 1);
	else core.simd.prefetch!(false, 3)(v);
}


/* 
 * struct Chrono
 */
struct Chrono {
	private {
		TickDuration tick;
		bool on;
	}

	/* start */
	void start() {
		on = true;
		tick = TickDuration.currSystemTick();
	}

	/* stop */
	void stop() {
		on = false;
		tick = TickDuration.currSystemTick() - tick;
	}

	/* return spent time const secs with decimals */
	double time() const {
		double t;
		if (on) t = (TickDuration.currSystemTick() - tick).hnsecs;
		else t = tick.hnsecs;
		return 1e-7 * t;
	}
}

/* return the current date */
string date() {
	SysTime t = Clock.currTime();
	return format("%d.%d.%d", t.year, t.month, t.day);
}


/*
 * class Event
 */
shared class Event {
	private:
	string [] ring;
	size_t first, last;
	class Lock {};
	Lock lock;

	public:
	/* constructor */
	this () {
		ring.length = 4;
		lock = new shared Lock;
	}

	/* ring is empty */
	bool empty() const @property {
		return first == last;
	}

	/* ring is full */
	bool full() const @property {
		return first == (last + 1)  % ring.length;
	}

	/* push a new event to the ring */
	void push(string s) {
		synchronized (lock) {
			if (full) {
				const l = ring.length, δ = l - 1;
				ring.length = 2 * l;
				foreach (i ; 0 .. first) ring[i + δ] = ring[i];
				last = first + δ;
			}
			ring[last] = s;
			last = (last + 1) % ring.length;
		}
	}

	/* peek an event */
	string peek() {
		synchronized (lock) {
			string s;
			if (!empty) {
				s = ring[first];
				ring[first] = null;
				first = (first  + 1) % ring.length;
			}
			return s;
		}
	}

	/* wait for an event */
	string wait() {
		while (empty) Thread.sleep(1.msecs);
		return peek();
	}

	/* has event s */
	bool has(string s) {
		return !empty && ring[first] == s;
	}

	/* loop */
	void loop() {
		string line;
		do {
			line = readln().chomp();
			push(line);
		} while (line != "quit" && stdin.isOpen);
	}
}


/*
 * Miscellaneous utilities
 */

/* a replacement for assert that is more practical for debugging */
void claim(bool allegation, string file = __FILE__, const int line = __LINE__) {
	if (!allegation) {
		stderr.writeln(file, ":", line, ": Assertion failed.");
		abort();
	}
}

/* find a substring between two strings */
string findBetween(string s, string start, string end) {
	size_t i, j;

	for (; i < s.length; ++i) if (s[i .. i + start.length] == start) break;
	i += start.length; if (i > s.length) i = s.length;
	for (j = i; j < s.length; ++j) if (s[j .. j + end.length] == end) break;

	return s[i .. j];
}

/* check if a File is writeable/readable */
bool isOK(const std.stdio.File f) @property {
	return f.isOpen && !f.eof && !f.error;
}


/*
 * Unit test 
 */
unittest {
	claim(stdout.isOK);
	writeBitboard(0x55aa55aa55aa55aa);
	write("Testing utilities..."); stdout.flush();
	claim(swapBytes(0x1122334455667788) == 0x8877665544332211);
	claim(hasSingleBit(128));
	claim(!hasSingleBit(42));
	claim(firstBit(42) == 1);
	ulong b = 42;
	claim(popBit(b) == 1);
	claim(popBit(b) == 3);
	claim(popBit(b) == 5);
	claim(b == 0);
	claim(countBits(42) == 3);
	claim(findBetween("bm Qg6 Rh3;", "bm", ";") == " Qg6 Rh3");
	writeln("ok");
}

