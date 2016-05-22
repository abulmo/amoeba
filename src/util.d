/*
 * File util.d
 * Fast implementation on X86_64, portable algorithm for other platform
 * of some bit functions.
 * © 2016 Richard Delorme
 */

module util;
import std.stdio, std.array, std.string;
import core.bitop, core.time, core.thread;

version (LDC) import ldc.intrinsics;
version (GNU) import gcc.builtins;

/*
 * bit utilities
 */
/* Swap the bytes of a bitboard (vertical mirror of a Chess board) */
version (DMD) alias swapBytes = bswap;
else version (LDC) alias swapBytes = llvm_bswap;
else version(GNU) alias swapBytes = __builtin_bswap64;
else {
	ulong swapBytes(ulong b) pure {
		b = ((b >>  8) & 0x00FF00FF00FF00FF) | ((b <<  8) & 0xFF00FF00FF00FF00);
		b = ((b >> 16) & 0x0000FFFF0000FFFF) | ((b << 16) & 0xFFFF0000FFFF0000);
		b = ((b >> 32) & 0x00000000FFFFFFFF) | ((b << 32) & 0xFFFFFFFF00000000);
		return b;
	}
}

/* Check if a single bit is set */
bool hasSingleBit(in ulong b) pure {
	return (b & (b - 1)) == 0;
}

/* Get the first bit set */
version (LDC) int firstBit(ulong b) pure {return cast (int) llvm_cttz(b, true);}
else alias firstBit = bsf;

/* Extract a bit */
int popBit(ref ulong b) pure {
	immutable int i = firstBit(b);
	b &= b - 1;
	return i;
}

/* Count the number of bits */
version (withPopCount) {
	version (GNU) alias countBits = __builtin_popcountll;
	else version (LDC) int countBits(in ulong b) pure {return cast (int) llvm_ctpop(b);}
	else alias countBits = _popcnt;
} else {
	int countBits(in ulong b) pure {
		ulong c = b
			- ((b >> 1) & 0x7777777777777777)
			- ((b >> 2) & 0x3333333333333333)
			- ((b >> 3) & 0x1111111111111111);
		c = ((c + (c >> 4)) & 0x0F0F0F0F0F0F0F0FU) * 0x0101010101010101;

		return  cast (int) (c >> 56);
	}
}

/* Print bits */
void writeBitboard(in ulong b, File f = stdout) {
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

	/* return spent time in secs with decimals */
	real time() const {
		real t;
		if (on) t = (TickDuration.currSystemTick() - tick).hnsecs;
		else t = tick.hnsecs;
		return 1e-7 * t;
	}
}

/* remove characters between two strings */
string findBetween(in string s, in string start, in string end) pure {
	size_t i, j;

	for (; i < s.length; ++i) if (s[i .. i + start.length] == start) break;
	i += start.length; if (i > s.length) i = s.length;
	for (j = i; j < s.length; ++j) if (s[j .. j + end.length] == end) break;

	return s[i .. j];
}

/* remove characters between two strings. TODO: ("(((blabla)))", "(", ")") remove all */
string removeBetween(in string s, in string start, in string end) pure {
	size_t i, j;

	for (; i < s.length; ++i) if (s[i .. i + start.length] == start) break;
	for (j = i + start.length; j < s.length; ++j) if (s[j .. j + end.length] == end) break;
	j += end.length; 
	if (j > s.length) return s[0 .. i];
	else return s[0 .. i] ~ s[j .. $];
}

/*
 * class Event
 */
class Event {
	private:
	string [] ring;
	size_t first, last;
	class Lock {};
	Lock lock;

	public:
	/* constructor */
	this () shared {
		ring.length = 4;
		lock = new shared Lock;
	}

	/* ring is empty */
	shared bool empty() pure const @property {
		return first == last;
	}

	/* ring is full */
	shared bool full() pure const @property {
		return first == (last + 1) % ring.length;
	}

	/* push a new event to the ring */
	shared void push(string s) {
		synchronized (lock) {
			if (full) {
				auto l = ring.length;
				ring.length = 2 * l;
				foreach (i ; 0 .. first) ring[i + l] = ring[i];
				last = last + l;
			}
			ring[last] = s;
			last = (last + 1) % ring.length;
		}
	}

	/* peek an event */
	shared string peek() {
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
	shared string wait() {
		while (empty) Thread.sleep(1.msecs);
		return peek();
	}

	/* has event s */
	shared bool has(string s) pure {
		return !empty && ring[first] == s;
	}

	/* loop */
	shared void loop() {
		string line;
		do {
			line = readln().chomp();
			push(line);
		} while (stdin.isOpen && line != "quit");
	}
}

/* an ugly batracian */
double toad(in real v) pure {
	return cast (double) v;
}

/* Unit test */
unittest {
	assert(swapBytes(0x1122334455667788) == 0x8877665544332211);
	assert(hasSingleBit(128));
	assert(!hasSingleBit(42));
	assert(firstBit(42) == 1);
	ulong b = 42;
	assert(popBit(b) == 1);
	assert(popBit(b) == 3);
	assert(popBit(b) == 5);
	assert(b == 0);
	assert(countBits(42) == 3);
	assert(findBetween("bm Qg6 Rh3;", "bm", ";") == " Qg6 Rh3");
}

