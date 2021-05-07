/*
 * File tt.d
 * Transposition Table.
 * © 2016-2020 Richard Delorme
 */

module tt;

import board, move, util;
import std.format;

/* Hash table score bound */
enum Bound {none, upper, lower, exact}

/* 
 * Hash Table Entry 
 */
struct Entry {
	uint code;    // zobrist key
	ushort info;  // date (6 bits) / depth (7 bits) / bound (2 bits) / singular (1 bit)
	short score;  // from search
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
	bool extended() const @property {
		return (info & 1);
	}

	/* refresh the aging date */
	void refresh(const int date) {
		info = cast (ushort) ((info & 1023) | (date << 10));
	}

	/* store common data to update & set into this entry */
	void store(const Bound b, const int d, const int date, const bool e, const int s, const int v) {
		info = cast (ushort) (e | (b << 1) | (d << 3) | (date << 10));
		score = cast (short) s;
		value = cast (short) v;
	}

	/* update an existing entry */
	void update(const int d, const int date, const Bound b, const bool e, const int s, const int v, const Move m) {
		store(b, d, date, e, s, v);
		if (m != move[0]) { move[1] = move[0]; move[0] = m; }
	}

	/* set a new entry */
	void set(const Key k, const int d, const int date, const Bound b, const bool e, const int s, const int v, const Move m) {
		code = k.code;
		store(b, d, date, e, s, v);
		move = [m, 0];
	}

	/* toString */
	string toString() const {
		return format("{ key = %016x, extended %s, depth = %s, bound = %s, age = %s, score = %s, move = %s }", code, extended, depth, bound, date, value, move);
	}
}

/*
 * Transposition table
 */
struct TranspositionTable {
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
		const size_t l = 1UL << lastBit(size / Entry.sizeof);
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
			foreach (ref h; entry) {
				if (cleaner) h = Entry.init;
				else h.info &= 1023; // reset date to 0;
			}
		}
		++date;
	}

	/* look for an entry matching the zobrist key */
	bool probe(const Key k, ref Entry found) {
		const size_t i = k.index(mask);
		foreach (ref h; entry[i .. i + bucketSize]) {
			if (h.code == k.code) {
				found = h;
				h.refresh(date);
				return found.code == k.code;
			}
		}
		return false;
	}

	/* store search data */
	void store(const Key k, const int depth, const Bound b, const bool singular, const int v, const int e, const Move m) {
		const size_t i = k.index(mask);
		Entry *w = &entry[i];
		foreach (ref h; entry[i .. i + bucketSize]) {
			if (h.code == k.code) {
				h.update(depth, date, b, singular, v, e, m);
				return;
			} else if (w.info > h.info) w = &h;
		}
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
		int n = 0;
		foreach (e; entry[0 .. 1000]) n += (e.info != 0);
		return n;
	}

}

