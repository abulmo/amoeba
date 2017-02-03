/*
 * main function
 * © 2016-2017 Richard Delorme
 */

module amoeba;

import board, eval, kpk, search, uci;
import std.stdio;

void main(string[] arg) {
	kpk.init();

	version (unittest) {}
	else {
		if (arg.length > 1) {
			if (arg[1] == "perft") perft(arg[1 .. $], null);
			else if (arg[1] == "bench") epdTest(arg[1 .. $], false);
			else if (arg[1] == "epd") epdTest(arg[1 .. $], true);
		} else {
			Uci uci = new Uci;
			uci.loop();
		}
	}
}

