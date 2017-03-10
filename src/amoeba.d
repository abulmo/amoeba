/*
 * main function
 * © 2016-2017 Richard Delorme
 */

module amoeba;

import board, eval, kpk, search, uci;
import std.stdio;

/* main function */
void main(string[] arg) {
	Uci uci;
	kpk.init();

	try {
		version (unittest) {}
		else {
			if (arg.length > 1) {
				if (arg[1] == "perft") perft(arg[1 .. $], null);
				else if (arg[1] == "bench") epdTest(arg[1 .. $], false);
				else if (arg[1] == "epd") epdTest(arg[1 .. $], true);
				else if (arg[1] == "--debug" || arg[1] == "-g") uci = new Uci(true);
				else if (arg[1] == "--help" || arg[1] == "-h") {
					stderr.writeln(arg[0] ~ " [perft <args>] | [bench <args>] | [epd <args] | [--debug|-g] | [--help|-h] | [--version|-v]");
					stderr.writeln("    perft <args>         Test move generation correctness & performance");
					stderr.writeln("    bench <args>         Test search speed from a set of positions");
					stderr.writeln("    epd <args>           Test search quality from a set of positions");
					stderr.writeln("                         Launch the engine waiting for UCI compatible input");
					stderr.writeln("    --debug|-g           Turn logging to a debug file on by default");
					stderr.writeln("    --version|-v         Display version number");
					stderr.writeln("    --help|-h            Display this help");
					stderr.writeln("    <command> --help|-h  Display specific command line options");
				} else if (arg[1] == "--version" || arg[1] == "-v") {
					stderr.writeln("Amoeba " ~ versionNumber ~ "." ~ arch ~ ": an UCI chess engine.");
					stderr.writeln("Copyright © 2016 - 2017 Richard Delorme");
					stderr.writeln("");
					stderr.writeln("This program is free software: you can redistribute it and/or modify");
					stderr.writeln("it under the terms of the GNU General Public License as published by");
					stderr.writeln("the Free Software Foundation, either version 3 of the License, or");
					stderr.writeln("(at your option) any later version.");
					stderr.writeln("");
					stderr.writeln("This program is distributed in the hope that it will be useful,");
					stderr.writeln("but WITHOUT ANY WARRANTY; without even the implied warranty of");
					stderr.writeln("MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the");
					stderr.writeln("GNU General Public License for more details.");
					stderr.writeln("");
					stderr.writeln("You should have received a copy of the GNU General Public License");
					stderr.writeln("along with this program.  If not, see <http://www.gnu.org/licenses/>.");
				}
			} else {
				uci = new Uci(false);
			}
		}
		if (uci !is null) uci.loop();
	} catch (Exception e) {
		stdout.writeln("FATAL ERROR: ", e.msg);
	}
}

