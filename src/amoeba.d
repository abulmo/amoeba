/*
 * main function
 * © 2016-2020 Richard Delorme
 */

module amoeba;

import board, eval, kpk, search, uci;
import std.conv, std.stdio;

/* main function */
void main(string[] args) {
	Uci uci;
	bool dbg = false;
	int depth = -1, nThreads = 1;
	double time = 0.0;
	size_t hashSize = 64;
	string affinity = "0:0";

	string help = args[0] ~ "[--debug|-g] | [--help|-h] | [--version|-v]\n" ~
		"                           Launch the engine waiting for UCI compatible input\n" ~
		"    --depth|-d <depth>     Search at depth d\n" ~
		"    --time|-t <seconds>    Search at fixed time t\n" ~
		"    --hash|-H <Mb>         Set default HashSize\n" ~
		"    --cpu|-c <threads>     Set default number of threads\n" ~
		"    --affinity|-a <[o:]s>  Set cpu affinity as 'offset:step'\n" ~
		"    --debug|-g           Turn logging to a debug file on by default\n" ~
		"    --version|-v         Display version number\n" ~
		"    --help|-h            Display this help\n";
	string ver = "Amoeba " ~ versionNumber ~ "." ~ arch ~ ": an UCI chess engine.\n\n" ~
		"Copyright © 2016 - 2020 Richard Delorme\n\n" ~
		"This program is free software: you can redistribute it and/or modify\n" ~
		"it under the terms of the GNU General Public License as published by\n" ~
		"the Free Software Foundation, either version 3 of the License, or\n" ~
		"(at your option) any later version.\n\n" ~
		"This program is distributed in the hope that it will be useful,\n" ~
		"but WITHOUT ANY WARRANTY; without even the implied warranty of\n" ~
		"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n" ~
		"GNU General Public License for more details.\n\n" ~
		"You should have received a copy of the GNU General Public License\n" ~
		"along with this program.  If not, see <http://www.gnu.org/licenses/>.\n";

	void start() {
		uci = new Uci(dbg, book);
		if (depth > -1) uci.forcedDepth = depth;
		if (time > 0.0) uci.forcedTime = time;
		uci.resize(hashSize);
		uci.threads(nThreads);
		uci.affinity = affinity;
		uci.loop();
	}

	try {
		kpk.init();
		foreach(i, arg; args) {
			if (arg == "--debug" || arg == "-g") dbg = true;
			if (arg == "--depth" || arg == "-d") depth = to!int(args[i + 1]); // i < args.length ?
			if (arg == "--time" || arg == "-t") time = to!double(args[i + 1]);
			if (arg == "--hash" || arg == "-H") hashSize = to!size_t(args[i + 1]);
			if (arg == "--cpu" || arg == "-c") nThreads = to!int(args[i + 1]);
			if (arg == "--affinity" || arg == "-a") affinity = args[i + 1];
		}
		if (args.length > 1) {
			if (args[1] == "--help" || args[1] == "-h") stderr.writeln(help);
			else if (args[1] == "--version" || args[1] == "-v") stderr.writeln(ver);
			else start();
		} else start();
	} catch (Exception e) {
		stderr.writeln("FATAL ERROR: ", e.msg);
	}
}

