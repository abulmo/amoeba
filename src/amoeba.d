/*
 * main function
 * © 2016-2019 Richard Delorme
 */

module amoeba;

import board, eval, kpk, search, uci;
import std.stdio;

/* main function */
void main(string[] arg) {
	Uci uci;
	string help = arg[0] ~ "[--debug|-g] | [--help|-h] | [--version|-v]\n" ~
		"                         Launch the engine waiting for UCI compatible input\n" ~
		"    --debug|-g           Turn logging to a debug file on by default\n" ~
		"    --version|-v         Display version number\n" ~
		"    --help|-h            Display this help\n";
	string ver = "Amoeba " ~ versionNumber ~ "." ~ arch ~ ": an UCI chess engine.\n\n" ~
		"Copyright © 2016 - 2017 Richard Delorme\n\n" ~
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
	try {
		kpk.init();
		if (arg.length > 1) {
			if (arg[1] == "--debug" || arg[1] == "-g") uci = new Uci(true);
			else if (arg[1] == "--help" || arg[1] == "-h") stderr.writeln(help);
			else if (arg[1] == "--version" || arg[1] == "-v") stderr.writeln(ver);
		} else {
			uci = new Uci(false);
		}
		if (uci !is null) uci.loop();
	} catch (Exception e) {
		stdout.writeln("FATAL ERROR: ", e.msg);
	}
}

