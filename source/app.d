import std.stdio;
import interactive.run;
import interactive.create;
import interactive.info;
import interactive.extract;

string stringWithSpaces(size_t count) {
	string ret;
	while (count--) {
		ret ~= " ";
	}
	return ret;
}

// int main(string[] args) {
// 	if (args.length < 2 || args[1] == "-h" || args[1] == "--help" || args[1] == "help") {
// 	HELP_SCREEN:
// 		writeln(
// 			"TiUtils - A powerful, open source, Ti-83+ / Ti-84+ disassembler. (https://github.com/HeronErin/tiUtils)");

// 		writeln("Options:\n");
// 		static foreach (CommandDesc; [
// 				["TI help", "Shows this menu"],
// 				[
// 					"TI create [Project directory] [Path file to disassemble]",
// 					"Create a new project in a given directory"
// 				],
// 				[
// 					"TI run [Project directory]",
// 					"Go back to an interactive editing session with a given file"
// 				],
// 				[
// 					"TI extract [Path file to extract] [Dir to extract to]",
// 					"Gets the bins out of a link file (Ex: 8xk -> *.bin, 8xk -> x.bin)"
// 				],
// 				[
// 					"TI info [Path file to look into]",
// 					"Get a bunch of info for a given binary"
// 				],
// 			]) {
// 			write(CommandDesc[0]);
// 			write(stringWithSpaces(60 - CommandDesc[0].length));
// 			write("-");
// 			writeln(CommandDesc[1]);
// 		}

// 		return 1;
// 	}
// 	import std.string;

// 	string midArg = args[1].toLower();
// 	if (midArg == "run" && args.length == 3) {
// 		return runInteractive(args[2]);
// 	}
// 	if (midArg == "info" && args.length == 3) {
// 		return getInfoForBinary(args[2]);
// 	}
// 	else if (midArg == "extract" && args.length == 4) {
// 		return extractInteractive(args[2], args[3]);
// 	}
// 	else if (midArg == "create" && args.length == 4) {
// 		return createInteractive(args[2], args[3]);
// 	}
// 	else {
// 		goto HELP_SCREEN;
// 	}
// 	return 0;
// }
import dissasembly.z80;
int main(){
	size_t i;
	auto f = getInstruction_nullable([0xE4, 1, 2], i);
	if (f == null)
		"null".writeln;
	else
		f.value.toAssembly.value.writeln;
	return 0;
}