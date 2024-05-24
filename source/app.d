import std.stdio;
import interactive.run;
import interactive.create;
import interactive.info;

string stringWithSpaces(size_t count) {
	string ret;
	while (count--) {
		ret ~= " ";
	}
	return ret;
}

int main(string[] args) {
	if (args.length < 2 || args[1] == "-h" || args[1] == "--help" || args[1] == "help") {
	HELP_SCREEN:
		writeln(
			"TiUtils - A powerful, open source, Ti-83+ / Ti-84+ decompiler. (https://github.com/HeronErin/tiUtils)");

		writeln("Options:\n");
		static foreach (CommandDesc; [
				["TI help", "Shows this menu"],
				[
					"TI create [Project directory] [Path file to disassemble]",
					"Create a new project in a given directory"
				],
				[
					"TI run [Project directory]",
					"Go back to an interactive editing session with a given file"
				],
				[
					"TI info [Path file to look into]",
					"Get a bunch of info for a given binary"
				],
			]) {
			write(CommandDesc[0]);
			write(stringWithSpaces(60 - CommandDesc[0].length));
			write("-");
			writeln(CommandDesc[1]);
		}

		return 1;
	}
	if (args[1] == "run" && args.length == 3) {
		return runInteractive(args[2]);
	}
	if (args[1] == "info" && args.length == 3) {
		return getInfoForBinary(args[2]);
	}
	else if (args[1] == "create" && args.length == 4) {
		return createInteractive(args[2], args[3]);
	}
	else {
		goto HELP_SCREEN;
	}
	// BinParseBlock prog = genFlashFileParser();
	// prog.fromFile("bins/world_signed.8xk");

	// auto pages = decodeIntellHex(prog.findById("Data").data);

	// // stdout.rawWrite(pages[0].data);

	// import dissasembly.smartFlowAnalisis;
	// import dissasembly.z80Decompiler;
	// size_t index;
	// auto f = headerGen(pages[0].data, index);
	// AnalysisState state = analyse(0x4000, 0x4000, [cast(ushort) (index + 0x4000)], pages[0].data);
	// // import dissasembly.z80;
	// // import std.conv;
	// // AnalysisState state = analyse(0x4000, 0x4000, [cast(ushort)(0x4000)],
	// // 	cast(ubyte[])hexString!"7818027F7F7F213412C818F7"
	// // );
	// auto comp = state.genCompleteUnit;

	// comp.toAsm.writeln;
	// // decompPage(pages[0].data).toAsm.writeln;

	// // ( cast(ubyte[])[0xED, 0x02] ).getInstruction(index).unknownData.writeln;
	// // index.writeln;
	return 0;
}
