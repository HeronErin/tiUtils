import std.stdio;
import parseUtils.flashFile;
import parseUtils.baseFile;
import parseUtils.intellHex;
import std.stdio;

void main()
{
	BinParseBlock prog = genFlashFileParser();
	prog.fromFile("bins/8xkFiles/cabrijr.8xk");
	// prog.findById("Data").data.length.writeln;
	// prog.findById("HexData length").as!ushort.writeln;

	// prog.findById("HexData length").as!ushort.writeln;
	getIntellHexLines(prog.findById("Data").data);
	// (prog.findById("Name length").data).writeln;
	// (cast(string)prog.findById("Data").data).writeln;
}
