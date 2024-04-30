import std.stdio;
import parseUtils.flashFile;
import parseUtils.baseFile;
import parseUtils.intellHex;
import std.stdio;

void main()
{
	BinParseBlock prog = genFlashFileParser();
	prog.fromFile("bins/8xkFiles/whatangl.8xk");

	decodeIntellHex(prog.findById("Data").data).length.writeln;
	// (prog.findById("Name length").data).writeln;
	// (cast(string)prog.findById("Data").data).writeln;
}
