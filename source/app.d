import std.stdio;
import parseUtils.flashFile;
import parseUtils.baseFile;
import std.stdio;

void main()
{
	BinParseBlock prog = genFlashFileParser();
	prog.fromFile("bins/8xkFiles/cabrijr.8xk");
	// prog.findById("Data").data.length.writeln;
	// prog.findById("HexData length").as!ushort.writeln;

	ulong sum = 0;
	foreach (ubyte ub ; prog.findById("Data").data){
		sum+=ub;
	}
	(sum & 0xFFFF).writeln;
	prog.findById("Checksum").as!ushort.writeln;

}
