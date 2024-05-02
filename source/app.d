import std.stdio;
import parseUtils.flashFile;
import parseUtils.baseFile;
import parseUtils.intellHex;
import parseUtils.appHeader;
import std.stdio;

void main()
{
	BinParseBlock prog = genFlashFileParser();
	prog.fromFile("bins/8xkFiles/helloWorld.8xk");

	ubyte[] data = decodeIntellHex(prog.findById("Data").data)[0].data;
	size_t index;


	import dissasembly.appDecompiler;
	decompPage(data);

	// ( cast(ubyte[])[0xED, 0x02] ).getInstruction(index).unknownData.writeln;
	// index.writeln;
}
