import std.stdio;
import parseUtils.flashFile;
import parseUtils.baseFile;
import parseUtils.intellHex;
import parseUtils.appHeader;
import std.stdio;

void main()
{
	BinParseBlock prog = genFlashFileParser();
	prog.fromFile("bins/8xkFiles/cabrijr.8xk");

	ubyte[] data = decodeIntellHex(prog.findById("Data").data)[0].data;
	size_t index;
	headerGen(data, index);
	// stdout.rawWrite(data);

}
