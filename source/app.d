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
	// headerGen(data, index).writeln;
	// stdout.rawWrite(data);
	import dissasembly.z80;
	MAIN.length.writeln;

	MAIN[0xFF].writeln;
	
	foreach (ubyte i; 0..0xFF){
		if (!(i in MAIN)){
			import std.array : appender;
			import std.format.spec : singleSpec;
			import std.format;
			auto w2 = appender!string();
			auto spec2 = singleSpec("%x");
			formatValue(w2, i, spec2);
			w2.data.writeln;
			break;
		}
	}


}
