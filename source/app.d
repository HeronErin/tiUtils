import std.stdio;
import parseUtils.flashFile;
import parseUtils.baseFile;
import parseUtils.intellHex;
import parseUtils.appHeader;
import std.stdio;

void main()
{
	BinParseBlock prog = genFlashFileParser();
	prog.fromFile("bins/world_signed.8xk");

	auto pages = decodeIntellHex(prog.findById("Data").data);

	// stdout.rawWrite(pages[0].data);

	import dissasembly.smartFlowAnalisis;
	import dissasembly.z80Decompiler;
	size_t index;
	auto f = headerGen(pages[0].data, index);
	AnalysisState state = analyse(0x4000, 0x4000, [cast(ushort) (index + 0x4000)], pages[0].data);
	// import dissasembly.z80;
	// import std.conv;
	// AnalysisState state = analyse(0x4000, 0x4000, [cast(ushort)(0x4000)],
	// 	cast(ubyte[])hexString!"7818027F7F7F213412C818F7"
	// );
	auto comp = state.genCompleteUnit;

	comp.toAsm.writeln;
	// decompPage(pages[0].data).toAsm.writeln;

	// ( cast(ubyte[])[0xED, 0x02] ).getInstruction(index).unknownData.writeln;
	// index.writeln;
}
