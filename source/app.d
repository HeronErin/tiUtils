import std.stdio;
import parseUtils.programFile;
import parseUtils.baseFile;
import std.stdio;

void main()
{
	BinParseBlock prog = genProgramParser();
	prog.fromFile("bins/8xpFiles/TEST.8xp");
	(cast(string) prog.findById("VarData").data).writeln;
}
