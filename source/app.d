import std.stdio;
import parseUtils.variableFiles;
import parseUtils.baseFile;
import std.stdio;

void main()
{
	BinParseBlock xp = genVarParser();
	xp.fromFile("bins/8xvFiles/test.8xs");
	BinParseBlock varData = genVarEntrieParser();
	varData.fromBytes(xp.findById("Data").data);
	(cast(string) varData.findById("Data").data).writeln();
	// writeln("Edit source/app.d to start your project.");
}
