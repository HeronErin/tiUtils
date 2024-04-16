import std.stdio;
import parseUtils.baseFile;
import std.stdio;

void main()
{
	BinParseBlock xp = gen8xvParser();
	xp.fromFile("bins/8xvFiles/test.8xs");
	xp.findById("Data").data.writeln;
	// writeln("Edit source/app.d to start your project.");
}
