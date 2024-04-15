import std.stdio;
import parseUtils.baseFile;

void main()
{
	BinParseBlock xp = gen8xvParser();
	xp.fromFile("bins/8xvFiles/test.8xs");
	// writeln("Edit source/app.d to start your project.");
}
