module unittests.fileTests;
import parseUtils.baseFile;
import parseUtils.variableFiles;

string[] listdir(string pathname){
    import std.algorithm.iteration : map, filter;
    import std.array : array;
    import std.path : baseName;
    import std.file;

    return dirEntries(pathname, SpanMode.shallow)
        .map!((return a) => a.name)
        .array;
}
import std.stdio;

unittest{
    foreach(string varFilePath ; listdir("bins/8xvFiles")){
        	BinParseBlock xp = genVarParser();
	        xp.fromFile(varFilePath);
        
    }
}