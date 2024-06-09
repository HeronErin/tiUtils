module common;
import std.file;
import std.path;
import std.stdio;
import tern.algorithm.searching : contains, indexOf;

void quickDump(ubyte[] data, string path){
    File file = File(path, "w");
    file.rawWrite(data);
    file.close();
}

string[] listdir(string pathname) {
    import std.algorithm.iteration : map, filter;
    import std.array : array;

    return dirEntries(pathname, SpanMode.shallow)
        .map!((return a) => baseName(a.name))
        .array;
}
import std.format : format;
string toHex(ubyte data) {
    return format("%02X", data);
}
string toHex(ushort data) {
    return format("%04X", data);
}

string toHex(ubyte[] data) {
    string s = "[";
    foreach (i, ubyte ub; data) {
        s ~= ub.toHex ~ "h";
        if (i + 1 != data.length)
            s ~= ", ";
    }
    return s ~ "]";
}


enum BinExt : string {
    App = ".8xk",
    OS = ".8xu",
    String = ".8xs",
    Variable = ".8xv",
    BasicOrBinaryProgram = ".8xp",
}

bool isVar(BinExt ext) => ext == BinExt.String || ext == BinExt.Variable || ext == BinExt.BasicOrBinaryProgram;
bool isFlash(BinExt ext) => ext == BinExt.App || ext == BinExt.OS;



import std.traits : EnumMembers;

bool isValidMember(E, V)(V value) {
    static foreach (member; EnumMembers!E) {
        if (value == member)
            return true;
    }
    return false;
}

bool isValidExt(string value) {
    return isValidMember!(BinExt, string)(value);
}
