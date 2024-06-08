module common;
import std.file;
import std.path;
import std.stdio;
import tern.algorithm.searching : contains, indexOf;

string[] listdir(string pathname) {
    import std.algorithm.iteration : map, filter;
    import std.array : array;

    return dirEntries(pathname, SpanMode.shallow)
        .map!((return a) => baseName(a.name))
        .array;
}

enum BinExt : string {
    App = ".8xk",
    BasicOrBinaryProgram = ".8xp",
    OS = ".8xu",
}

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

