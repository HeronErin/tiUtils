module dissasembly.symbolLookup;

enum string DEFAULT_SYMBOL_DATA_STR = import("default.symbols");
const Pair[] pairs = pairsFromSymbolText(DEFAULT_SYMBOL_DATA_STR);

struct Pair {
    string k;
    uint v;
}

import std.stdio;

pure Pair[] pairsFromSymbolText(const(string) data) {
    import std.json;

    JSONValue[] dataJson = parseJSON(data).array;

    Pair[] ret;
    ret.reserve(dataJson.length);

    foreach (pair; dataJson) {
        JSONValue[] pairJson = pair.array();

        ret ~= Pair(pairJson[0].get!string, pairJson[1].get!uint);
    }
    return ret;
}
import tern.typecons.common : Nullable, nullable;


string[] lookupMultiple(T)(const(Pair[][]) lookupPrecedense, T lookup){
    string[] found;
    foreach_reverse(const(Pair[]) orderLevel; lookupPrecedense){
        foreach (const(Pair) testWith; orderLevel) {
            if (testWith.v == lookup)
                found ~= testWith.k;
        }
    }
    return found;
}

Nullable!string lookupSingle(T)(const(Pair[][]) lookupPrecedense, T lookup){
    foreach_reverse(const(Pair[]) orderLevel; lookupPrecedense){
        foreach (const(Pair) testWith; orderLevel) {
            if (testWith.v == lookup)
                return nullable!string(testWith.k);
        }
    }
    return nullable!string(null);
}




enum ushort SYS_BCALL_MASK = 0xC000;

Nullable!string lookupSysBcall(const(Pair[][]) lookupPrecedense, ushort lookup){
    // If the highest bytes are zero, it must be a local bcall
    if ((SYS_BCALL_MASK & lookup) == 0)
        return nullable!string(null);

    return lookupSingle(lookupPrecedense, lookup);
}