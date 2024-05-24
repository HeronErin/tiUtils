module interactive.project;
import std.file;
import std.path;
import std.stdio;
import mir.ser.json : serializeJson;
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
    Program = ".8xp",
    OS = ".8xu",
}
import std.traits : EnumMembers;

bool isValidMember(E, V)(V value) {
    foreach (member; EnumMembers!E) {
        if (value == member) {
            return true;
        }
    }
    return false;
}

bool isValidExt(string value) {
    return isValidMember!(BinExt, string)(value);
}

import parseUtils.baseFile;
import parseUtils.intellHex;
import parseUtils.flashFile;

class AppProject {
    size_t pageCount;
    BinParseBlock binFile;
    
    // From file
    this(string pathToFile) {
        import parseUtils.baseFile;
        import parseUtils.intellHex;
        import parseUtils.flashFile;

        binFile = genFlashFileParser();
        binFile.fromFile(pathToFile);

        auto pages = decodeIntellHex(binFile.findById("Data").data);
        pageCount = pages.length;
    }
}
import std.typecons : Nullable;
class Project {
    string path;
    string binPath;
    BinExt binExt;
    Nullable!AppProject appProject;

    // Create new project
    this(string pathToProject, string pathToBin) {
        path = pathToProject;
        binExt = cast(BinExt) extension(pathToBin);
        binPath = buildPath(pathToProject, "bin" ~ binExt);

        copy(pathToBin, binPath);

        switch (binExt){
            case BinExt.App:
                appProject = new AppProject(binPath);
                break;
            default:
                assert(0, "Unsupported binary extension!");
        }
        serializeJson(this).writeln;

    }

    // Open existing project
    this(string pathToProject) {
        assert(0);
    }
}
