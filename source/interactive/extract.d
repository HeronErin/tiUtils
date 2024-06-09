module interactive.extract;
import std.stdio;
import std.file;
import common;

int extractInteractive(string pathToBin, string pathToProj) {
    if (!exists(pathToBin)) {
        stderr.write("Error: File \"");
        stderr.write(pathToBin);
        stderr.writeln("\" not found!");
        return 1;
    }
    if (exists(pathToProj) && isFile(pathToProj)) {
        stderr.write("Error: \"");
        stderr.write(pathToProj);
        stderr.writeln("\" is an existing file! It must be a non existent (or empty) folder!");
        return 1;
    }
    import std.path;

    string ext = extension(pathToBin);
    BinExt binExt = cast(BinExt) ext;

    if (!isValidExt(ext)) {
    InvalidExt:
        stderr.write("Error: \"");
        stderr.write(ext);
        stderr.writeln("\" is not a supported extension!");
        return 1;
    }
    if (!exists(pathToProj))
        mkdir(pathToProj);

    if (binExt.isFlash) {
        import parseUtils.flashFile;
        import parseUtils.intellHex;

        auto binFile = genFlashFileParser();
        binFile.fromFile(pathToBin);

        quickDump(cast(ubyte[]) binFile.asJson().toPrettyString(), buildPath(pathToProj, "header.json"));

        HexData[] pages = decodeIntellHex(binFile.findById("Data").data);
        import std.conv;

        foreach (i, HexData page; pages) {
            string name;
            if (page.declaredPageInfo == 0xFFFF) {
                if (i == 0)
                    name = "headerPage.bin";
                else if (i == pages.length - 1)
                    name = "signaturePage.bin";
                else
                    name = "unknownPage" ~ i.to!string ~ ".bin";
            }
            else
                name = "page_" ~ page.declaredPageInfo.toHex ~ ".bin";
            quickDump(page.data, buildPath(pathToProj, name));
        }

    }
    else if (binExt.isVar) {
        import parseUtils.variableFiles;

        auto binFile = genVarParser();
        binFile.fromFile(pathToBin);

        quickDump(cast(ubyte[]) binFile.asJson().toPrettyString(), buildPath(pathToProj, "header.json"));
        quickDump(binFile.findById("VarData").data, buildPath(pathToProj, "data.bin"));
    }
    else
        assert(0, "Unsupported ext: " ~ ext);

    return 0;
}
