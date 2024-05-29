module interactive.info;
import interactive.project;
import std.path;
import std.file;
import std.stdio;

import std.string : format;

string humanReadableSize(ulong bytes) {
    import std.conv : to;

    enum string[] units = ["B", "KB", "MB", "GB", "TB", "PB", "EB"];
    double size = bytes;
    int unitIndex = 0;

    while (size >= 1024 && unitIndex < units.length - 1) {
        size /= 1024;
        unitIndex++;
    }

    return format("%.2f %s", size, units[unitIndex]);
}

string toHex(ubyte data) {
    return format("%02X", data);
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

import parseUtils.baseFile;

private void flashFormatSpecificInfo(string pathToBin) {
    import parseUtils.flashFile;
    
    // ================================================
    // ||          FILE HEADER INFOMATION            ||
    // ================================================

    auto binFile = genFlashFileParser();
    binFile.fromFile(pathToBin);

    write("\tMajor Version: ");
    writeln(binFile.findById("Major Version").as!ubyte);
    write("\tMinor Version: ");
    writeln(binFile.findById("Minor Version").as!ubyte);
    write("\tFlags: ");
    writeln(binFile.findById("Flags").as!ubyte);
    write("\tObject type: ");
    write(binFile.findById("Object type").as!ubyte.toHex);
    writeln("h");

    write("\tBinary date (likely meaningless): ");
    ubyte[] bts = binFile.findById("Binary coded date").data;
    bts[0].toHex.write;
    "/".write;
    bts[1].toHex.write;
    "/".write;
    bts[2].toHex.write;
    bts[3].toHex.writeln;

    write("\tName: \"");
    (cast(string) binFile.findById("Name").data).write;
    write("\" (");
    binFile.findById("Name").data.toHex.write;
    writeln(")");

    write("\tDevice Type: ");
    ubyte dt = binFile.findById("Device Type").as!ubyte;
    if (isValidDeviceType(dt))
        write(cast(DeviceType) dt);
    else
        write("Invalid");
    write(" (");
    write(dt.toHex);
    writeln("h)");

    write("\tData Type: ");
    dt = binFile.findById("Data Type").as!ubyte;
    if (isValidDataType(dt))
        write(cast(DataType) dt);
    else
        write("Invalid");
    write(" (");
    write(dt.toHex);
    writeln("h)");

    write("\tIntellHex length: ");
    writeln(humanReadableSize(binFile.findById("HexData length").as!uint));
    
    // ================================================
    // ||          Binary header information         ||
    // ================================================
    
    import parseUtils.flashHeader;
    import parseUtils.intellHex;

    
    HexData[] pages = decodeIntellHex(binFile.findById("Data").data);
    "---------".writeln;
    foreach (HexData p; pages) {
        p.declaredPageInfo.writeln;
    }

    size_t headerLength;
    
    // FlashHeaderField[] fields = headerGen(binFile.findById("Data").data, headerLength);

    // write("Binary header information (" ~ humanReadableSize(headerLength) ~ ") consiting of ");
    // write(fields.length);
    // writeln(" fields:");

    // decodeIntellHex()
}


private void variableSpecificInfo(string pathToBin) {
    import parseUtils.intellHex;
    import parseUtils.variableFiles;

    auto binFile = genVarParser();
    binFile.fromFile(pathToBin);

    // ================================================
    // ||          FILE HEADER INFOMATION            ||
    // ================================================

    write("\tFile comment: \"");
    write(cast(string) binFile.findById("Comment").data);
    write("\" ");
    writeln(binFile.findById("Comment").data.toHex);

    ubyte id = binFile.findById("Var id").as!ubyte;
    write("\tVariable Type: ");
    if (isValidTypeId(id))
        write(cast(TypeID) id);
    else
        write("Invalid");
    writeln(" (" ~ id.toHex ~ "h)");

    write("\tName: \"");
    (cast(string) binFile.findById("Name").data).write;
    write("\" (");
    binFile.findById("Name").data.toHex.write;
    writeln(")");

    write("\tVersion: ");
    writeln(binFile.findById("Version").as!ubyte.toHex ~ "h");

    ubyte flag = binFile.findById("Flag").as!ubyte;
    write("\tFlags: ");
    write(flag.toHex);
    writeln(flag & 0x80 ? "h (Archived)" : "h (Not Archived)");    
}

int getInfoForBinary(string pathToBin) {
    if (!exists(pathToBin)) {
        stderr.write("Error: \"");
        stderr.write(pathToBin);
        stderr.writeln("\" Does NOT exit!");
        return 1;
    }
    if (!isFile(pathToBin)) {
        stderr.write("Error: \"");
        stderr.write(pathToBin);
        stderr.writeln("\" is NOT a file!");
        return 1;
    }
    string ext = extension(pathToBin);
    if (!isValidExt(ext)) {
        stderr.write("Error: \"");
        stderr.write(ext);
        stderr.writeln("\" is NOT a supported Ti file extension!");
        return 1;
    }
    BinExt bext = cast(BinExt) ext;

    writeln("Basic file overview:");
    write("\tfile type: ");
    writeln(bext);
    write("\tFile Size: ");
    writeln(humanReadableSize(getSize(pathToBin)));

    writeln("In-File information (link file header information):");
    switch (bext) {
        case BinExt.OS:
        case BinExt.App:
            flashFormatSpecificInfo(pathToBin);
            break;
        case BinExt.BasicOrBinaryProgram:
            variableSpecificInfo(pathToBin);
            break;
        default:
            assert(0);
    }

    return 0;
}
