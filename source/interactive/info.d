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

string toHex(T)(T data){
    return format("%02X", data);
}

import parseUtils.baseFile;

void appFormatSpecificInfo(string pathToBin) {
    import parseUtils.intellHex;
    import parseUtils.flashFile;

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
    (cast(string)binFile.findById("Name").data).write;
    write("\" (");
    binFile.findById("Name").data.write;
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

    writeln("In-File information (header data):");
    switch (bext) {
        case BinExt.OS:
        case BinExt.App:
            appFormatSpecificInfo(pathToBin);
            break;
        default:
            assert(0);
    }
    return 0;
}
