module interactive.info;
import std.path;
import std.file;
import std.stdio;
import tern.object;
import common;

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

    return format(unitIndex ? "%.2f %s" : "%g %s", size, units[unitIndex]);
}


import parseUtils.baseFile;

private void flashFormatSpecificInfo(string pathToBin, bool isOS) {
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
    writeln(humanReadableSize(
            makeEndian(*cast(uint*) binFile.findById("HexData length")
            .data.ptr, Endianness.LittleEndian)
    ));

    // ================================================
    // ||          Binary data information           ||
    // ================================================

    import parseUtils.flashHeader;
    import parseUtils.intellHex;

    HexData[] pages = decodeIntellHex(binFile.findById("Data").data);

    size_t headerLength;
    FlashHeaderField[] fields = headerGen(pages[0].data, headerLength);

    write("Binary header information (" ~ humanReadableSize(headerLength) ~ ") consiting of ");
    write(fields.length);
    writeln(" fields:");

    foreach (FlashHeaderField field; fields) {
        if (field.type == FieldType.LastField)
            continue;
        write("\t");
        field.type.write;
        ": ".write;
        switch (field.type) {
            case FieldType.ProgramLength:
                ubyte[] rdata = field.info[2 .. $][];

                uint size = makeEndian(*cast(uint*) rdata.ptr, Endianness.BigEndian);
                humanReadableSize(size).writeln;

                break;

            case FieldType.Name:
                "\"".write;
                (cast(string) field.data).write;
                "\" ".write;
                goto default;
            case FieldType.DateStamp:
                if (field.data.length != 4 && field.data.length != 6)
                    goto default;
                auto date = field.parseDate;
                if (date != null) {
                    date.value.toSimpleString.write;
                    "  ".write;
                }
                goto default;
            default:
                field.data.toHex.writeln;
                break;
        }

    }
    // ================================================
    // ||                 Signature                  ||
    // ================================================
    if (isOS) {
        size_t sigStart, sigSize, index;

        getFieldSize(pages[$ - 1].data, index, sigStart, sigSize);
        "Signature on final page: ".write;
        pages[$ - 1].data[sigStart + index .. sigStart + index + sigSize].toHex.writeln;
    }
    else {
        assert(fields[0].type == FieldType.ProgramLength);

        uint size = makeEndian(*cast(uint*)(fields[0].info.ptr + 2), Endianness.BigEndian);
        // Master field length
        size += 6;
        // Take into acount pages
        size %= 0x4000;

        size_t index = size;

        size_t sigStart, sigSize;

        if (pages[$ - 1].data[index] != 2){
            write("Signature on final page appears corrupt!");
            return;
        }
        getFieldSize(pages[$ - 1].data, index, sigStart, sigSize);
        index += sigStart;

        "Signature on final page (".write;
        sigSize.write;
        " bytes): ".write;

        pages[$ - 1].data[index .. index + sigSize].toHex.writeln;
    }
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

    if (id == TypeID.String){
        "String contents (Interpreted as text):".writeln;
        stdout.rawWrite(binFile.findById("VarData").data);
        writeln();
    }
    if (id == TypeID.Program || id == TypeID.EditLockedProgram){
        bool programStart = binFile.findById("VarData").data[0..2] == [0xBB, 0x6D];
        write("Is marked as being a compiled asm program: ");
        writeln(programStart);
    }
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
    BinExt binExt = cast(BinExt) ext;

    writeln("Basic file overview:");
    write("\tfile type: ");
    writeln(binExt);
    write("\tFile Size: ");
    writeln(humanReadableSize(getSize(pathToBin)));

    writeln("In-File information (link file header information):");

    if (binExt.isFlash)
        flashFormatSpecificInfo(pathToBin, binExt == BinExt.OS);
    else if (binExt.isVar)
        variableSpecificInfo(pathToBin);
    else
        assert(0, "Unsupported ext: " ~ ext);

    return 0;
}
