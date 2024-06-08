module parseUtils.variableFiles;
import parseUtils.baseFile;

BinParseBlock genVarHeaderParser() {
    with (blobContentVariety) {
        return new BinParseBlock(
            [
            //     Field id            type of field      depends    byte data                     len (optional)
            Field("Magic Number", requiredBytes, null, cast(ubyte[]) "**TI83F*"),
            Field("Further signature", requiredBytes, null, [0x1A, 0x0A, 0x00]),
            Field("Comment", fixedSizeBytes, null, null, 42),
            Field("Data length", uShortField, null),
            Field("Data", floatingBytesField, "Data length"),
            Field("Checksum", uShortChecksum, "Data"),
        ]
        );
    }
}

BinParseBlock genVarEntryParser() {
    with (blobContentVariety) {
        return new BinParseBlock([
            Field("Constant", fixedSizeBytes, null, null, 2), // Always has a value of 11 or 13 (Bh or Dh).
            Field("Var length", uShortField, null, null),
            Field("Var id", ubyteField, null),
            Field("Name", fixedStringFieled, null, null, 8),
            Field("Version", ubyteField, null),
            Field("Flag", ubyteField, null),
            Field("Var length2", uShortField, null, null),
            Field("Var length3", uShortField, null, null),
            Field("VarData", floatingBytesField, "Var length3"),

        ]);
    }
}

import common : isValidMember;
enum TypeID : ubyte {
    RealNumber = 0x00,
    RealList = 0x01,
    Matrix = 0x02,
    YVariable = 0x03,
    String = 0x04,
    Program = 0x05,
    EditLockedProgram = 0x06,
    Picture = 0x07,
    GraphicsDatabase = 0x08,
    WindowSettings = 0x0B,
    ComplexNumber = 0x0C,
    ComplexListNumber = 0x0D,
    WindowSettings2 = 0x0F,
    SavedWindowSettings = 0x10,
    TableSetup = 0x11,
    Backup = 0x13,
    DeleteFlashApplication = 0x14,
    ApplicationVariable = 0x15,
    GroupVariable = 0x16,
    Directory = 0x19,
    OS = 0x23,
    IdList = 0x26,
    GetCertificate = 0x27,
    Clock = 0x29
}
bool isValidTypeId(ubyte id){
    return isValidMember!(TypeID, ubyte)(id);
}

BinParseBlock genVarParser() {
    BinParseBlock header = genVarHeaderParser();
    header.combine("Data", genVarEntryParser());
    return header;
}
