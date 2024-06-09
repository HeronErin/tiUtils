module parseUtils.variableFiles;
import parseUtils.baseFile;

BinParseBlock genVarHeaderParser() {
    with (BlobContentVariety) {
        return new BinParseBlock(
            [
            //    ShowToUsr    Field id            type of field      depends    byte data                     len (optional)
            Field(false,       "Magic Number",      requiredBytes,     null,     cast(ubyte[]) "**TI83F*"),
            Field(false,       "Further signature", requiredBytes,     null,     [0x1A, 0x0A, 0x00]),
            Field(true,        "Comment",          fixedStringFieled,     null,     null,                         42),
            Field(false,       "Data length",      uShortField,        null ),
            Field(false,       "Data",             floatingBytesField, "Data length"),
            Field(true,        "Checksum",         uShortChecksum,     "Data"),
        ]
        );
    }
}

BinParseBlock genVarEntryParser() {
    with (BlobContentVariety) {
        return new BinParseBlock([
            // Always has a value of 11 or 13 (Bh or Dh).
            Field(false, "Constant",    fixedSizeBytes, null, null, 2), 
            Field(false, "Var length",  uShortField,    null, null),
            Field(true,  "Var id",      ubyteField,           null),
            Field(true,  "Name",        fixedStringFieled,    null, null, 8),
            Field(true,  "Version",     ubyteField,           null),
            Field(true,  "Flag",        ubyteField,           null),
            Field(false, "Var length2", uShortField,          null, null),
            Field(false, "Var length3", uShortField,          null, null),
            Field(false, "VarData",     floatingBytesField, "Var length3"),

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
