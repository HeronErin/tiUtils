module parseUtils.flashFile;
import parseUtils.baseFile;
import common : isValidMember;

enum DeviceType : ubyte{
    TI73 = 0x74,
    TI83p = 0x73,
    TI89 = 0x98,
    TI92 = 0x88
}
enum DataType : ubyte{
    OS = 0x23,
    APPLICATION = 0x24,
    CERTIFICATE =  0x25,
    LICENSE = 0x3E
}

bool isValidDeviceType(ubyte value) {
    return isValidMember!(DeviceType, ubyte)(value);
}
bool isValidDataType(ubyte value) {
    return isValidMember!(DataType, ubyte)(value);
}



BinParseBlock genFlashFileParser(){
    with (BlobContentVariety){
        return new BinParseBlock(
            [
                // showToUsr     Field id            type of field      depends    byte data                     len (optional)
                Field(false,    "Magic Number",      requiredBytes,      null,      cast(ubyte[]) "**TIFL**"),
                Field(true,     "Major Version",     ubyteField),
                Field(true,     "Minor Version",     ubyteField),
                Field(true,     "Flags",             ubyteField),
                Field(true,     "Object type",       ubyteField),
                Field(true,     "Binary coded date", fixedSizeBytes,     null,      null,                           4),

                Field(false,    "Name length",       ubyteField,        null),
                Field(true,     "Name",              fixedStringFieled, null,       null,                           8),

                Field(false,    "Filler Data",       fixedSizeBytes,     null,      null,                          23),
                Field(true,     "Device Type",       ubyteField),
                Field(true,     "Data Type",         ubyteField),
                Field(true,     "Filler Data",       fixedSizeBytes,     null,      null,                          24),

                
                Field(false,    "HexData length",    uintField,        null),
                Field(false,    "Data",              greedyByteField,    null,      null,                           0),

//               Think the spec might be wrong?
//                 Field("Checksum",          uShortField,     "Data"),
            ]
        );
    }
}