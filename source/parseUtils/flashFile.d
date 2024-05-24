module parseUtils.flashFile;
import parseUtils.baseFile;
import interactive.project : isValidMember;

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
    with (blobContentVariety){
        return new BinParseBlock(
            [
                //     Field id            type of field      depends    byte data                     len (optional)
                Field("Magic Number",      requiredBytes,      null,      cast(ubyte[]) "**TIFL**"),
                Field("Major Version",     ubyteField),
                Field("Minor Version",     ubyteField),
                Field("Flags",             ubyteField),
                Field("Object type",       ubyteField),
                Field("Binary coded date", fixedSizeBytes,     null,      null,                           4),

                Field("Name length",       ubyteField,        null),
                Field("Name",              fixedStringFieled, null,       null,                           8),

                Field("Filler Data",       fixedSizeBytes,     null,      null,                          23),
                Field("Device Type",       ubyteField),
                Field("Data Type",         ubyteField),
                Field("Filler Data",       fixedSizeBytes,     null,      null,                          24),

                
                Field("HexData length",    uintField,        null),
                Field("Data",              greedyByteField,    null,      null,                           0),

//               Think the spec might be wrong?
//                 Field("Checksum",          uShortField,     "Data"),
            ]
        );
    }
}