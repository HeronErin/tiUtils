module parseUtils.flashFile;
import parseUtils.baseFile;


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
                Field("Object type",       ubyteField),
                Field("Binary coded date", fixedSizeBytes,     null,      null,                           4),

                Field("Name length",       uShortField,        null),
                Field("Name",              floatingStringField,"Name length"),

                Field("Filler Data",       fixedSizeBytes,     null,      null,                          23),
                Field("Device Type",       ubyteField),
                Field("Data Type",         ubyteField),
                Field("Filler Data",       fixedSizeBytes,     null,      null,                          24),

                
                Field("HexData length",    uShortField,        null),
                Field("Data",              greedyByteField,    null,      null,                           0),

//               Think the spec might be wrong?
//                 Field("Checksum",          uShortField,     "Data"),
            ]
        );
    }
}