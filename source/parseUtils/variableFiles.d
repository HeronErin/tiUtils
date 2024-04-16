module parseUtils.variableFiles;
import parseUtils.baseFile;


BinParseBlock genVarParser(){
    with (blobContentVariety){
        return new BinParseBlock(
            [
                //     Field id            type of field      depends    byte data                     len (optional)
                Field("Magic Number",      requiredBytes,     null,      cast(ubyte[]) "**TI83F*"),
                Field("Further signature", requiredBytes,     null,      [0x1A, 0x0A, 0x00]),
                Field("Comment",           fixedSizeBytes,    null,      null,                          42),
                Field("Data length",       uShortField,       null),
                Field("Data",              floatingBytesField,"Data length"),
                Field("Checksum",          uShortChecksum,    "Data"),
            ]
        );
    }
}
BinParseBlock genVarEntrieParser(){
    with (blobContentVariety){
        return new BinParseBlock([
            Field("Constant",   fixedSizeBytes, null, null, 2),
            Field("Var length", uShortField, null, null),
            Field("Var id",     ubyteField, null),
            Field("Name",       fixedStringFieled, null, null, 8),
            Field("Version",       ubyteField, null),
            Field("Flag",       ubyteField, null),
            Field("Var length2", uShortField, null, null),
            Field("Data",              floatingBytesField, "Var length"),

        ]);
    }
}