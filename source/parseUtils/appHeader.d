module parseUtils.appHeader;
import std.stdio;



void getFieldSize(const(ubyte[]) data, size_t index, ref ulong fieldstart, ref ulong fieldsize)
{
    switch (data[index+1] & 0x0f)
    {
        case 0x0D:
            fieldstart = 3;
            fieldsize = data[index+2];
            break;

        case 0x0E:
            fieldstart = 4;
            fieldsize = (cast(ulong)data[index+2] << 8) | data[index+3];
            break;

        case 0x0F:
            fieldstart = 6;
            fieldsize = ((cast(ulong)data[index+2] << 24)
                        | (cast(ulong)data[index+3] << 16)
                        | (cast(ulong)data[index+4] << 8)
                        | cast(ulong)data[index+5]);
            break;

        default:
            fieldstart = 2;
            fieldsize = data[index+1] & 0x0f;
            break;
    }
}



import tern.typecons.common : Nullable, nullable;

// http://z80-heaven.wikidot.com/fappheader
// https://wikiti.brandonw.net/index.php?title=Category:83Plus:OS:Certificate/Headers:Fields_By_Number
enum FieldType
{
    UNKNOWN,
    Signature,
    DateStamp,
    Padding,
    ProgramLength,

    DeveloperKey,
    RevisionNumber,
    BuildNumber,
    Name,
    ExpirationDate,
    OveruseCount,
    LastField,
    NumberOfPages,
    DisableSplashScreen,
    MaxHardwareRevision,
    LowerstBasecode
}
FieldType[ubyte] idsStartingWith80 = [
    0x10 : FieldType.DeveloperKey,
    0x20 : FieldType.RevisionNumber,
    0x30 : FieldType.BuildNumber,
    0x40 : FieldType.Name,
    0x50 : FieldType.ExpirationDate,
    0x60 : FieldType.OveruseCount,
    0x70 : FieldType.LastField,
    0x80 : FieldType.NumberOfPages,
    0x90 : FieldType.DisableSplashScreen,
    0xA0 : FieldType.MaxHardwareRevision,
    0xC0 : FieldType.LowerstBasecode,

];

struct AppHeaderField
{
    FieldType type;
    ubyte[] info;
    ubyte[] data;
    void classify()
    {
        ubyte firstIdByte = info[0];
        ubyte secoundIdByte = info[1];
        if (firstIdByte == 0x80 && secoundIdByte == 0x0F)
            type = FieldType.ProgramLength;
        else if (firstIdByte == 0x02)
            type = FieldType.Signature;
        else if (firstIdByte == 0x03)
            type = FieldType.DateStamp;
        else if (firstIdByte == 0)
            type = FieldType.Padding;
        else if (firstIdByte == 0x80){
            if ((secoundIdByte & 0xF0) in idsStartingWith80)
                type = idsStartingWith80[secoundIdByte & 0xF0];
            else
                secoundIdByte.writeln;
        }
    }
}
static AppHeaderField[] headerGen(ubyte[] data, ref size_t index)
{
    AppHeaderField[] fields;
    size_t fieldStart, fieldSize;
    while (true)
    {
        AppHeaderField field;

        getFieldSize(data, index, fieldStart, fieldSize);

        if (data[index] == 0x80 && data[index+1] == 0x0F){
            field.info = data[index .. index += 6];
            field.classify;
            fields ~= field;
            continue;
        }else{
            bool isFinalToken = (data[index + 1] & 0xF0) == 0x70;
            field.info = data[index .. index+=fieldStart];
            field.data = data[index .. index+=fieldSize];
            field.classify;
            fields ~= field;

            if (isFinalToken){
                size_t old_index = index;
                while (0 == data[index++]){}
                AppHeaderField padding;
                padding.info = data[old_index..index-1];
                padding.classify;
                fields ~= padding;
                return fields;
            }
        }
        // break;
    }
}
