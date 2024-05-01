module parseUtils.appHeader;
import std.stdio;

// http://z80-heaven.wikidot.com/fappheader
enum FieldType
{
    ProgramLength,
    ProgramType,
    RevisionNumber,
    MaximumRevision,
    AppBuildNumber,
    Name,
    NumberOfPages,
    DisableSplashScreen,
    DateStamp,
    Signature,
    ProgramImagelength,
    Padding,
}

void rs_get_field_size(const(ubyte[]) data, ref ulong fieldstart, ref ulong fieldsize)
{
    switch (data[1] & 0x0f)
    {
        case 0x0D:
            fieldstart = 3;
            fieldsize = data[2];
            break;

        case 0x0E:
            fieldstart = 4;
            fieldsize = (cast(ulong)data[2] << 8) | data[3];
            break;

        case 0x0F:
            fieldstart = 6;
            fieldsize = ((cast(ulong)data[2] << 24)
                        | (cast(ulong)data[3] << 16)
                        | (cast(ulong)data[4] << 8)
                        | cast(ulong)data[5]);
            break;

        default:
            fieldstart = 2;
            fieldsize = data[1] & 0x0f;
            break;
    }
}



import tern.typecons.common : Nullable, nullable;

struct AppHeaderField
{
    FieldType type;
    ubyte[] data;
    static Nullable!AppHeaderField parse(ubyte[] data, ref size_t index)
    {
        AppHeaderField field;

        ubyte firstIdByte = data[index];
        ubyte secoundIdByte = data[index + 1];
        if (firstIdByte == 0x80 && secoundIdByte == 0x0F){
            field.type = FieldType.ProgramLength;
            field.data = data[index+=2 .. index += 4];
            return nullable!AppHeaderField(field);
        }
        if (firstIdByte == 0x80 && (secoundIdByte & 0xF0) == 0x10){
            field.type = FieldType.ProgramType;
            field.data = data[index+=2 .. index += secoundIdByte & 0x0F];
            return nullable!AppHeaderField(field);
        }
        if (firstIdByte == 0x80 && (secoundIdByte & 0xF0) == 0x20){
            field.type = FieldType.RevisionNumber;
            field.data = data[index+=2 .. index += secoundIdByte & 0x0F];
            return nullable!AppHeaderField(field);
        }
        if (firstIdByte == 0x80 && (secoundIdByte & 0xF0) == 0x30){
            field.type = FieldType.AppBuildNumber;
            field.data = data[index+=2 .. index += secoundIdByte & 0x0F];
            return nullable!AppHeaderField(field);
        }
        if (firstIdByte == 0x80 && (secoundIdByte & 0xF0) == 0x80){
            field.type = FieldType.NumberOfPages;
            field.data = data[index+=2 .. index += secoundIdByte & 0x0F];
            return nullable!AppHeaderField(field);
        }
        if (firstIdByte == 0x80 && secoundIdByte == 0x90){
            field.type = FieldType.DisableSplashScreen;
            index+=2;
            return nullable!AppHeaderField(field);
        }
        if (firstIdByte == 0x80 && (secoundIdByte & 0xA0) == 0xA0){
            field.type = FieldType.MaximumRevision;
            field.data = data[index+=2 .. index += secoundIdByte & 0x0F];
            return nullable!AppHeaderField(field);
        }
        if (firstIdByte == 0x03 && (secoundIdByte & 0xF0) == 0x20){
            field.type = FieldType.DateStamp;
            field.data = data[index+=2 .. index += secoundIdByte & 0x0F];
            return nullable!AppHeaderField(field);
        }
        if (firstIdByte == 0x02){
            field.type = FieldType.Signature;
            
            size_t fieldStart;
            size_t fieldSize;
            rs_get_field_size(data[index..$], fieldStart, fieldSize);

            field.data = data[index+=fieldStart ..index+=fieldSize];
            return nullable!AppHeaderField(field);
        }
        if (firstIdByte == 0x80 && (secoundIdByte & 0xF0) == 0x40){
            field.type = FieldType.Name;
            field.data = data[index+=2 .. index += secoundIdByte & 0x0F];

            return nullable!AppHeaderField(field);
        }
        if (firstIdByte == 0x80 && (secoundIdByte & 0xF0) == 0x70){
            field.type = FieldType.ProgramImagelength;
            field.data = data[index+=2 .. index += secoundIdByte & 0x0F];

            return nullable!AppHeaderField(field);
        }
        if (firstIdByte == 0){
            field.type = FieldType.Padding;
            
            while (data[index] == 0)
                field.data ~= data[index++];
            return nullable!AppHeaderField(field);
            // return nullable!AppHeaderField(null);
        }
        data[index-5..index+10].writeln;
        firstIdByte.writeln;
        secoundIdByte.writeln;
        assert(0);
        return nullable!AppHeaderField(null);
    }
}

AppHeaderField[] headerGen(ubyte[] data, ref size_t index)
{
    AppHeaderField[] appHeaderFields;
    while (data.length > index)
    {
        auto maybeField = AppHeaderField.parse(data, index);
        if (maybeField == null) break;

        AppHeaderField field = maybeField;
        appHeaderFields ~= field;
        field.writeln;
        if (field.type == FieldType.Padding) break;
        // Acts as final field
        if (field.type == FieldType.ProgramImagelength && field.data.length == 0) break;
    }
    
    return appHeaderFields;
}
