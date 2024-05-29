module parseUtils.flashHeader;
import std.stdio;
import tern.object;

void getFieldSize(const(ubyte[]) data, size_t index, ref ulong fieldstart, ref ulong fieldsize) {
    switch (data[index + 1] & 0x0f) {
        case 0x0D:
            fieldstart = 3;
            fieldsize = data[index + 2];
            break;

        case 0x0E:
            fieldstart = 4;
            fieldsize = (cast(ulong) data[index + 2] << 8) | data[index + 3];
            break;

        case 0x0F:
            fieldstart = 6;
            fieldsize = ((cast(ulong) data[index + 2] << 24)
                    | (cast(
                        ulong) data[index + 3] << 16)
                    | (cast(
                        ulong) data[index + 4] << 8)
                    | cast(ulong) data[index + 5]);
            break;

        default:
            fieldstart = 2;
            fieldsize = data[index + 1] & 0x0f;
            break;
    }
}

import tern.typecons.common : Nullable, nullable;

// http://z80-heaven.wikidot.com/fappheader
// https://wikiti.brandonw.net/index.php?title=Category:83Plus:OS:Certificate/Headers:Fields_By_Number
enum FieldType {
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
    LowerstBasecode,

    CalculatorIdRequired,
    ValidationNumber,

    AboutScreenData,
    StandardKeyHeader,
    StandardKeyData,
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
import conversion;
import std.conv;
import std.datetime;
const auto TI_EPOCH = SysTime(DateTime(1997, 1, 1), UTC());


struct FlashHeaderField {
    FieldType type;
    ubyte[] info;
    ubyte[] data;
    void classify() {
        ubyte firstIdByte = info[0];
        ubyte secoundIdByte = info[1];
        ubyte secoundByteHighNibble = secoundIdByte & 0xF0;
        if (firstIdByte == 0x80 && secoundIdByte == 0x0F)
            type = FieldType.ProgramLength;
        else if (firstIdByte == 0x02)
            type = FieldType.Signature;
        else if (firstIdByte == 0x03)
            type = FieldType.DateStamp;
        else if (firstIdByte == 0)
            type = FieldType.Padding;
        else if (firstIdByte == 0x04 && secoundByteHighNibble == 0x10)
            type = FieldType.ValidationNumber;
        else if (firstIdByte == 0x04 && secoundByteHighNibble == 0)
            type = FieldType.CalculatorIdRequired;
        else if (firstIdByte == 0x05 && secoundByteHighNibble == 0x10)
            type = FieldType.AboutScreenData;
        else if (firstIdByte == 0x07 && secoundByteHighNibble == 0x10)
            type = FieldType.StandardKeyHeader;
        else if (firstIdByte == 0x07 && secoundByteHighNibble == 0x30)
            type = FieldType.StandardKeyData;
        else if (firstIdByte == 0x80) {
            if (secoundByteHighNibble in idsStartingWith80)
                type = idsStartingWith80[secoundByteHighNibble];
            else
                assert(0, "Unknown 0x80 style header: " ~ secoundByteHighNibble.to!string);
        }
    }

    string toAssembly() {
        string s;
        alias fieldTypeGen() = {
            s = bytesToDefb(info) ~ " ; Field: " ~ type.to!string ~ "\n";
        };
        import std.ascii : isASCII;

        switch (type) {
            case FieldType.Name:
                fieldTypeGen();
                ubyte[] stringData;
                ubyte[] byteData;
                foreach (i, ubyte b; data) {
                    if (!isASCII(b)) {
                        byteData = data[i .. $];
                        break;
                    }
                    else
                        stringData ~= escapeString("" ~ b);

                }
                if (stringData.length)
                    s ~= "DEFM \"" ~ (cast(string) stringData) ~ "\"\n";
                if (byteData.length)
                    s ~= bytesToDefb(byteData) ~ "\n";
                break;
            default:
                fieldTypeGen();
                if (data.length) {
                    s ~= bytesToDefb(data) ~ "\n";
                }
        }

        return s;
    }
    ulong toNumber(){
        import std.algorithm : min;
        ulong ret = 0;
        foreach (i; 0..min(8, data.length)) {
            ret |= data[i] << (8 * i);
        }
        return makeEndian(ret, Endianness.LittleEndian);
    }
    Nullable!SysTime parseDate(){
        if (data.length != 4 && data.length != 6)
            return nullable!SysTime(null);
        ubyte[] rdata = data;
        
        if (data.length == 6)
            rdata = rdata[2..$];

        uint secoundsPastEpoch = makeEndian(*cast(uint*)rdata.ptr, Endianness.BigEndian);
        
        Duration duration = seconds(secoundsPastEpoch);
        return nullable!SysTime(TI_EPOCH + duration);
    }
}

static FlashHeaderField[] headerGen(ubyte[] data, ref size_t index) {
    FlashHeaderField[] fields;
    size_t fieldStart, fieldSize;
    while (1) {
        FlashHeaderField field;

        getFieldSize(data, index, fieldStart, fieldSize);

        if (data[index] == 0x80 && data[index + 1] == 0x0F) {
            field.info = data[index .. index += 6];
            field.classify;
            fields ~= field;
            continue;
        }
        else {
            bool isFinalToken = (data[index + 1] & 0xF0) == 0x70;
            field.info = data[index .. index += fieldStart];
            field.data = data[index .. index += fieldSize];
            field.classify;
            fields ~= field;

            if (!isFinalToken)
                continue;
            if (data.length <= index)
                return fields;

            size_t old_index = index;
            while (0 == data[index++]) {}
            if (old_index == index - 1) {
                index--;
                return fields;
            }
            FlashHeaderField padding;
            padding.info = data[old_index .. index - 1];
            padding.classify;
            fields ~= padding;
            return fields;

        }
        // break;
    }
}
