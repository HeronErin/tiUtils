module parseUtils.intellHex;
import parseUtils.baseFile;
import std.conv;
import std.exception;

// mapping of ASCII characters to hex values
// (see: https://stackoverflow.com/questions/3408706/hexadecimal-string-to-byte-array-in-c)
const ubyte[] hashmap = [
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, // 01234567
    0x08, 0x09, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // 89:;<=>?
    0x00, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x00, // @ABCDEFG
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // HIJKLMNO
];

T popNum(T)(ubyte[] hex, ref size_t index)
{
    T ret;
    ubyte upper, lower;
    static foreach (currentByte; 0 .. T.sizeof)
    {
        upper = hashmap[(hex[index++] & 0x1F) ^ 0x10] << 4 & 0xF0;
        lower = hashmap[(hex[index++] & 0x1F) ^ 0x10];
        ret |= cast(T)(upper | lower) << ((T.sizeof - currentByte - 1) * 8);
    }
    return ret;
}

ubyte[] bulkHexDecode(ubyte[] hex, size_t size, ref size_t index, ref ubyte checksum)
{
    size_t endIndex = index + size * 2;
    size_t dataIndex = 0;
    ubyte upper, lower;
    ubyte[] data = new ubyte[size];
    while (index < endIndex)
    {
        upper = hashmap[(hex[index++] & 0x1F) ^ 0x10] << 4 & 0xF0;
        lower = hashmap[(hex[index++] & 0x1F) ^ 0x10];
        checksum += cast(ubyte) upper | lower;
        data[dataIndex++] = upper | lower;
    }
    return data;
}

class IntellHexError : Exception
{
    mixin basicExceptionCtors;
}

enum RecordType : ubyte{
    Data = 0,
    EndOfFile,
    ExtendedSegmentAddress,
    StartSegmentAddress,
    ExtendedLinearAddress,
    StartLinearAddress
}

import std.format.spec : singleSpec;
import std.array : appender;
auto hex = singleSpec("%x");

private struct Hexline
{
    ubyte count;
    ushort addr;
    RecordType recordType;
    ubyte[] data;
    ubyte sum;

    void toString(scope void delegate(const(char)[]) sink) const {
        import std.format;
        auto str = appender!string();
        auto hex = singleSpec("%x");
        
        str ~= "Hexline{0x";
        formatValue(str, addr, hex);
        str ~= " for " ~ count.to!string ~ " bytes of type " ~ recordType.to!string ~ "}";


        sink(str.data);

    }
}

import std.stdio;
import std.algorithm.searching;

// https://en.wikipedia.org/wiki/Intel_HEX
Hexline[] getIntellHexLines(ubyte[] intellhex)
{
    size_t currentLine = 1;
    size_t index;
    Hexline[] lines;
    while (true)
    {
        scope (exit)
            currentLine += 1;
        
        Hexline line;

        // Seek to colon symbol (everthing else is ignored)
        size_t diff = intellhex[index .. $].countUntil(':');
        if (diff == -1)
            break;
        index += diff + 1;

        line.count = intellhex.popNum!ubyte(index);
        line.addr = intellhex.popNum!ushort(index);
        line.recordType = cast(RecordType)intellhex.popNum!ubyte(index);

        ubyte sumToCheck = (line.count + (line.addr >> 8) + (line.addr & 0xFF) + line.recordType) & 0xFF;

        line.data = bulkHexDecode(intellhex, line.count, index, sumToCheck);
        line.sum = intellhex.popNum!ubyte(index);
        sumToCheck = cast(ubyte)(-sumToCheck);

        if (sumToCheck != line.sum)
            throw new IntellHexError("Invalid checksum on line " ~ currentLine.to!string);
        lines ~= line;
    }
    return lines;
}
