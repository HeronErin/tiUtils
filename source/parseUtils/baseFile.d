module parseUtils.baseFile;
import tern.object;


enum blobContentVariety{
    ubyteField,
    uShortField,
    uintField,
    fixedStringFieled,
    floatingStringField,
    fixedSizeBytes,
    floatingBytesField,

    requiredBytes
}

struct Field{
    string id;
    blobContentVariety variety; 
    string dependsOn = null;
    ubyte[] byteInput = null;
    int size = -1;
    
    ubyte[] data = null;
    int parse(BinParseBlock obj, ubyte[] input){
        with (blobContentVariety){
            switch (variety){
                case ubyteField:
                    data = input[0..1];
                    return 1;
                case uShortField:
                    data = input[0..2];
                    return 2;
                case uintField:
                    data = input[0..4];
                    return 4;
                case fixedStringFieled:
                    assert(size != -1);
                    data = input[0..size];
                    return size;
                case floatingStringField:
                    Field sizeof = obj.findById(dependsOn);
                    size = sizeof.as!ushort;

                    data = input[0..size];
                    return size;
                case requiredBytes:
                    if (size == -1){
                        assert(byteInput != null);
                        size = cast(int) byteInput.length;
                    }
                    data = input[0..size];
                    assert(data.length == size);
                    data.writeln;
                    byteInput.writeln;
                    assert(data == byteInput);
                    return size;
                case fixedSizeBytes:
                    assert(size != -1);
                    data = input[0..size];
                    assert(data.length == size);
                    return size;
                case floatingBytesField:
                    Field sizeof = obj.findById(dependsOn);
                    sizeof.writeln;
                    size = sizeof.as!ushort;
                    
                    data = input[0..size];
                    assert(data.length == size);
                    return size;

                default:
                    variety.writeln;
                    assert(false);
            }
            return -1;
        }
    }

    T as(T)(){
        with (blobContentVariety){
            static if(is(T == ubyte)){
                assert(variety == ubyteField);
                return data[0];
            }
            static if(is(T == ushort) || is(T == uint)){
                static if(is(T == uint)){
                    assert(variety == uintField);
                }
                static if(is(T == ushort)){
                    assert(variety == uShortField);
                }
                // data.ptr.writeln;
                return makeEndian(*cast(T*)data.ptr, Endianness.LittleEndian);
            }
        }
    }
}

import std.stdio;

class BinParseBlock{
    Field[] fields;
    this(Field[] fields_){
        fields=fields_;
    }
    void fromBytes(ubyte[] bytes){
        int index = 0;
        foreach(ref Field field ; fields){
            index += field.parse(this, bytes[index..$]);
        }
    }
    void fromFile(string path){
        import std.stdio;
        File file = File(path, "r");
        ubyte[] bytes = new ubyte[file.size];
        ubyte[] slice = file.rawRead(bytes);
        assert(slice.length == file.size);

        fromBytes(slice);
    }
    Field findById(string id){
        foreach(Field field ; fields){
            if (field.id == id)
                return field;
        }
        assert(false);
    }

}



BinParseBlock gen8xvParser(){
    with (blobContentVariety){
        return new BinParseBlock(
            [
                Field("Magic Number",      requiredBytes, null, cast(ubyte[]) "**TI83F*"),
                Field("Further signature", requiredBytes, null, [0x1A, 0x0A, 0x00]),
                Field("Comment",           fixedSizeBytes,null, null, 42),
                Field("Data length",       uShortField,   null, null, 2),
                Field("Data",             floatingBytesField, "Data length", null, 2),
                Field("Checksum",          uShortField,   null, null, 2),
            ]
        );
    }
}
