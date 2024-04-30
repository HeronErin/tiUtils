module parseUtils.baseFile;
import tern.object;


enum blobContentVariety{
    ubyteField,
    uShortField,
    uShortChecksum,
    
    uintField,
    fixedStringFieled,
    floatingStringField,
    fixedSizeBytes,
    floatingBytesField,
    greedyByteField,    // Size here is instead amount to leave LEFT in the buffer AFTER consuming everything

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
                case uShortChecksum:
                    data = input[0..2];
                    ushort sum = this.as!ushort;
                    Field connectedData = obj.findByIdInternal(dependsOn);
                    uint currSum = 0;
                    foreach (ubyte b ; connectedData.data){
                        currSum = (currSum+b) & 0xFFFF;
                    }
                    assert(cast(ushort)currSum == sum);

                    return 2;
                case uintField:
                    data = input[0..4];
                    return 4;
                case fixedStringFieled:
                    assert(size != -1);
                    data = input[0..size];
                    return size;
                case floatingStringField:
                    Field sizeof = obj.findByIdInternal(dependsOn);
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
                    assert(data == byteInput);
                    return size;
                case fixedSizeBytes:
                    assert(size != -1);
                    data = input[0..size];
                    assert(data.length == size);
                    return size;
                case floatingBytesField:
                    Field sizeof = obj.findByIdInternal(dependsOn);
                    size = sizeof.as!ushort;
                    
                    data = input[0..size];
                    assert(data.length == size);
                    return size;
                case greedyByteField:
                    data = input[0..$-size];
                    return cast(int) data.length;

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
                    assert(variety == uShortField || variety == uShortChecksum);
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
    BinParseBlock[] following = new BinParseBlock[0];
    string[] followingDependants = new string[0];
    this(Field[] fields_){
        fields=fields_;
    }
    void fromBytes(ubyte[] bytes){
        int index = 0;
        foreach(ref Field field ; fields){
            index += field.parse(this, bytes[index..$]);
        }
        bytes[index..$].writeln;
        foreach (size_t i; 0..following.length){
            Field f = findByIdInternal(followingDependants[i]);
            following[i].fromBytes(f.data);
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
    Field findByIdInternal(string id){
        foreach(Field field ; fields){
            if (field.id == id)
                return field;
        }
        assert(false);
    }
    Field findById(string id){
        foreach(Field field ; fields){
            if (field.id == id)
                return field;
        }
        foreach(BinParseBlock block ; following){
            foreach(Field field ; block.fields){
                if (field.id == id)
                    return field;
            }
        }
        assert(false, "id not found: " ~ id);
    }
    void combine(string dependsOn, BinParseBlock other){
        following ~= other;
        followingDependants ~= dependsOn;
    }

}
