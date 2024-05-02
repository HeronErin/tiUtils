module dissasembly.appDecompiler;
import dissasembly.z80;
import parseUtils.appHeader;

struct AppPage{
    AppHeaderField[] fields;
}
import std.stdio;
AppPage decompPage(ubyte[] page, bool isFirst = true){
    AppPage pageData;
    size_t index;
    if (isFirst){
        pageData.fields = headerGen(page, index);
    }
    foreach (AppHeaderField field; pageData.fields)
    {
        field.toAssembly.writeln;
    }


    return pageData;
}