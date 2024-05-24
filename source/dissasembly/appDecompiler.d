module dissasembly.appDecompiler;
import dissasembly.z80;
import parseUtils.appHeader;
import dissasembly.z80Decompiler;

struct AppPage {
    AppHeaderField[] fields;
    DecompilerUnit decomUnit;
}

string toAsm(AppPage page) {
    string assembly = "";
    foreach (field; page.fields) {
        assembly ~= field.toAssembly;
    }
    import dissasembly.z80Decompiler : toAsm;

    return assembly ~ "\n" ~ toAsm(page.decomUnit);
}

import std.stdio;

AppPage decompPage(ubyte[] page, bool isFirst = true) {
    AppPage pageData;
    size_t index;
    if (isFirst) {
        pageData.fields = headerGen(page, index);
    }

    pageData.decomUnit.org = cast(ushort)(0x4000 + index);

    while (index < page.length)
        pageData.decomUnit.lines ~= parseZ80Line(pageData.decomUnit, page, index);

    return pageData;
}
