module dissasembly.programDissassembly;
import parseUtils.variableFiles;
import std.stdio;
import std.path;
import common;
/+
    =========================================
    ||                                     ||
    ||     DISASSEMBLING A RAM PROGRAM     ||
    ||                                     ||
    =========================================


    Simplist of them all. They all start at $9D95 (ORG)
    Most start with $BB,$6D, and are single page!
+/


import dissasembly.smartFlowAnalisis;
void createDecompEnvForProgram(string projectDir){
    // At the point this is being called:
    
    // projectDir contains:
    // .  ..  original.8xp  src

    // So it is time to disassemble the program, this function creates:
    // src/main.asm
    // .comment
    // Makefile
    string pathToBin = buildPath(projectDir, "original.8xp");
    
    auto binFile = genVarParser();
    binFile.fromFile(pathToBin);
    ubyte[42] comment = binFile.findById("Comment").data;
    quickDump(comment, buildPath(projectDir, ".comment"));


    ubyte[] programData = binFile.findById("VarData").data;

    auto unit = createDecomeUnitForProgram(programData);
    unit.toAsm().writeln;
}

DecompilerUnit createDecomeUnitForProgram(ubyte[] data){
    bool programHasMagicNumber = data[0..2] == [0xBB, 0x6D];

    AnalysisState state = analyse(
        0x9D95,    // The org

        16 * 1024, // Technically should be 8kb, but it apears after 8kb
                   // ram simply is nolonger executable, so people might
                   // use it as data

        [programHasMagicNumber ? 0x9D97 : 0x9D95], // Skip magic bytes for analyse
        data
    );
    return state.genCompleteUnit();
}

