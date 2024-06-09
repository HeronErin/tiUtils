module interactive.create;
import std.stdio;
import std.file;
import common;


int createInteractive(string pathToProj, string pathToBin) {
    if (!exists(pathToBin)){
        stderr.write("Error: File \"");
        stderr.write(pathToBin);
        stderr.writeln("\" not found!");
        return 1;
    }
    if (exists(pathToProj) && isFile(pathToProj)){
        stderr.write("Error: \"");
        stderr.write(pathToProj);
        stderr.writeln("\" is an existing file! It must be a non existent (or empty) folder!");
        return 1;
    }
    if (exists(pathToProj) && listdir(pathToProj).length){
        stderr.write("Error: \"");
        stderr.write(pathToProj);
        stderr.writeln("\" is folder that has contents! It must be a non existent (or empty) folder!");
        return 1;
    }
    import std.path;
    string ext = extension(pathToBin);
    if (!isValidExt(ext)){
        InvalidExt:
        stderr.write("Error: \"");
        stderr.write(ext);
        stderr.writeln("\" is not a supported extension!");
        return 1;
    }
    if (!exists(pathToProj))
        mkdir(pathToProj);
    string binPath = buildPath(pathToProj, "original" ~ ext);
    string srcPath = buildPath(pathToProj, "src");
    copy(pathToBin, binPath);
    mkdir(srcPath);

    if (ext == BinExt.BasicOrBinaryProgram){
        import dissasembly.programDissassembly : createDecompEnvForProgram;
        createDecompEnvForProgram(pathToProj);
        return 0;
    }else {
        goto InvalidExt;
    }
    
}
