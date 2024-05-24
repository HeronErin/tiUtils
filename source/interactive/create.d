module interactive.create;
import std.stdio;
import std.file;
import interactive.project;


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
        stderr.write("Error: \"");
        stderr.write(ext);
        stderr.writeln("\" is not a supported extension!");
        return 1;
    }
    Project p = new Project(pathToProj, pathToBin);
    return 0;
}
