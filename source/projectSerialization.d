module projectSerialization;
import std.typecons : Nullable;
enum StepType{
    Comment,

    RenamePageFile,
    SplitFile,

    KnownLabelDefinition,
    KnownLabelUsage,

    KnownBcallDefinition,
    KnownBcallUsage,
}

struct SerStep{
    StepType type;
    ushort page;
    ushort addr;

    string data;
}

