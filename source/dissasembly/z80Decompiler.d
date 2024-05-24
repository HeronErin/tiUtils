module dissasembly.z80Decompiler;
import std.conv;
import std.format;
import dissasembly.z80;
import parseUtils.appHeader;

class Label {
    ushort addr;
    bool isRel = false;
    byte relOffset = 0;
    DecompLine[] refs = [];
    string genName() {
        string r = "_";
        if (isRel) {
            string roff = relOffset.to!string;
            r ~= "rel_";
            if (roff[0] == '-')
                roff = "negative_" ~ roff[1 .. $];
            r ~= roff ~ "_";
        }
        else
            r ~= "abs_";
        r ~= format("%04X", addr) ~ "h";
        return r;
    }
}

enum LineVarity {
    AsmInstruction,
    AppHeaderField,
    AssemblersNote, // Comment used to alert user of something
    Data,
    Label,
    Bcall
}

struct DecompLine {
    LineVarity lineVarity;
    size_t location;
    Label usesLabel = null;
    ubyte[] data;
    union {
        Instruction asmInstruction;
        AppHeaderField header;
        ushort bcall;
        bool isAsciiData;
        Label label;
    }

    size_t getSize() {
        if (lineVarity == LineVarity.AsmInstruction)
            return asmInstruction.byteSize;
        if (lineVarity == LineVarity.AppHeaderField)
            return header.info.length + header.data.length;
        if (lineVarity == LineVarity.Data)
            return data.length;
        if (lineVarity == LineVarity.Label || lineVarity == LineVarity.AssemblersNote)
            return 0;
        if (lineVarity == LineVarity.Bcall)
            return 3;
        assert(0);
    }
}

struct DecompilerUnit {
    ushort org;
    ushort data_size_limit = 0x4000; // If org <= imm16 < org + data_size_limit: it is a label
    DecompLine[] lines;
    DecompLine[] labels;

    void attachLabel(ushort location, byte imm8, ref DecompLine line) {
        ushort labelLocation = cast(ushort)(
            cast(ptrdiff_t) location + cast(ptrdiff_t) imm8
        );
        Label newLabel = new Label();
        newLabel.isRel = true;
        newLabel.addr = labelLocation;
        newLabel.relOffset = imm8;
        DecompLine newLine;
        newLine.label = newLabel;
        newLine.lineVarity = LineVarity.Label;
        line.usesLabel = newLabel;
        labels ~= newLine;
    }

    void attachLabel(ushort imm16, ref DecompLine line) {
        foreach (DecompLine testLabel; labels) {
            if (testLabel.label.addr == imm16) {
                line.usesLabel = testLabel.label;
                return;
            }
        }
        Label newLabel = new Label();
        newLabel.addr = imm16;
        DecompLine newLine;
        newLine.label = newLabel;
        newLine.lineVarity = LineVarity.Label;
        line.usesLabel = newLabel;
        labels ~= newLine;
    }
}

import conversion;

string toAsm(DecompilerUnit unit) {
    ushort location = unit.org;
    string assembly = "";
    foreach (DecompLine line; unit.lines) {
        scope (exit)
            location += line.getSize;
        if (line.lineVarity != LineVarity.AssemblersNote)
            foreach (DecompLine label; unit.labels) {
                if (label.label.addr == location)
                    assembly ~= label.label.genName ~ ":\n";
            }
        switch (line.lineVarity) {
            case LineVarity.AsmInstruction:
                auto s = line.asmInstruction.toAssembly(line.usesLabel);
                assert(s != null, "Null error");
                assembly ~= "\t" ~ s.value ~ "\n";
                break;
            case LineVarity.Bcall:
                assembly ~= "\tbcall(" ~ format("%04X", line.bcall) ~ "h)\n";
                break;
            case LineVarity.Data:
                if (line.isAsciiData) {
                    assembly ~= "\tDEFM \"" ~ escapeString(cast(string) line.data) ~ "\"\n";
                    assembly ~= "\tDEFB 00h\n";
                }
                else {
                    assembly ~= "\t" ~ bytesToDefb(line.data) ~ "\n";
                }
                break;
            case LineVarity.AssemblersNote:
                assembly ~= "; " ~ (cast(string) line.data) ~ "\n";
                break;
            default:
                assert(0, line.lineVarity.to!string);
        }
    }
    return assembly;
}

import std.ascii : isASCII;

size_t possibleAsciiTest(const(ubyte[]) ub, size_t index) {
    size_t correctCount;
    while (index < ub.length) {
        ubyte possible = ub[index++];
        if (isASCII(possible) || possible == 0) {
            correctCount++;
            if (possible == 0)
                return correctCount;
        }
        else
            return 0;
    }
    return 0;
}

DecompLine parseZ80Line(ref DecompilerUnit unit, const(ubyte[]) data, ref size_t index) {
    DecompLine line;
    line.location = index;
    size_t oldIndex = index;

    auto instruction = getInstruction_nullable(data, index);
    line.asmInstruction = instruction.value;

    // Detect Bcalls
    if (line.asmInstruction.type == InstructionType.Rst && line.asmInstruction
        .operands[0].rst == 0x28) {
        ushort bcallAddr = data[index++] | (data[index++] << 8);
        line.lineVarity = LineVarity.Bcall;
        line.bcall = bcallAddr;
        return line;
    }
    bool isJr = line.asmInstruction.type == InstructionType.Jr;
    foreach (operand; line.asmInstruction.operands) {
        if (operand.variety == OperandVariety.Imm16 && operand.isLabel) {
            unit.attachLabel(operand.imm16, line);
        }
        if (isJr && operand.variety == OperandVariety.Imm8) {
            unit.attachLabel(cast(ushort)(index + unit.org), operand.imm8, line);
        }
        else if (operand.variety == OperandVariety.Imm16 && operand.imm16 >= unit.org && operand.imm16 < unit.org + unit
            .data_size_limit) {
            unit.attachLabel(operand.imm16, line);
        }
    }
    line.data = new ubyte[index - oldIndex];
    line.data[0 .. $] = cast(ubyte[]) data[oldIndex .. index];

    return line;
}

import std.stdio;

unittest {
    ubyte[] bts = cast(ubyte[]) hexString!"EF4045210000224B8421A69DEF0A45EF2E45C948656C6C6F20776F726C642100";
    size_t i;
    DecompilerUnit u;
    u.org = 0x9D93;

    while (i < bts.length)
        u.lines ~= parseZ80Line(u, bts, i);

    u.toAsm.writeln;

}
