module dissasembly.smartFlowAnalisis;
public import dissasembly.z80Decompiler;

struct RangeInfo {
    ushort start;
    ushort end;
    DecompLine[] asmLines;
}

import tern.typecons.common;

struct AnalysisState {
    DecompilerUnit unit;
    RangeInfo[] knownCode;
    ubyte[] data;
    ushort org;
    ushort maxSize;

    Nullable!RangeInfo isKnownCode(ushort dataPtr) {
        foreach (RangeInfo codeBlock; knownCode) {
            if (codeBlock.start <= dataPtr && codeBlock.end >= dataPtr)
                return nullable(codeBlock);
        }
        return nullable!RangeInfo(null);
    }

    DecompilerUnit genCompleteUnit() {
        DecompilerUnit complete = unit;
        complete.lines.length = 0;
        size_t index;

        bool isInData = false;
        size_t dataSize;

        while (index < maxSize && index < data.length) {
            ushort pc = cast(ushort)(index + org);
            Nullable!RangeInfo rangeInfo = isKnownCode(pc);
            // Handle data
            if (rangeInfo == null) {
                isInData = true;
                dataSize++;
                index++;
                continue;
            }
            else if (isInData) {
                DecompLine dataLine;
                dataLine.lineVarity = LineVarity.Data;
                dataLine.data = data[index - dataSize .. index];
                dataLine.location = pc;
                complete.lines ~= dataLine;
                dataSize = 0;
            }
            isInData = false;
            RangeInfo range = rangeInfo;
            complete.lines ~= range.asmLines;
            index = range.end - org + 1;
        }
        if (isInData) {
            DecompLine dataLine;
            dataLine.lineVarity = LineVarity.Data;
            dataLine.data = data[index - dataSize .. index];
            dataLine.location = index + org;
            complete.lines ~= dataLine;
        }
        return complete;
    }

    bool inAbsRange(ushort ptr) => ptr >= org && ptr < maxSize + org;
}

import std.stdio;
import dissasembly.z80;

private void handleCodeSpot(ref AnalysisState state, ushort pc) {
    Nullable!RangeInfo knowCode = state.isKnownCode(pc);
    assert(pc >= state.org);
    assert(pc < state.org + state.maxSize);

    if (knowCode != null)
        return;
    size_t index = pc - state.org;
    RangeInfo currentRange;
    currentRange.start = pc;

    alias endRange = {
        currentRange.end = cast(ushort)(index + state.org - 1);
        state.knownCode ~= currentRange;
    };
    ushort[] hitBatches;

    scope (exit) {
        foreach (ushort hitMe; hitBatches) {
            handleCodeSpot(state, hitMe);
        }
    }

    while (index < state.data.length) {
        DecompLine line = parseZ80Line(state.unit, state.data, index);
        // line.writeln;
        currentRange.asmLines ~= line;
        if (line.lineVarity == LineVarity.Bcall)
            continue;
        if (line.lineVarity == LineVarity.AsmInstruction) {
            auto instruction = line.asmInstruction;

            if (instruction.type == InstructionType.Ret && 0 == instruction.operands.length) {
                endRange();
                return;
            }
            if (instruction.type == InstructionType.Call) {
                ushort ptr = (instruction.operands.length == 1 ? instruction
                        .operands[0] : instruction.operands[1])
                    .assertAs(OperandVariety.Imm16).imm16;
                if (!state.inAbsRange(ptr))
                    continue;
                hitBatches ~= ptr;
            }
            if (instruction.type == InstructionType.Jp) {
                Nullable!Operand jpTo = instruction.findOfOperandVariety(OperandVariety.Imm16);
                if (jpTo == null)
                    return endRange();

                ushort ptr = jpTo.value.imm16;

                if (!state.inAbsRange(ptr))
                    return endRange();
                hitBatches ~= ptr;
                return endRange();

            }
        }
    }

    endRange();
}

AnalysisState analyse(ushort org, ushort maxSize, ushort[] knownEntryPoints, ubyte[] data) {
    AnalysisState ret;
    ret.unit = DecompilerUnit(org, maxSize, [], []);
    ret.data = data;
    ret.org = org;
    ret.maxSize = maxSize;

    foreach (ushort entryPoint; knownEntryPoints) {
        handleCodeSpot(ret, entryPoint);
    }

    return ret;
}
