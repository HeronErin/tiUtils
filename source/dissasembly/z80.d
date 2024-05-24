module dissasembly.z80;
import std.format;
import std.string : strip;
import std.stdio;
import tern.typecons.common : Nullable, nullable;

enum Register : ubyte {
    UNKNOWN,
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    F,
    AF,
    BC,
    DE,
    HL,

    SP,
    IX,
    IY,
    R,
    I,
    SHADOW_AF,

    IXH,
    IXL,
    IYH,
    IYL
}

string toAsmString(Register reg) {
    import std.conv : to;
    import std.string : toLower;

    assert(reg != Register.UNKNOWN);
    if (reg == Register.SHADOW_AF)
        return "af'";
    return reg.to!string.toLower;
}

enum ConditionVariety : ubyte {
    NZ,
    Z,
    NC,
    C,
    PO,
    PE,
    P,
    M
}

enum OperandVariety : ubyte {
    Reg8,
    Reg8Lookup,
    Reg16,
    Imm8,
    Imm8Lookup,
    Imm16,

    Reg16Lookup,
    Imm16Lookup,
    Condition,
    Rst,
    PreSetImm8,

    IxOffset,
    IyOffset
}

struct Operand {
    OperandVariety variety;
    bool isLabel;

    Register register = Register.UNKNOWN;
    ConditionVariety condition;
    ubyte rst;
    ubyte imm8;
    ushort imm16;

    string toString() {
        import std.conv;

        if (variety == OperandVariety.Reg8 || variety == OperandVariety.Reg16)
            return register.to!string;
        if (variety == OperandVariety.Rst)
            return rst.to!string;
        if (variety == OperandVariety.Imm8)
            return "Imm8";
        if (variety == OperandVariety.Imm16)
            return "Imm16";
        if (variety == OperandVariety.Imm16Lookup)
            return "(Imm16)";
        if (variety == OperandVariety.Reg16Lookup)
            return "(" ~ register.to!string ~ ")";
        if (variety == OperandVariety.Condition)
            return condition.to!string;
        assert(0);
    }

    Operand assertAs(OperandVariety testType) {
        assert(variety == testType);
        return this;
    }
}

Operand OR8(Register register) {
    Operand operand;
    operand.variety = OperandVariety.Reg8;
    operand.register = register;
    return operand;
}
// Used for when specified masks in an opcode specify the register
const Operand[] Reg8_rrr = [
    OR8(Register.B), OR8(Register.C), OR8(Register.D), OR8(Register.E),
    OR8(Register.H), OR8(Register.L), OR8(Register.UNKNOWN), OR8(Register.A)
];
const Operand[] Reg8_j = [
    OR8(Register.IXH), OR8(Register.IXL), OR8(Register.IYH), OR8(Register.IYL)
];

Operand OR16(Register register) {
    Operand operand;
    operand.variety = OperandVariety.Reg16;
    operand.register = register;
    return operand;
}
// Maybe Hl??
const Operand[] Reg16_qq = [
    OR16(Register.BC), OR16(Register.DE), OR16(Register.HL), OR16(Register.SP)
];
const Operand[] Reg16_pp = [
    OR16(Register.BC), OR16(Register.DE), OR16(Register.HL), OR16(Register.AF)
];
const Operand[] Reg16_I = [OR16(Register.IX), OR16(Register.IY)];

Operand OR16_LK(Register register) {
    Operand operand;
    operand.variety = OperandVariety.Reg16Lookup;
    operand.register = register;
    return operand;
}

Operand IMM8() {
    Operand operand;
    operand.variety = OperandVariety.Imm8;
    return operand;
}

Operand IMM8_LK() {
    Operand operand;
    operand.variety = OperandVariety.Imm8Lookup;
    return operand;
}

Operand OR8_LK(Register r) {
    Operand operand;
    operand.variety = OperandVariety.Reg8Lookup;
    operand.register = r;
    return operand;
}

Operand PIMM8(ubyte ub) {
    Operand operand;
    operand.variety = OperandVariety.PreSetImm8;
    operand.imm8 = ub;
    return operand;
}

Operand IX() {
    Operand operand;
    operand.variety = OperandVariety.IxOffset;
    return operand;
}

Operand IY() {
    Operand operand;
    operand.variety = OperandVariety.IyOffset;
    return operand;
}

Operand LIMM8() {
    Operand operand;
    operand.variety = OperandVariety.Imm8;
    operand.isLabel = true;
    return operand;
}

Operand LIMM16() {
    Operand operand;
    operand.variety = OperandVariety.Imm16;
    operand.isLabel = true;
    return operand;
}

Operand IMM16() {
    Operand operand;
    operand.variety = OperandVariety.Imm16;

    return operand;
}

Operand IMM16_LK() {
    Operand operand;
    operand.variety = OperandVariety.Imm16Lookup;
    return operand;
}

Operand Con(ConditionVariety condition) {
    Operand operand;
    operand.variety = OperandVariety.Condition;
    operand.condition = condition;
    return operand;
}

const Operand[] Condition_ccc = [
    Con(ConditionVariety.NZ), Con(ConditionVariety.Z), Con(ConditionVariety.NC),
    Con(ConditionVariety.C), Con(ConditionVariety.PO), Con(ConditionVariety.PE),
    Con(ConditionVariety.P), Con(ConditionVariety.M)
];

string asOpString(InstructionType type) {
    import std.conv : to;
    import std.string : toLower;

    assert(type != InstructionType.Unknown, "Opcode can't be determined");
    assert(type != InstructionType.Indirection);
    return type.to!string.toLower;
}

Nullable!Operand findOfOperandVariety(Instruction inst, OperandVariety variety) {
    foreach (operand; inst.operands) {
        if (operand.variety == variety)
            return nullable(operand);
    }

    return nullable!Operand(null);
}

struct Instruction {
    InstructionType type;
    Operand[] operands;
    Instruction[ubyte] indirection = null;
    bool isIBitIndirection = false;
    ubyte[] unknownData = null;
    size_t byteSize = -1;
}

import dissasembly.z80Decompiler : Label;

Nullable!string toAssembly(Instruction instruction, Label usingLabel = null) {
    if (instruction.type == instruction.type.Unknown)
        return nullable!string(null);

    string ret = instruction.type.asOpString ~ " ";

    size_t oprLength = instruction.operands.length;
    for (size_t i; i < oprLength; i++) {
        bool isFinal = oprLength - 1 == i;
        Operand operand = instruction.operands[i];
        import std.string : toLower;
        import std.conv;

        switch (operand.variety) {
            case OperandVariety.Reg8:
            case OperandVariety.Reg16:
                ret ~= operand.register.toAsmString;
                break;
            case OperandVariety.Reg16Lookup:
            case OperandVariety.Reg8Lookup:
                ret ~= "(" ~ operand.register.toAsmString ~ ")";
                break;
            case OperandVariety.Condition:
                ret ~= operand.condition.to!string.toLower;
                break;
            case OperandVariety.Rst:
                ret ~= format("%02X", operand.rst) ~ "h";
                break;
            case OperandVariety.PreSetImm8:
                ret ~= operand.imm8.to!string;
                break;
            case OperandVariety.Imm8:
                if (instruction.type == InstructionType.Jr && usingLabel !is null) {
                    ret ~= usingLabel.genName;
                    break;
                }
                if (instruction.type == InstructionType.Out || instruction.type == InstructionType
                    .In)
                    ret ~= "(";
                ret ~= format("%02X", operand.imm8) ~ "h";
                if (instruction.type == InstructionType.Out || instruction.type == InstructionType
                    .In)
                    ret ~= ")";
                break;
            case OperandVariety.Imm16:
                if (usingLabel is null)
                    ret ~= format("%04X", operand.imm16) ~ "h";
                else
                    ret ~= usingLabel.genName;
                break;
            case OperandVariety.Imm8Lookup:
                ret ~= "(" ~ format("%02X", operand.imm16) ~ "h)";
                break;
            case OperandVariety.Imm16Lookup:
                if (usingLabel is null)
                    ret ~= "(" ~ format("%04X", operand.imm16) ~ "h)";
                else
                    ret ~= "(" ~ usingLabel.genName ~ "h)";
                break;
            case OperandVariety.IxOffset:
                ret ~= "(ix+" ~ format("%02X", operand.imm8) ~ "h)";
                break;
            case OperandVariety.IyOffset:
                ret ~= "(iy+" ~ format("%02X", operand.imm8) ~ "h)";
                break;
            default:
                assert(0, operand.variety.to!string);
        }
        if (!isFinal)
            ret ~= ", ";
    }
    return nullable!string(ret.strip());
}

enum InstructionType {
    Unknown,
    Indirection,

    Nop,
    Ld,
    Cp,
    Inc,
    Dec,
    Rrca,
    Rla,
    Add,
    Sub,
    And,
    Or,
    Xor,
    Adc,
    Sbc,
    Ex,
    Exx,
    Daa,
    Cpl,
    Ccf,
    Rlca,
    Halt,
    Rra,
    Scf,
    Pop,
    Push,

    Djnz,
    Jp,
    Jr,
    Call,
    Ret,
    Rst,
    Out,
    In,
    Di,
    Ei,
    Rlc,
    Rrc,
    Rl,
    Rr,
    Sla,
    Sra,
    Sll,
    Srl,
    Bit,
    Res,
    Set,

    Ini,
    Cpdr,
    Im,
    Tst,
    Ind,
    Neg,
    Cpi,
    Otimr,
    Otdmr,
    Cpd,
    Otir,
    Ldd,
    Rld,
    Cpir,
    Outd,
    Otdr,
    Lddr,
    Retn,
    Indr,
    Rrd,
    Mlt,
    Ldi,
    Otdm,
    Ldir,
    Outi,
    Otim,
    Slp,
    In0,
    Tstio,
    Out0,
    Reti,
    Inir
}

private Instruction[ubyte] genMainInstructions() {
    Instruction[ubyte] instructions;

    instructions[0] = Instruction(InstructionType.Nop);
    instructions[0x02] = Instruction(InstructionType.Ld, [
            OR16_LK(Register.BC), OR8(Register.A)
        ]);
    instructions[0x07] = Instruction(InstructionType.Rlca, []);
    instructions[0x08] = Instruction(InstructionType.Ex, [
            OR16(Register.AF), OR16(Register.SHADOW_AF)
        ]);
    instructions[0x0A] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR16_LK(Register.BC)
        ]);
    instructions[0x0F] = Instruction(InstructionType.Rrca, []);
    instructions[0x10] = Instruction(InstructionType.Djnz, [LIMM8]);
    instructions[0x17] = Instruction(InstructionType.Rla, []);
    instructions[0x18] = Instruction(InstructionType.Jr, [LIMM8]);
    instructions[0x1a] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR16_LK(Register.DE)
        ]);
    instructions[0x3a] = Instruction(InstructionType.Ld, [
            OR8(Register.A), IMM16_LK
        ]);
    instructions[0x1F] = Instruction(InstructionType.Rra, []);
    instructions[0x27] = Instruction(InstructionType.Daa, []);
    instructions[0x2F] = Instruction(InstructionType.Cpl, []);
    instructions[0x37] = Instruction(InstructionType.Scf, []);
    instructions[0x3f] = Instruction(InstructionType.Ccf, []);
    instructions[0x76] = Instruction(InstructionType.Halt, []);
    instructions[0x34] = Instruction(InstructionType.Inc, [OR16_LK(Register.HL)]);
    instructions[0x35] = Instruction(InstructionType.Dec, [OR16_LK(Register.HL)]);

    instructions[0x12] = Instruction(InstructionType.Ld, [
            OR16_LK(Register.DE), OR8(Register.A)
        ]);
    instructions[0x22] = Instruction(InstructionType.Ld, [
            IMM16_LK, OR16(Register.HL)
        ]);
    instructions[0x32] = Instruction(InstructionType.Ld, [
            IMM16_LK, OR8(Register.A)
        ]);
    instructions[0x36] = Instruction(InstructionType.Ld, [
            OR16_LK(Register.HL), IMM8
        ]);

    instructions[0x2A] = Instruction(InstructionType.Ld, [
            OR16(Register.HL), IMM16_LK
        ]);

    instructions[0x20] = Instruction(InstructionType.Jr, [
            Con(ConditionVariety.NZ), LIMM8
        ]);
    instructions[0x28] = Instruction(InstructionType.Jr, [
            Con(ConditionVariety.Z), LIMM8
        ]);
    instructions[0x30] = Instruction(InstructionType.Jr, [
            Con(ConditionVariety.NC), LIMM8
        ]);
    instructions[0x38] = Instruction(InstructionType.Jr, [
            Con(ConditionVariety.C), LIMM8
        ]);

    instructions[0x86] = Instruction(InstructionType.Add, [
            OR8(Register.A), OR16_LK(Register.HL)
        ]);
    instructions[0x8E] = Instruction(InstructionType.Adc, [
            OR8(Register.A), OR16_LK(Register.HL)
        ]);
    instructions[0x9E] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), OR16_LK(Register.HL)
        ]);
    instructions[0xDE] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), IMM8
        ]);
    instructions[0xa6] = Instruction(InstructionType.And, [OR16_LK(Register.HL)]);
    instructions[0xb6] = Instruction(InstructionType.Or, [OR16_LK(Register.HL)]);
    instructions[0x96] = Instruction(InstructionType.Sub, [OR16_LK(Register.HL)]);
    instructions[0xae] = Instruction(InstructionType.Xor, [OR16_LK(Register.HL)]);
    instructions[0xbe] = Instruction(InstructionType.Cp, [OR16_LK(Register.HL)]);
    instructions[0xc3] = Instruction(InstructionType.Jp, [LIMM16]);
    instructions[0xc9] = Instruction(InstructionType.Ret, []);
    instructions[0xcb] = Instruction(InstructionType.Indirection, [], BitIndirection());
    instructions[0xdd] = Instruction(InstructionType.Indirection, [], IxIndirection());
    instructions[0xed] = Instruction(InstructionType.Indirection, [], MiscIndirection());
    instructions[0xfd] = Instruction(InstructionType.Indirection, [], IyIndirection());

    instructions[0xc6] = Instruction(InstructionType.Add, [
            OR8(Register.A), IMM8
        ]);
    instructions[0xd3] = Instruction(InstructionType.Out, [
            IMM8, OR8(Register.A)
        ]);
    instructions[0xd6] = Instruction(InstructionType.Sub, [IMM8]);
    instructions[0xd9] = Instruction(InstructionType.Exx, []);
    instructions[0xdb] = Instruction(InstructionType.In, [OR8(Register.A), IMM8]);

    instructions[0xCD] = Instruction(InstructionType.Call, [LIMM16]);
    instructions[0xCE] = Instruction(InstructionType.Adc, [
            OR8(Register.A), IMM8
        ]);

    instructions[0xe3] = Instruction(InstructionType.Ex, [
            OR16_LK(Register.SP), OR16(Register.HL)
        ]);
    instructions[0xe6] = Instruction(InstructionType.And, [IMM8]);
    instructions[0xe9] = Instruction(InstructionType.Jp, [OR16_LK(Register.HL)]);
    instructions[0xeb] = Instruction(InstructionType.Ex, [
            OR16(Register.DE), OR16(Register.HL)
        ]);
    instructions[0xee] = Instruction(InstructionType.Xor, [IMM8]);
    instructions[0xf6] = Instruction(InstructionType.Or, [IMM8]);
    instructions[0xfe] = Instruction(InstructionType.Cp, [IMM8]);
    instructions[0xf3] = Instruction(InstructionType.Di, []);
    instructions[0xfb] = Instruction(InstructionType.Ei, []);
    instructions[0xf9] = Instruction(InstructionType.Ld, [
            OR16(Register.SP), OR16(Register.HL)
        ]);
    static foreach (i, cond; Condition_ccc) {
        instructions[0b1100_0100 | (i << 3)] = Instruction(InstructionType.Call, [
                cond, LIMM16
            ]);
        instructions[0b1100_0010 | (i << 3)] = Instruction(InstructionType.Jp, [
                cond, LIMM16
            ]);
        instructions[0b1100_0000 | (i << 3)] = Instruction(InstructionType.Ret, [
                cond
            ]);
    }
    static foreach (rst_v; 0 .. 0b111 + 1) {
        {
            Operand rstOperand;
            rstOperand.variety = OperandVariety.Rst;
            rstOperand.rst = [0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38][rst_v];
            instructions[0b1100_0111 | (rst_v << 3)] = Instruction(InstructionType.Rst, [
                    rstOperand
                ]);
        }
    }
    static foreach (i, opr; Reg8_rrr)
        if (opr.register != Register.UNKNOWN) {
            instructions[0b0111_0000 | i] = Instruction(InstructionType.Ld, [
                    OR16_LK(Register.HL), opr
                ]);
            instructions[0b1000_1000 | i] = Instruction(InstructionType.Adc, [
                    OR8(Register.A), opr
                ]);
            instructions[0b1000_0000 | i] = Instruction(InstructionType.Add, [
                    OR8(Register.A), opr
                ]);
            instructions[0b1010_0000 | i] = Instruction(InstructionType.And, [
                    opr
                ]);
            instructions[0b1011_1000 | i] = Instruction(InstructionType.Cp, [
                    opr
                ]);
            instructions[0b1011_0000 | i] = Instruction(InstructionType.Or, [
                    opr
                ]);
            instructions[0b0000_0101 | (i << 3)] = Instruction(InstructionType.Dec, [
                    opr
                ]);
            instructions[0b0000_0100 | (i << 3)] = Instruction(InstructionType.Inc, [
                    opr
                ]);
            instructions[0b0000_0110 | (i << 3)] = Instruction(InstructionType.Ld, [
                    opr, IMM8
                ]);
            instructions[0b0100_0110 | (i << 3)] = Instruction(InstructionType.Ld, [
                    opr, OR16_LK(Register.HL)
                ]);
            instructions[0b1001_1000 | i] = Instruction(InstructionType.Sbc, [
                    OR8(Register.A), opr
                ]);
            instructions[0b1001_0000 | i] = Instruction(InstructionType.Sub, [
                    opr
                ]);
            instructions[0b1010_1000 | i] = Instruction(InstructionType.Xor, [
                    opr
                ]);

        }
    static foreach (i, opr; Reg16_pp) {
        instructions[0b1100_0001 | (i << 4)] = Instruction(InstructionType.Pop, [
                opr
            ]);
        instructions[0b1100_0101 | (i << 4)] = Instruction(InstructionType.Push, [
                opr
            ]);
    }
    static foreach (i, opr; Reg16_qq) {
        instructions[0b0000_1001 | (i << 4)] = Instruction(InstructionType.Add, [
                OR16(Register.HL), opr
            ]);
        instructions[0b0000_1011 | (i << 4)] = Instruction(InstructionType.Dec, [
                opr
            ]);
        instructions[0b0000_0011 | (i << 4)] = Instruction(InstructionType.Inc, [
                opr
            ]);
        instructions[0b0000_0001 | (i << 4)] = Instruction(InstructionType.Ld, [
                opr, IMM16
            ]);
    }
    static foreach (to_index, reg_to; Reg8_rrr)
        if (reg_to.register != Register.UNKNOWN) {
            static foreach (from_index, reg_from; Reg8_rrr)
                if (reg_from.register != Register.UNKNOWN) {
                    instructions[cast(ubyte) 0b0100_0000 | (to_index << 3) | from_index] = Instruction(
                        InstructionType.Ld, [reg_to, reg_from]);
                }
        }

    return instructions;
}

private Instruction[ubyte] BitIndirection() {
    Instruction[ubyte] instructions;
    instructions[0b00000110] = Instruction(InstructionType.Rlc, [
            OR16_LK(Register.HL)
        ]);
    instructions[0b00001110] = Instruction(InstructionType.Rrc, [
            OR16_LK(Register.HL)
        ]);
    instructions[0b00010110] = Instruction(InstructionType.Rl, [
            OR16_LK(Register.HL)
        ]);
    instructions[0b00011110] = Instruction(InstructionType.Rr, [
            OR16_LK(Register.HL)
        ]);
    instructions[0b00100110] = Instruction(InstructionType.Sla, [
            OR16_LK(Register.HL)
        ]);
    instructions[0b00101110] = Instruction(InstructionType.Sra, [
            OR16_LK(Register.HL)
        ]);
    instructions[0b00110110] = Instruction(InstructionType.Sll, [
            OR16_LK(Register.HL)
        ]);
    instructions[0b00111110] = Instruction(InstructionType.Srl, [
            OR16_LK(Register.HL)
        ]);
    static foreach (i, opr; Reg8_rrr)
        if (opr.register != Register.UNKNOWN) {
            instructions[i] = Instruction(InstructionType.Rlc, [opr]);
            instructions[0b00001000 | i] = Instruction(InstructionType.Rrc, [
                    opr
                ]);
            instructions[0b00010000 | i] = Instruction(InstructionType.Rl, [opr]);
            instructions[0b00011000 | i] = Instruction(InstructionType.Rr, [opr]);
            instructions[0b00100000 | i] = Instruction(InstructionType.Sla, [
                    opr
                ]);
            instructions[0b00101000 | i] = Instruction(InstructionType.Sra, [
                    opr
                ]);
            instructions[0b00110000 | i] = Instruction(InstructionType.Sll, [
                    opr
                ]);
            instructions[0b00111000 | i] = Instruction(InstructionType.Srl, [
                    opr
                ]);
        }
    static foreach (b; 0 .. 0b111 + 1) {
        {
            enum bmask = b << 3;
            instructions[0b0100_0110 | bmask] = Instruction(InstructionType.Bit, [
                    PIMM8(b), OR16_LK(Register.HL)
                ]);
            instructions[0b1100_0110 | bmask] = Instruction(InstructionType.Set, [
                    PIMM8(b), OR16_LK(Register.HL)
                ]);
            instructions[0b1000_0110 | bmask] = Instruction(InstructionType.Res, [
                    PIMM8(b), OR16_LK(Register.HL)
                ]);
            static foreach (i, opr; Reg8_rrr)
                if (opr.register != Register.UNKNOWN) {
                    instructions[0b0100_0000 | bmask | i] = Instruction(InstructionType.Bit, [
                            PIMM8(b), opr
                        ]);
                    instructions[0b1000_0000 | bmask | i] = Instruction(InstructionType.Res, [
                            PIMM8(b), opr
                        ]);
                    instructions[0b1100_0000 | bmask | i] = Instruction(InstructionType.Set, [
                            PIMM8(b), opr
                        ]);
                }
        }
    }
    return instructions;
}

private Instruction[ubyte] IxBitIndirection() {
    Instruction[ubyte] instructions;
    instructions[0x00] = Instruction(InstructionType.Rlc, [IX, OR8(Register.B)], null, true);
    instructions[0x01] = Instruction(InstructionType.Rlc, [IX, OR8(Register.C)], null, true);
    instructions[0x02] = Instruction(InstructionType.Rlc, [IX, OR8(Register.D)], null, true);
    instructions[0x03] = Instruction(InstructionType.Rlc, [IX, OR8(Register.E)], null, true);
    instructions[0x04] = Instruction(InstructionType.Rlc, [IX, OR8(Register.H)], null, true);
    instructions[0x05] = Instruction(InstructionType.Rlc, [IX, OR8(Register.L)], null, true);
    instructions[0x06] = Instruction(InstructionType.Rlc, [IX], null, true);
    instructions[0x07] = Instruction(InstructionType.Rlc, [IX, OR8(Register.A)], null, true);
    instructions[0x08] = Instruction(InstructionType.Rrc, [IX, OR8(Register.B)], null, true);
    instructions[0x09] = Instruction(InstructionType.Rrc, [IX, OR8(Register.C)], null, true);
    instructions[0x0A] = Instruction(InstructionType.Rrc, [IX, OR8(Register.D)], null, true);
    instructions[0x0B] = Instruction(InstructionType.Rrc, [IX, OR8(Register.E)], null, true);
    instructions[0x0C] = Instruction(InstructionType.Rrc, [IX, OR8(Register.H)], null, true);
    instructions[0x0D] = Instruction(InstructionType.Rrc, [IX, OR8(Register.L)], null, true);
    instructions[0x0E] = Instruction(InstructionType.Rrc, [IX], null, true);
    instructions[0x0F] = Instruction(InstructionType.Rrc, [IX, OR8(Register.A)], null, true);
    instructions[0x10] = Instruction(InstructionType.Rl, [IX, OR8(Register.B)], null, true);
    instructions[0x11] = Instruction(InstructionType.Rl, [IX, OR8(Register.C)], null, true);
    instructions[0x12] = Instruction(InstructionType.Rl, [IX, OR8(Register.D)], null, true);
    instructions[0x13] = Instruction(InstructionType.Rl, [IX, OR8(Register.E)], null, true);
    instructions[0x14] = Instruction(InstructionType.Rl, [IX, OR8(Register.H)], null, true);
    instructions[0x15] = Instruction(InstructionType.Rl, [IX, OR8(Register.L)], null, true);
    instructions[0x16] = Instruction(InstructionType.Rl, [IX], null, true);
    instructions[0x17] = Instruction(InstructionType.Rl, [IX, OR8(Register.A)], null, true);
    instructions[0x18] = Instruction(InstructionType.Rr, [IX, OR8(Register.B)], null, true);
    instructions[0x19] = Instruction(InstructionType.Rr, [IX, OR8(Register.C)], null, true);
    instructions[0x1A] = Instruction(InstructionType.Rr, [IX, OR8(Register.D)], null, true);
    instructions[0x1B] = Instruction(InstructionType.Rr, [IX, OR8(Register.E)], null, true);
    instructions[0x1C] = Instruction(InstructionType.Rr, [IX, OR8(Register.H)], null, true);
    instructions[0x1D] = Instruction(InstructionType.Rr, [IX, OR8(Register.L)], null, true);
    instructions[0x1E] = Instruction(InstructionType.Rr, [IX], null, true);
    instructions[0x1F] = Instruction(InstructionType.Rr, [IX, OR8(Register.A)], null, true);
    instructions[0x20] = Instruction(InstructionType.Sla, [IX, OR8(Register.B)], null, true);
    instructions[0x21] = Instruction(InstructionType.Sla, [IX, OR8(Register.C)], null, true);
    instructions[0x22] = Instruction(InstructionType.Sla, [IX, OR8(Register.D)], null, true);
    instructions[0x23] = Instruction(InstructionType.Sla, [IX, OR8(Register.E)], null, true);
    instructions[0x24] = Instruction(InstructionType.Sla, [IX, OR8(Register.H)], null, true);
    instructions[0x25] = Instruction(InstructionType.Sla, [IX, OR8(Register.L)], null, true);
    instructions[0x26] = Instruction(InstructionType.Sla, [IX], null, true);
    instructions[0x27] = Instruction(InstructionType.Sla, [IX, OR8(Register.A)], null, true);
    instructions[0x28] = Instruction(InstructionType.Sra, [IX, OR8(Register.B)], null, true);
    instructions[0x29] = Instruction(InstructionType.Sra, [IX, OR8(Register.C)], null, true);
    instructions[0x2A] = Instruction(InstructionType.Sra, [IX, OR8(Register.D)], null, true);
    instructions[0x2B] = Instruction(InstructionType.Sra, [IX, OR8(Register.E)], null, true);
    instructions[0x2C] = Instruction(InstructionType.Sra, [IX, OR8(Register.H)], null, true);
    instructions[0x2D] = Instruction(InstructionType.Sra, [IX, OR8(Register.L)], null, true);
    instructions[0x2E] = Instruction(InstructionType.Sra, [IX], null, true);
    instructions[0x2F] = Instruction(InstructionType.Sra, [IX, OR8(Register.A)], null, true);
    instructions[0x30] = Instruction(InstructionType.Sll, [IX, OR8(Register.B)], null, true);
    instructions[0x31] = Instruction(InstructionType.Sll, [IX, OR8(Register.C)], null, true);
    instructions[0x32] = Instruction(InstructionType.Sll, [IX, OR8(Register.D)], null, true);
    instructions[0x33] = Instruction(InstructionType.Sll, [IX, OR8(Register.E)], null, true);
    instructions[0x34] = Instruction(InstructionType.Sll, [IX, OR8(Register.H)], null, true);
    instructions[0x35] = Instruction(InstructionType.Sll, [IX, OR8(Register.L)], null, true);
    instructions[0x36] = Instruction(InstructionType.Sll, [IX], null, true);
    instructions[0x37] = Instruction(InstructionType.Sll, [IX, OR8(Register.A)], null, true);
    instructions[0x38] = Instruction(InstructionType.Srl, [IX, OR8(Register.B)], null, true);
    instructions[0x39] = Instruction(InstructionType.Srl, [IX, OR8(Register.C)], null, true);
    instructions[0x3A] = Instruction(InstructionType.Srl, [IX, OR8(Register.D)], null, true);
    instructions[0x3B] = Instruction(InstructionType.Srl, [IX, OR8(Register.E)], null, true);
    instructions[0x3C] = Instruction(InstructionType.Srl, [IX, OR8(Register.H)], null, true);
    instructions[0x3D] = Instruction(InstructionType.Srl, [IX, OR8(Register.L)], null, true);
    instructions[0x3E] = Instruction(InstructionType.Srl, [IX], null, true);
    instructions[0x3F] = Instruction(InstructionType.Srl, [IX, OR8(Register.A)], null, true);
    instructions[0x40] = Instruction(InstructionType.Bit, [PIMM8(0), IX], null, true);
    instructions[0x41] = Instruction(InstructionType.Bit, [PIMM8(0), IX], null, true);
    instructions[0x42] = Instruction(InstructionType.Bit, [PIMM8(0), IX], null, true);
    instructions[0x43] = Instruction(InstructionType.Bit, [PIMM8(0), IX], null, true);
    instructions[0x44] = Instruction(InstructionType.Bit, [PIMM8(0), IX], null, true);
    instructions[0x45] = Instruction(InstructionType.Bit, [PIMM8(0), IX], null, true);
    instructions[0x46] = Instruction(InstructionType.Bit, [PIMM8(0), IX], null, true);
    instructions[0x47] = Instruction(InstructionType.Bit, [PIMM8(0), IX], null, true);
    instructions[0x48] = Instruction(InstructionType.Bit, [PIMM8(1), IX], null, true);
    instructions[0x49] = Instruction(InstructionType.Bit, [PIMM8(1), IX], null, true);
    instructions[0x4A] = Instruction(InstructionType.Bit, [PIMM8(1), IX], null, true);
    instructions[0x4B] = Instruction(InstructionType.Bit, [PIMM8(1), IX], null, true);
    instructions[0x4C] = Instruction(InstructionType.Bit, [PIMM8(1), IX], null, true);
    instructions[0x4D] = Instruction(InstructionType.Bit, [PIMM8(1), IX], null, true);
    instructions[0x4E] = Instruction(InstructionType.Bit, [PIMM8(1), IX], null, true);
    instructions[0x4F] = Instruction(InstructionType.Bit, [PIMM8(1), IX], null, true);
    instructions[0x50] = Instruction(InstructionType.Bit, [PIMM8(2), IX], null, true);
    instructions[0x51] = Instruction(InstructionType.Bit, [PIMM8(2), IX], null, true);
    instructions[0x52] = Instruction(InstructionType.Bit, [PIMM8(2), IX], null, true);
    instructions[0x53] = Instruction(InstructionType.Bit, [PIMM8(2), IX], null, true);
    instructions[0x54] = Instruction(InstructionType.Bit, [PIMM8(2), IX], null, true);
    instructions[0x55] = Instruction(InstructionType.Bit, [PIMM8(2), IX], null, true);
    instructions[0x56] = Instruction(InstructionType.Bit, [PIMM8(2), IX], null, true);
    instructions[0x57] = Instruction(InstructionType.Bit, [PIMM8(2), IX], null, true);
    instructions[0x58] = Instruction(InstructionType.Bit, [PIMM8(3), IX], null, true);
    instructions[0x59] = Instruction(InstructionType.Bit, [PIMM8(3), IX], null, true);
    instructions[0x5A] = Instruction(InstructionType.Bit, [PIMM8(3), IX], null, true);
    instructions[0x5B] = Instruction(InstructionType.Bit, [PIMM8(3), IX], null, true);
    instructions[0x5C] = Instruction(InstructionType.Bit, [PIMM8(3), IX], null, true);
    instructions[0x5D] = Instruction(InstructionType.Bit, [PIMM8(3), IX], null, true);
    instructions[0x5E] = Instruction(InstructionType.Bit, [PIMM8(3), IX], null, true);
    instructions[0x5F] = Instruction(InstructionType.Bit, [PIMM8(3), IX], null, true);
    instructions[0x60] = Instruction(InstructionType.Bit, [PIMM8(4), IX], null, true);
    instructions[0x61] = Instruction(InstructionType.Bit, [PIMM8(4), IX], null, true);
    instructions[0x62] = Instruction(InstructionType.Bit, [PIMM8(4), IX], null, true);
    instructions[0x63] = Instruction(InstructionType.Bit, [PIMM8(4), IX], null, true);
    instructions[0x64] = Instruction(InstructionType.Bit, [PIMM8(4), IX], null, true);
    instructions[0x65] = Instruction(InstructionType.Bit, [PIMM8(4), IX], null, true);
    instructions[0x66] = Instruction(InstructionType.Bit, [PIMM8(4), IX], null, true);
    instructions[0x67] = Instruction(InstructionType.Bit, [PIMM8(4), IX], null, true);
    instructions[0x68] = Instruction(InstructionType.Bit, [PIMM8(5), IX], null, true);
    instructions[0x69] = Instruction(InstructionType.Bit, [PIMM8(5), IX], null, true);
    instructions[0x6A] = Instruction(InstructionType.Bit, [PIMM8(5), IX], null, true);
    instructions[0x6B] = Instruction(InstructionType.Bit, [PIMM8(5), IX], null, true);
    instructions[0x6C] = Instruction(InstructionType.Bit, [PIMM8(5), IX], null, true);
    instructions[0x6D] = Instruction(InstructionType.Bit, [PIMM8(5), IX], null, true);
    instructions[0x6E] = Instruction(InstructionType.Bit, [PIMM8(5), IX], null, true);
    instructions[0x6F] = Instruction(InstructionType.Bit, [PIMM8(5), IX], null, true);
    instructions[0x70] = Instruction(InstructionType.Bit, [PIMM8(6), IX], null, true);
    instructions[0x71] = Instruction(InstructionType.Bit, [PIMM8(6), IX], null, true);
    instructions[0x72] = Instruction(InstructionType.Bit, [PIMM8(6), IX], null, true);
    instructions[0x73] = Instruction(InstructionType.Bit, [PIMM8(6), IX], null, true);
    instructions[0x74] = Instruction(InstructionType.Bit, [PIMM8(6), IX], null, true);
    instructions[0x75] = Instruction(InstructionType.Bit, [PIMM8(6), IX], null, true);
    instructions[0x76] = Instruction(InstructionType.Bit, [PIMM8(6), IX], null, true);
    instructions[0x77] = Instruction(InstructionType.Bit, [PIMM8(6), IX], null, true);
    instructions[0x78] = Instruction(InstructionType.Bit, [PIMM8(7), IX], null, true);
    instructions[0x79] = Instruction(InstructionType.Bit, [PIMM8(7), IX], null, true);
    instructions[0x7A] = Instruction(InstructionType.Bit, [PIMM8(7), IX], null, true);
    instructions[0x7B] = Instruction(InstructionType.Bit, [PIMM8(7), IX], null, true);
    instructions[0x7C] = Instruction(InstructionType.Bit, [PIMM8(7), IX], null, true);
    instructions[0x7D] = Instruction(InstructionType.Bit, [PIMM8(7), IX], null, true);
    instructions[0x7E] = Instruction(InstructionType.Bit, [PIMM8(7), IX], null, true);
    instructions[0x7F] = Instruction(InstructionType.Bit, [PIMM8(7), IX], null, true);
    instructions[0x80] = Instruction(InstructionType.Res, [
            PIMM8(0), IX, OR8(Register.B)
        ], null, true);
    instructions[0x81] = Instruction(InstructionType.Res, [
            PIMM8(0), IX, OR8(Register.C)
        ], null, true);
    instructions[0x82] = Instruction(InstructionType.Res, [
            PIMM8(0), IX, OR8(Register.D)
        ], null, true);
    instructions[0x83] = Instruction(InstructionType.Res, [
            PIMM8(0), IX, OR8(Register.E)
        ], null, true);
    instructions[0x84] = Instruction(InstructionType.Res, [
            PIMM8(0), IX, OR8(Register.H)
        ], null, true);
    instructions[0x85] = Instruction(InstructionType.Res, [
            PIMM8(0), IX, OR8(Register.L)
        ], null, true);
    instructions[0x86] = Instruction(InstructionType.Res, [PIMM8(0), IX], null, true);
    instructions[0x87] = Instruction(InstructionType.Res, [
            PIMM8(0), IX, OR8(Register.A)
        ], null, true);
    instructions[0x88] = Instruction(InstructionType.Res, [
            PIMM8(1), IX, OR8(Register.B)
        ], null, true);
    instructions[0x89] = Instruction(InstructionType.Res, [
            PIMM8(1), IX, OR8(Register.C)
        ], null, true);
    instructions[0x8A] = Instruction(InstructionType.Res, [
            PIMM8(1), IX, OR8(Register.D)
        ], null, true);
    instructions[0x8B] = Instruction(InstructionType.Res, [
            PIMM8(1), IX, OR8(Register.E)
        ], null, true);
    instructions[0x8C] = Instruction(InstructionType.Res, [
            PIMM8(1), IX, OR8(Register.H)
        ], null, true);
    instructions[0x8D] = Instruction(InstructionType.Res, [
            PIMM8(1), IX, OR8(Register.L)
        ], null, true);
    instructions[0x8E] = Instruction(InstructionType.Res, [PIMM8(1), IX], null, true);
    instructions[0x8F] = Instruction(InstructionType.Res, [
            PIMM8(1), IX, OR8(Register.A)
        ], null, true);
    instructions[0x90] = Instruction(InstructionType.Res, [
            PIMM8(2), IX, OR8(Register.B)
        ], null, true);
    instructions[0x91] = Instruction(InstructionType.Res, [
            PIMM8(2), IX, OR8(Register.C)
        ], null, true);
    instructions[0x92] = Instruction(InstructionType.Res, [
            PIMM8(2), IX, OR8(Register.D)
        ], null, true);
    instructions[0x93] = Instruction(InstructionType.Res, [
            PIMM8(2), IX, OR8(Register.E)
        ], null, true);
    instructions[0x94] = Instruction(InstructionType.Res, [
            PIMM8(2), IX, OR8(Register.H)
        ], null, true);
    instructions[0x95] = Instruction(InstructionType.Res, [
            PIMM8(2), IX, OR8(Register.L)
        ], null, true);
    instructions[0x96] = Instruction(InstructionType.Res, [PIMM8(2), IX], null, true);
    instructions[0x97] = Instruction(InstructionType.Res, [
            PIMM8(2), IX, OR8(Register.A)
        ], null, true);
    instructions[0x98] = Instruction(InstructionType.Res, [
            PIMM8(3), IX, OR8(Register.B)
        ], null, true);
    instructions[0x99] = Instruction(InstructionType.Res, [
            PIMM8(3), IX, OR8(Register.C)
        ], null, true);
    instructions[0x9A] = Instruction(InstructionType.Res, [
            PIMM8(3), IX, OR8(Register.D)
        ], null, true);
    instructions[0x9B] = Instruction(InstructionType.Res, [
            PIMM8(3), IX, OR8(Register.E)
        ], null, true);
    instructions[0x9C] = Instruction(InstructionType.Res, [
            PIMM8(3), IX, OR8(Register.H)
        ], null, true);
    instructions[0x9D] = Instruction(InstructionType.Res, [
            PIMM8(3), IX, OR8(Register.L)
        ], null, true);
    instructions[0x9E] = Instruction(InstructionType.Res, [PIMM8(3), IX], null, true);
    instructions[0x9F] = Instruction(InstructionType.Res, [
            PIMM8(3), IX, OR8(Register.A)
        ], null, true);
    instructions[0xA0] = Instruction(InstructionType.Res, [
            PIMM8(4), IX, OR8(Register.B)
        ], null, true);
    instructions[0xA1] = Instruction(InstructionType.Res, [
            PIMM8(4), IX, OR8(Register.C)
        ], null, true);
    instructions[0xA2] = Instruction(InstructionType.Res, [
            PIMM8(4), IX, OR8(Register.D)
        ], null, true);
    instructions[0xA3] = Instruction(InstructionType.Res, [
            PIMM8(4), IX, OR8(Register.E)
        ], null, true);
    instructions[0xA4] = Instruction(InstructionType.Res, [
            PIMM8(4), IX, OR8(Register.H)
        ], null, true);
    instructions[0xA5] = Instruction(InstructionType.Res, [
            PIMM8(4), IX, OR8(Register.L)
        ], null, true);
    instructions[0xA6] = Instruction(InstructionType.Res, [PIMM8(4), IX], null, true);
    instructions[0xA7] = Instruction(InstructionType.Res, [
            PIMM8(4), IX, OR8(Register.A)
        ], null, true);
    instructions[0xA8] = Instruction(InstructionType.Res, [
            PIMM8(5), IX, OR8(Register.B)
        ], null, true);
    instructions[0xA9] = Instruction(InstructionType.Res, [
            PIMM8(5), IX, OR8(Register.C)
        ], null, true);
    instructions[0xAA] = Instruction(InstructionType.Res, [
            PIMM8(5), IX, OR8(Register.D)
        ], null, true);
    instructions[0xAB] = Instruction(InstructionType.Res, [
            PIMM8(5), IX, OR8(Register.E)
        ], null, true);
    instructions[0xAC] = Instruction(InstructionType.Res, [
            PIMM8(5), IX, OR8(Register.H)
        ], null, true);
    instructions[0xAD] = Instruction(InstructionType.Res, [
            PIMM8(5), IX, OR8(Register.L)
        ], null, true);
    instructions[0xAE] = Instruction(InstructionType.Res, [PIMM8(5), IX], null, true);
    instructions[0xAF] = Instruction(InstructionType.Res, [
            PIMM8(5), IX, OR8(Register.A)
        ], null, true);
    instructions[0xB0] = Instruction(InstructionType.Res, [
            PIMM8(6), IX, OR8(Register.B)
        ], null, true);
    instructions[0xB1] = Instruction(InstructionType.Res, [
            PIMM8(6), IX, OR8(Register.C)
        ], null, true);
    instructions[0xB2] = Instruction(InstructionType.Res, [
            PIMM8(6), IX, OR8(Register.D)
        ], null, true);
    instructions[0xB3] = Instruction(InstructionType.Res, [
            PIMM8(6), IX, OR8(Register.E)
        ], null, true);
    instructions[0xB4] = Instruction(InstructionType.Res, [
            PIMM8(6), IX, OR8(Register.H)
        ], null, true);
    instructions[0xB5] = Instruction(InstructionType.Res, [
            PIMM8(6), IX, OR8(Register.L)
        ], null, true);
    instructions[0xB6] = Instruction(InstructionType.Res, [PIMM8(6), IX], null, true);
    instructions[0xB7] = Instruction(InstructionType.Res, [
            PIMM8(6), IX, OR8(Register.A)
        ], null, true);
    instructions[0xB8] = Instruction(InstructionType.Res, [
            PIMM8(7), IX, OR8(Register.B)
        ], null, true);
    instructions[0xB9] = Instruction(InstructionType.Res, [
            PIMM8(7), IX, OR8(Register.C)
        ], null, true);
    instructions[0xBA] = Instruction(InstructionType.Res, [
            PIMM8(7), IX, OR8(Register.D)
        ], null, true);
    instructions[0xBB] = Instruction(InstructionType.Res, [
            PIMM8(7), IX, OR8(Register.E)
        ], null, true);
    instructions[0xBC] = Instruction(InstructionType.Res, [
            PIMM8(7), IX, OR8(Register.H)
        ], null, true);
    instructions[0xBD] = Instruction(InstructionType.Res, [
            PIMM8(7), IX, OR8(Register.L)
        ], null, true);
    instructions[0xBE] = Instruction(InstructionType.Res, [PIMM8(7), IX], null, true);
    instructions[0xBF] = Instruction(InstructionType.Res, [
            PIMM8(7), IX, OR8(Register.A)
        ], null, true);
    instructions[0xC0] = Instruction(InstructionType.Set, [
            PIMM8(0), IX, OR8(Register.B)
        ], null, true);
    instructions[0xC1] = Instruction(InstructionType.Set, [
            PIMM8(0), IX, OR8(Register.C)
        ], null, true);
    instructions[0xC2] = Instruction(InstructionType.Set, [
            PIMM8(0), IX, OR8(Register.D)
        ], null, true);
    instructions[0xC3] = Instruction(InstructionType.Set, [
            PIMM8(0), IX, OR8(Register.E)
        ], null, true);
    instructions[0xC4] = Instruction(InstructionType.Set, [
            PIMM8(0), IX, OR8(Register.H)
        ], null, true);
    instructions[0xC5] = Instruction(InstructionType.Set, [
            PIMM8(0), IX, OR8(Register.L)
        ], null, true);
    instructions[0xC6] = Instruction(InstructionType.Set, [PIMM8(0), IX], null, true);
    instructions[0xC7] = Instruction(InstructionType.Set, [
            PIMM8(0), IX, OR8(Register.A)
        ], null, true);
    instructions[0xC8] = Instruction(InstructionType.Set, [
            PIMM8(1), IX, OR8(Register.B)
        ], null, true);
    instructions[0xC9] = Instruction(InstructionType.Set, [
            PIMM8(1), IX, OR8(Register.C)
        ], null, true);
    instructions[0xCA] = Instruction(InstructionType.Set, [
            PIMM8(1), IX, OR8(Register.D)
        ], null, true);
    instructions[0xCB] = Instruction(InstructionType.Set, [
            PIMM8(1), IX, OR8(Register.E)
        ], null, true);
    instructions[0xCC] = Instruction(InstructionType.Set, [
            PIMM8(1), IX, OR8(Register.H)
        ], null, true);
    instructions[0xCD] = Instruction(InstructionType.Set, [
            PIMM8(1), IX, OR8(Register.L)
        ], null, true);
    instructions[0xCE] = Instruction(InstructionType.Set, [PIMM8(1), IX], null, true);
    instructions[0xCF] = Instruction(InstructionType.Set, [
            PIMM8(1), IX, OR8(Register.A)
        ], null, true);
    instructions[0xD0] = Instruction(InstructionType.Set, [
            PIMM8(2), IX, OR8(Register.B)
        ], null, true);
    instructions[0xD1] = Instruction(InstructionType.Set, [
            PIMM8(2), IX, OR8(Register.C)
        ], null, true);
    instructions[0xD2] = Instruction(InstructionType.Set, [
            PIMM8(2), IX, OR8(Register.D)
        ], null, true);
    instructions[0xD3] = Instruction(InstructionType.Set, [
            PIMM8(2), IX, OR8(Register.E)
        ], null, true);
    instructions[0xD4] = Instruction(InstructionType.Set, [
            PIMM8(2), IX, OR8(Register.H)
        ], null, true);
    instructions[0xD5] = Instruction(InstructionType.Set, [
            PIMM8(2), IX, OR8(Register.L)
        ], null, true);
    instructions[0xD6] = Instruction(InstructionType.Set, [PIMM8(2), IX], null, true);
    instructions[0xD7] = Instruction(InstructionType.Set, [
            PIMM8(2), IX, OR8(Register.A)
        ], null, true);
    instructions[0xD8] = Instruction(InstructionType.Set, [
            PIMM8(3), IX, OR8(Register.B)
        ], null, true);
    instructions[0xD9] = Instruction(InstructionType.Set, [
            PIMM8(3), IX, OR8(Register.C)
        ], null, true);
    instructions[0xDA] = Instruction(InstructionType.Set, [
            PIMM8(3), IX, OR8(Register.D)
        ], null, true);
    instructions[0xDB] = Instruction(InstructionType.Set, [
            PIMM8(3), IX, OR8(Register.E)
        ], null, true);
    instructions[0xDC] = Instruction(InstructionType.Set, [
            PIMM8(3), IX, OR8(Register.H)
        ], null, true);
    instructions[0xDD] = Instruction(InstructionType.Set, [
            PIMM8(3), IX, OR8(Register.L)
        ], null, true);
    instructions[0xDE] = Instruction(InstructionType.Set, [PIMM8(3), IX], null, true);
    instructions[0xDF] = Instruction(InstructionType.Set, [
            PIMM8(3), IX, OR8(Register.A)
        ], null, true);
    instructions[0xE0] = Instruction(InstructionType.Set, [
            PIMM8(4), IX, OR8(Register.B)
        ], null, true);
    instructions[0xE1] = Instruction(InstructionType.Set, [
            PIMM8(4), IX, OR8(Register.C)
        ], null, true);
    instructions[0xE2] = Instruction(InstructionType.Set, [
            PIMM8(4), IX, OR8(Register.D)
        ], null, true);
    instructions[0xE3] = Instruction(InstructionType.Set, [
            PIMM8(4), IX, OR8(Register.E)
        ], null, true);
    instructions[0xE4] = Instruction(InstructionType.Set, [
            PIMM8(4), IX, OR8(Register.H)
        ], null, true);
    instructions[0xE5] = Instruction(InstructionType.Set, [
            PIMM8(4), IX, OR8(Register.L)
        ], null, true);
    instructions[0xE6] = Instruction(InstructionType.Set, [PIMM8(4), IX], null, true);
    instructions[0xE7] = Instruction(InstructionType.Set, [
            PIMM8(4), IX, OR8(Register.A)
        ], null, true);
    instructions[0xE8] = Instruction(InstructionType.Set, [
            PIMM8(5), IX, OR8(Register.B)
        ], null, true);
    instructions[0xE9] = Instruction(InstructionType.Set, [
            PIMM8(5), IX, OR8(Register.C)
        ], null, true);
    instructions[0xEA] = Instruction(InstructionType.Set, [
            PIMM8(5), IX, OR8(Register.D)
        ], null, true);
    instructions[0xEB] = Instruction(InstructionType.Set, [
            PIMM8(5), IX, OR8(Register.E)
        ], null, true);
    instructions[0xEC] = Instruction(InstructionType.Set, [
            PIMM8(5), IX, OR8(Register.H)
        ], null, true);
    instructions[0xED] = Instruction(InstructionType.Set, [
            PIMM8(5), IX, OR8(Register.L)
        ], null, true);
    instructions[0xEE] = Instruction(InstructionType.Set, [PIMM8(5), IX], null, true);
    instructions[0xEF] = Instruction(InstructionType.Set, [
            PIMM8(5), IX, OR8(Register.A)
        ], null, true);
    instructions[0xF0] = Instruction(InstructionType.Set, [
            PIMM8(6), IX, OR8(Register.B)
        ], null, true);
    instructions[0xF1] = Instruction(InstructionType.Set, [
            PIMM8(6), IX, OR8(Register.C)
        ], null, true);
    instructions[0xF2] = Instruction(InstructionType.Set, [
            PIMM8(6), IX, OR8(Register.D)
        ], null, true);
    instructions[0xF3] = Instruction(InstructionType.Set, [
            PIMM8(6), IX, OR8(Register.E)
        ], null, true);
    instructions[0xF4] = Instruction(InstructionType.Set, [
            PIMM8(6), IX, OR8(Register.H)
        ], null, true);
    instructions[0xF5] = Instruction(InstructionType.Set, [
            PIMM8(6), IX, OR8(Register.L)
        ], null, true);
    instructions[0xF6] = Instruction(InstructionType.Set, [PIMM8(6), IX], null, true);
    instructions[0xF7] = Instruction(InstructionType.Set, [
            PIMM8(6), IX, OR8(Register.A)
        ], null, true);
    instructions[0xF8] = Instruction(InstructionType.Set, [
            PIMM8(7), IX, OR8(Register.B)
        ], null, true);
    instructions[0xF9] = Instruction(InstructionType.Set, [
            PIMM8(7), IX, OR8(Register.C)
        ], null, true);
    instructions[0xFA] = Instruction(InstructionType.Set, [
            PIMM8(7), IX, OR8(Register.D)
        ], null, true);
    instructions[0xFB] = Instruction(InstructionType.Set, [
            PIMM8(7), IX, OR8(Register.E)
        ], null, true);
    instructions[0xFC] = Instruction(InstructionType.Set, [
            PIMM8(7), IX, OR8(Register.H)
        ], null, true);
    instructions[0xFD] = Instruction(InstructionType.Set, [
            PIMM8(7), IX, OR8(Register.L)
        ], null, true);
    instructions[0xFE] = Instruction(InstructionType.Set, [PIMM8(7), IX], null, true);
    instructions[0xFF] = Instruction(InstructionType.Set, [
            PIMM8(7), IX, OR8(Register.A)
        ], null, true);
    return instructions;
}

private Instruction[ubyte] IyBitIndirection() {
    Instruction[ubyte] instructions;
    instructions[0x00] = Instruction(InstructionType.Rlc, [IY, OR8(Register.B)], null, true);
    instructions[0x01] = Instruction(InstructionType.Rlc, [IY, OR8(Register.C)], null, true);
    instructions[0x02] = Instruction(InstructionType.Rlc, [IY, OR8(Register.D)], null, true);
    instructions[0x03] = Instruction(InstructionType.Rlc, [IY, OR8(Register.E)], null, true);
    instructions[0x04] = Instruction(InstructionType.Rlc, [IY, OR8(Register.H)], null, true);
    instructions[0x05] = Instruction(InstructionType.Rlc, [IY, OR8(Register.L)], null, true);
    instructions[0x06] = Instruction(InstructionType.Rlc, [IY], null, true);
    instructions[0x07] = Instruction(InstructionType.Rlc, [IY, OR8(Register.A)], null, true);
    instructions[0x08] = Instruction(InstructionType.Rrc, [IY, OR8(Register.B)], null, true);
    instructions[0x09] = Instruction(InstructionType.Rrc, [IY, OR8(Register.C)], null, true);
    instructions[0x0A] = Instruction(InstructionType.Rrc, [IY, OR8(Register.D)], null, true);
    instructions[0x0B] = Instruction(InstructionType.Rrc, [IY, OR8(Register.E)], null, true);
    instructions[0x0C] = Instruction(InstructionType.Rrc, [IY, OR8(Register.H)], null, true);
    instructions[0x0D] = Instruction(InstructionType.Rrc, [IY, OR8(Register.L)], null, true);
    instructions[0x0E] = Instruction(InstructionType.Rrc, [IY], null, true);
    instructions[0x0F] = Instruction(InstructionType.Rrc, [IY, OR8(Register.A)], null, true);
    instructions[0x10] = Instruction(InstructionType.Rl, [IY, OR8(Register.B)], null, true);
    instructions[0x11] = Instruction(InstructionType.Rl, [IY, OR8(Register.C)], null, true);
    instructions[0x12] = Instruction(InstructionType.Rl, [IY, OR8(Register.D)], null, true);
    instructions[0x13] = Instruction(InstructionType.Rl, [IY, OR8(Register.E)], null, true);
    instructions[0x14] = Instruction(InstructionType.Rl, [IY, OR8(Register.H)], null, true);
    instructions[0x15] = Instruction(InstructionType.Rl, [IY, OR8(Register.L)], null, true);
    instructions[0x16] = Instruction(InstructionType.Rl, [IY], null, true);
    instructions[0x17] = Instruction(InstructionType.Rl, [IY, OR8(Register.A)], null, true);
    instructions[0x18] = Instruction(InstructionType.Rr, [IY, OR8(Register.B)], null, true);
    instructions[0x19] = Instruction(InstructionType.Rr, [IY, OR8(Register.C)], null, true);
    instructions[0x1A] = Instruction(InstructionType.Rr, [IY, OR8(Register.D)], null, true);
    instructions[0x1B] = Instruction(InstructionType.Rr, [IY, OR8(Register.E)], null, true);
    instructions[0x1C] = Instruction(InstructionType.Rr, [IY, OR8(Register.H)], null, true);
    instructions[0x1D] = Instruction(InstructionType.Rr, [IY, OR8(Register.L)], null, true);
    instructions[0x1E] = Instruction(InstructionType.Rr, [IY], null, true);
    instructions[0x1F] = Instruction(InstructionType.Rr, [IY, OR8(Register.A)], null, true);
    instructions[0x20] = Instruction(InstructionType.Sla, [IY, OR8(Register.B)], null, true);
    instructions[0x21] = Instruction(InstructionType.Sla, [IY, OR8(Register.C)], null, true);
    instructions[0x22] = Instruction(InstructionType.Sla, [IY, OR8(Register.D)], null, true);
    instructions[0x23] = Instruction(InstructionType.Sla, [IY, OR8(Register.E)], null, true);
    instructions[0x24] = Instruction(InstructionType.Sla, [IY, OR8(Register.H)], null, true);
    instructions[0x25] = Instruction(InstructionType.Sla, [IY, OR8(Register.L)], null, true);
    instructions[0x26] = Instruction(InstructionType.Sla, [IY], null, true);
    instructions[0x27] = Instruction(InstructionType.Sla, [IY, OR8(Register.A)], null, true);
    instructions[0x28] = Instruction(InstructionType.Sra, [IY, OR8(Register.B)], null, true);
    instructions[0x29] = Instruction(InstructionType.Sra, [IY, OR8(Register.C)], null, true);
    instructions[0x2A] = Instruction(InstructionType.Sra, [IY, OR8(Register.D)], null, true);
    instructions[0x2B] = Instruction(InstructionType.Sra, [IY, OR8(Register.E)], null, true);
    instructions[0x2C] = Instruction(InstructionType.Sra, [IY, OR8(Register.H)], null, true);
    instructions[0x2D] = Instruction(InstructionType.Sra, [IY, OR8(Register.L)], null, true);
    instructions[0x2E] = Instruction(InstructionType.Sra, [IY], null, true);
    instructions[0x2F] = Instruction(InstructionType.Sra, [IY, OR8(Register.A)], null, true);
    instructions[0x30] = Instruction(InstructionType.Sll, [IY, OR8(Register.B)], null, true);
    instructions[0x31] = Instruction(InstructionType.Sll, [IY, OR8(Register.C)], null, true);
    instructions[0x32] = Instruction(InstructionType.Sll, [IY, OR8(Register.D)], null, true);
    instructions[0x33] = Instruction(InstructionType.Sll, [IY, OR8(Register.E)], null, true);
    instructions[0x34] = Instruction(InstructionType.Sll, [IY, OR8(Register.H)], null, true);
    instructions[0x35] = Instruction(InstructionType.Sll, [IY, OR8(Register.L)], null, true);
    instructions[0x36] = Instruction(InstructionType.Sll, [IY], null, true);
    instructions[0x37] = Instruction(InstructionType.Sll, [IY, OR8(Register.A)], null, true);
    instructions[0x38] = Instruction(InstructionType.Srl, [IY, OR8(Register.B)], null, true);
    instructions[0x39] = Instruction(InstructionType.Srl, [IY, OR8(Register.C)], null, true);
    instructions[0x3A] = Instruction(InstructionType.Srl, [IY, OR8(Register.D)], null, true);
    instructions[0x3B] = Instruction(InstructionType.Srl, [IY, OR8(Register.E)], null, true);
    instructions[0x3C] = Instruction(InstructionType.Srl, [IY, OR8(Register.H)], null, true);
    instructions[0x3D] = Instruction(InstructionType.Srl, [IY, OR8(Register.L)], null, true);
    instructions[0x3E] = Instruction(InstructionType.Srl, [IY], null, true);
    instructions[0x3F] = Instruction(InstructionType.Srl, [IY, OR8(Register.A)], null, true);
    instructions[0x40] = Instruction(InstructionType.Bit, [PIMM8(0), IY], null, true);
    instructions[0x41] = Instruction(InstructionType.Bit, [PIMM8(0), IY], null, true);
    instructions[0x42] = Instruction(InstructionType.Bit, [PIMM8(0), IY], null, true);
    instructions[0x43] = Instruction(InstructionType.Bit, [PIMM8(0), IY], null, true);
    instructions[0x44] = Instruction(InstructionType.Bit, [PIMM8(0), IY], null, true);
    instructions[0x45] = Instruction(InstructionType.Bit, [PIMM8(0), IY], null, true);
    instructions[0x46] = Instruction(InstructionType.Bit, [PIMM8(0), IY], null, true);
    instructions[0x47] = Instruction(InstructionType.Bit, [PIMM8(0), IY], null, true);
    instructions[0x48] = Instruction(InstructionType.Bit, [PIMM8(1), IY], null, true);
    instructions[0x49] = Instruction(InstructionType.Bit, [PIMM8(1), IY], null, true);
    instructions[0x4A] = Instruction(InstructionType.Bit, [PIMM8(1), IY], null, true);
    instructions[0x4B] = Instruction(InstructionType.Bit, [PIMM8(1), IY], null, true);
    instructions[0x4C] = Instruction(InstructionType.Bit, [PIMM8(1), IY], null, true);
    instructions[0x4D] = Instruction(InstructionType.Bit, [PIMM8(1), IY], null, true);
    instructions[0x4E] = Instruction(InstructionType.Bit, [PIMM8(1), IY], null, true);
    instructions[0x4F] = Instruction(InstructionType.Bit, [PIMM8(1), IY], null, true);
    instructions[0x50] = Instruction(InstructionType.Bit, [PIMM8(2), IY], null, true);
    instructions[0x51] = Instruction(InstructionType.Bit, [PIMM8(2), IY], null, true);
    instructions[0x52] = Instruction(InstructionType.Bit, [PIMM8(2), IY], null, true);
    instructions[0x53] = Instruction(InstructionType.Bit, [PIMM8(2), IY], null, true);
    instructions[0x54] = Instruction(InstructionType.Bit, [PIMM8(2), IY], null, true);
    instructions[0x55] = Instruction(InstructionType.Bit, [PIMM8(2), IY], null, true);
    instructions[0x56] = Instruction(InstructionType.Bit, [PIMM8(2), IY], null, true);
    instructions[0x57] = Instruction(InstructionType.Bit, [PIMM8(2), IY], null, true);
    instructions[0x58] = Instruction(InstructionType.Bit, [PIMM8(3), IY], null, true);
    instructions[0x59] = Instruction(InstructionType.Bit, [PIMM8(3), IY], null, true);
    instructions[0x5A] = Instruction(InstructionType.Bit, [PIMM8(3), IY], null, true);
    instructions[0x5B] = Instruction(InstructionType.Bit, [PIMM8(3), IY], null, true);
    instructions[0x5C] = Instruction(InstructionType.Bit, [PIMM8(3), IY], null, true);
    instructions[0x5D] = Instruction(InstructionType.Bit, [PIMM8(3), IY], null, true);
    instructions[0x5E] = Instruction(InstructionType.Bit, [PIMM8(3), IY], null, true);
    instructions[0x5F] = Instruction(InstructionType.Bit, [PIMM8(3), IY], null, true);
    instructions[0x60] = Instruction(InstructionType.Bit, [PIMM8(4), IY], null, true);
    instructions[0x61] = Instruction(InstructionType.Bit, [PIMM8(4), IY], null, true);
    instructions[0x62] = Instruction(InstructionType.Bit, [PIMM8(4), IY], null, true);
    instructions[0x63] = Instruction(InstructionType.Bit, [PIMM8(4), IY], null, true);
    instructions[0x64] = Instruction(InstructionType.Bit, [PIMM8(4), IY], null, true);
    instructions[0x65] = Instruction(InstructionType.Bit, [PIMM8(4), IY], null, true);
    instructions[0x66] = Instruction(InstructionType.Bit, [PIMM8(4), IY], null, true);
    instructions[0x67] = Instruction(InstructionType.Bit, [PIMM8(4), IY], null, true);
    instructions[0x68] = Instruction(InstructionType.Bit, [PIMM8(5), IY], null, true);
    instructions[0x69] = Instruction(InstructionType.Bit, [PIMM8(5), IY], null, true);
    instructions[0x6A] = Instruction(InstructionType.Bit, [PIMM8(5), IY], null, true);
    instructions[0x6B] = Instruction(InstructionType.Bit, [PIMM8(5), IY], null, true);
    instructions[0x6C] = Instruction(InstructionType.Bit, [PIMM8(5), IY], null, true);
    instructions[0x6D] = Instruction(InstructionType.Bit, [PIMM8(5), IY], null, true);
    instructions[0x6E] = Instruction(InstructionType.Bit, [PIMM8(5), IY], null, true);
    instructions[0x6F] = Instruction(InstructionType.Bit, [PIMM8(5), IY], null, true);
    instructions[0x70] = Instruction(InstructionType.Bit, [PIMM8(6), IY], null, true);
    instructions[0x71] = Instruction(InstructionType.Bit, [PIMM8(6), IY], null, true);
    instructions[0x72] = Instruction(InstructionType.Bit, [PIMM8(6), IY], null, true);
    instructions[0x73] = Instruction(InstructionType.Bit, [PIMM8(6), IY], null, true);
    instructions[0x74] = Instruction(InstructionType.Bit, [PIMM8(6), IY], null, true);
    instructions[0x75] = Instruction(InstructionType.Bit, [PIMM8(6), IY], null, true);
    instructions[0x76] = Instruction(InstructionType.Bit, [PIMM8(6), IY], null, true);
    instructions[0x77] = Instruction(InstructionType.Bit, [PIMM8(6), IY], null, true);
    instructions[0x78] = Instruction(InstructionType.Bit, [PIMM8(7), IY], null, true);
    instructions[0x79] = Instruction(InstructionType.Bit, [PIMM8(7), IY], null, true);
    instructions[0x7A] = Instruction(InstructionType.Bit, [PIMM8(7), IY], null, true);
    instructions[0x7B] = Instruction(InstructionType.Bit, [PIMM8(7), IY], null, true);
    instructions[0x7C] = Instruction(InstructionType.Bit, [PIMM8(7), IY], null, true);
    instructions[0x7D] = Instruction(InstructionType.Bit, [PIMM8(7), IY], null, true);
    instructions[0x7E] = Instruction(InstructionType.Bit, [PIMM8(7), IY], null, true);
    instructions[0x7F] = Instruction(InstructionType.Bit, [PIMM8(7), IY], null, true);
    instructions[0x80] = Instruction(InstructionType.Res, [
            PIMM8(0), IY, OR8(Register.B)
        ], null, true);
    instructions[0x81] = Instruction(InstructionType.Res, [
            PIMM8(0), IY, OR8(Register.C)
        ], null, true);
    instructions[0x82] = Instruction(InstructionType.Res, [
            PIMM8(0), IY, OR8(Register.D)
        ], null, true);
    instructions[0x83] = Instruction(InstructionType.Res, [
            PIMM8(0), IY, OR8(Register.E)
        ], null, true);
    instructions[0x84] = Instruction(InstructionType.Res, [
            PIMM8(0), IY, OR8(Register.H)
        ], null, true);
    instructions[0x85] = Instruction(InstructionType.Res, [
            PIMM8(0), IY, OR8(Register.L)
        ], null, true);
    instructions[0x86] = Instruction(InstructionType.Res, [PIMM8(0), IY], null, true);
    instructions[0x87] = Instruction(InstructionType.Res, [
            PIMM8(0), IY, OR8(Register.A)
        ], null, true);
    instructions[0x88] = Instruction(InstructionType.Res, [
            PIMM8(1), IY, OR8(Register.B)
        ], null, true);
    instructions[0x89] = Instruction(InstructionType.Res, [
            PIMM8(1), IY, OR8(Register.C)
        ], null, true);
    instructions[0x8A] = Instruction(InstructionType.Res, [
            PIMM8(1), IY, OR8(Register.D)
        ], null, true);
    instructions[0x8B] = Instruction(InstructionType.Res, [
            PIMM8(1), IY, OR8(Register.E)
        ], null, true);
    instructions[0x8C] = Instruction(InstructionType.Res, [
            PIMM8(1), IY, OR8(Register.H)
        ], null, true);
    instructions[0x8D] = Instruction(InstructionType.Res, [
            PIMM8(1), IY, OR8(Register.L)
        ], null, true);
    instructions[0x8E] = Instruction(InstructionType.Res, [PIMM8(1), IY], null, true);
    instructions[0x8F] = Instruction(InstructionType.Res, [
            PIMM8(1), IY, OR8(Register.A)
        ], null, true);
    instructions[0x90] = Instruction(InstructionType.Res, [
            PIMM8(2), IY, OR8(Register.B)
        ], null, true);
    instructions[0x91] = Instruction(InstructionType.Res, [
            PIMM8(2), IY, OR8(Register.C)
        ], null, true);
    instructions[0x92] = Instruction(InstructionType.Res, [
            PIMM8(2), IY, OR8(Register.D)
        ], null, true);
    instructions[0x93] = Instruction(InstructionType.Res, [
            PIMM8(2), IY, OR8(Register.E)
        ], null, true);
    instructions[0x94] = Instruction(InstructionType.Res, [
            PIMM8(2), IY, OR8(Register.H)
        ], null, true);
    instructions[0x95] = Instruction(InstructionType.Res, [
            PIMM8(2), IY, OR8(Register.L)
        ], null, true);
    instructions[0x96] = Instruction(InstructionType.Res, [PIMM8(2), IY], null, true);
    instructions[0x97] = Instruction(InstructionType.Res, [
            PIMM8(2), IY, OR8(Register.A)
        ], null, true);
    instructions[0x98] = Instruction(InstructionType.Res, [
            PIMM8(3), IY, OR8(Register.B)
        ], null, true);
    instructions[0x99] = Instruction(InstructionType.Res, [
            PIMM8(3), IY, OR8(Register.C)
        ], null, true);
    instructions[0x9A] = Instruction(InstructionType.Res, [
            PIMM8(3), IY, OR8(Register.D)
        ], null, true);
    instructions[0x9B] = Instruction(InstructionType.Res, [
            PIMM8(3), IY, OR8(Register.E)
        ], null, true);
    instructions[0x9C] = Instruction(InstructionType.Res, [
            PIMM8(3), IY, OR8(Register.H)
        ], null, true);
    instructions[0x9D] = Instruction(InstructionType.Res, [
            PIMM8(3), IY, OR8(Register.L)
        ], null, true);
    instructions[0x9E] = Instruction(InstructionType.Res, [PIMM8(3), IY], null, true);
    instructions[0x9F] = Instruction(InstructionType.Res, [
            PIMM8(3), IY, OR8(Register.A)
        ], null, true);
    instructions[0xA0] = Instruction(InstructionType.Res, [
            PIMM8(4), IY, OR8(Register.B)
        ], null, true);
    instructions[0xA1] = Instruction(InstructionType.Res, [
            PIMM8(4), IY, OR8(Register.C)
        ], null, true);
    instructions[0xA2] = Instruction(InstructionType.Res, [
            PIMM8(4), IY, OR8(Register.D)
        ], null, true);
    instructions[0xA3] = Instruction(InstructionType.Res, [
            PIMM8(4), IY, OR8(Register.E)
        ], null, true);
    instructions[0xA4] = Instruction(InstructionType.Res, [
            PIMM8(4), IY, OR8(Register.H)
        ], null, true);
    instructions[0xA5] = Instruction(InstructionType.Res, [
            PIMM8(4), IY, OR8(Register.L)
        ], null, true);
    instructions[0xA6] = Instruction(InstructionType.Res, [PIMM8(4), IY], null, true);
    instructions[0xA7] = Instruction(InstructionType.Res, [
            PIMM8(4), IY, OR8(Register.A)
        ], null, true);
    instructions[0xA8] = Instruction(InstructionType.Res, [
            PIMM8(5), IY, OR8(Register.B)
        ], null, true);
    instructions[0xA9] = Instruction(InstructionType.Res, [
            PIMM8(5), IY, OR8(Register.C)
        ], null, true);
    instructions[0xAA] = Instruction(InstructionType.Res, [
            PIMM8(5), IY, OR8(Register.D)
        ], null, true);
    instructions[0xAB] = Instruction(InstructionType.Res, [
            PIMM8(5), IY, OR8(Register.E)
        ], null, true);
    instructions[0xAC] = Instruction(InstructionType.Res, [
            PIMM8(5), IY, OR8(Register.H)
        ], null, true);
    instructions[0xAD] = Instruction(InstructionType.Res, [
            PIMM8(5), IY, OR8(Register.L)
        ], null, true);
    instructions[0xAE] = Instruction(InstructionType.Res, [PIMM8(5), IY], null, true);
    instructions[0xAF] = Instruction(InstructionType.Res, [
            PIMM8(5), IY, OR8(Register.A)
        ], null, true);
    instructions[0xB0] = Instruction(InstructionType.Res, [
            PIMM8(6), IY, OR8(Register.B)
        ], null, true);
    instructions[0xB1] = Instruction(InstructionType.Res, [
            PIMM8(6), IY, OR8(Register.C)
        ], null, true);
    instructions[0xB2] = Instruction(InstructionType.Res, [
            PIMM8(6), IY, OR8(Register.D)
        ], null, true);
    instructions[0xB3] = Instruction(InstructionType.Res, [
            PIMM8(6), IY, OR8(Register.E)
        ], null, true);
    instructions[0xB4] = Instruction(InstructionType.Res, [
            PIMM8(6), IY, OR8(Register.H)
        ], null, true);
    instructions[0xB5] = Instruction(InstructionType.Res, [
            PIMM8(6), IY, OR8(Register.L)
        ], null, true);
    instructions[0xB6] = Instruction(InstructionType.Res, [PIMM8(6), IY], null, true);
    instructions[0xB7] = Instruction(InstructionType.Res, [
            PIMM8(6), IY, OR8(Register.A)
        ], null, true);
    instructions[0xB8] = Instruction(InstructionType.Res, [
            PIMM8(7), IY, OR8(Register.B)
        ], null, true);
    instructions[0xB9] = Instruction(InstructionType.Res, [
            PIMM8(7), IY, OR8(Register.C)
        ], null, true);
    instructions[0xBA] = Instruction(InstructionType.Res, [
            PIMM8(7), IY, OR8(Register.D)
        ], null, true);
    instructions[0xBB] = Instruction(InstructionType.Res, [
            PIMM8(7), IY, OR8(Register.E)
        ], null, true);
    instructions[0xBC] = Instruction(InstructionType.Res, [
            PIMM8(7), IY, OR8(Register.H)
        ], null, true);
    instructions[0xBD] = Instruction(InstructionType.Res, [
            PIMM8(7), IY, OR8(Register.L)
        ], null, true);
    instructions[0xBE] = Instruction(InstructionType.Res, [PIMM8(7), IY], null, true);
    instructions[0xBF] = Instruction(InstructionType.Res, [
            PIMM8(7), IY, OR8(Register.A)
        ], null, true);
    instructions[0xC0] = Instruction(InstructionType.Set, [
            PIMM8(0), IY, OR8(Register.B)
        ], null, true);
    instructions[0xC1] = Instruction(InstructionType.Set, [
            PIMM8(0), IY, OR8(Register.C)
        ], null, true);
    instructions[0xC2] = Instruction(InstructionType.Set, [
            PIMM8(0), IY, OR8(Register.D)
        ], null, true);
    instructions[0xC3] = Instruction(InstructionType.Set, [
            PIMM8(0), IY, OR8(Register.E)
        ], null, true);
    instructions[0xC4] = Instruction(InstructionType.Set, [
            PIMM8(0), IY, OR8(Register.H)
        ], null, true);
    instructions[0xC5] = Instruction(InstructionType.Set, [
            PIMM8(0), IY, OR8(Register.L)
        ], null, true);
    instructions[0xC6] = Instruction(InstructionType.Set, [PIMM8(0), IY], null, true);
    instructions[0xC7] = Instruction(InstructionType.Set, [
            PIMM8(0), IY, OR8(Register.A)
        ], null, true);
    instructions[0xC8] = Instruction(InstructionType.Set, [
            PIMM8(1), IY, OR8(Register.B)
        ], null, true);
    instructions[0xC9] = Instruction(InstructionType.Set, [
            PIMM8(1), IY, OR8(Register.C)
        ], null, true);
    instructions[0xCA] = Instruction(InstructionType.Set, [
            PIMM8(1), IY, OR8(Register.D)
        ], null, true);
    instructions[0xCB] = Instruction(InstructionType.Set, [
            PIMM8(1), IY, OR8(Register.E)
        ], null, true);
    instructions[0xCC] = Instruction(InstructionType.Set, [
            PIMM8(1), IY, OR8(Register.H)
        ], null, true);
    instructions[0xCD] = Instruction(InstructionType.Set, [
            PIMM8(1), IY, OR8(Register.L)
        ], null, true);
    instructions[0xCE] = Instruction(InstructionType.Set, [PIMM8(1), IY], null, true);
    instructions[0xCF] = Instruction(InstructionType.Set, [
            PIMM8(1), IY, OR8(Register.A)
        ], null, true);
    instructions[0xD0] = Instruction(InstructionType.Set, [
            PIMM8(2), IY, OR8(Register.B)
        ], null, true);
    instructions[0xD1] = Instruction(InstructionType.Set, [
            PIMM8(2), IY, OR8(Register.C)
        ], null, true);
    instructions[0xD2] = Instruction(InstructionType.Set, [
            PIMM8(2), IY, OR8(Register.D)
        ], null, true);
    instructions[0xD3] = Instruction(InstructionType.Set, [
            PIMM8(2), IY, OR8(Register.E)
        ], null, true);
    instructions[0xD4] = Instruction(InstructionType.Set, [
            PIMM8(2), IY, OR8(Register.H)
        ], null, true);
    instructions[0xD5] = Instruction(InstructionType.Set, [
            PIMM8(2), IY, OR8(Register.L)
        ], null, true);
    instructions[0xD6] = Instruction(InstructionType.Set, [PIMM8(2), IY], null, true);
    instructions[0xD7] = Instruction(InstructionType.Set, [
            PIMM8(2), IY, OR8(Register.A)
        ], null, true);
    instructions[0xD8] = Instruction(InstructionType.Set, [
            PIMM8(3), IY, OR8(Register.B)
        ], null, true);
    instructions[0xD9] = Instruction(InstructionType.Set, [
            PIMM8(3), IY, OR8(Register.C)
        ], null, true);
    instructions[0xDA] = Instruction(InstructionType.Set, [
            PIMM8(3), IY, OR8(Register.D)
        ], null, true);
    instructions[0xDB] = Instruction(InstructionType.Set, [
            PIMM8(3), IY, OR8(Register.E)
        ], null, true);
    instructions[0xDC] = Instruction(InstructionType.Set, [
            PIMM8(3), IY, OR8(Register.H)
        ], null, true);
    instructions[0xDD] = Instruction(InstructionType.Set, [
            PIMM8(3), IY, OR8(Register.L)
        ], null, true);
    instructions[0xDE] = Instruction(InstructionType.Set, [PIMM8(3), IY], null, true);
    instructions[0xDF] = Instruction(InstructionType.Set, [
            PIMM8(3), IY, OR8(Register.A)
        ], null, true);
    instructions[0xE0] = Instruction(InstructionType.Set, [
            PIMM8(4), IY, OR8(Register.B)
        ], null, true);
    instructions[0xE1] = Instruction(InstructionType.Set, [
            PIMM8(4), IY, OR8(Register.C)
        ], null, true);
    instructions[0xE2] = Instruction(InstructionType.Set, [
            PIMM8(4), IY, OR8(Register.D)
        ], null, true);
    instructions[0xE3] = Instruction(InstructionType.Set, [
            PIMM8(4), IY, OR8(Register.E)
        ], null, true);
    instructions[0xE4] = Instruction(InstructionType.Set, [
            PIMM8(4), IY, OR8(Register.H)
        ], null, true);
    instructions[0xE5] = Instruction(InstructionType.Set, [
            PIMM8(4), IY, OR8(Register.L)
        ], null, true);
    instructions[0xE6] = Instruction(InstructionType.Set, [PIMM8(4), IY], null, true);
    instructions[0xE7] = Instruction(InstructionType.Set, [
            PIMM8(4), IY, OR8(Register.A)
        ], null, true);
    instructions[0xE8] = Instruction(InstructionType.Set, [
            PIMM8(5), IY, OR8(Register.B)
        ], null, true);
    instructions[0xE9] = Instruction(InstructionType.Set, [
            PIMM8(5), IY, OR8(Register.C)
        ], null, true);
    instructions[0xEA] = Instruction(InstructionType.Set, [
            PIMM8(5), IY, OR8(Register.D)
        ], null, true);
    instructions[0xEB] = Instruction(InstructionType.Set, [
            PIMM8(5), IY, OR8(Register.E)
        ], null, true);
    instructions[0xEC] = Instruction(InstructionType.Set, [
            PIMM8(5), IY, OR8(Register.H)
        ], null, true);
    instructions[0xED] = Instruction(InstructionType.Set, [
            PIMM8(5), IY, OR8(Register.L)
        ], null, true);
    instructions[0xEE] = Instruction(InstructionType.Set, [PIMM8(5), IY], null, true);
    instructions[0xEF] = Instruction(InstructionType.Set, [
            PIMM8(5), IY, OR8(Register.A)
        ], null, true);
    instructions[0xF0] = Instruction(InstructionType.Set, [
            PIMM8(6), IY, OR8(Register.B)
        ], null, true);
    instructions[0xF1] = Instruction(InstructionType.Set, [
            PIMM8(6), IY, OR8(Register.C)
        ], null, true);
    instructions[0xF2] = Instruction(InstructionType.Set, [
            PIMM8(6), IY, OR8(Register.D)
        ], null, true);
    instructions[0xF3] = Instruction(InstructionType.Set, [
            PIMM8(6), IY, OR8(Register.E)
        ], null, true);
    instructions[0xF4] = Instruction(InstructionType.Set, [
            PIMM8(6), IY, OR8(Register.H)
        ], null, true);
    instructions[0xF5] = Instruction(InstructionType.Set, [
            PIMM8(6), IY, OR8(Register.L)
        ], null, true);
    instructions[0xF6] = Instruction(InstructionType.Set, [PIMM8(6), IY], null, true);
    instructions[0xF7] = Instruction(InstructionType.Set, [
            PIMM8(6), IY, OR8(Register.A)
        ], null, true);
    instructions[0xF8] = Instruction(InstructionType.Set, [
            PIMM8(7), IY, OR8(Register.B)
        ], null, true);
    instructions[0xF9] = Instruction(InstructionType.Set, [
            PIMM8(7), IY, OR8(Register.C)
        ], null, true);
    instructions[0xFA] = Instruction(InstructionType.Set, [
            PIMM8(7), IY, OR8(Register.D)
        ], null, true);
    instructions[0xFB] = Instruction(InstructionType.Set, [
            PIMM8(7), IY, OR8(Register.E)
        ], null, true);
    instructions[0xFC] = Instruction(InstructionType.Set, [
            PIMM8(7), IY, OR8(Register.H)
        ], null, true);
    instructions[0xFD] = Instruction(InstructionType.Set, [
            PIMM8(7), IY, OR8(Register.L)
        ], null, true);
    instructions[0xFE] = Instruction(InstructionType.Set, [PIMM8(7), IY], null, true);
    instructions[0xFF] = Instruction(InstructionType.Set, [
            PIMM8(7), IY, OR8(Register.A)
        ], null, true);
    return instructions;
}

private Instruction[ubyte] IxIndirection() {
    Instruction[ubyte] instructions;
    instructions[0xCB] = Instruction(InstructionType.Indirection, [], IxBitIndirection(), true);

    instructions[0x04] = Instruction(InstructionType.Inc, [OR8(Register.B)]);
    instructions[0x05] = Instruction(InstructionType.Dec, [OR8(Register.B)]);
    instructions[0x06] = Instruction(InstructionType.Ld, [OR8(Register.B), IMM8]);
    instructions[0x09] = Instruction(InstructionType.Add, [
            OR16(Register.IX), OR16(Register.BC)
        ]);
    instructions[0x0C] = Instruction(InstructionType.Inc, [OR8(Register.C)]);
    instructions[0x0D] = Instruction(InstructionType.Dec, [OR8(Register.C)]);
    instructions[0x0E] = Instruction(InstructionType.Ld, [OR8(Register.C), IMM8]);
    instructions[0x14] = Instruction(InstructionType.Inc, [OR8(Register.D)]);
    instructions[0x15] = Instruction(InstructionType.Dec, [OR8(Register.D)]);
    instructions[0x16] = Instruction(InstructionType.Ld, [OR8(Register.D), IMM8]);
    instructions[0x19] = Instruction(InstructionType.Add, [
            OR16(Register.IX), OR16(Register.DE)
        ]);
    instructions[0x1C] = Instruction(InstructionType.Inc, [OR8(Register.E)]);
    instructions[0x1D] = Instruction(InstructionType.Dec, [OR8(Register.E)]);
    instructions[0x1E] = Instruction(InstructionType.Ld, [OR8(Register.E), IMM8]);
    instructions[0x21] = Instruction(InstructionType.Ld, [
            OR16(Register.IX), IMM16
        ]);
    instructions[0x22] = Instruction(InstructionType.Ld, [
            IMM16_LK, OR16(Register.IX)
        ]);
    instructions[0x23] = Instruction(InstructionType.Inc, [OR16(Register.IX)]);
    instructions[0x24] = Instruction(InstructionType.Inc, [OR8(Register.IXH)]);
    instructions[0x25] = Instruction(InstructionType.Dec, [OR8(Register.IXH)]);
    instructions[0x26] = Instruction(InstructionType.Ld, [
            OR8(Register.IXH), IMM8
        ]);
    instructions[0x29] = Instruction(InstructionType.Add, [
            OR16(Register.IX), OR16(Register.IX)
        ]);
    instructions[0x2A] = Instruction(InstructionType.Ld, [
            OR16(Register.IX), IMM16_LK
        ]);
    instructions[0x2B] = Instruction(InstructionType.Dec, [OR16(Register.IX)]);
    instructions[0x2C] = Instruction(InstructionType.Inc, [OR8(Register.IXL)]);
    instructions[0x2D] = Instruction(InstructionType.Dec, [OR8(Register.IXL)]);
    instructions[0x2E] = Instruction(InstructionType.Ld, [
            OR8(Register.IXL), IMM8
        ]);
    instructions[0x34] = Instruction(InstructionType.Inc, [IX]);
    instructions[0x35] = Instruction(InstructionType.Dec, [IX]);
    instructions[0x36] = Instruction(InstructionType.Ld, [IX, IMM8]);
    instructions[0x39] = Instruction(InstructionType.Add, [
            OR16(Register.IX), OR16(Register.SP)
        ]);
    instructions[0x3C] = Instruction(InstructionType.Inc, [OR8(Register.A)]);
    instructions[0x3D] = Instruction(InstructionType.Dec, [OR8(Register.A)]);
    instructions[0x3E] = Instruction(InstructionType.Ld, [OR8(Register.A), IMM8]);
    instructions[0x40] = Instruction(InstructionType.Ld, [
            OR8(Register.B), OR8(Register.B)
        ]);
    instructions[0x41] = Instruction(InstructionType.Ld, [
            OR8(Register.B), OR8(Register.C)
        ]);
    instructions[0x42] = Instruction(InstructionType.Ld, [
            OR8(Register.B), OR8(Register.D)
        ]);
    instructions[0x43] = Instruction(InstructionType.Ld, [
            OR8(Register.B), OR8(Register.E)
        ]);
    instructions[0x44] = Instruction(InstructionType.Ld, [
            OR8(Register.B), OR8(Register.IXH)
        ]);
    instructions[0x45] = Instruction(InstructionType.Ld, [
            OR8(Register.B), OR8(Register.IXL)
        ]);
    instructions[0x46] = Instruction(InstructionType.Ld, [OR8(Register.B), IX]);
    instructions[0x47] = Instruction(InstructionType.Ld, [
            OR8(Register.B), OR8(Register.A)
        ]);
    instructions[0x48] = Instruction(InstructionType.Ld, [
            OR8(Register.C), OR8(Register.B)
        ]);
    instructions[0x49] = Instruction(InstructionType.Ld, [
            OR8(Register.C), OR8(Register.C)
        ]);
    instructions[0x4A] = Instruction(InstructionType.Ld, [
            OR8(Register.C), OR8(Register.D)
        ]);
    instructions[0x4B] = Instruction(InstructionType.Ld, [
            OR8(Register.C), OR8(Register.E)
        ]);
    instructions[0x4C] = Instruction(InstructionType.Ld, [
            OR8(Register.C), OR8(Register.IXH)
        ]);
    instructions[0x4D] = Instruction(InstructionType.Ld, [
            OR8(Register.C), OR8(Register.IXL)
        ]);
    instructions[0x4E] = Instruction(InstructionType.Ld, [OR8(Register.C), IX]);
    instructions[0x4F] = Instruction(InstructionType.Ld, [
            OR8(Register.C), OR8(Register.A)
        ]);
    instructions[0x50] = Instruction(InstructionType.Ld, [
            OR8(Register.D), OR8(Register.B)
        ]);
    instructions[0x51] = Instruction(InstructionType.Ld, [
            OR8(Register.D), OR8(Register.C)
        ]);
    instructions[0x52] = Instruction(InstructionType.Ld, [
            OR8(Register.D), OR8(Register.D)
        ]);
    instructions[0x53] = Instruction(InstructionType.Ld, [
            OR8(Register.D), OR8(Register.E)
        ]);
    instructions[0x54] = Instruction(InstructionType.Ld, [
            OR8(Register.D), OR8(Register.IXH)
        ]);
    instructions[0x55] = Instruction(InstructionType.Ld, [
            OR8(Register.D), OR8(Register.IXL)
        ]);
    instructions[0x56] = Instruction(InstructionType.Ld, [OR8(Register.D), IX]);
    instructions[0x57] = Instruction(InstructionType.Ld, [
            OR8(Register.D), OR8(Register.A)
        ]);
    instructions[0x58] = Instruction(InstructionType.Ld, [
            OR8(Register.E), OR8(Register.B)
        ]);
    instructions[0x59] = Instruction(InstructionType.Ld, [
            OR8(Register.E), OR8(Register.C)
        ]);
    instructions[0x5A] = Instruction(InstructionType.Ld, [
            OR8(Register.E), OR8(Register.D)
        ]);
    instructions[0x5B] = Instruction(InstructionType.Ld, [
            OR8(Register.E), OR8(Register.E)
        ]);
    instructions[0x5C] = Instruction(InstructionType.Ld, [
            OR8(Register.E), OR8(Register.IXH)
        ]);
    instructions[0x5D] = Instruction(InstructionType.Ld, [
            OR8(Register.E), OR8(Register.IXL)
        ]);
    instructions[0x5E] = Instruction(InstructionType.Ld, [OR8(Register.E), IX]);
    instructions[0x5F] = Instruction(InstructionType.Ld, [
            OR8(Register.E), OR8(Register.A)
        ]);
    instructions[0x60] = Instruction(InstructionType.Ld, [
            OR8(Register.IXH), OR8(Register.B)
        ]);
    instructions[0x61] = Instruction(InstructionType.Ld, [
            OR8(Register.IXH), OR8(Register.C)
        ]);
    instructions[0x62] = Instruction(InstructionType.Ld, [
            OR8(Register.IXH), OR8(Register.D)
        ]);
    instructions[0x63] = Instruction(InstructionType.Ld, [
            OR8(Register.IXH), OR8(Register.E)
        ]);
    instructions[0x64] = Instruction(InstructionType.Ld, [
            OR8(Register.IXH), OR8(Register.IXH)
        ]);
    instructions[0x65] = Instruction(InstructionType.Ld, [
            OR8(Register.IXH), OR8(Register.IXL)
        ]);
    instructions[0x66] = Instruction(InstructionType.Ld, [OR8(Register.H), IX]);
    instructions[0x67] = Instruction(InstructionType.Ld, [
            OR8(Register.IXH), OR8(Register.A)
        ]);
    instructions[0x68] = Instruction(InstructionType.Ld, [
            OR8(Register.IXL), OR8(Register.B)
        ]);
    instructions[0x69] = Instruction(InstructionType.Ld, [
            OR8(Register.IXL), OR8(Register.C)
        ]);
    instructions[0x6A] = Instruction(InstructionType.Ld, [
            OR8(Register.IXL), OR8(Register.D)
        ]);
    instructions[0x6B] = Instruction(InstructionType.Ld, [
            OR8(Register.IXL), OR8(Register.E)
        ]);
    instructions[0x6C] = Instruction(InstructionType.Ld, [
            OR8(Register.IXL), OR8(Register.IXH)
        ]);
    instructions[0x6D] = Instruction(InstructionType.Ld, [
            OR8(Register.IXL), OR8(Register.IXL)
        ]);
    instructions[0x6E] = Instruction(InstructionType.Ld, [OR8(Register.L), IX]);
    instructions[0x6F] = Instruction(InstructionType.Ld, [
            OR8(Register.IXL), OR8(Register.A)
        ]);
    instructions[0x70] = Instruction(InstructionType.Ld, [IX, OR8(Register.B)]);
    instructions[0x71] = Instruction(InstructionType.Ld, [IX, OR8(Register.C)]);
    instructions[0x72] = Instruction(InstructionType.Ld, [IX, OR8(Register.D)]);
    instructions[0x73] = Instruction(InstructionType.Ld, [IX, OR8(Register.E)]);
    instructions[0x74] = Instruction(InstructionType.Ld, [IX, OR8(Register.H)]);
    instructions[0x75] = Instruction(InstructionType.Ld, [IX, OR8(Register.L)]);
    instructions[0x77] = Instruction(InstructionType.Ld, [IX, OR8(Register.A)]);
    instructions[0x78] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.B)
        ]);
    instructions[0x79] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.C)
        ]);
    instructions[0x7A] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.D)
        ]);
    instructions[0x7B] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.E)
        ]);
    instructions[0x7C] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.IXH)
        ]);
    instructions[0x7D] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.IXL)
        ]);
    instructions[0x7E] = Instruction(InstructionType.Ld, [OR8(Register.A), IX]);
    instructions[0x7F] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.A)
        ]);
    instructions[0x80] = Instruction(InstructionType.Add, [
            OR8(Register.A), OR8(Register.B)
        ]);
    instructions[0x81] = Instruction(InstructionType.Add, [
            OR8(Register.A), OR8(Register.C)
        ]);
    instructions[0x82] = Instruction(InstructionType.Add, [
            OR8(Register.A), OR8(Register.D)
        ]);
    instructions[0x83] = Instruction(InstructionType.Add, [
            OR8(Register.A), OR8(Register.E)
        ]);
    instructions[0x84] = Instruction(InstructionType.Add, [
            OR8(Register.A), OR8(Register.IXH)
        ]);
    instructions[0x85] = Instruction(InstructionType.Add, [
            OR8(Register.A), OR8(Register.IXL)
        ]);
    instructions[0x86] = Instruction(InstructionType.Add, [OR8(Register.A), IX]);
    instructions[0x87] = Instruction(InstructionType.Add, [
            OR8(Register.A), OR8(Register.A)
        ]);
    instructions[0x88] = Instruction(InstructionType.Adc, [
            OR8(Register.A), OR8(Register.B)
        ]);
    instructions[0x89] = Instruction(InstructionType.Adc, [
            OR8(Register.A), OR8(Register.C)
        ]);
    instructions[0x8A] = Instruction(InstructionType.Adc, [
            OR8(Register.A), OR8(Register.D)
        ]);
    instructions[0x8B] = Instruction(InstructionType.Adc, [
            OR8(Register.A), OR8(Register.E)
        ]);
    instructions[0x8C] = Instruction(InstructionType.Adc, [
            OR8(Register.A), OR8(Register.IXH)
        ]);
    instructions[0x8D] = Instruction(InstructionType.Adc, [
            OR8(Register.A), OR8(Register.IXL)
        ]);
    instructions[0x8E] = Instruction(InstructionType.Adc, [OR8(Register.A), IX]);
    instructions[0x8F] = Instruction(InstructionType.Adc, [
            OR8(Register.A), OR8(Register.A)
        ]);
    instructions[0x90] = Instruction(InstructionType.Sub, [OR8(Register.B)]);
    instructions[0x91] = Instruction(InstructionType.Sub, [OR8(Register.C)]);
    instructions[0x92] = Instruction(InstructionType.Sub, [OR8(Register.D)]);
    instructions[0x93] = Instruction(InstructionType.Sub, [OR8(Register.E)]);
    instructions[0x94] = Instruction(InstructionType.Sub, [OR8(Register.IXH)]);
    instructions[0x95] = Instruction(InstructionType.Sub, [OR8(Register.IXL)]);
    instructions[0x96] = Instruction(InstructionType.Sub, [IX]);
    instructions[0x97] = Instruction(InstructionType.Sub, [OR8(Register.A)]);
    instructions[0x98] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), OR8(Register.B)
        ]);
    instructions[0x99] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), OR8(Register.C)
        ]);
    instructions[0x9A] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), OR8(Register.D)
        ]);
    instructions[0x9B] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), OR8(Register.E)
        ]);
    instructions[0x9C] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), OR8(Register.IXH)
        ]);
    instructions[0x9D] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), OR8(Register.IXL)
        ]);
    instructions[0x9E] = Instruction(InstructionType.Sbc, [OR8(Register.A), IX]);
    instructions[0x9F] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), OR8(Register.A)
        ]);
    instructions[0xA0] = Instruction(InstructionType.And, [OR8(Register.B)]);
    instructions[0xA1] = Instruction(InstructionType.And, [OR8(Register.C)]);
    instructions[0xA2] = Instruction(InstructionType.And, [OR8(Register.D)]);
    instructions[0xA3] = Instruction(InstructionType.And, [OR8(Register.E)]);
    instructions[0xA4] = Instruction(InstructionType.And, [OR8(Register.IXH)]);
    instructions[0xA5] = Instruction(InstructionType.And, [OR8(Register.IXL)]);
    instructions[0xA6] = Instruction(InstructionType.And, [IX]);
    instructions[0xA7] = Instruction(InstructionType.And, [OR8(Register.A)]);
    instructions[0xA8] = Instruction(InstructionType.Xor, [OR8(Register.B)]);
    instructions[0xA9] = Instruction(InstructionType.Xor, [OR8(Register.C)]);
    instructions[0xAA] = Instruction(InstructionType.Xor, [OR8(Register.D)]);
    instructions[0xAB] = Instruction(InstructionType.Xor, [OR8(Register.E)]);
    instructions[0xAC] = Instruction(InstructionType.Xor, [OR8(Register.IXH)]);
    instructions[0xAD] = Instruction(InstructionType.Xor, [OR8(Register.IXL)]);
    instructions[0xAE] = Instruction(InstructionType.Xor, [IX]);
    instructions[0xAF] = Instruction(InstructionType.Xor, [OR8(Register.A)]);
    instructions[0xB0] = Instruction(InstructionType.Or, [OR8(Register.B)]);
    instructions[0xB1] = Instruction(InstructionType.Or, [OR8(Register.C)]);
    instructions[0xB2] = Instruction(InstructionType.Or, [OR8(Register.D)]);
    instructions[0xB3] = Instruction(InstructionType.Or, [OR8(Register.E)]);
    instructions[0xB4] = Instruction(InstructionType.Or, [OR8(Register.IXH)]);
    instructions[0xB5] = Instruction(InstructionType.Or, [OR8(Register.IXL)]);
    instructions[0xB6] = Instruction(InstructionType.Or, [IX]);
    instructions[0xB7] = Instruction(InstructionType.Or, [OR8(Register.A)]);
    instructions[0xB8] = Instruction(InstructionType.Cp, [OR8(Register.B)]);
    instructions[0xB9] = Instruction(InstructionType.Cp, [OR8(Register.C)]);
    instructions[0xBA] = Instruction(InstructionType.Cp, [OR8(Register.D)]);
    instructions[0xBB] = Instruction(InstructionType.Cp, [OR8(Register.E)]);
    instructions[0xBC] = Instruction(InstructionType.Cp, [OR8(Register.IXH)]);
    instructions[0xBD] = Instruction(InstructionType.Cp, [OR8(Register.IXL)]);
    instructions[0xBE] = Instruction(InstructionType.Cp, [IX]);
    instructions[0xBF] = Instruction(InstructionType.Cp, [OR8(Register.A)]);
    instructions[0xE1] = Instruction(InstructionType.Pop, [OR16(Register.IX)]);
    instructions[0xE3] = Instruction(InstructionType.Ex, [
            OR16_LK(Register.SP), OR16(Register.IX)
        ]);
    instructions[0xE5] = Instruction(InstructionType.Push, [OR16(Register.IX)]);
    instructions[0xE9] = Instruction(InstructionType.Jp, [OR16_LK(Register.IX)]);
    instructions[0xF9] = Instruction(InstructionType.Ld, [
            OR16(Register.SP), OR16(Register.IX)
        ]);
    return instructions;
}

private Instruction[ubyte] MiscIndirection() {
    Instruction[ubyte] instructions;
    instructions[0x00] = Instruction(InstructionType.In0, [
            OR8(Register.B), IMM8_LK
        ]);
    instructions[0x01] = Instruction(InstructionType.Out0, [
            IMM8_LK, OR8(Register.B)
        ]);
    instructions[0x04] = Instruction(InstructionType.Tst, [OR8(Register.B)]);
    instructions[0x08] = Instruction(InstructionType.In0, [
            OR8(Register.C), IMM8_LK
        ]);
    instructions[0x09] = Instruction(InstructionType.Out0, [
            IMM8_LK, OR8(Register.C)
        ]);
    instructions[0x0C] = Instruction(InstructionType.Tst, [OR8(Register.C)]);
    instructions[0x10] = Instruction(InstructionType.In0, [
            OR8(Register.D), IMM8_LK
        ]);
    instructions[0x11] = Instruction(InstructionType.Out0, [
            IMM8_LK, OR8(Register.D)
        ]);
    instructions[0x14] = Instruction(InstructionType.Tst, [OR8(Register.D)]);
    instructions[0x18] = Instruction(InstructionType.In0, [
            OR8(Register.E), IMM8_LK
        ]);
    instructions[0x19] = Instruction(InstructionType.Out0, [
            IMM8_LK, OR8(Register.E)
        ]);
    instructions[0x1C] = Instruction(InstructionType.Tst, [OR8(Register.E)]);
    instructions[0x20] = Instruction(InstructionType.In0, [
            OR8(Register.H), IMM8_LK
        ]);
    instructions[0x21] = Instruction(InstructionType.Out0, [
            IMM8_LK, OR8(Register.H)
        ]);
    instructions[0x24] = Instruction(InstructionType.Tst, [OR8(Register.H)]);
    instructions[0x28] = Instruction(InstructionType.In0, [
            OR8(Register.L), IMM8_LK
        ]);
    instructions[0x29] = Instruction(InstructionType.Out0, [
            IMM8_LK, OR8(Register.L)
        ]);
    instructions[0x2C] = Instruction(InstructionType.Tst, [OR8(Register.L)]);
    instructions[0x34] = Instruction(InstructionType.Tst, [OR16_LK(Register.HL)]);
    instructions[0x38] = Instruction(InstructionType.In0, [
            OR8(Register.A), IMM8_LK
        ]);
    instructions[0x39] = Instruction(InstructionType.Out0, [
            IMM8_LK, OR8(Register.A)
        ]);
    instructions[0x3C] = Instruction(InstructionType.Tst, [OR8(Register.A)]);
    instructions[0x40] = Instruction(InstructionType.In, [
            OR8(Register.B), OR8_LK(Register.C)
        ]);
    instructions[0x41] = Instruction(InstructionType.Out, [
            OR8_LK(Register.C), OR8(Register.B)
        ]);
    instructions[0x42] = Instruction(InstructionType.Sbc, [
            OR16(Register.HL), OR16(Register.BC)
        ]);
    instructions[0x43] = Instruction(InstructionType.Ld, [
            IMM16_LK, OR16(Register.BC)
        ]);
    instructions[0x44] = Instruction(InstructionType.Neg, []);
    instructions[0x45] = Instruction(InstructionType.Retn, []);
    instructions[0x46] = Instruction(InstructionType.Im, [PIMM8(0)]);
    instructions[0x47] = Instruction(InstructionType.Ld, [
            OR8(Register.I), OR8(Register.A)
        ]);
    instructions[0x48] = Instruction(InstructionType.In, [
            OR8(Register.C), OR8_LK(Register.C)
        ]);
    instructions[0x49] = Instruction(InstructionType.Out, [
            OR8_LK(Register.C), OR8(Register.C)
        ]);
    instructions[0x4A] = Instruction(InstructionType.Adc, [
            OR16(Register.HL), OR16(Register.BC)
        ]);
    instructions[0x4B] = Instruction(InstructionType.Ld, [
            OR16(Register.BC), IMM16_LK
        ]);
    instructions[0x4C] = Instruction(InstructionType.Mlt, [OR16(Register.BC)]);
    instructions[0x4D] = Instruction(InstructionType.Reti, []);
    instructions[0x4F] = Instruction(InstructionType.Ld, [
            OR8(Register.R), OR8(Register.A)
        ]);
    instructions[0x50] = Instruction(InstructionType.In, [
            OR8(Register.D), OR8_LK(Register.C)
        ]);
    instructions[0x51] = Instruction(InstructionType.Out, [
            OR8_LK(Register.C), OR8(Register.D)
        ]);
    instructions[0x52] = Instruction(InstructionType.Sbc, [
            OR16(Register.HL), OR16(Register.DE)
        ]);
    instructions[0x53] = Instruction(InstructionType.Ld, [
            IMM16_LK, OR16(Register.DE)
        ]);
    instructions[0x56] = Instruction(InstructionType.Im, [PIMM8(1)]);
    instructions[0x57] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.I)
        ]);
    instructions[0x58] = Instruction(InstructionType.In, [
            OR8(Register.E), OR8_LK(Register.C)
        ]);
    instructions[0x59] = Instruction(InstructionType.Out, [
            OR8_LK(Register.C), OR8(Register.E)
        ]);
    instructions[0x5A] = Instruction(InstructionType.Adc, [
            OR16(Register.HL), OR16(Register.DE)
        ]);
    instructions[0x5B] = Instruction(InstructionType.Ld, [
            OR16(Register.DE), IMM16_LK
        ]);
    instructions[0x5C] = Instruction(InstructionType.Mlt, [OR16(Register.DE)]);
    instructions[0x5E] = Instruction(InstructionType.Im, [PIMM8(2)]);
    instructions[0x5F] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.R)
        ]);
    instructions[0x60] = Instruction(InstructionType.In, [
            OR8(Register.H), OR8_LK(Register.C)
        ]);
    instructions[0x61] = Instruction(InstructionType.Out, [
            OR8_LK(Register.C), OR8(Register.H)
        ]);
    instructions[0x62] = Instruction(InstructionType.Sbc, [
            OR16(Register.HL), OR16(Register.HL)
        ]);
    instructions[0x63] = Instruction(InstructionType.Ld, [
            IMM16_LK, OR16(Register.HL)
        ]);
    instructions[0x64] = Instruction(InstructionType.Tst, [IMM8]);
    instructions[0x67] = Instruction(InstructionType.Rrd, []);
    instructions[0x68] = Instruction(InstructionType.In, [
            OR8(Register.L), OR8_LK(Register.C)
        ]);
    instructions[0x69] = Instruction(InstructionType.Out, [
            OR8_LK(Register.C), OR8(Register.L)
        ]);
    instructions[0x6A] = Instruction(InstructionType.Adc, [
            OR16(Register.HL), OR16(Register.HL)
        ]);
    instructions[0x6B] = Instruction(InstructionType.Ld, [
            OR16(Register.HL), IMM16_LK
        ]);
    instructions[0x6C] = Instruction(InstructionType.Mlt, [OR16(Register.HL)]);
    instructions[0x6F] = Instruction(InstructionType.Rld, []);
    instructions[0x70] = Instruction(InstructionType.In, [OR8_LK(Register.C)]);
    instructions[0x71] = Instruction(InstructionType.Out, [
            OR8_LK(Register.C), PIMM8(0)
        ]);
    instructions[0x72] = Instruction(InstructionType.Sbc, [
            OR16(Register.HL), OR16(Register.SP)
        ]);
    instructions[0x73] = Instruction(InstructionType.Ld, [
            IMM16_LK, OR16(Register.SP)
        ]);
    instructions[0x74] = Instruction(InstructionType.Tstio, [IMM8]);
    instructions[0x76] = Instruction(InstructionType.Slp, []);
    instructions[0x78] = Instruction(InstructionType.In, [
            OR8(Register.A), OR8_LK(Register.C)
        ]);
    instructions[0x79] = Instruction(InstructionType.Out, [
            OR8_LK(Register.C), OR8(Register.A)
        ]);
    instructions[0x7A] = Instruction(InstructionType.Adc, [
            OR16(Register.HL), OR16(Register.SP)
        ]);
    instructions[0x7B] = Instruction(InstructionType.Ld, [
            OR16(Register.SP), IMM16_LK
        ]);
    instructions[0x7C] = Instruction(InstructionType.Mlt, [OR16(Register.SP)]);
    instructions[0x83] = Instruction(InstructionType.Otim, []);
    instructions[0x8B] = Instruction(InstructionType.Otdm, []);
    instructions[0x93] = Instruction(InstructionType.Otimr, []);
    instructions[0x9B] = Instruction(InstructionType.Otdmr, []);
    instructions[0xA0] = Instruction(InstructionType.Ldi, []);
    instructions[0xA1] = Instruction(InstructionType.Cpi, []);
    instructions[0xA2] = Instruction(InstructionType.Ini, []);
    instructions[0xA3] = Instruction(InstructionType.Outi, []);
    instructions[0xA8] = Instruction(InstructionType.Ldd, []);
    instructions[0xA9] = Instruction(InstructionType.Cpd, []);
    instructions[0xAA] = Instruction(InstructionType.Ind, []);
    instructions[0xAB] = Instruction(InstructionType.Outd, []);
    instructions[0xB0] = Instruction(InstructionType.Ldir, []);
    instructions[0xB1] = Instruction(InstructionType.Cpir, []);
    instructions[0xB2] = Instruction(InstructionType.Inir, []);
    instructions[0xB3] = Instruction(InstructionType.Otir, []);
    instructions[0xB8] = Instruction(InstructionType.Lddr, []);
    instructions[0xB9] = Instruction(InstructionType.Cpdr, []);
    instructions[0xBA] = Instruction(InstructionType.Indr, []);
    instructions[0xBB] = Instruction(InstructionType.Otdr, []);
    return instructions;
}

private Instruction[ubyte] IyIndirection() {
    Instruction[ubyte] instructions;
    instructions[0xCB] = Instruction(InstructionType.Indirection, [], IyBitIndirection(), true);
    instructions[0x04] = Instruction(InstructionType.Inc, [OR8(Register.B)]);
    instructions[0x05] = Instruction(InstructionType.Dec, [OR8(Register.B)]);
    instructions[0x06] = Instruction(InstructionType.Ld, [OR8(Register.B), IMM8]);
    instructions[0x09] = Instruction(InstructionType.Add, [
            OR16(Register.IY), OR16(Register.BC)
        ]);
    instructions[0x0C] = Instruction(InstructionType.Inc, [OR8(Register.C)]);
    instructions[0x0D] = Instruction(InstructionType.Dec, [OR8(Register.C)]);
    instructions[0x0E] = Instruction(InstructionType.Ld, [OR8(Register.C), IMM8]);
    instructions[0x14] = Instruction(InstructionType.Inc, [OR8(Register.D)]);
    instructions[0x15] = Instruction(InstructionType.Dec, [OR8(Register.D)]);
    instructions[0x16] = Instruction(InstructionType.Ld, [OR8(Register.D), IMM8]);
    instructions[0x19] = Instruction(InstructionType.Add, [
            OR16(Register.IY), OR16(Register.DE)
        ]);
    instructions[0x1C] = Instruction(InstructionType.Inc, [OR8(Register.E)]);
    instructions[0x1D] = Instruction(InstructionType.Dec, [OR8(Register.E)]);
    instructions[0x1E] = Instruction(InstructionType.Ld, [OR8(Register.E), IMM8]);
    instructions[0x21] = Instruction(InstructionType.Ld, [
            OR16(Register.IY), IMM16
        ]);
    instructions[0x22] = Instruction(InstructionType.Ld, [
            IMM16_LK, OR16(Register.IY)
        ]);
    instructions[0x23] = Instruction(InstructionType.Inc, [OR16(Register.IY)]);
    instructions[0x24] = Instruction(InstructionType.Inc, [OR8(Register.IYH)]);
    instructions[0x25] = Instruction(InstructionType.Dec, [OR8(Register.IYH)]);
    instructions[0x26] = Instruction(InstructionType.Ld, [
            OR8(Register.IYH), IMM8
        ]);
    instructions[0x29] = Instruction(InstructionType.Add, [
            OR16(Register.IY), OR16(Register.IY)
        ]);
    instructions[0x2A] = Instruction(InstructionType.Ld, [
            OR16(Register.IY), IMM16_LK
        ]);
    instructions[0x2B] = Instruction(InstructionType.Dec, [OR16(Register.IY)]);
    instructions[0x2C] = Instruction(InstructionType.Inc, [OR8(Register.IYL)]);
    instructions[0x2D] = Instruction(InstructionType.Dec, [OR8(Register.IYL)]);
    instructions[0x2E] = Instruction(InstructionType.Ld, [
            OR8(Register.IYL), IMM8
        ]);
    instructions[0x34] = Instruction(InstructionType.Inc, [IY]);
    instructions[0x35] = Instruction(InstructionType.Dec, [IY]);
    instructions[0x36] = Instruction(InstructionType.Ld, [IY, IMM8]);
    instructions[0x39] = Instruction(InstructionType.Add, [
            OR16(Register.IY), OR16(Register.SP)
        ]);
    instructions[0x3C] = Instruction(InstructionType.Inc, [OR8(Register.A)]);
    instructions[0x3D] = Instruction(InstructionType.Dec, [OR8(Register.A)]);
    instructions[0x3E] = Instruction(InstructionType.Ld, [OR8(Register.A), IMM8]);
    instructions[0x40] = Instruction(InstructionType.Ld, [
            OR8(Register.B), OR8(Register.B)
        ]);
    instructions[0x41] = Instruction(InstructionType.Ld, [
            OR8(Register.B), OR8(Register.C)
        ]);
    instructions[0x42] = Instruction(InstructionType.Ld, [
            OR8(Register.B), OR8(Register.D)
        ]);
    instructions[0x43] = Instruction(InstructionType.Ld, [
            OR8(Register.B), OR8(Register.E)
        ]);
    instructions[0x44] = Instruction(InstructionType.Ld, [
            OR8(Register.B), OR8(Register.IYH)
        ]);
    instructions[0x45] = Instruction(InstructionType.Ld, [
            OR8(Register.B), OR8(Register.IYL)
        ]);
    instructions[0x46] = Instruction(InstructionType.Ld, [OR8(Register.B), IY]);
    instructions[0x47] = Instruction(InstructionType.Ld, [
            OR8(Register.B), OR8(Register.A)
        ]);
    instructions[0x48] = Instruction(InstructionType.Ld, [
            OR8(Register.C), OR8(Register.B)
        ]);
    instructions[0x49] = Instruction(InstructionType.Ld, [
            OR8(Register.C), OR8(Register.C)
        ]);
    instructions[0x4A] = Instruction(InstructionType.Ld, [
            OR8(Register.C), OR8(Register.D)
        ]);
    instructions[0x4B] = Instruction(InstructionType.Ld, [
            OR8(Register.C), OR8(Register.E)
        ]);
    instructions[0x4C] = Instruction(InstructionType.Ld, [
            OR8(Register.C), OR8(Register.IYH)
        ]);
    instructions[0x4D] = Instruction(InstructionType.Ld, [
            OR8(Register.C), OR8(Register.IYL)
        ]);
    instructions[0x4E] = Instruction(InstructionType.Ld, [OR8(Register.C), IY]);
    instructions[0x4F] = Instruction(InstructionType.Ld, [
            OR8(Register.C), OR8(Register.A)
        ]);
    instructions[0x50] = Instruction(InstructionType.Ld, [
            OR8(Register.D), OR8(Register.B)
        ]);
    instructions[0x51] = Instruction(InstructionType.Ld, [
            OR8(Register.D), OR8(Register.C)
        ]);
    instructions[0x52] = Instruction(InstructionType.Ld, [
            OR8(Register.D), OR8(Register.D)
        ]);
    instructions[0x53] = Instruction(InstructionType.Ld, [
            OR8(Register.D), OR8(Register.E)
        ]);
    instructions[0x54] = Instruction(InstructionType.Ld, [
            OR8(Register.D), OR8(Register.IYH)
        ]);
    instructions[0x55] = Instruction(InstructionType.Ld, [
            OR8(Register.D), OR8(Register.IYL)
        ]);
    instructions[0x56] = Instruction(InstructionType.Ld, [OR8(Register.D), IY]);
    instructions[0x57] = Instruction(InstructionType.Ld, [
            OR8(Register.D), OR8(Register.A)
        ]);
    instructions[0x58] = Instruction(InstructionType.Ld, [
            OR8(Register.E), OR8(Register.B)
        ]);
    instructions[0x59] = Instruction(InstructionType.Ld, [
            OR8(Register.E), OR8(Register.C)
        ]);
    instructions[0x5A] = Instruction(InstructionType.Ld, [
            OR8(Register.E), OR8(Register.D)
        ]);
    instructions[0x5B] = Instruction(InstructionType.Ld, [
            OR8(Register.E), OR8(Register.E)
        ]);
    instructions[0x5C] = Instruction(InstructionType.Ld, [
            OR8(Register.E), OR8(Register.IYH)
        ]);
    instructions[0x5D] = Instruction(InstructionType.Ld, [
            OR8(Register.E), OR8(Register.IYL)
        ]);
    instructions[0x5E] = Instruction(InstructionType.Ld, [OR8(Register.E), IY]);
    instructions[0x5F] = Instruction(InstructionType.Ld, [
            OR8(Register.E), OR8(Register.A)
        ]);
    instructions[0x60] = Instruction(InstructionType.Ld, [
            OR8(Register.IYH), OR8(Register.B)
        ]);
    instructions[0x61] = Instruction(InstructionType.Ld, [
            OR8(Register.IYH), OR8(Register.C)
        ]);
    instructions[0x62] = Instruction(InstructionType.Ld, [
            OR8(Register.IYH), OR8(Register.D)
        ]);
    instructions[0x63] = Instruction(InstructionType.Ld, [
            OR8(Register.IYH), OR8(Register.E)
        ]);
    instructions[0x64] = Instruction(InstructionType.Ld, [
            OR8(Register.IYH), OR8(Register.IYH)
        ]);
    instructions[0x65] = Instruction(InstructionType.Ld, [
            OR8(Register.IYH), OR8(Register.IYL)
        ]);
    instructions[0x66] = Instruction(InstructionType.Ld, [OR8(Register.H), IY]);
    instructions[0x67] = Instruction(InstructionType.Ld, [
            OR8(Register.IYH), OR8(Register.A)
        ]);
    instructions[0x68] = Instruction(InstructionType.Ld, [
            OR8(Register.IYL), OR8(Register.B)
        ]);
    instructions[0x69] = Instruction(InstructionType.Ld, [
            OR8(Register.IYL), OR8(Register.C)
        ]);
    instructions[0x6A] = Instruction(InstructionType.Ld, [
            OR8(Register.IYL), OR8(Register.D)
        ]);
    instructions[0x6B] = Instruction(InstructionType.Ld, [
            OR8(Register.IYL), OR8(Register.E)
        ]);
    instructions[0x6C] = Instruction(InstructionType.Ld, [
            OR8(Register.IYL), OR8(Register.IYH)
        ]);
    instructions[0x6D] = Instruction(InstructionType.Ld, [
            OR8(Register.IYL), OR8(Register.IYL)
        ]);
    instructions[0x6E] = Instruction(InstructionType.Ld, [OR8(Register.L), IY]);
    instructions[0x6F] = Instruction(InstructionType.Ld, [
            OR8(Register.IYL), OR8(Register.A)
        ]);
    instructions[0x70] = Instruction(InstructionType.Ld, [IY, OR8(Register.B)]);
    instructions[0x71] = Instruction(InstructionType.Ld, [IY, OR8(Register.C)]);
    instructions[0x72] = Instruction(InstructionType.Ld, [IY, OR8(Register.D)]);
    instructions[0x73] = Instruction(InstructionType.Ld, [IY, OR8(Register.E)]);
    instructions[0x74] = Instruction(InstructionType.Ld, [IY, OR8(Register.H)]);
    instructions[0x75] = Instruction(InstructionType.Ld, [IY, OR8(Register.L)]);
    instructions[0x77] = Instruction(InstructionType.Ld, [IY, OR8(Register.A)]);
    instructions[0x78] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.B)
        ]);
    instructions[0x79] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.C)
        ]);
    instructions[0x7A] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.D)
        ]);
    instructions[0x7B] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.E)
        ]);
    instructions[0x7C] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.IYH)
        ]);
    instructions[0x7D] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.IYL)
        ]);
    instructions[0x7E] = Instruction(InstructionType.Ld, [OR8(Register.A), IY]);
    instructions[0x7F] = Instruction(InstructionType.Ld, [
            OR8(Register.A), OR8(Register.A)
        ]);
    instructions[0x80] = Instruction(InstructionType.Add, [
            OR8(Register.A), OR8(Register.B)
        ]);
    instructions[0x81] = Instruction(InstructionType.Add, [
            OR8(Register.A), OR8(Register.C)
        ]);
    instructions[0x82] = Instruction(InstructionType.Add, [
            OR8(Register.A), OR8(Register.D)
        ]);
    instructions[0x83] = Instruction(InstructionType.Add, [
            OR8(Register.A), OR8(Register.E)
        ]);
    instructions[0x84] = Instruction(InstructionType.Add, [
            OR8(Register.A), OR8(Register.IYH)
        ]);
    instructions[0x85] = Instruction(InstructionType.Add, [
            OR8(Register.A), OR8(Register.IYL)
        ]);
    instructions[0x86] = Instruction(InstructionType.Add, [OR8(Register.A), IY]);
    instructions[0x87] = Instruction(InstructionType.Add, [
            OR8(Register.A), OR8(Register.A)
        ]);
    instructions[0x88] = Instruction(InstructionType.Adc, [
            OR8(Register.A), OR8(Register.B)
        ]);
    instructions[0x89] = Instruction(InstructionType.Adc, [
            OR8(Register.A), OR8(Register.C)
        ]);
    instructions[0x8A] = Instruction(InstructionType.Adc, [
            OR8(Register.A), OR8(Register.D)
        ]);
    instructions[0x8B] = Instruction(InstructionType.Adc, [
            OR8(Register.A), OR8(Register.E)
        ]);
    instructions[0x8C] = Instruction(InstructionType.Adc, [
            OR8(Register.A), OR8(Register.IYH)
        ]);
    instructions[0x8D] = Instruction(InstructionType.Adc, [
            OR8(Register.A), OR8(Register.IYL)
        ]);
    instructions[0x8E] = Instruction(InstructionType.Adc, [OR8(Register.A), IY]);
    instructions[0x8F] = Instruction(InstructionType.Adc, [
            OR8(Register.A), OR8(Register.A)
        ]);
    instructions[0x90] = Instruction(InstructionType.Sub, [OR8(Register.B)]);
    instructions[0x91] = Instruction(InstructionType.Sub, [OR8(Register.C)]);
    instructions[0x92] = Instruction(InstructionType.Sub, [OR8(Register.D)]);
    instructions[0x93] = Instruction(InstructionType.Sub, [OR8(Register.E)]);
    instructions[0x94] = Instruction(InstructionType.Sub, [OR8(Register.IYH)]);
    instructions[0x95] = Instruction(InstructionType.Sub, [OR8(Register.IYL)]);
    instructions[0x96] = Instruction(InstructionType.Sub, [IY]);
    instructions[0x97] = Instruction(InstructionType.Sub, [OR8(Register.A)]);
    instructions[0x98] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), OR8(Register.B)
        ]);
    instructions[0x99] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), OR8(Register.C)
        ]);
    instructions[0x9A] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), OR8(Register.D)
        ]);
    instructions[0x9B] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), OR8(Register.E)
        ]);
    instructions[0x9C] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), OR8(Register.IYH)
        ]);
    instructions[0x9D] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), OR8(Register.IYL)
        ]);
    instructions[0x9E] = Instruction(InstructionType.Sbc, [OR8(Register.A), IY]);
    instructions[0x9F] = Instruction(InstructionType.Sbc, [
            OR8(Register.A), OR8(Register.A)
        ]);
    instructions[0xA0] = Instruction(InstructionType.And, [OR8(Register.B)]);
    instructions[0xA1] = Instruction(InstructionType.And, [OR8(Register.C)]);
    instructions[0xA2] = Instruction(InstructionType.And, [OR8(Register.D)]);
    instructions[0xA3] = Instruction(InstructionType.And, [OR8(Register.E)]);
    instructions[0xA4] = Instruction(InstructionType.And, [OR8(Register.IYH)]);
    instructions[0xA5] = Instruction(InstructionType.And, [OR8(Register.IYL)]);
    instructions[0xA6] = Instruction(InstructionType.And, [IY]);
    instructions[0xA7] = Instruction(InstructionType.And, [OR8(Register.A)]);
    instructions[0xA8] = Instruction(InstructionType.Xor, [OR8(Register.B)]);
    instructions[0xA9] = Instruction(InstructionType.Xor, [OR8(Register.C)]);
    instructions[0xAA] = Instruction(InstructionType.Xor, [OR8(Register.D)]);
    instructions[0xAB] = Instruction(InstructionType.Xor, [OR8(Register.E)]);
    instructions[0xAC] = Instruction(InstructionType.Xor, [OR8(Register.IYH)]);
    instructions[0xAD] = Instruction(InstructionType.Xor, [OR8(Register.IYL)]);
    instructions[0xAE] = Instruction(InstructionType.Xor, [IY]);
    instructions[0xAF] = Instruction(InstructionType.Xor, [OR8(Register.A)]);
    instructions[0xB0] = Instruction(InstructionType.Or, [OR8(Register.B)]);
    instructions[0xB1] = Instruction(InstructionType.Or, [OR8(Register.C)]);
    instructions[0xB2] = Instruction(InstructionType.Or, [OR8(Register.D)]);
    instructions[0xB3] = Instruction(InstructionType.Or, [OR8(Register.E)]);
    instructions[0xB4] = Instruction(InstructionType.Or, [OR8(Register.IYH)]);
    instructions[0xB5] = Instruction(InstructionType.Or, [OR8(Register.IYL)]);
    instructions[0xB6] = Instruction(InstructionType.Or, [IY]);
    instructions[0xB7] = Instruction(InstructionType.Or, [OR8(Register.A)]);
    instructions[0xB8] = Instruction(InstructionType.Cp, [OR8(Register.B)]);
    instructions[0xB9] = Instruction(InstructionType.Cp, [OR8(Register.C)]);
    instructions[0xBA] = Instruction(InstructionType.Cp, [OR8(Register.D)]);
    instructions[0xBB] = Instruction(InstructionType.Cp, [OR8(Register.E)]);
    instructions[0xBC] = Instruction(InstructionType.Cp, [OR8(Register.IYH)]);
    instructions[0xBD] = Instruction(InstructionType.Cp, [OR8(Register.IYL)]);
    instructions[0xBE] = Instruction(InstructionType.Cp, [IY]);
    instructions[0xBF] = Instruction(InstructionType.Cp, [OR8(Register.A)]);
    instructions[0xE1] = Instruction(InstructionType.Pop, [OR16(Register.IY)]);
    instructions[0xE3] = Instruction(InstructionType.Ex, [
            OR16_LK(Register.SP), OR16(Register.IY)
        ]);
    instructions[0xE5] = Instruction(InstructionType.Push, [OR16(Register.IY)]);
    instructions[0xE9] = Instruction(InstructionType.Jp, [OR16_LK(Register.IY)]);
    instructions[0xF9] = Instruction(InstructionType.Ld, [
            OR16(Register.SP), OR16(Register.IY)
        ]);
    return instructions;
}

static Instruction[ubyte] MAIN = null;

Instruction getInstruction(const(ubyte[]) data, ref size_t index) => getInstruction_nullable(
    data, index);

Nullable!Instruction getInstruction_nullable(const(ubyte[]) data, ref size_t index) {
    size_t oldindex = index;
    Instruction ins;
    const(Instruction)[ubyte] indexMe;
    if (MAIN == null)
        MAIN = genMainInstructions();
    indexMe = MAIN;
    ubyte[] opcodeCollection;
    bool isIBit = false;
    size_t temp;
    do {
        temp = index++ + cast(size_t) isIBit;
        if (temp >= data.length)
            return nullable!Instruction(null);
        ubyte indexWith = data[temp];
        opcodeCollection ~= indexWith;
        if (indexWith in indexMe) {
            ins = cast(Instruction) indexMe[indexWith];
            if (ins.type != InstructionType.Indirection)
                break;
            indexMe = ins.indirection;
            isIBit = ins.isIBitIndirection;
        }
        else {
            return nullable!Instruction(null);
        }
    }
    while (ins.type == InstructionType.Indirection);

    Operand[] operands = new Operand[ins.operands.length];
    operands[0 .. ins.operands.length] = ins.operands;
    ins.operands = operands;

    // if (ins.type == InstructionType.Ret){
    // ins.writeln;
    // opcodeCollection.writeln;
    // }

    foreach (ref value; ins.operands) {
        switch (value.variety) {
            case OperandVariety.Reg8:
            case OperandVariety.Reg16:
            case OperandVariety.Reg16Lookup:
            case OperandVariety.Reg8Lookup:
            case OperandVariety.Condition:
            case OperandVariety.Rst:
            case OperandVariety.PreSetImm8:
                break;
            case OperandVariety.IxOffset:
            case OperandVariety.IyOffset:
                temp = index++ - isIBit;
                if (temp >= data.length)
                    return nullable!Instruction(null);
                value.imm8 = data[temp];
                break;
            case OperandVariety.Imm8Lookup:
            case OperandVariety.Imm8:
                temp = index++;
                if (temp >= data.length)
                    return nullable!Instruction(null);
                value.imm8 = data[temp];
                break;
            case OperandVariety.Imm16:
            case OperandVariety.Imm16Lookup:
                if (index + 1 >= data.length)
                    return nullable!Instruction(null);
                value.imm16 = data[index++] | (data[index++] << 8);
                break;
            default:
                import std.conv;

                assert(0, value.variety.to!string);
        }
    }
    ins.byteSize = index - oldindex;
    return nullable!Instruction(ins);
}

unittest {
    // size_t i;
    // getInstruction([0xCB,0x03], i).toAssembly.writeln;

}
