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
    Unknown,
    Reg8,
    Reg16,
    Imm8,
    Imm16,

    Reg8Lookup,
    Reg16Lookup,
    Imm8Lookup,
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

pure Operand OR8(Register register) {
    Operand operand;
    operand.variety = OperandVariety.Reg8;
    operand.register = register;
    return operand;
}

pure Operand OR16(Register register) {
    Operand operand;
    operand.variety = OperandVariety.Reg16;
    operand.register = register;
    return operand;
}

pure Operand OR16_LK(Register register) {
    Operand operand;
    operand.variety = OperandVariety.Reg16Lookup;
    operand.register = register;
    return operand;
}

pure Operand IMM8() {
    Operand operand;
    operand.variety = OperandVariety.Imm8;
    return operand;
}

pure Operand IMM8_LK() {
    Operand operand;
    operand.variety = OperandVariety.Imm8Lookup;
    return operand;
}

pure Operand OR8_LK(Register r) {
    Operand operand;
    operand.variety = OperandVariety.Reg8Lookup;
    operand.register = r;
    return operand;
}

pure Operand RST(ubyte rst) {
    Operand operand;
    operand.variety = OperandVariety.Rst;
    operand.rst = rst;
    return operand;
}

pure Operand PIMM8(ubyte ub) {
    Operand operand;
    operand.variety = OperandVariety.PreSetImm8;
    operand.imm8 = ub;
    return operand;
}

pure Operand IXOFF() {
    Operand operand;
    operand.variety = OperandVariety.IxOffset;
    return operand;
}

pure Operand IYOFF() {
    Operand operand;
    operand.variety = OperandVariety.IyOffset;
    return operand;
}

pure Operand LIMM8() {
    Operand operand;
    operand.variety = OperandVariety.Imm8;
    operand.isLabel = true;
    return operand;
}

pure Operand LIMM16() {
    Operand operand;
    operand.variety = OperandVariety.Imm16;
    operand.isLabel = true;
    return operand;
}

pure Operand IMM16() {
    Operand operand;
    operand.variety = OperandVariety.Imm16;

    return operand;
}

pure Operand IMM16_LK() {
    Operand operand;
    operand.variety = OperandVariety.Imm16Lookup;
    return operand;
}

pure Operand ONULL() {
    Operand operand;
    operand.variety = OperandVariety.Unknown;
    return operand;
}

enum Operand[][] EIGHTBIT_REGS = [
        // Normal
        [
            OR8(Register.B), OR8(Register.C), OR8(Register.D), OR8(Register.E),
            OR8(Register.H), OR8(Register.L), ONULL, OR8(Register.A)
        ],
        // IX
        [
            OR8(Register.B), OR8(Register.C), OR8(Register.D), OR8(Register.E),
            OR8(Register.IXH), OR8(Register.IXL), ONULL, OR8(Register.A)
        ],
        // IY
        [
            OR8(Register.B), OR8(Register.C), OR8(Register.D), OR8(Register.E),
            OR8(Register.IYH), OR8(Register.IYL), ONULL, OR8(Register.A)
        ]
    ];
enum Operand[][] SIXTEENBIT_REGS = [
        // Normal
        [
            OR16(Register.BC), OR16(Register.DE), OR16(Register.HL),
            OR16(Register.SP)
        ],
        // IX
        [
            OR16(Register.BC), OR16(Register.DE), OR16(Register.IX),
            OR16(Register.SP)
        ],
        // IY
        [
            OR16(Register.BC), OR16(Register.DE), OR16(Register.IY),
            OR16(Register.SP)
        ],
    ];

pure Operand CON(ConditionVariety condition) {
    Operand operand;
    operand.variety = OperandVariety.Condition;
    operand.condition = condition;
    return operand;
}

string asOpString(InstructionType type) {
    import std.conv : to;
    import std.string : toLower;

    assert(type != InstructionType.Unknown, "Opcode can't be determined");
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

enum LevelType {
    Unknown,
    Table,
    Operand,
    Instruction
}

class LookupLevel {
    LevelType type;
    union {
        LookupLevel[0xFF + 1] opcodes; // table
        LookupLevel nextLevel; // Imm8, Imm16
        Instruction instruction;
    }
}

class OpcodeHolder {
    LookupLevel level1;
    pure this() {
        level1 = new LookupLevel;
        level1.type = LevelType.Table;
        level1.opcodes = new LookupLevel[0xFF + 1];
    }

    pure void add(ushort[] opcode, Instruction instruction) {
        LookupLevel level = level1;
        foreach (i, ushort opseg; opcode) {
            if (level is null) {
                throw new Exception("Null level encountered before accessing its properties");
            }

            // Valid opcode (not operand)
            if (opseg <= ubyte.max) {
                if (level.type == LevelType.Unknown) {
                    level.type = LevelType.Table;
                    level.opcodes = new LookupLevel[0xFF + 1];
                }
                if (level.opcodes[opseg] is null)
                    level.opcodes[opseg] = new LookupLevel;
                level = level.opcodes[opseg];
                continue;
            }

            if (level.type == LevelType.Unknown)
                level.nextLevel = new LookupLevel;
            level.type = LevelType.Operand;
            level = level.nextLevel;

        }
        level.type = LevelType.Instruction;
        level.instruction = instruction;
    }

    const pure Nullable!Instruction lookup(const(ubyte[]) data, ref size_t index) {
        const(LookupLevel)* level = &level1;
        ubyte[] operand_bytes;
        while (1) {
            switch (level.type) {
                case LevelType.Table:
                    if (index + 1 > data.length)
                        return Nullable!Instruction(null);
                    level = &level.opcodes[data[index++]];
                    if (level is null)
                        return Nullable!Instruction(null);
                    continue;
                case LevelType.Operand:
                    operand_bytes ~= data[index++];
                    level = &level.nextLevel;
                    continue;
                case LevelType.Unknown:
                    return Nullable!Instruction(null);
                default:
                    assert(0);
                case LevelType.Instruction:
                    Instruction ins = cast(Instruction) level.instruction;

                    Operand[] operands = new Operand[ins.operands.length];
                    operands[0 .. ins.operands.length] = ins.operands;
                    ins.operands = operands;

                    size_t oprIndex;
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
                                value.imm8 = operand_bytes[oprIndex++];
                                break;
                            case OperandVariety.Imm8Lookup:
                            case OperandVariety.Imm8:
                                value.imm8 = operand_bytes[oprIndex++];
                                break;
                            case OperandVariety.Imm16:
                            case OperandVariety.Imm16Lookup:
                                value.imm16 = operand_bytes[oprIndex++] | (
                                    operand_bytes[oprIndex++] << 8);
                                break;
                            default:
                                import std.conv;

                                assert(0, value.variety.to!string);
                        }
                    }
                    return nullable(ins);
            }
        }
    }
}

pure OpcodeHolder makeGlobalLookup() {
    OpcodeHolder hold = new OpcodeHolder;
    hold.add([0x8e], Instruction(InstructionType.Adc, [
                OR8(Register.A), OR16_LK(Register.HL)
            ]));
    hold.add([0xdd, 0x8e, 0xfff], Instruction(InstructionType.Adc, [
                OR8(Register.A), IXOFF
            ]));
    hold.add([0xfd, 0x8e, 0xfff], Instruction(InstructionType.Adc, [
                OR8(Register.A), IYOFF
            ]));
    hold.add([0xce, 0xfff], Instruction(InstructionType.Adc, [
                OR8(Register.A), IMM8
            ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([(rI & 0xFF) | 0x88], Instruction(InstructionType.Adc, [
                        OR8(Register.A), r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[1])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, (rI & 0xFF) | 0x88], Instruction(InstructionType.Adc, [
                        OR8(Register.A), r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[2])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, (rI & 0xFF) | 0x88], Instruction(InstructionType.Adc, [
                        OR8(Register.A), r
                    ]));
    foreach (reg16I, reg16; SIXTEENBIT_REGS[0])
        hold.add([0xed, (reg16I << 4 & 0xFF) | 0x4a], Instruction(InstructionType.Adc, [
                    OR16(Register.HL), reg16
                ]));
    hold.add([0x86], Instruction(InstructionType.Add, [
                OR8(Register.A), OR16_LK(Register.HL)
            ]));
    hold.add([0xdd, 0x86, 0xfff], Instruction(InstructionType.Add, [
                OR8(Register.A), IXOFF
            ]));
    hold.add([0xfd, 0x86, 0xfff], Instruction(InstructionType.Add, [
                OR8(Register.A), IYOFF
            ]));
    hold.add([0xc6, 0xfff], Instruction(InstructionType.Add, [
                OR8(Register.A), IMM8
            ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([(rI & 0xFF) | 0x80], Instruction(InstructionType.Add, [
                        OR8(Register.A), r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[1])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, (rI & 0xFF) | 0x80], Instruction(InstructionType.Add, [
                        OR8(Register.A), r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[2])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, (rI & 0xFF) | 0x80], Instruction(InstructionType.Add, [
                        OR8(Register.A), r
                    ]));
    foreach (reg16I, reg16; SIXTEENBIT_REGS[0])
        hold.add([(reg16I << 4 & 0xFF) | 0x9], Instruction(InstructionType.Add, [
                    OR16(Register.HL), reg16
                ]));
    foreach (reg16I, reg16; SIXTEENBIT_REGS[1])
        hold.add([0xdd, (reg16I << 4 & 0xFF) | 0x9], Instruction(InstructionType.Add, [
                    OR16(Register.IX), reg16
                ]));
    foreach (reg16I, reg16; SIXTEENBIT_REGS[2])
        hold.add([0xfd, (reg16I << 4 & 0xFF) | 0x9], Instruction(InstructionType.Add, [
                    OR16(Register.IY), reg16
                ]));
    hold.add([0xa6], Instruction(InstructionType.And, [OR16_LK(Register.HL)]));
    hold.add([0xdd, 0xa6, 0xfff], Instruction(InstructionType.And, [IXOFF]));
    hold.add([0xfd, 0xa6, 0xfff], Instruction(InstructionType.And, [IYOFF]));
    hold.add([0xe6, 0xfff], Instruction(InstructionType.And, [IMM8]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([(rI & 0xFF) | 0xa0], Instruction(InstructionType.And, [r]));
    foreach (rI, r; EIGHTBIT_REGS[1])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, (rI & 0xFF) | 0xa0], Instruction(InstructionType.And, [
                        r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[2])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, (rI & 0xFF) | 0xa0], Instruction(InstructionType.And, [
                        r
                    ]));
    foreach (bit; 0 .. 8)
        hold.add([0xcb, (bit << 3 & 0xFF) | 0x46], Instruction(InstructionType.Bit, [
                    PIMM8(cast(ubyte) bit), OR16_LK(Register.HL)
                ]));
    foreach (bit; 0 .. 8)
        hold.add([0xdd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x46], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IXOFF]));
    foreach (bit; 0 .. 8)
        hold.add([0xdd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x40], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IXOFF]));
    foreach (bit; 0 .. 8)
        hold.add([0xdd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x41], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IXOFF]));
    foreach (bit; 0 .. 8)
        hold.add([0xdd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x42], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IXOFF]));
    foreach (bit; 0 .. 8)
        hold.add([0xdd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x43], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IXOFF]));
    foreach (bit; 0 .. 8)
        hold.add([0xdd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x44], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IXOFF]));
    foreach (bit; 0 .. 8)
        hold.add([0xdd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x45], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IXOFF]));
    foreach (bit; 0 .. 8)
        hold.add([0xdd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x47], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IXOFF]));
    foreach (bit; 0 .. 8)
        hold.add([0xfd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x46], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IYOFF]));
    foreach (bit; 0 .. 8)
        hold.add([0xfd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x40], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IYOFF]));
    foreach (bit; 0 .. 8)
        hold.add([0xfd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x41], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IYOFF]));
    foreach (bit; 0 .. 8)
        hold.add([0xfd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x42], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IYOFF]));
    foreach (bit; 0 .. 8)
        hold.add([0xfd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x43], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IYOFF]));
    foreach (bit; 0 .. 8)
        hold.add([0xfd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x44], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IYOFF]));
    foreach (bit; 0 .. 8)
        hold.add([0xfd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x45], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IYOFF]));
    foreach (bit; 0 .. 8)
        hold.add([0xfd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x47], Instruction(
                InstructionType.Bit, [PIMM8(cast(ubyte) bit), IYOFF]));
    foreach (bit; 0 .. 8)
        foreach (rI, r; EIGHTBIT_REGS[0])
            if (r.variety != OperandVariety.Unknown)
                hold.add([0xcb, (bit << 3 & 0xFF) | (rI & 0xFF) | 0x40], Instruction(
                        InstructionType.Bit, [PIMM8(cast(ubyte) bit), r]));
    hold.add([0xdc, 0xfff, 0xfff], Instruction(InstructionType.Call, [
                OR8(Register.C), IMM16
            ]));
    hold.add([0xfc, 0xfff, 0xfff], Instruction(InstructionType.Call, [
                CON(ConditionVariety.M), IMM16
            ]));
    hold.add([0xd4, 0xfff, 0xfff], Instruction(InstructionType.Call, [
                CON(ConditionVariety.NC), IMM16
            ]));
    hold.add([0xc4, 0xfff, 0xfff], Instruction(InstructionType.Call, [
                CON(ConditionVariety.NZ), IMM16
            ]));
    hold.add([0xf4, 0xfff, 0xfff], Instruction(InstructionType.Call, [
                CON(ConditionVariety.P), IMM16
            ]));
    hold.add([0xec, 0xfff, 0xfff], Instruction(InstructionType.Call, [
                CON(ConditionVariety.PE), IMM16
            ]));
    hold.add([0xe4, 0xfff, 0xfff], Instruction(InstructionType.Call, [
                CON(ConditionVariety.PO), IMM16
            ]));
    hold.add([0xcc, 0xfff, 0xfff], Instruction(InstructionType.Call, [
                CON(ConditionVariety.Z), IMM16
            ]));
    hold.add([0xcd, 0xfff, 0xfff], Instruction(InstructionType.Call, [IMM16]));
    hold.add([0x3f], Instruction(InstructionType.Ccf, []));
    hold.add([0xbe], Instruction(InstructionType.Cp, [OR16_LK(Register.HL)]));
    hold.add([0xdd, 0xbe, 0xfff], Instruction(InstructionType.Cp, [IXOFF]));
    hold.add([0xfd, 0xbe, 0xfff], Instruction(InstructionType.Cp, [IYOFF]));
    hold.add([0xfe, 0xfff], Instruction(InstructionType.Cp, [IMM8]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([(rI & 0xFF) | 0xb8], Instruction(InstructionType.Cp, [r]));
    foreach (rI, r; EIGHTBIT_REGS[1])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, (rI & 0xFF) | 0xb8], Instruction(InstructionType.Cp, [
                        r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[2])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, (rI & 0xFF) | 0xb8], Instruction(InstructionType.Cp, [
                        r
                    ]));
    hold.add([0xed, 0xa9], Instruction(InstructionType.Cpd, []));
    hold.add([0xed, 0xb9], Instruction(InstructionType.Cpdr, []));
    hold.add([0xed, 0xa1], Instruction(InstructionType.Cpi, []));
    hold.add([0xed, 0xb1], Instruction(InstructionType.Cpir, []));
    hold.add([0x2f], Instruction(InstructionType.Cpl, []));
    hold.add([0x27], Instruction(InstructionType.Daa, []));
    hold.add([0x35], Instruction(InstructionType.Dec, [OR16_LK(Register.HL)]));
    hold.add([0xdd, 0x35, 0xfff], Instruction(InstructionType.Dec, [IXOFF]));
    hold.add([0xfd, 0x35, 0xfff], Instruction(InstructionType.Dec, [IYOFF]));
    hold.add([0xdd, 0x2b], Instruction(InstructionType.Dec, [OR16(Register.IX)]));
    hold.add([0xfd, 0x2b], Instruction(InstructionType.Dec, [OR16(Register.IY)]));
    foreach (reg16I, reg16; SIXTEENBIT_REGS[0])
        hold.add([(reg16I << 4 & 0xFF) | 0xb], Instruction(InstructionType.Dec, [
                    reg16
                ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([(rI << 3 & 0xFF) | 0x5], Instruction(InstructionType.Dec, [
                        r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[1])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, (rI << 3 & 0xFF) | 0x5], Instruction(InstructionType.Dec, [
                        r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[2])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, (rI << 3 & 0xFF) | 0x5], Instruction(InstructionType.Dec, [
                        r
                    ]));
    hold.add([0xf3], Instruction(InstructionType.Di, []));
    hold.add([0x10, 0xfff], Instruction(InstructionType.Djnz, [LIMM8]));
    hold.add([0xfb], Instruction(InstructionType.Ei, []));
    hold.add([0xe3], Instruction(InstructionType.Ex, [
                OR16_LK(Register.SP), OR16(Register.HL)
            ]));
    hold.add([0xdd, 0xe3], Instruction(InstructionType.Ex, [
                OR16_LK(Register.SP), OR16(Register.IX)
            ]));
    hold.add([0xfd, 0xe3], Instruction(InstructionType.Ex, [
                OR16_LK(Register.SP), OR16(Register.IY)
            ]));
    hold.add([0x8], Instruction(InstructionType.Ex, [
                OR16(Register.AF), OR16(Register.SHADOW_AF)
            ]));
    hold.add([0xeb], Instruction(InstructionType.Ex, [
                OR16(Register.DE), OR16(Register.HL)
            ]));
    hold.add([0xd9], Instruction(InstructionType.Exx, []));
    hold.add([0x76], Instruction(InstructionType.Halt, []));
    hold.add([0xed, 0x46], Instruction(InstructionType.Im, [PIMM8(0)]));
    hold.add([0xed, 0x56], Instruction(InstructionType.Im, [PIMM8(1)]));
    hold.add([0xed, 0x5e], Instruction(InstructionType.Im, [PIMM8(2)]));
    hold.add([0xed, 0x70], Instruction(InstructionType.In, [OR8_LK(Register.C)]));
    hold.add([0xdb, 0xfff], Instruction(InstructionType.In, [
                OR8(Register.A), IMM8_LK
            ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xed, (rI << 3 & 0xFF) | 0x40], Instruction(InstructionType.In, [
                        r, OR8_LK(Register.C)
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xed, (rI << 3 & 0xFF) | 0x0, 0xfff], Instruction(InstructionType.In0, [
                        r, IMM8_LK
                    ]));
    hold.add([0x34], Instruction(InstructionType.Inc, [OR16_LK(Register.HL)]));
    hold.add([0xdd, 0x34, 0xfff], Instruction(InstructionType.Inc, [IXOFF]));
    hold.add([0xfd, 0x34, 0xfff], Instruction(InstructionType.Inc, [IYOFF]));
    hold.add([0xdd, 0x23], Instruction(InstructionType.Inc, [OR16(Register.IX)]));
    hold.add([0xfd, 0x23], Instruction(InstructionType.Inc, [OR16(Register.IY)]));
    foreach (reg16I, reg16; SIXTEENBIT_REGS[0])
        hold.add([(reg16I << 4 & 0xFF) | 0x3], Instruction(InstructionType.Inc, [
                    reg16
                ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([(rI << 3 & 0xFF) | 0x4], Instruction(InstructionType.Inc, [
                        r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[1])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, (rI << 3 & 0xFF) | 0x4], Instruction(InstructionType.Inc, [
                        r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[2])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, (rI << 3 & 0xFF) | 0x4], Instruction(InstructionType.Inc, [
                        r
                    ]));
    hold.add([0xed, 0xaa], Instruction(InstructionType.Ind, []));
    hold.add([0xed, 0xba], Instruction(InstructionType.Indr, []));
    hold.add([0xed, 0xa2], Instruction(InstructionType.Ini, []));
    hold.add([0xed, 0xb2], Instruction(InstructionType.Inir, []));
    hold.add([0xe9], Instruction(InstructionType.Jp, [OR16_LK(Register.HL)]));
    hold.add([0xdd, 0xe9], Instruction(InstructionType.Jp, [
                OR16_LK(Register.IX)
            ]));
    hold.add([0xfd, 0xe9], Instruction(InstructionType.Jp, [
                OR16_LK(Register.IY)
            ]));
    hold.add([0xda, 0xfff, 0xfff], Instruction(InstructionType.Jp, [
                OR8(Register.C), IMM16
            ]));
    hold.add([0xfa, 0xfff, 0xfff], Instruction(InstructionType.Jp, [
                CON(ConditionVariety.M), IMM16
            ]));
    hold.add([0xd2, 0xfff, 0xfff], Instruction(InstructionType.Jp, [
                CON(ConditionVariety.NC), IMM16
            ]));
    hold.add([0xc2, 0xfff, 0xfff], Instruction(InstructionType.Jp, [
                CON(ConditionVariety.NZ), IMM16
            ]));
    hold.add([0xf2, 0xfff, 0xfff], Instruction(InstructionType.Jp, [
                CON(ConditionVariety.P), IMM16
            ]));
    hold.add([0xea, 0xfff, 0xfff], Instruction(InstructionType.Jp, [
                CON(ConditionVariety.PE), IMM16
            ]));
    hold.add([0xe2, 0xfff, 0xfff], Instruction(InstructionType.Jp, [
                CON(ConditionVariety.PO), IMM16
            ]));
    hold.add([0xca, 0xfff, 0xfff], Instruction(InstructionType.Jp, [
                CON(ConditionVariety.Z), IMM16
            ]));
    hold.add([0xc3, 0xfff, 0xfff], Instruction(InstructionType.Jp, [IMM16]));
    hold.add([0x38, 0xfff], Instruction(InstructionType.Jr, [
                OR8(Register.C), LIMM8
            ]));
    hold.add([0x30, 0xfff], Instruction(InstructionType.Jr, [
                CON(ConditionVariety.NC), LIMM8
            ]));
    hold.add([0x20, 0xfff], Instruction(InstructionType.Jr, [
                CON(ConditionVariety.NZ), LIMM8
            ]));
    hold.add([0x28, 0xfff], Instruction(InstructionType.Jr, [
                CON(ConditionVariety.Z), LIMM8
            ]));
    hold.add([0x18, 0xfff], Instruction(InstructionType.Jr, [LIMM8]));
    hold.add([0x2], Instruction(InstructionType.Ld, [
                OR16_LK(Register.BC), OR8(Register.A)
            ]));
    hold.add([0x12], Instruction(InstructionType.Ld, [
                OR16_LK(Register.DE), OR8(Register.A)
            ]));
    hold.add([0x36, 0xfff], Instruction(InstructionType.Ld, [
                OR16_LK(Register.HL), IMM8
            ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([(rI & 0xFF) | 0x70], Instruction(InstructionType.Ld, [
                        OR16_LK(Register.HL), r
                    ]));
    hold.add([0xdd, 0x36, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                IXOFF, IMM8
            ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, (rI & 0xFF) | 0x70, 0xfff], Instruction(InstructionType.Ld, [
                        IXOFF, r
                    ]));
    hold.add([0xfd, 0x36, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                IYOFF, IMM8
            ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, (rI & 0xFF) | 0x70, 0xfff], Instruction(InstructionType.Ld, [
                        IYOFF, r
                    ]));
    hold.add([0x32, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                IMM16_LK, OR8(Register.A)
            ]));
    hold.add([0xed, 0x43, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                IMM16_LK, OR16(Register.BC)
            ]));
    hold.add([0xed, 0x53, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                IMM16_LK, OR16(Register.DE)
            ]));
    hold.add([0x22, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                IMM16_LK, OR16(Register.HL)
            ]));
    hold.add([0xed, 0x63, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                IMM16_LK, OR16(Register.HL)
            ]));
    hold.add([0xdd, 0x22, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                IMM16_LK, OR16(Register.IX)
            ]));
    hold.add([0xfd, 0x22, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                IMM16_LK, OR16(Register.IY)
            ]));
    hold.add([0xed, 0x73, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                IMM16_LK, OR16(Register.SP)
            ]));
    hold.add([0xa], Instruction(InstructionType.Ld, [
                OR8(Register.A), OR16_LK(Register.BC)
            ]));
    hold.add([0x1a], Instruction(InstructionType.Ld, [
                OR8(Register.A), OR16_LK(Register.DE)
            ]));
    hold.add([0x3a, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                OR8(Register.A), IMM16_LK
            ]));
    hold.add([0xed, 0x57], Instruction(InstructionType.Ld, [
                OR8(Register.A), OR8(Register.I)
            ]));
    hold.add([0xed, 0x5f], Instruction(InstructionType.Ld, [
                OR8(Register.A), OR8(Register.R)
            ]));
    hold.add([0xed, 0x4b, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                OR16(Register.BC), IMM16_LK
            ]));
    hold.add([0xed, 0x5b, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                OR16(Register.DE), IMM16_LK
            ]));
    hold.add([0x2a, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                OR16(Register.HL), IMM16_LK
            ]));
    hold.add([0xed, 0x6b, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                OR16(Register.HL), IMM16_LK
            ]));
    hold.add([0xed, 0x47], Instruction(InstructionType.Ld, [
                OR8(Register.I), OR8(Register.A)
            ]));
    hold.add([0xdd, 0x2a, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                OR16(Register.IX), IMM16_LK
            ]));
    hold.add([0xdd, 0x21, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                OR16(Register.IX), IMM16
            ]));
    hold.add([0xfd, 0x2a, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                OR16(Register.IY), IMM16_LK
            ]));
    hold.add([0xfd, 0x21, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                OR16(Register.IY), IMM16
            ]));
    hold.add([0xed, 0x4f], Instruction(InstructionType.Ld, [
                OR8(Register.R), OR8(Register.A)
            ]));
    hold.add([0xed, 0x7b, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                OR16(Register.SP), IMM16_LK
            ]));
    hold.add([0xf9], Instruction(InstructionType.Ld, [
                OR16(Register.SP), OR16(Register.HL)
            ]));
    hold.add([0xdd, 0xf9], Instruction(InstructionType.Ld, [
                OR16(Register.SP), OR16(Register.IX)
            ]));
    hold.add([0xfd, 0xf9], Instruction(InstructionType.Ld, [
                OR16(Register.SP), OR16(Register.IY)
            ]));
    foreach (reg16I, reg16; SIXTEENBIT_REGS[0])
        hold.add([(reg16I << 4 & 0xFF) | 0x1, 0xfff, 0xfff], Instruction(InstructionType.Ld, [
                    reg16, IMM16
                ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([(rI << 3 & 0xFF) | 0x46], Instruction(InstructionType.Ld, [
                        r, OR16_LK(Register.HL)
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, (rI << 3 & 0xFF) | 0x46, 0xfff], Instruction(InstructionType.Ld, [
                        r, IXOFF
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, (rI << 3 & 0xFF) | 0x46, 0xfff], Instruction(InstructionType.Ld, [
                        r, IYOFF
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([(rI << 3 & 0xFF) | 0x6, 0xfff], Instruction(InstructionType.Ld, [
                        r, IMM8
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[1])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, (rI << 3 & 0xFF) | 0x6, 0xfff], Instruction(InstructionType.Ld, [
                        r, IMM8
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[2])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, (rI << 3 & 0xFF) | 0x6, 0xfff], Instruction(InstructionType.Ld, [
                        r, IMM8
                    ]));
    foreach (r1I, r1; EIGHTBIT_REGS[0])
        if (r1.variety != OperandVariety.Unknown)
            foreach (r2I, r2; EIGHTBIT_REGS[0])
                if (r2.variety != OperandVariety.Unknown)
                    hold.add([(r1I << 3 & 0xFF) | (r2I & 0xFF) | 0x40], Instruction(
                            InstructionType.Ld, [r1, r2]));
    foreach (r1I, r1; EIGHTBIT_REGS[1])
        if (r1.variety != OperandVariety.Unknown)
            foreach (r2I, r2; EIGHTBIT_REGS[1])
                if (r2.variety != OperandVariety.Unknown)
                    hold.add([0xdd, (r1I << 3 & 0xFF) | (r2I & 0xFF) | 0x40], Instruction(
                            InstructionType.Ld, [r1, r2]));
    foreach (r1I, r1; EIGHTBIT_REGS[2])
        if (r1.variety != OperandVariety.Unknown)
            foreach (r2I, r2; EIGHTBIT_REGS[2])
                if (r2.variety != OperandVariety.Unknown)
                    hold.add([0xfd, (r1I << 3 & 0xFF) | (r2I & 0xFF) | 0x40], Instruction(
                            InstructionType.Ld, [r1, r2]));
    hold.add([0xed, 0xa8], Instruction(InstructionType.Ldd, []));
    hold.add([0xed, 0xb8], Instruction(InstructionType.Lddr, []));
    hold.add([0xed, 0xa0], Instruction(InstructionType.Ldi, []));
    hold.add([0xed, 0xb0], Instruction(InstructionType.Ldir, []));
    hold.add([0xed, 0x4c], Instruction(InstructionType.Mlt, [OR16(Register.BC)]));
    hold.add([0xed, 0x5c], Instruction(InstructionType.Mlt, [OR16(Register.DE)]));
    hold.add([0xed, 0x6c], Instruction(InstructionType.Mlt, [OR16(Register.HL)]));
    hold.add([0xed, 0x7c], Instruction(InstructionType.Mlt, [OR16(Register.SP)]));
    hold.add([0xed, 0x44], Instruction(InstructionType.Neg, []));
    hold.add([0x0], Instruction(InstructionType.Nop, []));
    hold.add([0xb6], Instruction(InstructionType.Or, [OR16_LK(Register.HL)]));
    hold.add([0xdd, 0xb6, 0xfff], Instruction(InstructionType.Or, [IXOFF]));
    hold.add([0xfd, 0xb6, 0xfff], Instruction(InstructionType.Or, [IYOFF]));
    hold.add([0xf6, 0xfff], Instruction(InstructionType.Or, [IMM8]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([(rI & 0xFF) | 0xb0], Instruction(InstructionType.Or, [r]));
    foreach (rI, r; EIGHTBIT_REGS[1])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, (rI & 0xFF) | 0xb0], Instruction(InstructionType.Or, [
                        r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[2])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, (rI & 0xFF) | 0xb0], Instruction(InstructionType.Or, [
                        r
                    ]));
    hold.add([0xed, 0x8b], Instruction(InstructionType.Otdm, []));
    hold.add([0xed, 0x9b], Instruction(InstructionType.Otdmr, []));
    hold.add([0xed, 0xbb], Instruction(InstructionType.Otdr, []));
    hold.add([0xed, 0x83], Instruction(InstructionType.Otim, []));
    hold.add([0xed, 0x93], Instruction(InstructionType.Otimr, []));
    hold.add([0xed, 0xb3], Instruction(InstructionType.Otir, []));
    hold.add([0xed, 0x71], Instruction(InstructionType.Out, [
                OR8_LK(Register.C), PIMM8(0)
            ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xed, (rI << 3 & 0xFF) | 0x41], Instruction(InstructionType.Out, [
                        OR8_LK(Register.C), r
                    ]));
    hold.add([0xd3, 0xfff], Instruction(InstructionType.Out, [
                IMM8_LK, OR8(Register.A)
            ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xed, (rI << 3 & 0xFF) | 0x1, 0xfff], Instruction(InstructionType.Out0, [
                        IMM8_LK, r
                    ]));
    hold.add([0xed, 0xab], Instruction(InstructionType.Outd, []));
    hold.add([0xed, 0xa3], Instruction(InstructionType.Outi, []));
    hold.add([0xf1], Instruction(InstructionType.Pop, [OR16(Register.AF)]));
    hold.add([0xc1], Instruction(InstructionType.Pop, [OR16(Register.BC)]));
    hold.add([0xd1], Instruction(InstructionType.Pop, [OR16(Register.DE)]));
    hold.add([0xe1], Instruction(InstructionType.Pop, [OR16(Register.HL)]));
    hold.add([0xdd, 0xe1], Instruction(InstructionType.Pop, [OR16(Register.IX)]));
    hold.add([0xfd, 0xe1], Instruction(InstructionType.Pop, [OR16(Register.IY)]));
    hold.add([0xf5], Instruction(InstructionType.Push, [OR16(Register.AF)]));
    hold.add([0xc5], Instruction(InstructionType.Push, [OR16(Register.BC)]));
    hold.add([0xd5], Instruction(InstructionType.Push, [OR16(Register.DE)]));
    hold.add([0xe5], Instruction(InstructionType.Push, [OR16(Register.HL)]));
    hold.add([0xdd, 0xe5], Instruction(InstructionType.Push, [OR16(Register.IX)]));
    hold.add([0xfd, 0xe5], Instruction(InstructionType.Push, [OR16(Register.IY)]));
    foreach (bit; 0 .. 8)
        hold.add([0xcb, (bit << 3 & 0xFF) | 0x86], Instruction(InstructionType.Res, [
                    PIMM8(cast(ubyte) bit), OR16_LK(Register.HL)
                ]));
    foreach (bit; 0 .. 8)
        hold.add([0xdd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x86], Instruction(
                InstructionType.Res, [PIMM8(cast(ubyte) bit), IXOFF]));
    foreach (bit; 0 .. 8)
        foreach (rI, r; EIGHTBIT_REGS[0])
            if (r.variety != OperandVariety.Unknown)
                hold.add([0xdd, 0xcb, 0xfff, (bit << 3 & 0xFF) | (rI & 0xFF) | 0x80], Instruction(
                        InstructionType.Res, [PIMM8(cast(ubyte) bit), IXOFF, r]));
    foreach (bit; 0 .. 8)
        hold.add([0xfd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0x86], Instruction(
                InstructionType.Res, [PIMM8(cast(ubyte) bit), IYOFF]));
    foreach (bit; 0 .. 8)
        foreach (rI, r; EIGHTBIT_REGS[0])
            if (r.variety != OperandVariety.Unknown)
                hold.add([0xfd, 0xcb, 0xfff, (bit << 3 & 0xFF) | (rI & 0xFF) | 0x80], Instruction(
                        InstructionType.Res, [PIMM8(cast(ubyte) bit), IYOFF, r]));
    foreach (bit; 0 .. 8)
        foreach (rI, r; EIGHTBIT_REGS[0])
            if (r.variety != OperandVariety.Unknown)
                hold.add([0xcb, (bit << 3 & 0xFF) | (rI & 0xFF) | 0x80], Instruction(
                        InstructionType.Res, [PIMM8(cast(ubyte) bit), r]));
    hold.add([0xc9], Instruction(InstructionType.Ret, []));
    hold.add([0xd8], Instruction(InstructionType.Ret, [OR8(Register.C)]));
    hold.add([0xf8], Instruction(InstructionType.Ret, [CON(ConditionVariety.M)]));
    hold.add([0xd0], Instruction(InstructionType.Ret, [CON(ConditionVariety.NC)]));
    hold.add([0xc0], Instruction(InstructionType.Ret, [CON(ConditionVariety.NZ)]));
    hold.add([0xf0], Instruction(InstructionType.Ret, [CON(ConditionVariety.P)]));
    hold.add([0xe8], Instruction(InstructionType.Ret, [CON(ConditionVariety.PE)]));
    hold.add([0xe0], Instruction(InstructionType.Ret, [CON(ConditionVariety.PO)]));
    hold.add([0xc8], Instruction(InstructionType.Ret, [CON(ConditionVariety.Z)]));
    hold.add([0xed, 0x4d], Instruction(InstructionType.Reti, []));
    hold.add([0xed, 0x45], Instruction(InstructionType.Retn, []));
    hold.add([0xcb, 0x16], Instruction(InstructionType.Rl, [
                OR16_LK(Register.HL)
            ]));
    hold.add([0xdd, 0xcb, 0xfff, 0x16], Instruction(InstructionType.Rl, [IXOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, 0xcb, 0xfff, (rI & 0xFF) | 0x10], Instruction(InstructionType.Rl, [
                        IXOFF, r
                    ]));
    hold.add([0xfd, 0xcb, 0xfff, 0x16], Instruction(InstructionType.Rl, [IYOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, 0xcb, 0xfff, (rI & 0xFF) | 0x10], Instruction(InstructionType.Rl, [
                        IYOFF, r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xcb, (rI & 0xFF) | 0x10], Instruction(InstructionType.Rl, [
                        r
                    ]));
    hold.add([0x17], Instruction(InstructionType.Rla, []));
    hold.add([0xcb, 0x6], Instruction(InstructionType.Rlc, [
                OR16_LK(Register.HL)
            ]));
    hold.add([0xdd, 0xcb, 0xfff, 0x6], Instruction(InstructionType.Rlc, [IXOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, 0xcb, 0xfff, (rI & 0xFF)], Instruction(InstructionType.Rlc, [
                        IXOFF, r
                    ]));
    hold.add([0xfd, 0xcb, 0xfff, 0x6], Instruction(InstructionType.Rlc, [IYOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, 0xcb, 0xfff, (rI & 0xFF)], Instruction(InstructionType.Rlc, [
                        IYOFF, r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xcb, (rI & 0xFF)], Instruction(InstructionType.Rlc, [r]));
    hold.add([0x7], Instruction(InstructionType.Rlca, []));
    hold.add([0xed, 0x6f], Instruction(InstructionType.Rld, []));
    hold.add([0xcb, 0x1e], Instruction(InstructionType.Rr, [
                OR16_LK(Register.HL)
            ]));
    hold.add([0xdd, 0xcb, 0xfff, 0x1e], Instruction(InstructionType.Rr, [IXOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, 0xcb, 0xfff, (rI & 0xFF) | 0x18], Instruction(InstructionType.Rr, [
                        IXOFF, r
                    ]));
    hold.add([0xfd, 0xcb, 0xfff, 0x1e], Instruction(InstructionType.Rr, [IYOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, 0xcb, 0xfff, (rI & 0xFF) | 0x18], Instruction(InstructionType.Rr, [
                        IYOFF, r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xcb, (rI & 0xFF) | 0x18], Instruction(InstructionType.Rr, [
                        r
                    ]));
    hold.add([0x1f], Instruction(InstructionType.Rra, []));
    hold.add([0xcb, 0xe], Instruction(InstructionType.Rrc, [
                OR16_LK(Register.HL)
            ]));
    hold.add([0xdd, 0xcb, 0xfff, 0xe], Instruction(InstructionType.Rrc, [IXOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, 0xcb, 0xfff, (rI & 0xFF) | 0x8], Instruction(InstructionType.Rrc, [
                        IXOFF, r
                    ]));
    hold.add([0xfd, 0xcb, 0xfff, 0xe], Instruction(InstructionType.Rrc, [IYOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, 0xcb, 0xfff, (rI & 0xFF) | 0x8], Instruction(InstructionType.Rrc, [
                        IYOFF, r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xcb, (rI & 0xFF) | 0x8], Instruction(InstructionType.Rrc, [
                        r
                    ]));
    hold.add([0xf], Instruction(InstructionType.Rrca, []));
    hold.add([0xed, 0x67], Instruction(InstructionType.Rrd, []));
    foreach (rstI, ubyte rst; [
            0x00, 0x08,
            0x10, 0x18,
            0x20, 0x28,
            0x30, 0x38
        ])
        hold.add([(rstI << 3 & 0xFF) | 0xc7], Instruction(InstructionType.Rst, [
                    RST(rst)
                ]));
    hold.add([0x9e], Instruction(InstructionType.Sbc, [
                OR8(Register.A), OR16_LK(Register.HL)
            ]));
    hold.add([0xdd, 0x9e, 0xfff], Instruction(InstructionType.Sbc, [
                OR8(Register.A), IXOFF
            ]));
    hold.add([0xfd, 0x9e, 0xfff], Instruction(InstructionType.Sbc, [
                OR8(Register.A), IYOFF
            ]));
    hold.add([0xde, 0xfff], Instruction(InstructionType.Sbc, [
                OR8(Register.A), IMM8
            ]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([(rI & 0xFF) | 0x98], Instruction(InstructionType.Sbc, [
                        OR8(Register.A), r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[1])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, (rI & 0xFF) | 0x98], Instruction(InstructionType.Sbc, [
                        OR8(Register.A), r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[2])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, (rI & 0xFF) | 0x98], Instruction(InstructionType.Sbc, [
                        OR8(Register.A), r
                    ]));
    foreach (reg16I, reg16; SIXTEENBIT_REGS[0])
        hold.add([0xed, (reg16I << 4 & 0xFF) | 0x42], Instruction(InstructionType.Sbc, [
                    OR16(Register.HL), reg16
                ]));
    hold.add([0x37], Instruction(InstructionType.Scf, []));
    foreach (bit; 0 .. 8)
        hold.add([0xcb, (bit << 3 & 0xFF) | 0xc6], Instruction(InstructionType.Set, [
                    PIMM8(cast(ubyte) bit), OR16_LK(Register.HL)
                ]));
    foreach (bit; 0 .. 8)
        hold.add([0xdd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0xc6], Instruction(
                InstructionType.Set, [PIMM8(cast(ubyte) bit), IXOFF]));
    foreach (bit; 0 .. 8)
        foreach (rI, r; EIGHTBIT_REGS[0])
            if (r.variety != OperandVariety.Unknown)
                hold.add([0xdd, 0xcb, 0xfff, (bit << 3 & 0xFF) | (rI & 0xFF) | 0xc0], Instruction(
                        InstructionType.Set, [PIMM8(cast(ubyte) bit), IXOFF, r]));
    foreach (bit; 0 .. 8)
        hold.add([0xfd, 0xcb, 0xfff, (bit << 3 & 0xFF) | 0xc6], Instruction(
                InstructionType.Set, [PIMM8(cast(ubyte) bit), IYOFF]));
    foreach (bit; 0 .. 8)
        foreach (rI, r; EIGHTBIT_REGS[0])
            if (r.variety != OperandVariety.Unknown)
                hold.add([0xfd, 0xcb, 0xfff, (bit << 3 & 0xFF) | (rI & 0xFF) | 0xc0], Instruction(
                        InstructionType.Set, [PIMM8(cast(ubyte) bit), IYOFF, r]));
    foreach (bit; 0 .. 8)
        foreach (rI, r; EIGHTBIT_REGS[0])
            if (r.variety != OperandVariety.Unknown)
                hold.add([0xcb, (bit << 3 & 0xFF) | (rI & 0xFF) | 0xc0], Instruction(
                        InstructionType.Set, [PIMM8(cast(ubyte) bit), r]));
    hold.add([0xcb, 0x26], Instruction(InstructionType.Sla, [
                OR16_LK(Register.HL)
            ]));
    hold.add([0xdd, 0xcb, 0xfff, 0x26], Instruction(InstructionType.Sla, [IXOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, 0xcb, 0xfff, (rI & 0xFF) | 0x20], Instruction(
                    InstructionType.Sla, [IXOFF, r]));
    hold.add([0xfd, 0xcb, 0xfff, 0x26], Instruction(InstructionType.Sla, [IYOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, 0xcb, 0xfff, (rI & 0xFF) | 0x20], Instruction(
                    InstructionType.Sla, [IYOFF, r]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xcb, (rI & 0xFF) | 0x20], Instruction(InstructionType.Sla, [
                        r
                    ]));
    hold.add([0xcb, 0x36], Instruction(InstructionType.Sll, [
                OR16_LK(Register.HL)
            ]));
    hold.add([0xdd, 0xcb, 0xfff, 0x36], Instruction(InstructionType.Sll, [IXOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, 0xcb, 0xfff, (rI & 0xFF) | 0x30], Instruction(
                    InstructionType.Sll, [IXOFF, r]));
    hold.add([0xfd, 0xcb, 0xfff, 0x36], Instruction(InstructionType.Sll, [IYOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, 0xcb, 0xfff, (rI & 0xFF) | 0x30], Instruction(
                    InstructionType.Sll, [IYOFF, r]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xcb, (rI & 0xFF) | 0x30], Instruction(InstructionType.Sll, [
                        r
                    ]));
    hold.add([0xed, 0x76], Instruction(InstructionType.Slp, []));
    hold.add([0xcb, 0x2e], Instruction(InstructionType.Sra, [
                OR16_LK(Register.HL)
            ]));
    hold.add([0xdd, 0xcb, 0xfff, 0x2e], Instruction(InstructionType.Sra, [IXOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, 0xcb, 0xfff, (rI & 0xFF) | 0x28], Instruction(
                    InstructionType.Sra, [IXOFF, r]));
    hold.add([0xfd, 0xcb, 0xfff, 0x2e], Instruction(InstructionType.Sra, [IYOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, 0xcb, 0xfff, (rI & 0xFF) | 0x28], Instruction(
                    InstructionType.Sra, [IYOFF, r]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xcb, (rI & 0xFF) | 0x28], Instruction(InstructionType.Sra, [
                        r
                    ]));
    hold.add([0xcb, 0x3e], Instruction(InstructionType.Srl, [
                OR16_LK(Register.HL)
            ]));
    hold.add([0xdd, 0xcb, 0xfff, 0x3e], Instruction(InstructionType.Srl, [IXOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, 0xcb, 0xfff, (rI & 0xFF) | 0x38], Instruction(
                    InstructionType.Srl, [IXOFF, r]));
    hold.add([0xfd, 0xcb, 0xfff, 0x3e], Instruction(InstructionType.Srl, [IYOFF]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, 0xcb, 0xfff, (rI & 0xFF) | 0x38], Instruction(
                    InstructionType.Srl, [IYOFF, r]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xcb, (rI & 0xFF) | 0x38], Instruction(InstructionType.Srl, [
                        r
                    ]));
    hold.add([0x96], Instruction(InstructionType.Sub, [OR16_LK(Register.HL)]));
    hold.add([0xdd, 0x96, 0xfff], Instruction(InstructionType.Sub, [IXOFF]));
    hold.add([0xfd, 0x96, 0xfff], Instruction(InstructionType.Sub, [IYOFF]));
    hold.add([0xd6, 0xfff], Instruction(InstructionType.Sub, [IMM8]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([(rI & 0xFF) | 0x90], Instruction(InstructionType.Sub, [r]));
    foreach (rI, r; EIGHTBIT_REGS[1])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, (rI & 0xFF) | 0x90], Instruction(InstructionType.Sub, [
                        r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[2])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, (rI & 0xFF) | 0x90], Instruction(InstructionType.Sub, [
                        r
                    ]));
    hold.add([0xed, 0x34], Instruction(InstructionType.Tst, [
                OR16_LK(Register.HL)
            ]));
    hold.add([0xed, 0x64, 0xfff], Instruction(InstructionType.Tst, [IMM8]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xed, (rI << 3 & 0xFF) | 0x4], Instruction(InstructionType.Tst, [
                        r
                    ]));
    hold.add([0xed, 0x74, 0xfff], Instruction(InstructionType.Tstio, [IMM8]));
    hold.add([0xae], Instruction(InstructionType.Xor, [OR16_LK(Register.HL)]));
    hold.add([0xdd, 0xae, 0xfff], Instruction(InstructionType.Xor, [IXOFF]));
    hold.add([0xfd, 0xae, 0xfff], Instruction(InstructionType.Xor, [IYOFF]));
    hold.add([0xee, 0xfff], Instruction(InstructionType.Xor, [IMM8]));
    foreach (rI, r; EIGHTBIT_REGS[0])
        if (r.variety != OperandVariety.Unknown)
            hold.add([(rI & 0xFF) | 0xa8], Instruction(InstructionType.Xor, [r]));
    foreach (rI, r; EIGHTBIT_REGS[1])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xdd, (rI & 0xFF) | 0xa8], Instruction(InstructionType.Xor, [
                        r
                    ]));
    foreach (rI, r; EIGHTBIT_REGS[2])
        if (r.variety != OperandVariety.Unknown)
            hold.add([0xfd, (rI & 0xFF) | 0xa8], Instruction(InstructionType.Xor, [
                        r
                    ]));
    return hold;

}
const OpcodeHolder GLOBAL_LOOKUP = makeGlobalLookup();
Instruction getInstruction(const(ubyte[]) data, ref size_t index) => getInstruction_nullable(
    data, index);

Nullable!Instruction getInstruction_nullable(const(ubyte[]) data, ref size_t index) {
    return GLOBAL_LOOKUP.lookup(data, index);
}
