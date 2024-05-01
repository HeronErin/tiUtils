module dissasembly.z80;
import std.format;
import std.string : strip;
enum Register : ubyte{
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

    IXH, IXL, IYH, IYL
}
string toAsmString(Register reg){
    import std.conv : to;
    import std.string : toLower;

    assert(reg != Register.UNKNOWN);
    if (reg == Register.SHADOW_AF) return "af'";
    return reg.to!string.toLower;
}
enum ConditionVariety : ubyte{
    NZ, Z, NC, C, PO, PE, P, M
}
enum OperandVariety : ubyte{
    Reg8,
    Reg16,
    Imm8,
    Imm16,

    Reg16Lookup,
    Imm16Lookup,
    Condition,
    Rst
}
struct Operand{
    OperandVariety variety;
    bool isLabel;

        Register register = Register.UNKNOWN;
        ConditionVariety condition;
        ubyte rst;
        ubyte imm8;
        ushort imm16;
    
    string toString(){
        import std.conv;
        if (variety == OperandVariety.Reg8 || variety == OperandVariety.Reg16 )
            return register.to!string;
        if (variety ==OperandVariety.Rst)
            return rst.to!string;
        if (variety ==OperandVariety.Imm8)
            return "Imm8";
        if (variety ==OperandVariety.Imm16)
            return "Imm16";
        if (variety ==OperandVariety.Imm16Lookup)
            return "(Imm16)";
        if (variety ==OperandVariety.Reg16Lookup)
            return "(" ~ register.to!string ~ ")";
        assert(0);
    }
}
Operand OR8(Register register){
    Operand operand;
    operand.variety = OperandVariety.Reg8;
    operand.register = register;
    return operand;
}
// Used for when specified masks in an opcode specify the register
const Operand[] Reg8_rrr = [OR8(Register.B), OR8(Register.C), OR8(Register.D), OR8(Register.E), OR8(Register.H), OR8(Register.L), OR8(Register.UNKNOWN), OR8(Register.A)];
const Operand[] Reg8_j   = [OR8(Register.IXH), OR8(Register.IXL), OR8(Register.IYH), OR8(Register.IYL)];

Operand OR16(Register register){
    Operand operand;
    operand.variety = OperandVariety.Reg16;
    operand.register = register;
    return operand;
}
                                                                    // Maybe Hl??
const Operand[] Reg16_qq = [OR16(Register.BC), OR16(Register.DE), OR16(Register.HL), OR16(Register.SP)];
const Operand[] Reg16_pp = [OR16(Register.BC), OR16(Register.DE), OR16(Register.HL), OR16(Register.AF)];
const Operand[] Reg16_I = [OR16(Register.IX), OR16(Register.IY)];

Operand OR16_LK(Register register){
    Operand operand;
    operand.variety = OperandVariety.Reg16Lookup;
    operand.register = register;
    return operand;
}
Operand IMM8(){
    Operand operand;
    operand.variety = OperandVariety.Imm8;
    return operand;
}

Operand LIMM8(){
    Operand operand;
    operand.variety = OperandVariety.Imm8;
    operand.isLabel = true;
    return operand;
}
Operand LIMM16(){
    Operand operand;
    operand.variety = OperandVariety.Imm16;
    operand.isLabel = true;
    return operand;
}

Operand IMM16(){
    Operand operand;
    operand.variety = OperandVariety.Imm16;

    return operand;
}
Operand IMM16_LK(){
    Operand operand;
    operand.variety = OperandVariety.Imm16Lookup;
    return operand;
}
Operand Con(ConditionVariety condition){
    Operand operand;
    operand.variety = OperandVariety.Condition;
    operand.condition = condition;
    return operand;
}
const Operand[] Condition_ccc = [Con(ConditionVariety.NZ), Con(ConditionVariety.Z), Con(ConditionVariety.NC), Con(ConditionVariety.C), Con(ConditionVariety.PO), Con(ConditionVariety.PE), Con(ConditionVariety.P), Con(ConditionVariety.M)];
enum InstructionType{
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
    Ei
}

string asOpString(InstructionType type){
    import std.conv : to;
    import std.string : toLower;
    assert(type != InstructionType.Unknown);
    assert(type != InstructionType.Indirection);
    return type.to!string.toLower;
}
struct Instruction{
    InstructionType type;
    Operand[] operands;
    Instruction[ubyte] indirection = null;
}
string toAssembly(Instruction instruction){
    string ret = instruction.type.asOpString ~ " ";

    size_t oprLength = instruction.operands.length;
    for (size_t i; i < oprLength; i++){
        bool isFinal = oprLength-1 == i;
        Operand operand = instruction.operands[i];
        switch (operand.variety){
            case OperandVariety.Reg8:
            case OperandVariety.Reg16:
                ret ~= operand.register.toAsmString;
                break;
            case OperandVariety.Reg16Lookup:
                ret ~= "(" ~ operand.register.toAsmString ~ ")";
                break;
            case OperandVariety.Condition:
                import std.string : toLower;
                import std.conv;
                ret ~= operand.condition.to!string.toLower;
                break;
            case OperandVariety.Rst:
                ret ~= format("%02X", operand.rst) ~ "h";
                break;
            case OperandVariety.Imm8:
                if (instruction.type == InstructionType.Out || instruction.type == InstructionType.In) ret~= "(";
                ret ~= format("%02X", operand.imm8) ~ "h";
                if (instruction.type == InstructionType.Out || instruction.type == InstructionType.In) ret~= ")";
                break;
            case OperandVariety.Imm16:
                ret ~= format("%04X", operand.imm16) ~ "h";
                break;
            case OperandVariety.Imm16Lookup:
                ret ~= "("~format("%04X", operand.imm16) ~ "h)";
                break;
            default: assert(0);
        }
        if (!isFinal)
            ret ~= ", ";
    }
    return ret.strip();
}



private Instruction[ubyte] genMainInstructions(){
    Instruction[ubyte] instructions;

    instructions[0] = Instruction(InstructionType.Nop);
    instructions[0x02] = Instruction(InstructionType.Ld, [OR16_LK(Register.BC), OR8(Register.A)]);
    instructions[0x07] = Instruction(InstructionType.Rlca, []);
    instructions[0x08] = Instruction(InstructionType.Ex, [OR16(Register.AF), OR16(Register.SHADOW_AF)]);
    instructions[0x0A] = Instruction(InstructionType.Ld, [OR8(Register.A), OR16_LK(Register.BC)]);
    instructions[0x0F] = Instruction(InstructionType.Rrca, []);
    instructions[0x10] = Instruction(InstructionType.Djnz, [LIMM8]);
    instructions[0x17] = Instruction(InstructionType.Rla, []);
    instructions[0x18] = Instruction(InstructionType.Jr, [LIMM8]);
    instructions[0x1a] = Instruction(InstructionType.Ld, [OR8(Register.A), OR16_LK(Register.DE)]);
    instructions[0x3a] = Instruction(InstructionType.Ld, [OR8(Register.A), IMM16_LK]);
    instructions[0x1F] = Instruction(InstructionType.Rra, []);
    instructions[0x27] = Instruction(InstructionType.Daa, []);
    instructions[0x2F] = Instruction(InstructionType.Cpl, []);
    instructions[0x37] = Instruction(InstructionType.Scf, []);
    instructions[0x3f] = Instruction(InstructionType.Ccf, []);
    instructions[0x76] = Instruction(InstructionType.Halt, []);
    instructions[0x34] = Instruction(InstructionType.Inc, [OR16_LK(Register.HL)]);
    instructions[0x35] = Instruction(InstructionType.Dec, [OR16_LK(Register.HL)]);

    instructions[0x12] = Instruction(InstructionType.Ld, [OR16_LK(Register.DE), OR8(Register.A)]);
    instructions[0x22] = Instruction(InstructionType.Ld, [IMM16_LK, OR16(Register.HL)]);
    instructions[0x32] = Instruction(InstructionType.Ld, [IMM16_LK, OR8(Register.A)]);
    instructions[0x36] = Instruction(InstructionType.Ld, [OR16_LK(Register.HL), IMM8]);

    instructions[0x2A] = Instruction(InstructionType.Ld, [OR16(Register.HL), IMM16_LK]);
    
    instructions[0x20] = Instruction(InstructionType.Jr, [Con(ConditionVariety.NZ), LIMM8]);
    instructions[0x28] = Instruction(InstructionType.Jr, [Con(ConditionVariety.Z), LIMM8]);
    instructions[0x30] = Instruction(InstructionType.Jr, [Con(ConditionVariety.NC), LIMM8]);
    instructions[0x38] = Instruction(InstructionType.Jr, [Con(ConditionVariety.C), LIMM8]);

    instructions[0x86] = Instruction(InstructionType.Add, [OR8(Register.A), OR16_LK(Register.HL)]);
    instructions[0x8E] = Instruction(InstructionType.Adc, [OR8(Register.A), OR16_LK(Register.HL)]);
    instructions[0x9E] = Instruction(InstructionType.Sbc, [OR8(Register.A), OR16_LK(Register.HL)]);
    instructions[0xDE] = Instruction(InstructionType.Sbc, [OR8(Register.A), IMM8]);
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

    instructions[0xc6] = Instruction(InstructionType.Add, [OR8(Register.A), IMM8]);
    instructions[0xd3] = Instruction(InstructionType.Out, [IMM8, OR8(Register.A)]);
    instructions[0xd6] = Instruction(InstructionType.Sub, [IMM8]);
    instructions[0xd9] = Instruction(InstructionType.Exx, []);
    instructions[0xdb] = Instruction(InstructionType.In, [OR8(Register.A), IMM8]);

    instructions[0xCD] = Instruction(InstructionType.Call, [LIMM16]);
    instructions[0xCE] = Instruction(InstructionType.Adc, [OR8(Register.A), IMM8]);

    instructions[0xe3] = Instruction(InstructionType.Ex, [OR16_LK(Register.SP), OR16(Register.HL)]);
    instructions[0xe6] = Instruction(InstructionType.And, [IMM8]);
    instructions[0xe9] = Instruction(InstructionType.Jp, [OR16_LK(Register.HL)]);
    instructions[0xeb] = Instruction(InstructionType.Ex, [OR16(Register.DE), OR16(Register.HL)]);
    instructions[0xee] = Instruction(InstructionType.Xor, [IMM8]);
    instructions[0xf6] = Instruction(InstructionType.Or, [IMM8]);
    instructions[0xfe] = Instruction(InstructionType.Cp, [IMM8]);
    instructions[0xf3] = Instruction(InstructionType.Di, []);
    instructions[0xfb] = Instruction(InstructionType.Ei, []);
    instructions[0xf9] = Instruction(InstructionType.Ld, [OR16(Register.SP), OR16(Register.HL)]);
    static foreach (i, cond; Condition_ccc)
    {
        instructions[0b1100_0100 | (i << 3) ] = Instruction(InstructionType.Call, [cond, LIMM16]);
        instructions[0b1100_0010 | (i << 3) ] = Instruction(InstructionType.Jp, [cond, LIMM16]);
        instructions[0b1100_0000 | (i << 3) ] = Instruction(InstructionType.Ret, [cond]);
    }
    static foreach (rst_v; 0..0b111+1)
    {{
        Operand rstOperand;
        rstOperand.variety = OperandVariety.Rst;
        rstOperand.rst = [0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38][rst_v];
        instructions[0b1100_0111 | (rst_v << 3) ] = Instruction(InstructionType.Rst, [rstOperand]);
    }}
    static foreach (i, opr; Reg8_rrr) if (opr.register != Register.UNKNOWN)
    {
        instructions[0b0111_0000 | i] = Instruction(InstructionType.Ld, [OR16_LK(Register.HL), opr]);
        instructions[0b1000_1000 | i] = Instruction(InstructionType.Adc, [OR8(Register.A), opr]);
        instructions[0b1000_0000 | i] = Instruction(InstructionType.Add, [OR8(Register.A), opr]);
        instructions[0b1010_0000 | i] = Instruction(InstructionType.And, [opr]);
        instructions[0b1011_1000 | i] = Instruction(InstructionType.Cp, [opr]);
        instructions[0b1011_0000 | i] = Instruction(InstructionType.Or, [opr]);
        instructions[0b0000_0101 | (i << 3)] = Instruction(InstructionType.Dec, [opr]);
        instructions[0b0000_0100 | (i << 3)] = Instruction(InstructionType.Inc, [opr]);
        instructions[0b0000_0110 | (i << 3)] = Instruction(InstructionType.Ld, [opr, IMM8]);
        instructions[0b0100_0110 | (i << 3)] = Instruction(InstructionType.Ld, [opr, OR16_LK(Register.HL)]);
        instructions[0b1001_1000 | i] = Instruction(InstructionType.Sbc, [OR8(Register.A), opr]);
        instructions[0b1001_0000 | i] = Instruction(InstructionType.Sub, [opr]);
        instructions[0b1010_1000 | i] = Instruction(InstructionType.Xor, [opr]);
        
    }
    static foreach (i, opr; Reg16_pp)
    {
        instructions[0b1100_0001 | (i << 4)] = Instruction(InstructionType.Pop, [opr]);
        instructions[0b1100_0101 | (i << 4)] = Instruction(InstructionType.Push, [opr]);
    }
    static foreach (i, opr; Reg16_qq)
    {
        instructions[0b0000_1001 | (i << 4)] = Instruction(InstructionType.Add, [OR16(Register.HL), opr]);
        instructions[0b0000_1011 | (i << 4)] = Instruction(InstructionType.Dec, [opr]);
        instructions[0b0000_0011 | (i << 4)] = Instruction(InstructionType.Inc, [opr]);
        instructions[0b0000_0001  | (i << 4)] = Instruction(InstructionType.Ld, [opr, IMM16]);
    }
    static foreach (to_index, reg_to; Reg8_rrr) if (reg_to.register != Register.UNKNOWN)
    {
        static foreach (from_index, reg_from; Reg8_rrr) if (reg_from.register != Register.UNKNOWN)
        {
            instructions[cast(ubyte)0b0100_0000 | (to_index << 3) | from_index] = Instruction(InstructionType.Ld, [reg_to, reg_from]);
        }
    }

    return instructions;
}
private Instruction[ubyte] BitIndirection(){
    Instruction[ubyte] instructions;
    return instructions;
}
private Instruction[ubyte] IxIndirection() => BitIndirection();
private Instruction[ubyte] MiscIndirection() =>BitIndirection();
private Instruction[ubyte] IyIndirection() => BitIndirection();

Instruction[ubyte] MAIN = genMainInstructions();

import std.stdio;
Instruction getInstruction(ubyte[] data, ref size_t index){
    Instruction ins; 
    const(Instruction)[ubyte] indexMe;

    indexMe = MAIN;
    do{
        ins = cast(Instruction) indexMe[data[index++]];
        if (ins.type != InstructionType.Indirection)
            break;
        indexMe = ins.indirection;
    }while(ins.type == InstructionType.Indirection);
    
    Operand[] operands = new Operand[ins.operands.length];
    operands[0..ins.operands.length] = ins.operands;
    ins.operands = operands;
    foreach (ref value; ins.operands)
    {
        switch (value.variety){
            case OperandVariety.Reg8:
            case OperandVariety.Reg16:
            case OperandVariety.Reg16Lookup:
            case OperandVariety.Condition:
            case OperandVariety.Rst:
                break;
            case OperandVariety.Imm8:
                value.imm8 = data[index++];
                break;
            case OperandVariety.Imm16:
            case OperandVariety.Imm16Lookup:
                value.imm16 = data[index++] | (data[index++] << 8);
                break;
            default: assert(0);
        }
    }
    
    return ins;
}