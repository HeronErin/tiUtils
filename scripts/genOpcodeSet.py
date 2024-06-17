import requests, string, enum

def isHexPair(pr):
    if len(pr) != 2:
        return False
    for c in pr:
        if not(c in string.digits or c in string.ascii_uppercase):
            return False
    return True

class Operand(enum.Enum):
    imm8 = enum.auto()
    imm16 = enum.auto()

    IOff = enum.auto()
    jr = enum.auto()

rst = """[
    0x00, 0x08,
    0x10, 0x18,
    0x20, 0x28,
    0x30, 0x38
]"""

CAT_TO_OFF = {
    "": 0,
    "ix": 1,
    "iy": 2
}


# 'b', 'dd', 'p', 'r', 'r1', 'r2'
def genSet(str, entry, shift):
    ret = []
    if str == "r" or str == "r1" or str == "r2":
        return ["reg8", CAT_TO_OFF[entry.get("category", "")], shift, str]
    elif str == 'dd':
        return ["reg16", CAT_TO_OFF[entry.get("category", "")], shift, str]
    elif str == "b":
        return ["bit", shift]
    elif str == "p":
        return ["rst", shift]
    else:
        print(str, entry)
table = requests.get("https://raw.githubusercontent.com/deeptoaster/opcode-table/master/opcode-table.json").json()

codes = []
for entry in table:
    info = {
        "length": -1
    }
    opcodeBase = []

    mutIndexes = []
    operandIndexes = []
    for i, b in enumerate(entry["bytes"]):
        if isHexPair(b):
            opcodeBase.append(int(b, 16))
        elif b == "n":
            operandIndexes.append([Operand.imm8, len(opcodeBase)])
            opcodeBase.append(0xFFF)
        elif b == "d":
            operandIndexes.append([Operand.IOff, len(opcodeBase)])
            opcodeBase.append(0xFFF)
        elif b == "d-$-2":
            operandIndexes.append([Operand.jr, len(opcodeBase)])
            opcodeBase.append(0xFFF)
        elif b == "nn":
            operandIndexes.append([Operand.imm16, len(opcodeBase)])
            opcodeBase.append(0xFFF)
            opcodeBase.append(0xFFF)
        else:
            mutData = []
            opcodeBase.append(-1)
            for mut in b.split("+"):
                if "$" == mut[0]:
                    mutData.append(["pre", int(mut[1:], 16)])
                elif "(" == mut[0]:
                    mut = mut[1:-1]
                    op, shift = mut.split("<<")
                    mutData.append(genSet(op, entry, int(shift)))
                else:
                    mutData.append(genSet(mut, entry, 0))
            mutIndexes.append([i, mutData])
    assert len(mutIndexes) < 2, "they updated the list in an unexpected way!!!"
    mutName = []
    tab = 0
    stack = []
    # Make for loop
    if len(mutIndexes):
        indexToMut, mutMethods = mutIndexes[0]
        for mut in mutMethods:
            if mut[0] != "pre":
                print("\t" * tab, end="")

            varName = ""
            if mut[0] == "reg8":
                varName = mut[-1]
                tab+=1
                print(f"foreach({varName}I, {varName} ; EIGHTBIT_REGS[{mut[1]}]) if ({varName}.variety != OperandVariety.Unknown)")
                stack.append((varName, mut[2], True))
            if mut[0] == "reg16":
                stack.append(("reg16", mut[2], True))
                tab+=1
                print(f"foreach(reg16I, reg16 ; SIXTEENBIT_REGS[{mut[1]}])")
            if mut[0] == "bit":
                stack.append(("bit", mut[1], False))
                tab+=1
                print(f"foreach(bit ; 0..8)")
            if mut[0] == "rst":
                stack.append(("rst", 3, True))
                tab+=1
                print(f"foreach(rstI, ubyte rst ; {rst})")
            if mut[0] == "pre":
                stack.append(("pre", mut[1]))
            
    dx = "hold.add([" + ", ".join([hex(o) for o in opcodeBase]) + "]"

    # Replace mut placeholder with mut expression    
    dx = dx.replace("-0x1", 
         " | ".join([
            # THIS IS BAD CODE, DO NOT CODE LIKE THIS! EVER! EVER! EVER! IT GOT OUT OF HAND

            # Test for if reg is offset: s[1] != 0 
            # Test for constant: s[0] == "pre"
            # s[2] is used for if it is not needed to use a different index var
            hex(s[1]) if s[0] == "pre" else (
                "("
                    + s[0] + ('I' if s[2] else '')  
                        + ((" << " + str(s[1])) if s[1] != 0 else "") + 
                " & 0xFF)" 
            ) for s in stack
        ])
    )
    
    iname = entry["mnemonic"].split(" ")[0].lower()
    iname = iname[0].upper() + iname[1:]
    
    dx += f", Instruction(InstructionType.{iname}, ["
    
    operands = []
    if len(entry["mnemonic"].split(" ")) != 1:
        for operand in entry["mnemonic"].split(" ")[1].split(","):
            if operand == "n":
                operands.append("IMM8")
            elif operand == "d":
                operands.append("LIMM8")
            elif operand == "nn":
                if entry["mnemonic"].split(" ")[0] in ["JP", "CALL"]:
                    operands.append("LIMM16")
                else:
                    operands.append("IMM16")
            elif operand == "(nn)":
                operands.append("IMM16_LK")
            elif operand == "(n)":
                operands.append("IMM8_LK")
            elif operand == "(C)":
                operands.append("OR8_LK(Register.C)")
            elif operand in ["A", "B", "C", "D", "E", "H", "L", "F", "R", "I", "IXH", "IXL", "IYH", "IYL"]:
                operands.append(f"OR8(Register.{operand})")
            elif operand in ["AF", "BC", "DE", "HL", "SP", "IX", "IY"]:
                operands.append(f"OR16(Register.{operand})")
            elif operand in ["(AF)", "(BC)", "(DE)", "(HL)", "(SP)", "(IX)", "(IY)"]:
                operands.append(f"OR16_LK(Register.{operand[1:-1]})")
            elif operand == "AF'":
                operands.append("OR16(Register.SHADOW_AF)")
            elif operand == "(IX+d)":
                operands.append("IXOFF")
            elif operand == "(IY+d)":
                operands.append("IYOFF")
            elif operand in ['r', 'r1', 'r2', 'dd']:
                while (reg := stack.pop(0))[0] == "pre":
                    pass
                operands.append(reg[0])
            elif operand == "p":
                while (reg := stack.pop(0))[0] == "pre":
                    pass
                assert reg[0] == "rst", reg
                operands.append("RST(rst)")
            elif operand == "b":
                while (reg := stack.pop(0))[0] == "pre":
                    pass
                operands.append("PIMM8(cast(ubyte) "+reg[0]+")")
            elif operand in ["NZ","Z", "NC", "C", "PO", "PE", "P", "M"]:
                operands.append(f"CON(ConditionVariety.{operand})")
            elif operand in "1234567890":
                operands.append("PIMM8("+operand+")")
            else:
                assert 0, entry
        dx += ", ".join(operands)
    dx += "]));"

    print("\t" * tab, end="")
    print(dx)
