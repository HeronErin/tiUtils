import json
# https://gist.github.com/HeronErin/377bd3bd7ec93f6b13e654259ec9edcd
inp = """[["in0 b,(<var>n</var>)","ED  00  <var>n</var>"],["out0 (<var>n</var>),b","ED  01  <var>n</var>"],["tst b","ED  04"],["in0 c,(<var>n</var>)","ED  08  <var>n</var>"],["out0 (<var>n</var>),c","ED  09  <var>n</var>"],["tst c","ED  0C"],["in0 d,(<var>n</var>)","ED  10  <var>n</var>"],["out0 (<var>n</var>),d","ED  11  <var>n</var>"],["tst d","ED  14"],["in0 e,(<var>n</var>)","ED  18  <var>n</var>"],["out0 (<var>n</var>),e","ED  19  <var>n</var>"],["tst e","ED  1C"],["in0 h,(<var>n</var>)","ED  20  <var>n</var>"],["out0 (<var>n</var>),h","ED  21  <var>n</var>"],["tst h","ED  24"],["in0 l,(<var>n</var>)","ED  28  <var>n</var>"],["out0 (<var>n</var>),l","ED  29  <var>n</var>"],["tst l","ED  2C"],["tst (hl)","ED  34"],["in0 a,(<var>n</var>)","ED  38  <var>n</var>"],["out0 (<var>n</var>),a","ED  39  <var>n</var>"],["tst a","ED  3C"],["in b,(c)","ED  40"],["out (c),b","ED  41"],["sbc hl,bc","ED  42"],["ld (<var>nn</var>),bc","ED  43  <var>nn</var>"],["neg","ED  44"],["retn","ED  45"],["im 0","ED  46"],["ld i,a","ED  47"],["in c,(c)","ED  48"],["out (c),c","ED  49"],["adc hl,bc","ED  4A"],["ld bc,(<var>nn</var>)","ED  4B  <var>nn</var>"],["mlt bc","ED  4C"],["reti","ED  4D"],["ld r,a","ED  4F"],["in d,(c)","ED  50"],["out (c),d","ED  51"],["sbc hl,de","ED  52"],["ld (<var>nn</var>),de","ED  53  <var>nn</var>"],["im 1","ED  56"],["ld a,i","ED  57"],["in e,(c)","ED  58"],["out (c),e","ED  59"],["adc hl,de","ED  5A"],["ld de,(<var>nn</var>)","ED  5B  <var>nn</var>"],["mlt de","ED  5C"],["im 2","ED  5E"],["ld a,r","ED  5F"],["in h,(c)","ED  60"],["out (c),h","ED  61"],["sbc hl,hl","ED  62"],["ld (<var>nn</var>),hl","ED  63  <var>nn</var>"],["tst <var>n</var>","ED  64  <var>n</var>"],["rrd","ED  67"],["in l,(c)","ED  68"],["out (c),l","ED  69"],["adc hl,hl","ED  6A"],["ld hl,(<var>nn</var>)","ED  6B  <var>nn</var>"],["mlt hl","ED  6C"],["rld","ED  6F"],["in (c)","ED  70"],["out (c),0","ED  71"],["sbc hl,sp","ED  72"],["ld (<var>nn</var>),sp","ED  73  <var>nn</var>"],["tstio <var>n</var>","ED  74  <var>n</var>"],["slp","ED  76"],["in a,(c)","ED  78"],["out (c),a","ED  79"],["adc hl,sp","ED  7A"],["ld sp,(<var>nn</var>)","ED  7B  <var>nn</var>"],["mlt sp","ED  7C"],["otim","ED  83"],["otdm","ED  8B"],["otimr","ED  93"],["otdmr","ED  9B"],["ldi","ED  A0"],["cpi","ED  A1"],["ini","ED  A2"],["outi","ED  A3"],["ldd","ED  A8"],["cpd","ED  A9"],["ind","ED  AA"],["outd","ED  AB"],["ldir","ED  B0"],["cpir","ED  B1"],["inir","ED  B2"],["otir","ED  B3"],["lddr","ED  B8"],["cpdr","ED  B9"],["indr","ED  BA"],["otdr","ED  BB"]]"""
def test():
    print("size_t i;")
    print("string asmStr;")
    for opcode in json.loads(inp):
        byteData = [v for v in opcode[1].replace("\t", " ").split(" ") if v != " " and v]
        hexByteData = []
        for b in byteData:
            if b == "<var>n</var>" or b == "<var>d</var>-$-2" or b == "<var>d</var>":
                hexByteData.append("0x00")
            elif b == "<var>nn</var>":
                hexByteData.append("0x00")
                hexByteData.append("0x00")
            else:
                hexByteData.append("0x"+b)
        pStr = opcode[0].replace(",", ", ").replace("<var>n</var>", "00h").replace("<var>nn</var>", "0000h").replace("<var>d</var>", "00h")

        print("i = 0;")
        print(f"asmStr = ( cast(ubyte[])[{', '.join(hexByteData)}] ).getInstruction(i).toAssembly;")
        print(f'assert(asmStr == "{pStr}",  "`"~ asmStr ~ "` != `{pStr}`");');
def tryInt(strr):
    try:
        int(strr)
        return True
    except:
        return False

reg16bits = ["af", "bc", "de", "hl", "ix", "iy", "sp", "pc"]
reg8bits = ["a", "f", "b", "c", "d", "e", "h", "l", "i", "r", "ixh", "ixl", "iyh", "iyl"]

def someWhatGenOps(ignore=""):
    opcodes = []
    for opcode in json.loads(inp):
        odata = opcode[0]
        while "  " in odata:
            odata=odata.replace("  ", " ")
        odata = odata.split(" ")
        bdata = opcode[1]
        while "  " in bdata:
            bdata=bdata.replace("  ", " ")
        if bdata.startswith(ignore):
            bdata=bdata[len(ignore):]
        bdata=bdata.replace("<var>n</var>", "").replace("<var>nn</var>", "").replace("<var>d</var>", "").strip()

        if (len(odata) == 1):
            opcodes.append(odata[0].capitalize())
            print(f'instructions[0x{bdata}] = Instruction(InstructionType.{odata[0].capitalize()}, []);')
        else:
            operands = []
            for operand in odata[1].split(","):
                if operand == "af'":
                    operands.append(f"OR16(Register.SHADOW_AF")
                elif operand in reg8bits:
                    operands.append(f"OR8(Register.{operand.upper()})")
                elif operand in reg16bits:
                    operands.append(f"OR16(Register.{operand.upper()})")
                elif operand[1:-1] in reg16bits:
                    operands.append(f"OR16_LK(Register.{operand[1:-1].upper()})")
                elif tryInt(operand):
                    operands.append(f"PIMM8({operand})")
                elif operand == "(c)":
                    operands.append(f"OR8_LK(Register.C)")
                elif operand == "(<var>n</var>)":
                    operands.append("IMM8_LK")
                elif operand == "(<var>nn</var>)":
                    operands.append("IMM16_LK")
                elif operand == "<var>n</var>":
                    operands.append("IMM8")
                else:
                    print(operand, [operand[1:-1]], operand[1:-1] in reg16bits)
                opcodes.append(odata[0].capitalize())
            print(f'instructions[0x{bdata}] = Instruction(InstructionType.{odata[0].capitalize()}, [{", ".join(operands)}]);')
    print(",\n".join(set(opcodes)))

# someWhatGenOps("ED ")
test()