/*
@author: Michael Rohs
@date: February 9, 2022

Volume 1, Unprivileged Spec v. 20191213
https://riscv.org/technical/specifications/
Ch. 24. Instruction Set Listings (p. 130)
*/

#include "util.h"
#include "risc.h"

*#define R_MEM_SIZE 4096 // memory size in bytes
*#define R_PROG_ORG 2048 // byte offset of program code in memory (program origin)

// The RISC-V RV32I target is a 32-bit processor.
*#define R_WORD_SIZE 4

/*
All instructions of the RISC processor are 32 bits long. There are six formats:
  Bits:          31:25 24:20 19:15  14:12      11:7    6:0
- R-format:     funct7   rs2   rs1 funct3        rd opcode
- I-format:          imm11:0   rs1 funct3        rd opcode
- S-format:    imm11:5   rs2   rs1 funct3    imm4:0 opcode
- B-format: imm12,10:5   rs2   rs1 funct3 imm4:1,11 opcode
- U-format:                      imm31:12        rd opcode
- J-format:           imm20,10:1,11,19:12        rd opcode
*/
*typedef enum {
    // R-format (register)
    ADD, SUB, XOR, OR, AND, SLL, SRL, SRA, SLT, SLTU, 
    // R-format (register): M standard extension
    MUL, MULH, MULHSU, MULHU, DIV, DIVU, REM, REMU,
    // I-format (immediate)
    ADDI, XORI, ORI, ANDI, SLLI, SRLI, SRAI, SLTI, SLTIU, 
    LB, LH, LW, LBU, LHU, JALR, ECALL, EBREAK, 
    // S-format (store)
    SB, SH, SW, 
    // B-format (branch)
    BEQ, BNE, BLT, BGE, BLTU, BGEU, 
    // U-format (upper immediate)
    LUI, AUIPC, 
    // J-format (jump)
    JAL
} R_Inst

*typedef enum { FormatR, FormatI, FormatS, FormatB, FormatU, FormatJ } R_InstructionFormat

*typedef signed int INTEGER

*typedef struct R_Instruction R_Instruction
*struct R_Instruction
    R_Inst inst
    char* name
    R_InstructionFormat format
    INTEGER opcode, funct7, funct3
    INTEGER rd, rs1, rs2
    INTEGER imm

R_Instruction instructions[] = {
    // R-format (register)
    { ADD, "ADD",       FormatR, 0b0110011, 0, 0, 0, 0, 0, 0 },
    { SUB, "SUB",       FormatR, 0b0110011, 0x20, 0, 0, 0, 0, 0 },
    { XOR, "XOR",       FormatR, 0b0110011, 0, 4, 0, 0, 0, 0 },
    { OR,  "OR",        FormatR, 0b0110011, 0, 6, 0, 0, 0, 0 },
    { AND, "AND",       FormatR, 0b0110011, 0, 7, 0, 0, 0, 0 },
    { SLL, "SLL",       FormatR, 0b0110011, 0, 1, 0, 0, 0, 0 },
    { SRL, "SRL",       FormatR, 0b0110011, 0, 5, 0, 0, 0, 0 },
    { SRA, "SRA",       FormatR, 0b0110011, 0x20, 5, 0, 0, 0, 0 },
    { SLT, "SLT",       FormatR, 0b0110011, 0, 2, 0, 0, 0, 0 },
    { SLTU, "SLTU",     FormatR, 0b0110011, 0, 3, 0, 0, 0, 0 },
    // R-format (register): M standard extension
    { MUL, "MUL",       FormatR, 0b0110011, 1, 0, 0, 0, 0, 0 },
    { MULH, "MULH",     FormatR, 0b0110011, 1, 1, 0, 0, 0, 0 },
    { MULHSU, "MULHSU", FormatR, 0b0110011, 1, 2, 0, 0, 0, 0 },
    { MULHU, "MULHU",   FormatR, 0b0110011, 1, 3, 0, 0, 0, 0 },
    { DIV, "DIV",       FormatR, 0b0110011, 1, 4, 0, 0, 0, 0 },
    { DIVU, "DIVU",     FormatR, 0b0110011, 1, 5, 0, 0, 0, 0 },
    { REM, "REM",       FormatR, 0b0110011, 1, 6, 0, 0, 0, 0 },
    { REMU, "REMU",     FormatR, 0b0110011, 1, 7, 0, 0, 0, 0 },
    // I-format (immediate)
    { ADDI, "ADDI",     FormatI, 0b0010011, 0, 0, 0, 0, 0, 0 },
    { XORI, "XORI",     FormatI, 0b0010011, 0, 4, 0, 0, 0, 0 },
    { ORI,  "ORI",      FormatI, 0b0010011, 0, 6, 0, 0, 0, 0 },
    { ANDI, "ANDI",     FormatI, 0b0010011, 0, 7, 0, 0, 0, 0 },
    { SLLI, "SLLI",     FormatI, 0b0010011, 0, 1, 0, 0, 0, 0 },
    { SRLI, "SRLI",     FormatI, 0b0010011, 0, 5, 0, 0, 0, 0 },
    { SRAI, "SRAI",     FormatI, 0b0010011, 0x20, 5, 0, 0, 0, 0 }, // imm11:5 = 0x20
    { SLTI, "SLTI",     FormatI, 0b0010011, 0, 2, 0, 0, 0, 0 },
    { SLTIU, "SLTIU",   FormatI, 0b0010011, 0, 3, 0, 0, 0, 0 },
    // I-format (load)
    { LB, "LB",         FormatI, 0b0000011, 0, 0, 0, 0, 0, 0 },
    { LH, "LH",         FormatI, 0b0000011, 0, 1, 0, 0, 0, 0 },
    { LW, "LW",         FormatI, 0b0000011, 0, 2, 0, 0, 0, 0 },
    { LBU, "LBU",       FormatI, 0b0000011, 0, 4, 0, 0, 0, 0 },
    { LHU, "LHU",       FormatI, 0b0000011, 0, 5, 0, 0, 0, 0 },
    // I-format (jump and call):
    { JALR, "JALR",     FormatI, 0b1100111, 0, 0, 0, 0, 0, 0 },
    { ECALL, "ECALL",   FormatI, 0b1110011, 0, 0, 0, 0, 0, 0 }, // imm = 0
    { EBREAK, "EBREAK", FormatI, 0b1110011, 0, 0, 0, 0, 0, 1 }, // imm = 1
    // S-format (store):
    { SB, "SB",         FormatS, 0b0100011, 0, 0, 0, 0, 0, 0 },
    { SH, "SH",         FormatS, 0b0100011, 0, 1, 0, 0, 0, 0 },
    { SW, "SW",         FormatS, 0b0100011, 0, 2, 0, 0, 0, 0 },
    // B-format (branch)
    { BEQ, "BEQ",       FormatB, 0b1100011, 0, 0, 0, 0, 0, 0 },
    { BNE, "BNE",       FormatB, 0b1100011, 0, 1, 0, 0, 0, 0 },
    { BLT, "BLT",       FormatB, 0b1100011, 0, 4, 0, 0, 0, 0 },
    { BGE, "BGE",       FormatB, 0b1100011, 0, 5, 0, 0, 0, 0 },
    { BLTU, "BLTU",     FormatB, 0b1100011, 0, 6, 0, 0, 0, 0 },
    { BGEU, "BGEU",     FormatB, 0b1100011, 0, 7, 0, 0, 0, 0 },
    // U-format (upper immediate)
    { LUI, "LUI",       FormatU, 0b0110111, 0, 0, 0, 0, 0, 0 },
    { AUIPC, "AUIPC",   FormatU, 0b0010111, 0, 0, 0, 0, 0, 0 },
    // J-format (jump)
    { JAL, "JAL",       FormatJ, 0b1101111, 0, 0, 0, 0, 0, 0 },
}
#define R_INSTRUCTION_COUNT (sizeof(instructions) / sizeof(instructions[0]))

*enum R_ECallFunctions { RD, WRD, WRH, WRL, RB, WRB }
char* ecall_functions_names[] = { "RD", "WRD", "WRH", "WRL", "RB", "WRB" }

int funct7(R_Inst inst)
    return instructions[inst].funct7

int funct3(R_Inst inst)
    return instructions[inst].funct3

int opcode(R_Inst inst)
    return instructions[inst].opcode

int format(R_Inst inst)
    return instructions[inst].format

// Decodes the given instruction.
*void R_decode_instruction(INTEGER x, /*out*/R_Instruction* inst)
    require_not_null(inst)
    int opcode = x & 0x7f
    int funct7 = (opcode == 0b0110011) ? (x >> 25) & 0x7f : 0
    int funct3 = (opcode == 0b0110111 || opcode == 0b0010111 || opcode == 0b1101111)\
               ? 0 : (x >> 12) & 7
    int rs2 = (x >> 20) & 0x1f
    int rs1 = (x >> 15) & 0x1f
    int rd = (x >> 7) & 0x1f
    int imm
    for int i = 0; i < R_INSTRUCTION_COUNT; i++ do
        R_Instruction* inst2 = instructions + i
        if inst2->opcode == opcode && inst2->funct7 == funct7 && inst2->funct3 == funct3 do
            *inst = *inst2
            switch opcode do
                case 0b0110011: // R
                    // Bits:      31:25 24:20 19:15  14:12 11:7    6:0
                    // R-format: funct7   rs2   rs1 funct3   rd opcode
                    inst->rd = rd
                    inst->rs1 = rs1
                    inst->rs2 = rs2
                    return
                case 0b0010011: // I
                case 0b0000011: // I (load)
                case 0b1100111: // I (JALR)
                    // Bits:     31:25 24:20 19:15  14:12 11:7    6:0
                    // I-format:     imm11:0   rs1 funct3   rd opcode
                    inst->rd = rd
                    inst->rs1 = rs1
                    inst->imm = x >> 20 // shift and sign-extend
                    return
                case 0b1110011: // I (ECALL, EBREAK)
                    imm = (x >> 20) & 0xfff
                    if imm == 0 do
                        *inst = instructions[ECALL]
                        // rd and rs1 should be 0 according to spec,
                        // used here to simplify "environment functions"
                        inst->rd = rd
                        inst->rs1 = rs1
                        return
                    else if imm == 1 do
                        *inst = instructions[EBREAK]
                        inst->rd = rd
                        inst->rs1 = rs1
                        return
                    else
                        break
                case 0b0100011: // S
                    // Bits:       31:25 24:20 19:15  14:12   11:7    6:0
                    // S-format: imm11:5   rs2   rs1 funct3 imm4:0 opcode
                    inst->rs1 = rs1
                    inst->rs2 = rs2
                    imm = ((x >> 20) & (0x7f << 5)) | ((x >> 7) & 0x1f)
                    imm = (imm << 20) >> 20 // sign extend
                    inst->imm = imm
                    return
                case 0b1100011: // B
                    // Bits:          31:25 24:20 19:15  14:12      11:7   6:0
                    // B-format: imm12,10:5   rs2   rs1 funct3 imm4:1,11 opcode
                    inst->rs1 = rs1
                    inst->rs2 = rs2
                    imm = ((x >> (31-12)) & (1 << 12)) | ((x << (11-7)) & (1 << 11))\
                        | ((x >> (25-5)) & (0x3f << 5)) | ((x >> (8-1)) & (0xf << 1))
                    imm = (imm << 19) >> 19 // sign extend
                    inst->imm = imm
                    return
                case 0b0110111: // U
                case 0b0010111:
                    // Bits:   31:25 24:20 19:15 14:12 11:7    6:0
                    // U-Type:                imm31:12   rd opcode
                    inst->rd = rd
                    inst->imm = x >> 12 // shift and sign-extend
                    return
                case 0b1101111: // J
                    // Bits:     31:25   24:20 19:15 14:12 11:7    6:0
                    // J-format: imm20,10:1,11,      19:12   rd opcode
                    inst->rd = rd
                    imm = ((x >> (31-20)) & (1 << 20)) | (x & (0xff << 12))\
                        | ((x >> (20-11)) & (1 << 11)) | ((x >> (21-1)) & (0x3ff << 1))
                    imm = (imm << 11) >> 11 // sign extend
                    inst->imm = imm
                    return
    exit_if(false, "not implemented: %08x %x %x %x", x, opcode, funct7, funct3)

// Prints the instruction encoded in x.
*void R_print_instruction(INTEGER x)
    R_Instruction i
    R_decode_instruction(x, &i)
    switch i.format do
        case FormatR:
            printf("%-6s x%d x%d x%d\n", i.name, i.rd, i.rs1, i.rs2)
            break
        case FormatI:
            if i.inst == ECALL || i.inst == EBREAK do
                printf("%-6s %s x%d\n", i.name, ecall_functions_names[i.rd], i.rs1)
            else
                printf("%-6s x%d x%d %d\n", i.name, i.rd, i.rs1, i.imm)
            break
        case FormatS:
            printf("%-6s x%d x%d %d\n", i.name, i.rs2, i.rs1, i.imm)
            break
        case FormatB:
            printf("%-6s x%d x%d %d\n", i.name, i.rs1, i.rs2, i.imm)
            break
        case FormatU:
            printf("%-6s x%d %d\n", i.name, i.rd, i.imm)
            break
        case FormatJ:
            printf("%-6s x%d %d\n", i.name, i.rd, i.imm)
            break
        default:
            printf("unknown format: %08x\n", x)
            break

/*
Example calls:
R_encode_R(ADD, rd, rs1, rs2)
R_encode_I(ADDI, rd, rs1, imm)
R_encode_I(LB, rd, rs1, imm)
R_encode_S(SB, rs2, rs1, imm)       M[rs1+imm][0:7] = rs2[0:7]
R_encode_B(BEQ, rs1, rs2, imm)      if(rs1 == rs2) PC += imm
R_encode_J(JAL, rd, imm)            rd = PC+4; PC += imm
R_encode_I(JALR, rd, rs1, imm)      rd = PC+4; PC = rs1 + imm
R_encode_U(LUI, rd, imm)

  Bits:          31:25 24:20 19:15  14:12      11:7    6:0
- R-format:     funct7   rs2   rs1 funct3        rd opcode
- I-format:          imm11:0   rs1 funct3        rd opcode
- S-format:    imm11:5   rs2   rs1 funct3    imm4:0 opcode
- B-format: imm12,10:5   rs2   rs1 funct3 imm4:1,11 opcode
- U-format:                      imm31:12        rd opcode
- J-format:           imm20,10:1,11,19:12        rd opcode
*/

/*
Encodes an instruction according to the given format.
R_encode_R(ADD, rd, rs1, rs2)

Bits:      31:25 24:20 19:15  14:12 11:7    6:0
R-format: funct7   rs2   rs1 funct3   rd opcode
*/
*INTEGER R_encode_R(int inst, int rd, int rs1, int rs2)
    require("valid format", format(inst) == FormatR)
    require("valid range", 0 <= rd && rd <= 31)
    require("valid range", 0 <= rs1 && rs1 <= 31)
    require("valid range", 0 <= rs2 && rs2 <= 31)
    return (funct7(inst) << 25) | (rs2 << 20) | (rs1 << 15) | (funct3(inst) << 12)\
         | (rd << 7) | opcode(inst)

/*
Encodes an instruction according to the given format.
R_encode_I(ADDI, rd, rs1, imm)

Bits:     31:25 24:20 19:15i 14:12 11:7    6:0
I-format:     imm11:0   rs1 funct3   rd opcode
*/
*INTEGER R_encode_I(int inst, int rd, int rs1, int imm)
    require("valid format", format(inst) == FormatI)
    require("valid range", 0 <= rd && rd <= 31)
    require("valid range", 0 <= rs1 && rs1 <= 31)
    require("valid range", -0x800 <= imm && imm <= 0x7ff)
    if inst == SRAI do imm |= (0x20 << 5)
    else if inst == EBREAK do imm = 1
    return (imm << 20) | (rs1 << 15) | (funct3(inst) << 12) | (rd << 7) | opcode(inst)

/*
Encodes an instruction according to the given format.
R_encode_S(SB, rs2, rs1, imm)       M[rs1+imm][0:7] = rs2[0:7]

Bits:       31:25 24:20 19:15  14:12   11:7    6:0
S-format: imm11:5   rs2   rs1 funct3 imm4:0 opcode
*/
*INTEGER R_encode_S(int inst, int rs2, int rs1, int imm)
    require("valid format", format(inst) == FormatS)
    require("valid range", 0 <= rs1 && rs1 <= 31)
    require("valid range", 0 <= rs2 && rs2 <= 31)
    require("valid range", -0x800 <= imm && imm <= 0x7ff)
    return ((imm >> 5) << 25) | (rs2 << 20) | (rs1 << 15) | (funct3(inst) << 12)\
         | ((imm & 0x1f) << 7) | opcode(inst)

/*
Encodes an instruction according to the given format.
R_encode_B(BEQ, rs1, rs2, imm)      if(rs1 == rs2) PC += imm

Bits:          31:25 24:20 19:15  14:12      11:7    6:0
B-format: imm12,10:5   rs2   rs1 funct3 imm4:1,11 opcode
*/
*INTEGER R_encode_B(int inst, int rs1, int rs2, int imm)
    require("valid format", format(inst) == FormatB)
    require("valid range", 0 <= rs1 && rs1 <= 31)
    require("valid range", 0 <= rs2 && rs2 <= 31)
    require("valid range", -0x1000 <= imm && imm <= 0xfff)
    require("even", (imm & 1) == 0)
    return ((imm << (31-12)) & (1 << 31)) | ((imm << (25-5)) & (0x3f << 25)) | (rs2 << 20)\
         | (rs1 << 15) | (funct3(inst) << 12) | ((imm << (8-1)) & (0xf << 8))\
         | ((imm >> (11-7)) & (1 << 7)) | opcode(inst)

/*
Encodes an instruction according to the given format.
R_encode_U(LUI, rd, imm)

Bits:   31:25 24:20 19:15 14:12 11:7    6:0
U-format:              imm31:12   rd opcode
*/
*INTEGER R_encode_U(int inst, int rd, int imm)
    require("valid format", format(inst) == FormatU)
    require("valid range", 0 <= rd && rd <= 31)
    require("valid range", -0x80000 <= imm && imm <= 0x7ffff)
    return (imm << 12) | (rd << 7) | opcode(inst)

/*
Encodes an instruction according to the given format.
R_encode_J(JAL, rd, imm)            rd = PC+4; PC += imm

Bits:       31:25 24:20 19:15 14:12 11:7    6:0
J-format: imm20,10:1,11,      19:12   rd opcode
*/
*INTEGER R_encode_J(int inst, int rd, int imm)
    require("valid format", format(inst) == FormatJ)
    require("valid range", 0 <= rd && rd <= 31)
    require("valid range", -0x100000 <= imm && imm <= 0xfffff)
    require("even", (imm & 1) == 0)
    return ((imm << (31-20)) & (1 << 31)) | ((imm << (21-1)) & (0x3ff << 21))\
         | ((imm << (20-11)) & (1 << 20)) | (imm & (0xff << 12))\
         | (rd << 7) | opcode(inst)

INTEGER IR // instruction register
/*
Integer registers:
  x0: zero
  x1: ra, return address (link register)
  x2: sp, stack pointer
  x3: gp, global pointer
  x4: tp, thread pointer
  x8: s0/fp, frame pointer
*/
INTEGER R[32] // integer registers
INTEGER PC // program counter
INTEGER M[R_MEM_SIZE / R_WORD_SIZE] // program and data memory, organized as 32-bit words

// Modulo operation, 0 <= mod < abs(y).
*INTEGER R_mod(INTEGER x, INTEGER y)
    require("valid divisor", y != 0)
    x = x % y
    if y < 0 do y = -y
    while x < 0 do
        x += y
    ensure("valid remainder", 0 <= x && x < y)
    return x

// Gets 32-bit word at address i.
INTEGER get_M(INTEGER i)
    exit_if(i < 0 || i + R_WORD_SIZE > R_MEM_SIZE, "invalid address (%d)", i)
    exit_if((i % R_WORD_SIZE) != 0, "word address not naturally aligned (%d)", i)
    return M[i / R_WORD_SIZE]

// Gets 16-bit halfword at address i.
INTEGER get_H(INTEGER i)
    exit_if(i < 0 || i + R_WORD_SIZE / 2 > R_MEM_SIZE, "invalid address (%d)", i)
    exit_if((i % (R_WORD_SIZE / 2)) != 0, "halfword address not naturally aligned (%d)", i)
    short* H = (short*)M; // halfword access to memory
    return H[i / (R_WORD_SIZE / 2)]

// Gets 8-bit byte at address i.
INTEGER get_B(INTEGER i)
    exit_if(i < 0 || i >= R_MEM_SIZE, "invalid address (%d)", i)
    char* B = (char*)M; // byte access to memory
    return B[i]

// Sets 32-bit word at address i to x.
void set_M(INTEGER i, INTEGER x)
    exit_if(i < 0 || i + R_WORD_SIZE > R_MEM_SIZE, "invalid address (%d)", i)
    exit_if((i % R_WORD_SIZE) != 0, "word address not naturally aligned (%d)", i)
    M[i / R_WORD_SIZE] = x

// Sets halfword at address i to x.
void set_H(INTEGER i, INTEGER x)
    exit_if(i < 0 || i + R_WORD_SIZE / 2 > R_MEM_SIZE, "invalid address (%d)", i)
    exit_if((i % (R_WORD_SIZE / 2)) != 0, "halfword address not naturally aligned (%d)", i)
    short* H = (short*)M; // halfword access to memory
    H[i / (R_WORD_SIZE / 2)] = x

// Sets byte at address i to x.
void set_B(INTEGER i, INTEGER x)
    exit_if(i < 0 || i >= R_MEM_SIZE, "invalid address (%d)", i)
    char* B = (char*)M; // byte access to memory
    B[i] = x

/*
Executes code starting at the given start address. The start address is given in
bytes relative to the code origin. The code must have been loaded into memory
before.
*/
*void R_execute(INTEGER start)
    require("valid start", start >= 0 
                           && R_PROG_ORG + start + R_WORD_SIZE <= R_MEM_SIZE 
                           && (start % R_WORD_SIZE) == 0)
    INTEGER nextPC
    R_Instruction i
    R[0] = 0 // always zero
    R[1] = 0 // link register
    R[2] = R_MEM_SIZE // stack pointer
    PC = R_PROG_ORG + start // program counter
    while true do // interpretation cycle
        nextPC = PC + R_WORD_SIZE // address of next instruction word
        exit_if(PC < 0 || PC > R_MEM_SIZE - R_WORD_SIZE || (PC & (R_WORD_SIZE - 1)),
                "invalid PC (%d)", PC)
        IR = M[PC / R_WORD_SIZE]; // memory is organized in 32-bit words
        R_decode_instruction(IR, &i)
        switch i.inst do
            // R-format (register)
            case ADD: R[i.rd] = R[i.rs1] + R[i.rs2]; break
            case SUB: R[i.rd] = R[i.rs1] - R[i.rs2]; break
            case XOR: R[i.rd] = R[i.rs1] ^ R[i.rs2]; break
            case OR:  R[i.rd] = R[i.rs1] | R[i.rs2]; break
            case AND: R[i.rd] = R[i.rs1] & R[i.rs2]; break
            case SLL: R[i.rd] = R[i.rs1] << (R[i.rs2] & 0x1f); break
            case SRL: R[i.rd] = (unsigned)R[i.rs1] >> (R[i.rs2] & 0x1f); break
            case SRA: R[i.rd] = R[i.rs1] >> (R[i.rs2] & 0x1f); break
            case SLT: R[i.rd] = R[i.rs1] < R[i.rs2] ? 1 : 0; break
            case SLTU: R[i.rd] = (unsigned)R[i.rs1] < (unsigned)R[i.rs2] ? 1 : 0; break
            // R-format (register): M standard extension
            case MUL: R[i.rd] = R[i.rs1] * R[i.rs2]; break
            case MULH: R[i.rd] = ((int64_t)R[i.rs1] * (int64_t)R[i.rs2]) >> 32; break
            case MULHU: R[i.rd] = ((uint64_t)R[i.rs1] * (uint64_t)R[i.rs2]) >> 32; break
            case MULHSU: R[i.rd] = ((int64_t)R[i.rs1] * (uint64_t)R[i.rs2]) >> 32; break
            // Condition              Dividend  Divisor  DIVU     REMU     DIV    REM
            // Division by zero          x         0    (2^32)-1   x       −1      x
            // Overflow (signed only)  −(2^31)    −1       –       –     −(2^31)   0
            case DIV:
                if R[i.rs2] == 0 do R[i.rd] = -1
                else if R[i.rs1] == 1 << 31 && R[i.rs2] == -1 do R[i.rd] = 1 << 31
                else R[i.rd] = R[i.rs1] / R[i.rs2]
                break
            case DIVU:
                if R[i.rs2] == 0 do R[i.rd] = -1
                else R[i.rd] = (unsigned)R[i.rs1] / (unsigned)R[i.rs2]
                break
            case REM:
                if R[i.rs2] == 0 do R[i.rd] = R[i.rs1]
                else if R[i.rs1] == 1 << 31 && R[i.rs2] == -1 do R[i.rd] = 0
                else R[i.rd] = R[i.rs1] % R[i.rs2]
                break
            case REMU:
                if R[i.rs2] == 0 do R[i.rd] = R[i.rs1]
                else R[i.rd] = (unsigned)R[i.rs1] % (unsigned)R[i.rs2]
                break
            // I-format (immediate)
            case ADDI: R[i.rd] = R[i.rs1] + i.imm; break
            case XORI: R[i.rd] = R[i.rs1] ^ i.imm; break
            case ORI:  R[i.rd] = R[i.rs1] | i.imm; break
            case ANDI: R[i.rd] = R[i.rs1] & i.imm; break
            case SLLI: R[i.rd] = R[i.rs1] << (i.imm & 0x1f); break
            case SRLI: R[i.rd] = (unsigned)R[i.rs1] >> (i.imm & 0x1f); break
            case SRAI: R[i.rd] = R[i.rs1] >> (i.imm & 0x1f); break
            case SLTI: R[i.rd] = R[i.rs1] < i.imm ? 1 : 0; break
            case SLTIU: R[i.rd] = (unsigned)R[i.rs1] < (unsigned)i.imm ? 1 : 0; break
            // I-format (load)
            case LB: R[i.rd] = get_B(R[i.rs1] + i.imm); break
            case LH: R[i.rd] = get_H(R[i.rs1] + i.imm); break
            case LW: R[i.rd] = get_M(R[i.rs1] + i.imm); break
            // I-format (jump and call)
            case JALR: R[i.rd] = PC + R_WORD_SIZE; nextPC = (R[i.rs1] + i.imm) & ~1; break
            case ECALL: // R_encode_I(ECALL, WRD, rs1, 0)
                switch i.rd do
                    case RD: scanf("%d", &R[i.rs1]); break
                    case WRD: printf(" %d", R[i.rs1]); break
                    case WRH: printf(" %xH", R[i.rs1]); break
                    case WRL: printf("\n"); break
                    case RB: R[i.rs1] = getchar(); break
                    case WRB: putchar(R[i.rs1]); break
                    default: exit_if(true, "invalid ECALL function (rd = %d, rs1 = %d)", i.rd, i.rs1)
                break
            // S-format (store): R_encode_S(SB, rs2, rs1, imm)  -->  M[rs1+imm][0:7] = rs2[0:7]
            case SB: set_B(R[i.rs1] + i.imm, R[i.rs2]); break
            case SH: set_H(R[i.rs1] + i.imm, R[i.rs2]); break
            case SW: set_M(R[i.rs1] + i.imm, R[i.rs2]); break
            // B-format (branch)
            case BEQ: if R[i.rs1] == R[i.rs2] do nextPC = PC + i.imm; break
            case BNE: if R[i.rs1] != R[i.rs2] do nextPC = PC + i.imm; break
            case BLT: if R[i.rs1] <  R[i.rs2] do nextPC = PC + i.imm; break
            case BGE: if R[i.rs1] >= R[i.rs2] do nextPC = PC + i.imm; break
            case BLTU: if (unsigned)R[i.rs1] <  (unsigned)R[i.rs2] do nextPC = PC + i.imm; break
            case BGEU: if (unsigned)R[i.rs1] >= (unsigned)R[i.rs2] do nextPC = PC + i.imm; break
            // U-format (upper immediate)
            case LUI: R[i.rd] = i.imm << 12; break
            case AUIPC: R[i.rd] = PC + (i.imm << 12); break
            // J-format (jump)
            case JAL: R[i.rd] = PC + R_WORD_SIZE; nextPC = PC + i.imm; break
            default: exit_if(true, "invalid instruction (inst = %d)", i.inst)
        end. switch
        R[0] = 0 // always zero
        if nextPC == 0 do break // exit interpretation cycle
        if nextPC & (R_WORD_SIZE - 1) do
            exit_if(true, "next instruction address misaligned (PC = %08x, next PC = %08x)", PC, nextPC)
        PC = nextPC
    end. while
end. execute

/*
Loads the code words into memory. The length refers to the number of code words
(not bytes).
*/
*void R_load(INTEGER* code, int len)
    require_not_null(code)
    require("valid length", len >= 0 && R_PROG_ORG + R_WORD_SIZE * len <= R_MEM_SIZE)
    for int i = 0; i < len; i++ do
        M[i + R_PROG_ORG / R_WORD_SIZE] = code[i]
end. load

// Prints the given instructions.
*void R_print_code(INTEGER* code, int len)
    require_not_null(code)
    require("not negative", len >= 0)
    for int i = 0; i < len; i++ do
        printf("%3d ", i)
        R_print_instruction(code[i])

/*
Prints memory words from start index to end index (end is exclusive) in rows of
the given length.
*/
*void R_print_memory(int from, int to, int row_length)
    if from < 0 do from = 0
    if to > R_MEM_SIZE do to = R_MEM_SIZE
    if row_length < 1 do row_length = 1
    int i = from
    while i < to do
        assert("valid index", 0 <= i && i < R_MEM_SIZE)
        for int j = 0; j < row_length; j++ do
            printf("%4d: %6d ", i, M[i])
            i++
            if i >= to do break
        printf("\n")

#define test_equal_instruction(a, e) base_test_equal_instruction(__FILE__, __LINE__, a, e)

bool base_test_equal_instruction(const char *file, int line, R_Instruction actual, R_Instruction expected)
    int ok = true
    if actual.inst != expected.inst do
        printf("inst not equal; ")
        ok = false
    if actual.format != expected.format do
        printf("format not equal; ")
        ok = false
    if actual.opcode != expected.opcode do
        printf("opcode not equal; ")
        ok = false
    if actual.funct7 != expected.funct7 do
        printf("funct7 not equal; ")
        ok = false
    if actual.funct3 != expected.funct3 do
        printf("funct3 not equal; ")
        ok = false
    if actual.rd != expected.rd do
        printf("rd not equal; ")
        ok = false
    if actual.rs1 != expected.rs1 do
        printf("rs1 not equal; ")
        ok = false
    if actual.rs2 != expected.rs2 do
        printf("rs2 not equal; ")
        ok = false
    if actual.rd != expected.rd do
        printf("rd not equal; ")
        ok = false
    if actual.imm != expected.imm do
        printf("imm not equal; ")
        ok = false
    base_test_equal_i(file, line, ok, 1)
    return ok

// Enters instruction into code array.
#define ER(inst, rd, rs1, rs2) code[len++] = R_encode_R(inst, rd, rs1, rs2)
#define EI(inst, rd, rs1, imm) code[len++] = R_encode_I(inst, rd, rs1, imm)
#define EB(inst, rs1, rs2, imm) code[len++] = R_encode_B(inst, rs1, rs2, imm)
#define EU(inst, rd, imm) code[len++] = R_encode_U(inst, rd, imm)
#define ES(inst, rs2, rs1, imm) code[len++] = R_encode_S(inst, rs2, rs1, imm)

int set_reg(INTEGER* code, int len, int reg, INTEGER imm)
    if -0x800 <= imm && imm < 0x800 do
        EI(ADDI, reg, 0, imm)
    else
        INTEGER xl = (imm << 20) >> 20 // lower 12 bits, sign-extend
        // assert: imm == (xu << 12) + xl
        INTEGER xu = (imm - xl) >> 12
        EU(LUI, reg, xu)
        EI(ADDI, reg, reg, xl)
    return len

INTEGER sign(INTEGER x)
    if x > 0 do return 1
    if x < 0 do return -1
    return 0

// Performs some tests.
void test_risc(void)
    require("valid word length", sizeof(INTEGER) == R_WORD_SIZE)

    INTEGER a
    R_Instruction x, y

    // R-format (register):

    a = R_encode_R(ADD, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)
    R_decode_instruction(a, &x)
    y = instructions[ADD]; y.rd = 1; y.rs1 = 2; y.rs2 = 3
    test_equal_instruction(x, y)

    a = R_encode_R(SUB, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)
    R_decode_instruction(a, &x)
    y = instructions[SUB]; y.rd = 1; y.rs1 = 2; y.rs2 = 3
    test_equal_instruction(x, y)

    a = R_encode_R(XOR, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)
    R_decode_instruction(a, &x)
    y = instructions[XOR]; y.rd = 1; y.rs1 = 2; y.rs2 = 3
    test_equal_instruction(x, y)

    a = R_encode_R(OR, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)
    R_decode_instruction(a, &x)
    y = instructions[OR]; y.rd = 1; y.rs1 = 2; y.rs2 = 3
    test_equal_instruction(x, y)

    a = R_encode_R(AND, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)
    R_decode_instruction(a, &x)
    y = instructions[AND]; y.rd = 1; y.rs1 = 2; y.rs2 = 3
    test_equal_instruction(x, y)

    a = R_encode_R(SLL, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)
    R_decode_instruction(a, &x)
    y = instructions[SLL]; y.rd = 1; y.rs1 = 2; y.rs2 = 3
    test_equal_instruction(x, y)

    a = R_encode_R(SRL, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)
    R_decode_instruction(a, &x)
    y = instructions[SRL]; y.rd = 1; y.rs1 = 2; y.rs2 = 3
    test_equal_instruction(x, y)

    a = R_encode_R(SRA, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)
    R_decode_instruction(a, &x)
    y = instructions[SRA]; y.rd = 1; y.rs1 = 2; y.rs2 = 3
    test_equal_instruction(x, y)

    a = R_encode_R(SLT, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)
    R_decode_instruction(a, &x)
    y = instructions[SLT]; y.rd = 1; y.rs1 = 2; y.rs2 = 3
    test_equal_instruction(x, y)

    a = R_encode_R(SLTU, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)
    R_decode_instruction(a, &x)
    y = instructions[SLTU]; y.rd = 1; y.rs1 = 2; y.rs2 = 3
    test_equal_instruction(x, y)

    // I-format (immediate):

    a = R_encode_I(ADDI, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)
    R_decode_instruction(a, &x)
    y = instructions[ADDI]; y.rd = 1; y.rs1 = 2; y.imm = 3
    test_equal_instruction(x, y)

    printf("%d\n", 0x7ff)
    a = R_encode_I(ADDI, 1, 2, 0x7ff)
    printf("%08x ", a); R_print_instruction(a)
    R_decode_instruction(a, &x)
    y = instructions[ADDI]; y.rd = 1; y.rs1 = 2; y.imm = 0x7ff
    test_equal_instruction(x, y)

    a = R_encode_I(ADDI, 1, 2, -1)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_I(ADDI, 1, 2, -2)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_I(ADDI, 1, 2, -2048)
    printf("%08x ", a); R_print_instruction(a)
    R_decode_instruction(a, &x)
    y = instructions[ADDI]; y.rd = 1; y.rs1 = 2; y.imm = -2048
    test_equal_instruction(x, y)

    a = R_encode_I(XORI, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_I(ORI, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_I(ANDI, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_I(SLLI, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_I(SRLI, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_I(SRAI, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_I(SLTI, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_I(SLTIU, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)
    R_decode_instruction(a, &x)
    y = instructions[SLTIU]; y.rd = 1; y.rs1 = 2; y.imm = 3
    test_equal_instruction(x, y)

    // I-format (load):

    a = R_encode_I(LB, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_I(LH, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_I(LW, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_I(LBU, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_I(LHU, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)

    // I-format (jump and call):

    a = R_encode_I(JALR, 1, 2, 3)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_I(ECALL, 0, 0, 0)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_I(EBREAK, 0, 0, 0)
    printf("%08x ", a); R_print_instruction(a)

    // S-format (store):

    a = R_encode_S(SB, 2, 1, 3) // R_encode_S(SB, rs2, rs1, imm)   M[rs1+imm][0:7] = rs2[0:7]
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_S(SH, 2, 1, 3)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_S(SW, 2, 1, 3)
    printf("%08x ", a); R_print_instruction(a)

    printf("%d ", 0x7ff)
    a = R_encode_S(SH, 2, 1, 0x7ff)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_S(SH, 2, 1, -1)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_S(SH, 2, 1, -2)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_S(SH, 2, 1, -2048)
    printf("%08x ", a); R_print_instruction(a)

    // B-format (branch):

    for int i = -10; i <= 10; i += 2 do
        a = R_encode_B(BEQ, 1, 2, i) // R_encode_B(BEQ, rs1, rs2, imm)    if(rs1 == rs2) PC += imm
        printf("%3d %08x ", i, a);     R_print_instruction(a)

    printf("%d ", 0xffe)
    a = R_encode_B(BEQ, 1, 2, 0xffe)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_B(BEQ, 1, 2, -0xffe - 2)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_B(BNE, 1, 2, 10)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_B(BLT, 1, 2, 20)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_B(BGE, 1, 2, 30)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_B(BLTU, 1, 2, 40)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_B(BGEU, 1, 2, 50)
    printf("%08x ", a); R_print_instruction(a)

    // U-format (upper immediate):

    a = R_encode_U(LUI, 1, 2)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_U(AUIPC, 1, 2)
    printf("%08x ", a); R_print_instruction(a)

    // J-format (jump):

    a = R_encode_J(JAL, 1, 2)
    printf("%08x ", a); R_print_instruction(a)

    printf("%d ", 0x0ffffe)
    a = R_encode_J(JAL, 1, 0x0ffffe)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_J(JAL, 1, -2)
    printf("%08x ", a); R_print_instruction(a)

    a = R_encode_J(JAL, 1, -0x0ffffe - 2)
    printf("%08x ", a); R_print_instruction(a)

    INTEGER code[100]
    int len = 0

    for INTEGER i = -0x1000; i <= 0x1000; i++ do
        len = 0
        len = set_reg(code, len, 5, i)
        if len == 1 do
            R_decode_instruction(code[0], &x)
            assert("", x.inst == ADDI && x.imm == i)
        else
            R_decode_instruction(code[0], &x)
            R_decode_instruction(code[1], &y)
            assert("", x.inst == LUI && y.inst == ADDI && (x.imm << 12) + y.imm == i)
    len = 0

    // 123 + 456
    EI(ADDI, 5, 0, 123)
    EI(ADDI, 6, 0, 456)
    EI(ADDI, 7, 5, 6)
    ER(ADD, 8, 5, 6)
    EI(ECALL, WRD, 5, 0)
    EI(ECALL, WRD, 6, 0)
    EI(ECALL, WRD, 7, 0)
    EI(ECALL, WRD, 8, 0)
    EI(ECALL, WRL, 0, 0)
    EI(JALR, 0, 1, 0) // rd = 0, rs1 = 1 (return), imm = 0
    R_print_code(code, len)
    R_load(code, len)
    R_execute(0)
    len = 0
    test_equal_i(R[5], 123)
    test_equal_i(R[6], 456)
    test_equal_i(R[7], 123 + 6)
    test_equal_i(R[8], 123 + 456)

    // 123 - 456
    EI(ADDI, 5, 0, 123)
    EI(ADDI, 6, 0, 456)
    EI(ADDI, 7, 5, -6)
    ER(SUB, 8, 5, 6)
    EI(ECALL, WRD, 5, 0)
    EI(ECALL, WRD, 6, 0)
    EI(ECALL, WRD, 7, 0)
    EI(ECALL, WRD, 8, 0)
    EI(ECALL, WRL, 0, 0)
    EI(JALR, 0, 1, 0) // rd = 0, rs1 = 1 (return), imm = 0
    R_print_code(code, len)
    R_load(code, len)
    R_execute(0)
    len = 0
    test_equal_i(R[5], 123)
    test_equal_i(R[6], 456)
    test_equal_i(R[7], 123 - 6)
    test_equal_i(R[8], 123 - 456)

    // 1 2 ... 10
    EI(ADDI, 5, 0, 1)
    EI(ADDI, 6, 0, 10)
    EI(ECALL, WRD, 5, 0)
    EI(ADDI, 5, 5, 1)
    EB(BGE, 6, 5, -2 * 4)
    EI(ECALL, WRL, 0, 0)

    // 1024 512 256 ... 1
    EI(ADDI, 5, 0, 1024)
    EI(ADDI, 6, 0, 0)
    EI(ECALL, WRD, 5, 0)
    EI(SRLI, 5, 5, 1)
    EB(BLT, 0, 5, -2 * 4)
    EI(ECALL, WRL, 0, 0)

    // immediate has a range of 12 bits, signed
    EI(ADDI, 5, 0, 0x7ff)
    EI(ECALL, WRD, 5, 0)
    EI(ECALL, WRH, 5, 0)
    EI(ECALL, WRL, 0, 0)

    EI(ADDI, 5, 0, -1)
    EI(ECALL, WRD, 5, 0)
    EI(ECALL, WRH, 5, 0)
    EI(ECALL, WRL, 0, 0)

    EI(ADDI, 5, 0, -0x7ff - 1)
    EI(ECALL, WRD, 5, 0)
    EI(ECALL, WRH, 5, 0)
    EI(ECALL, WRL, 0, 0)

    // set 32 bits
    EU(LUI, 5, 0x12345)
    EI(ECALL, WRH, 5, 0)
    EI(ADDI, 5, 5, 0x678)
    EI(ECALL, WRH, 5, 0)
    EI(ECALL, WRL, 0, 0)

    // store and load
    EU(LUI, 5, 0x12345)
    EI(ADDI, 5, 5, 0x678)
    ES(SW, 5, 0, 100)
    EI(ADDI, 5, 0, 0)
    EI(ECALL, WRH, 5, 0)
    EI(LW, 5, 0, 100)
    EI(ECALL, WRH, 5, 0)
    EI(ECALL, WRL, 0, 0)

    EU(LUI, 5, 0x12345)
    EI(ADDI, 5, 5, 0x678)
    ES(SH, 5, 0, 100)
    EI(ADDI, 5, 0, 0)
    EI(ECALL, WRH, 5, 0)
    EI(LH, 5, 0, 100)
    EI(ECALL, WRH, 5, 0)
    EI(ECALL, WRL, 0, 0)

    EU(LUI, 5, 0x12345)
    EI(ADDI, 5, 5, 0x678)
    ES(SB, 5, 0, 100)
    EI(ADDI, 5, 0, 0)
    EI(ECALL, WRH, 5, 0)
    EI(LB, 5, 0, 100)
    EI(ECALL, WRH, 5, 0)
    EI(ECALL, WRL, 0, 0)

    EI(ADDI, 5, 0, -100)
    ES(SH, 5, 0, 100)
    EI(ADDI, 5, 0, 0)
    EI(ECALL, WRH, 5, 0)
    EI(LH, 5, 0, 100)
    EI(ECALL, WRH, 5, 0)
    EI(ECALL, WRD, 5, 0)
    EI(ECALL, WRL, 0, 0)

    EI(ADDI, 5, 0, -100)
    ES(SB, 5, 0, 100)
    EI(ADDI, 5, 0, 0)
    EI(ECALL, WRH, 5, 0)
    EI(LB, 5, 0, 100)
    EI(ECALL, WRH, 5, 0)
    EI(ECALL, WRD, 5, 0)
    EI(ECALL, WRL, 0, 0)

    /* standard input/output
    // read/write decimal
    EI(ADDI, 5, 0, 0)
    EI(ECALL, RD, 5, 0)
    EI(ECALL, WRD, 5, 0)
    EI(ECALL, WRL, 0, 0)
    // read/write byte
    EI(ADDI, 5, 0, 0)
    EI(ECALL, RB, 5, 0)
    EI(ECALL, WRD, 5, 0)
    EI(ECALL, WRL, 0, 0)
    */

    EI(ADDI, 5, 0, 25)
    EI(ADDI, 6, 0, 3)
    ER(MUL, 7, 5, 6)
    ER(DIV, 8, 5, 6)
    ER(REM, 9, 5, 6)
    EI(ECALL, WRD, 7, 0)
    EI(ECALL, WRD, 8, 0)
    EI(ECALL, WRD, 9, 0)
    EI(ECALL, WRL, 0, 0)

    len = set_reg(code, len, 5, 0xffffffff)
    len = set_reg(code, len, 6, 0xffffffff)
    ER(MUL, 7, 5, 6)
    ER(DIV, 8, 5, 6)
    ER(REM, 9, 5, 6)
    EI(ECALL, WRH, 5, 0)
    EI(ECALL, WRH, 6, 0)
    EI(ECALL, WRH, 7, 0)
    EI(ECALL, WRD, 8, 0)
    EI(ECALL, WRD, 9, 0)
    EI(ECALL, WRL, 0, 0)

    EI(JALR, 0, 1, 0) // rd = 0, rs1 = 1 (return), imm = 0
    R_print_code(code, len)
    R_load(code, len)
    R_execute(0)

    int64_t k = 0xf
    int64_t l = 0xf
    int64_t m = k * l
    printf("k = %llx, l = %llx, m = %llx\n", k, l, m)

    k = 0xff
    l = 0xff
    m = k * l
    printf("k = %llx, l = %llx, m = %llx\n", k, l, m)

    k = 0xfff
    l = 0xfff
    m = k * l
    printf("k = %llx, l = %llx, m = %llx\n", k, l, m)

    k = 0xffffffff
    l = 0xffffffff
    m = k * l
    printf("k = %llx, l = %llx, m = %llx\n", k, l, m)
    printf("k = %lld, l = %lld, m = %lld\n", k, l, m)

    k = -1
    l = -1
    m = k * l
    printf("k = %llx, l = %llx, m = %llx\n", k, l, m)
    printf("k = %lld, l = %lld, m = %lld\n", k, l, m)

    uint64_t n = 0xf
    uint64_t o = 0xf
    uint64_t p = n * o
    printf("n = %llx, o = %llx, p = %llx\n", n, o, p)

    n = 0xff
    o = 0xff
    p = n * o
    printf("n = %llx, o = %llx, p = %llx\n", n, o, p)

    n = 0xfff
    o = 0xfff
    p = n * o
    printf("n = %llx, o = %llx, p = %llx\n", n, o, p)

    n = 0xffffffff
    o = 0xffffffff
    p = n * o
    printf("n = %llx, o = %llx, p = %llx\n", n, o, p)
    printf("n = %llu, o = %llu, p = %llu\n", n, o, p)

    n = -1
    o = -1 // 0
    p = n * o
    printf("n = %llx, o = %llx, p = %llx\n", n, o, p)
    printf("n = %llu, o = %llu, p = %llu\n", n, o, p)

    k = 5
    l = 2 // 0
    m = k / l
    printf("k = %lld, l = %lld, m = %lld\n", k, l, m)

    int ki = -(1 << 31)
    int li = 1 // -1
    int mi = ki / li
    printf("ki = %d, li = %d, mi = %d\n", ki, li, mi)

    for ki = -6; ki <= 6; ki++ do
        li = -3 // -3
        mi = ki % li
        int q = ki / li
        printf("%d = %d * %d + %d\n", ki, li, q, mi)
        assert("divident = divisor * quotient + remainder", ki == li * q + mi)
        assert("sign of remainder equals sign of dividend",
               sign(mi) == 0 || sign(mi) == sign(ki))
        assert("valid range", abs(mi) < abs(li))

int main(void)
    test_risc()
    return 0
