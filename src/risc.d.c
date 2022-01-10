/*
@author: Michael Rohs
@date: December 28, 2021
Debraced-C translation of: 
Niklaus Wirth, Compiler Construction, 2005
Chapter 9. A RISC-Architecture as Target (p. 46ff)
https://people.inf.ethz.ch/wirth/CompilerConstruction/
*/

#include "util.h"
#include "risc.h"

*#define R_MEM_SIZE 4096 // 1024 // memory size in bytes
*#define R_PROG_ORG 2048 //  512 // byte offset of program code in memory

// The RISC target is a 32-bit processor.
*#define R_WORD_SIZE 4

/*
All instructions of the RISC processor are 32 bits long. There are four formats:
- F0: 00 op[4] a[4] b[4] unused[14] c[4] 
- F1: 01 op[4] a[4] b[4] im[18] 
- F2: 10 op[4] a[4] b[4] disp[18] 
- F3: 11 op[4] disp[26] 
where a, b, and c refer to registers (R0-R15) and im and disp are 18-bit
immediate values. The opcodes occupy the upper 6 bits of the instruction, so
there can be at most 64 instructions.
*/
*typedef enum {
    // F0: 00 op[4] a[4] b[4] unused[14] c[4] 
    MOV = 0, MVN = 1, ADD = 2, SUB = 3, MUL = 4, Div = 5, Mod = 6, CMP = 7,
    // F1: 01 op[4] a[4] b[4] im[18] 
    MOVI = 16, MVNI = 17, ADDI = 18, SUBI = 19, MULI = 20, DIVI = 21, 
    MODI = 22, CMPI = 23, CHKI = 24,
    // F2: 10 op[4] a[4] b[4] disp[18] 
    LDW = 32, LDB = 33, POP = 34, STW = 36, STB = 37, PSH = 38,
    RD = 40, WRD = 41, WRH = 42, WRL = 43, RB = 44, WRB = 45, 
    // F3: 11 op[4] disp[26] 
    BEQ = 48, BNE = 49, BLT = 50, BGE = 51, BLE = 52, BGT = 53, BR = 56, 
    BSR = 57, RET = 58
} R_Opcode

#define NAME(opc) {opc, #opc}

struct { R_Opcode opc; char* name; } opcode_names[] = {
    NAME(MOV), NAME(MVN), NAME(ADD), NAME(SUB), NAME(MUL), NAME(Div), NAME(Mod), NAME(CMP),
    NAME(MOVI), NAME(MVNI), NAME(ADDI), NAME(SUBI), NAME(MULI), NAME(DIVI), 
    NAME(MODI), NAME(CMPI), NAME(CHKI),
    NAME(LDW), NAME(LDB), NAME(POP), NAME(STW), NAME(STB), NAME(PSH),
    NAME(RD), NAME(WRD), NAME(WRH), NAME(WRL), NAME(RB), NAME(WRB),
    NAME(BEQ), NAME(BNE), NAME(BLT), NAME(BGE), NAME(BLE), NAME(BGT), NAME(BR), 
    NAME(BSR), NAME(RET)
}
#define R_OPCODE_COUNT (sizeof(opcode_names) / sizeof(opcode_names[0]))

*typedef int INTEGER
*typedef bool BOOLEAN

INTEGER IR // instruction register
BOOLEAN N, Z // negative and zero flags
INTEGER R[16] // registers, R[15] is PC, R[14] used as link register by BSR instruction
INTEGER M[R_MEM_SIZE / R_WORD_SIZE] // program and data memory, organized as 32-bit words

// arithmetic shift (left)
#define ASH(x, s) ((x) << (s))

// Modulo operation, 0 <= mod < abs(y).
*INTEGER R_mod(INTEGER x, INTEGER y)
    require("valid divisor", y != 0)
    x = x % y
    if y < 0 do y = -y
    while x < 0 do
        x += y
    ensure("valid remainder", 0 <= x && x < y)
    return x

// Gets 32-bit word at word address i.
INTEGER get_M(INTEGER i)
    exit_if(i < 0 || i >= R_MEM_SIZE / R_WORD_SIZE, "invalid word address (%d)", i)
    // printf("get_M(%d) = %d\n", i, M[i])
    return M[i]

// Gets byte at byte address i.
INTEGER get_B(INTEGER i)
    exit_if(i < 0 || i >= R_MEM_SIZE, "invalid byte address (%d)", i)
    char* B = (char*)M; // byte access to memory
    return B[i] & 0xff

// Sets 32-bit word at word address i to x.
void set_M(INTEGER i, INTEGER x)
    exit_if(i < 0 || i >= R_MEM_SIZE / R_WORD_SIZE, "invalid word address (%d)", i)
    // printf("set_M(%d, %d)\n", i, x)
    M[i] = x

// Sets byte at byte address i to x.
void set_B(INTEGER i, INTEGER x)
    exit_if(i < 0 || i >= R_MEM_SIZE, "invalid byte address (%d)", i)
    char* B = (char*)M; // byte access to memory
    B[i] = x

// Decodes the given instruction.
*void R_decode_instruction(INTEGER x, INTEGER* opc, INTEGER* a, INTEGER* b, INTEGER* c)
    *opc = (x >> 26) & 0x3f // 6 bits
    *a = (x >> 22) & 0xf // 4 bits
    *b = (x >> 18) & 0xf // 4 bits
    if *opc < MOVI do 
        *c = x & 0xf // instruction format F0
    else if *opc < BEQ do
        *c = x & 0x3ffff // instruction format F1 or F2, 18 bits im or disp
        if *c >= 0x20000 do *c -= 0x40000 // sign extension
    else 
        *c = x & 0x3ffffff // instruction format F3, 26 bits disp
        if *c >= 0x2000000 do *c -= 0x4000000 // sign extension

// Prints the instruction encoded in x.
*void R_print_instruction(INTEGER x)
    INTEGER opc, a, b, c
    R_decode_instruction(x, &opc, &a, &b, &c)
    char* opc_name = "?"
    for int i = 0; i < R_OPCODE_COUNT; i++ do
        if opcode_names[i].opc == opc do 
            opc_name = opcode_names[i].name
            break
    if opc < MOVI do 
        printf("%-4s %2d %2d %2d\n", opc_name, a, b, c)
    else if opc < BEQ do
        printf("%-4s %2d %2d %2d\n", opc_name, a, b, c)
    else 
        printf("%-4s %2d\n", opc_name, c)

/*
Encodes an instruction according to the given format. Negative values for c have
to be limited to 4 bits (F0), 18 bits (F1 and F2), or 26 (F3) bits.
*/
*INTEGER R_encode_instruction(int opc, int a, int b, int c)
    exit_if(opc < 0 || opc > RET, "opc out of range (%d)", opc)
    exit_if(a < 0 || a > 0xf, "a out of range (%d)", a)
    exit_if(b < 0 || b > 0xf, "b out of range (%d)", b)
    opc &= 0x3f
    a &= 0xf
    b &= 0xf
    if opc < MOVI do 
        // instruction format F0
        exit_if(c < 0 || c > 0xf, "c out of range (%d), F0", c)
        c &= 0xf
    else if opc < BEQ do
        // instruction format F1 or F2, 18 bits im or disp
        exit_if(c < -0x20000 || c > 0x1ffff, "c out of range (%d), F1 or F2", c)
        c &= 0x3ffff 
    else
        // instruction format F3, 26 bits disp
        exit_if(a != 0, "a != 0 (%d), F3", a)
        exit_if(b != 0, "b != 0 (%d), F3", b)
        exit_if(c < -0x2000000 || c >= 0x2000000, "c out of range (%d), F3", c)
        a = 0
        b = 0
        c &= 0x3ffffff 
    INTEGER i = (opc << 26) | (a << 22) | (b << 18) | c
    // R_print_instruction(i)
    return i

/*
Executes code starting at the given start address. The start address is given in
bytes relative to the code origin. The code must have been loaded into memory
before.
*/
*void R_execute(INTEGER start)
    require("valid start", start >= 0 
                           && R_PROG_ORG + start + R_WORD_SIZE <= R_MEM_SIZE 
                           && (start & 0x3) == 0)
    INTEGER opc, a, b, c, nextPC
    R[14] = 0 // link register
    R[15] = start + R_PROG_ORG // program counter
    // printf("initial PC = %d\n", R[15])
    while true do // interpretation cycle
        nextPC = R[15] + R_WORD_SIZE // address of next instruction word
        exit_if(R[15] < 0 || R[15] > R_MEM_SIZE - R_WORD_SIZE || (R[15] & 0x3) != 0, 
                "invalid PC (%d)", R[15])
        IR = M[R[15] / R_WORD_SIZE]; // memory is organized in 32-bit words
        R_decode_instruction(IR, &opc, &a, &b, &c)
        if opc < MOVI do c = R[c & 0xf]
        switch opc do
            case MOV: case MOVI: R[a] = ASH(c, b); break // arithmetic shift
            case MVN: case MVNI: R[a] = -ASH(c, b); break
            case ADD: case ADDI: R[a] = R[b] + c; break
            case SUB: case SUBI: R[a] = R[b] - c; break
            case MUL: case MULI: R[a] = R[b] * c; break
            case Div: case DIVI: R[a] = R[b] / c; break
            case Mod: case MODI: R[a] = R_mod(R[b], c); break
            case CMP: case CMPI: Z = R[b] == c; N = R[b] < c; break
            case CHKI: exit_if(R[a] < 0 || R[a] >= c, \
                            "CHKI failed (%d, %d)", c, R[a]); break
            case LDW: R[a] = get_M((R[b] + c) / R_WORD_SIZE); break
            case LDB: R[a] = get_B(R[b] + c); break
            case POP: R[a] = get_M((R[b]) / R_WORD_SIZE); R[b] += c; break
            case STW: set_M((R[b] + c) / R_WORD_SIZE, R[a]); break
            case STB: set_B(R[b] + c, R[a]); break
            case PSH: R[b] -= c; set_M(R[b] / R_WORD_SIZE, R[a]); break
            case RD: scanf("%d", &R[a]); break
            case WRD: printf(" %d", R[c]); break
            case WRH: printf(" %xH", R[c]); break
            case WRL: printf("\n"); break
            case RB: R[a] = getchar(); break
            case WRB: putchar(R[c]); break
            case BEQ: if Z do nextPC = R[15] + c * R_WORD_SIZE; break
            case BNE: if !Z do nextPC = R[15] + c * R_WORD_SIZE; break
            case BLT: if N do nextPC = R[15] + c * R_WORD_SIZE; break
            case BGE: if !N do nextPC = R[15] + c * R_WORD_SIZE; break
            case BLE: if Z || N do nextPC = R[15] + c * R_WORD_SIZE; break
            case BGT: if !Z && !N do nextPC = R[15] + c * R_WORD_SIZE; break
            case BR:  nextPC = R[15] + c * R_WORD_SIZE; break
            case BSR: nextPC = R[15] + c * R_WORD_SIZE; R[14] = R[15] + R_WORD_SIZE; break
            case RET: nextPC = R[c & 0xf]; break
            default: exit_if(true, "invalid opcode (%d)", opc)
        end. switch
        if nextPC == 0 do break // exit interpretation cycle
        R[15] = nextPC
    end. while
end. execute

/*
Loads the code words into memory. The length refers to the number of code words
(not bytes).
*/
*void R_load(INTEGER* code, INTEGER len)
    require_not_null(code)
    require("valid length", len >= 0 && R_PROG_ORG + R_WORD_SIZE * len <= R_MEM_SIZE)
    for INTEGER i = 0; i < len; i++ do
        M[i + R_PROG_ORG / R_WORD_SIZE] = code[i]
end. load

// Prints the given instructions.
*void R_print_code(INTEGER* code, INTEGER len)
    require_not_null(code)
    require("not negative", len >= 0)
    for int i = 0; i < len; i++ do
        printf("%3d ", i)
        R_print_instruction(code[i])

/*
Prints memory from start index to end index (end is exclusive) in rows of the
given length.
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

// Enters instruction into code array.
#define I(opc, a, b, c) code[len++] = R_encode_instruction(opc, a, b, c)

// Performs some tests.
void test_risc(void)
    require("valid word length", sizeof(INTEGER) == R_WORD_SIZE)

    int i
    INTEGER opc, a, b, c
    INTEGER opc2, a2, b2, c2

    i = R_encode_instruction(opc = MOVI, a = 1, b = 2, c = 0x1ffff)
    R_print_instruction(i)
    R_decode_instruction(i, &opc2, &a2, &b2, &c2)
    test_equal_i(opc, opc2)
    test_equal_i(a, a2)
    test_equal_i(b, b2)
    test_equal_i(c, c2)

    i = R_encode_instruction(opc = MOVI, a = 3, b = 4, c = -0x20000)
    R_print_instruction(i)
    R_decode_instruction(i, &opc2, &a2, &b2, &c2)
    test_equal_i(opc, opc2)
    test_equal_i(a, a2)
    test_equal_i(b, b2)
    test_equal_i(c, c2)

    i = R_encode_instruction(opc = ADDI, a = 5, b = 6, c = -1)
    R_print_instruction(i)
    R_decode_instruction(i, &opc2, &a2, &b2, &c2)
    test_equal_i(opc, opc2)
    test_equal_i(a, a2)
    test_equal_i(b, b2)
    test_equal_i(c, c2)

    i = R_encode_instruction(opc = SUBI, a = 5, b = 6, c = 123)
    R_print_instruction(i)
    R_decode_instruction(i, &opc2, &a2, &b2, &c2)
    test_equal_i(opc, opc2)
    test_equal_i(a, a2)
    test_equal_i(b, b2)
    test_equal_i(c, c2)

    //R_encode_instruction(MOVI, 0, 0, 0x20000) // out of range
    //R_encode_instruction(MOVI, 0, 0, -0x20000-1) // out of range

    INTEGER code[100]
    int len = 0

    // 123 + 456
    I(MOVI, 0, 0, 123)
    I(WRD, 0, 0, 0)
    I(MOVI, 1, 0, 456)
    I(WRD, 0, 0, 1)
    I(ADD, 2, 0, 1)
    I(WRD, 0, 0, 2)
    I(WRL, 0, 0, 0)

    // 1 2 ... 10
    I(MOVI, 0, 0, 0)
    I(WRD, 0, 0, 0)
    I(ADDI, 0, 0, 1)
    I(CMPI, 0, 0, 10)
    I(BLE, 0, 0, -3)
    I(WRL, 0, 0, 0)

    // 1024 512 256 ... 1
    I(MOVI, 0, 0, 1024)
    I(WRD, 0, 0, 0)
    I(DIVI, 0, 0, 2)
    I(CMPI, 0, 0, 0)
    I(BGT, 0, 0, -3)
    I(WRL, 0, 0, 0)

    // x := input, x * 10, x / 10, x mod 10
    //I(RD, 0, 0, 0)
    I(MULI, 1, 0, 10)
    I(WRD, 0, 0, 1)
    I(DIVI, 1, 0, 10)
    I(WRD, 0, 0, 1)
    I(MODI, 1, 0, 10)
    I(WRD, 0, 0, 1)
    I(WRL, 0, 0, 0)

    // sign extension
    I(MOVI, 0, 0, 0x1ffff) // largest number (positive)
    I(WRD, 0, 0, 0)
    I(WRL, 0, 0, 0)
    I(MOVI, 0, 0, -0x20000) // smallest number (negative)
    I(WRD, 0, 0, 0)
    I(WRL, 0, 0, 0)
    I(MOVI, 0, 0, -1)
    I(WRD, 0, 0, 0)
    I(WRL, 0, 0, 0)
    I(MOVI, 0, 0, -2)
    I(WRD, 0, 0, 0)
    I(WRL, 0, 0, 0)

    // store and load (F2)
    I(MOVI, 0, 0, 0x12)
    I(MOV, 0, 8, 0)
    I(ADDI, 0, 0, 0x34)
    I(MOV, 0, 8, 0)
    I(ADDI, 0, 0, 0x56)
    I(MOV, 0, 8, 0)
    I(ADDI, 0, 0, 0x78)
    I(WRH, 0, 0, 0)
    I(MOVI, 1, 0, 0)
    I(STW, 0, 1, 0)
    I(LDW, 2, 1, 0)
    I(WRH, 0, 0, 2)
    I(LDB, 2, 1, 0)
    I(WRH, 0, 0, 2)
    I(WRL, 0, 0, 0)

    // read and write bytes from/to standard input/output
    /*
    I(RB, 0, 0, 0)
    I(ADDI, 0, 0, 1)
    I(WRB, 0, 0, 0)
    I(RB, 0, 0, 0)
    I(ADDI, 0, 0, 1)
    I(WRB, 0, 0, 0)
    I(RB, 0, 0, 0)
    I(ADDI, 0, 0, 1)
    I(WRB, 0, 0, 0)
    I(WRL, 0, 0, 0)
    */

    I(MOVI, 0, 0, 999)
    // I(MOVI, 0, 0, -1) // index out of bounds error
    // I(MOVI, 0, 0, 1000) // index out of bounds error
    I(CHKI, 0, 0, 1000)

    // case MOV: case MOVI: R[a] = ASH(c, b); break // arithmetic shift
    // MOV a, b, c       R.a := Shift(R.c, b)
    // MOVI a, b, im     R.a := Shift(im, b)
    // #define ASH(x, s) ((x) << (s))
    I(MOVI, 5, 1, 100)
    I(MOV, 1, 0, 5)
    I(WRD, 0, 0, 1)
    I(WRL, 0, 0, 0)
    I(MOVI, 1, 0, 5)
    I(WRD, 0, 0, 1)
    I(WRL, 0, 0, 0)

    I(RET, 0, 0, 14) // end execution

    R_print_code(code, len)
    R_load(code, len)
    R_execute(0)

int xmain(void)
    test_risc()
    return 0
