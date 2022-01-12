# Oberon0-DebracedC

## Oberon-0 Compiler in Debraced C

This project is a translation of Niklaus Wirth's Oberon-0 compiler as presented in [1] into debraced C. Oberon-0 is a simplified variant of Oberon with just the basic types INTEGER and BOOLEAN and without function procedures. It only compiles a single module. However, Oberon-0 has procedures, value and reference parameters, arrays, and records.

The source code is compiled for a hypothetical RISC processor as presented in [1]. The RISC processor is reduced to a bare minimum and mainly offers integer arithmetic, comparison, and branch instructions. Some details are given below.

Debraced C is C with optional braces, significant indentation, and automatic generation of header files. The programmer writes code with optional braces in a `.d.c` (debraced C) file. Public items are marked with a `*` at the beginning of the line. Public items are exported from the translation unit and included in the module's header file. The compilation process is automated in the Makefile.

The tool [embrace](https://github.com/mirohs/embrace) reintroduces braces into the debraced C file and the tool [headify](https://github.com/mirohs/headify) automatically generates header files. Both tools have to be available to compile this project.

[1] [Oberon0 Compiler, Niklaus Wirth, Compiler Construction, 2005](https://people.inf.ethz.ch/wirth/CompilerConstruction/)

## Building

```sh
git clone https://github.com/mirohs/Oberon0-DebracedC.git
git clone https://github.com/mirohs/embrace.git
git clone https://github.com/mirohs/headify.git
cd embrace
make
cd ../headify
make
cd ../Oberon0-DebracedC/src
make
```

## Compiling and Running an Example

```sh
cd ../examples
../src/oberon0 QuickSort.Mod
```

## Structure

The port of the Oberon-0 compiler is structured as follows: `scanner.d.c` contains the scanner. It performs lexical analysis, i.e., it transforms Oberon-0 source text into a stream of symbols (tokens). Syntax analysis is done by `parser.d.c`. The structure of the recursive descent parser directly mirrors the Oberon-0 EBNF grammar, given below. The parser calls functions in `generator.d.c` to emit code for the RISC processor. The parser also checks types, ranges of constants, etc. The compiler is a non-optimizing single-pass compiler. A simulator of the RISC processor is located in `risc.d.c`. 

The main function is located in `oberon0.d.c`. If source file could successfully be compiled, `oberon0` prints the generated instructions and directly executes the code using the simulator. The structure and the source code of the compiler are a direct port of the Oberon-0 compiler given in [1].

## Oberon-0 EBNF Grammar

```ebnf
ident = letter {letter | digit}.
integer = digit {digit}.

selector = {"." ident | "[" expression "]"}.

factor = ident selector | integer | "(" expression ")" | "~" factor.
term = factor {("*" | "DIV" | "MOD" | "&") factor}.
SimpleExpression = ["+"|"-"] term {("+"|"-" | "OR") term}.
expression = SimpleExpression [("=" | "#" | "<" | "<=" | ">" | ">=") SimpleExpression].

assignment = ident selector ":=" expression.

ActualParameters = "(" [expression {"," expression}] ")" .
ProcedureCall = ident [ActualParameters].

IfStatement = "IF" expression "THEN" StatementSequence
              {"ELSIF" expression "THEN" StatementSequence}
              ["ELSE" StatementSequence] "END".

WhileStatement = "WHILE" expression "DO" StatementSequence "END".

statement = [assignment | ProcedureCall | IfStatement | WhileStatement].
StatementSequence = statement {";" statement}.

IdentList = ident {"," ident}.
ArrayType = "ARRAY" expression "OF" type.
FieldList = [IdentList ":" type].
RecordType = "RECORD" FieldList {";" FieldList} "END".
type = ident | ArrayType | RecordType.

FPSection = ["VAR"] IdentList ":" type.
FormalParameters = "(" [FPSection {";" FPSection}] ")".

ProcedureHeading = "PROCEDURE" ident [FormalParameters].
ProcedureBody = declarations ["BEGIN" StatementSequence] "END".
ProcedureDeclaration = ProcedureHeading ";" ProcedureBody ident.
declarations = ["CONST" {ident "=" expression ";"}]
               ["TYPE" {ident "=" type ";"}]
               ["VAR" {IdentList ":" type ";"}]
               {ProcedureDeclaration ";"}.

module = "MODULE" ident ";" declarations ["BEGIN" StatementSequence] "END" ident "." .
```

## RISC Processor

The hypothetical RISC target is a 32-bit processor, i.e., the word size is 4 bytes. All instructions are one word long. There are 16 general-purpose registers (R0 to R15). R15 is used as the program counter (PC) and R14 is used as the link register, containing the return address for the branch-to-subroutine (BSR) instruction. The comparison instructions (CMP, CMPI) implicitly compute the difference of their operands and set the N (negative) and Z (zero) flags according to the result. There are four instruction formats:

* F0: 00 op[4] a[4] b[4] unused[14] c[4] 
* F1: 01 op[4] a[4] b[4] im[18] 
* F2: 10 op[4] a[4] b[4] disp[18] 
* F3: 11 op[4] disp[26] 

where a, b, and c refer to registers (R0-R15) and im and disp are 18-bit
immediate/displacement values. They are given in two's complement format. For most instructions, a is the destination register and b and c are the operand registers. The opcodes occupy the upper 6 bits of the instruction word. The opcodes are:

* F0: 00 op[4] a[4] b[4] unused[14] c[4] 
  * MOV = 0, MVN = 1 (move, move negated)
  * ADD = 2, SUB = 3, MUL = 4, Div = 5, Mod = 6
  * CMP = 7 (compare registers)
* F1: 01 op[4] a[4] b[4] im[18] 
  * MOVI = 16, MVNI = 17 (move immediate, move immediate negated)
  * ADDI = 18, SUBI = 19, MULI = 20, DIVI = 21, MODI = 22
  * CMPI = 23 (compare register with immediate operand)
  * CHKI = 24 (check index)
* F2: 10 op[4] a[4] b[4] disp[18] 
  * LDW = 32, LDB = 33, POP = 34, STW = 36, STB = 37, PSH = 38 (load from, store to memory)
  * RD = 40, WRD = 41, WRH = 42, WRL = 43, RB = 44, WRB = 45 (input/output)
* F3: 11 op[4] disp[26] 
  * BEQ = 48, BNE = 49, BLT = 50, BGE = 51, BLE = 52, BGT = 53 (conditional branch)
  * BR = 56 (unconditional branch)
  * BSR = 57, RET = 58 (branch to, return from subroutine)

The input/output instructions are included to allow the simulated RISC to read from standard input and write to standard output. RB/WRB reads and writes a single byte, RD/WRD reads and writes an integer number in decimal form, WRH writes an integer number in hexadecimal form, and WRL writes a line break. 


## Example Compilation

This example counts the number of bytes in the standard input stream.

```Oberon
MODULE CountBytes;
    VAR c, n: INTEGER;
BEGIN
    n := 0;
    ReadByte(c);
    WHILE c # -1 DO
        INC(n);
        ReadByte(c)
    END;
    Write(n); WriteLn
END CountBytes.
```

Compile and run the program to compute the size of its source text.

```sh
../src/oberon0 CountBytes.Mod < CountBytes.Mod
```

The output (with comments) is:

```
entry = 0
20 instructions generated
  0 MOVI 13  0 4096    set stack pointer to top of memory
  1 PSH  14 13  4      push link register (initially contains 0)
  2 MOVI  0  0  0      n := 0;
  3 STW   0 15 -20
  4 RB    0  0  0      ReadByte(c);
  5 STW   0 15 -24
  6 LDW   0 15 -28     c
  7 CMPI  0  0 -1      c # -1
  8 BEQ   8            WHILE c # -1 DO
  9 LDW   0 15 -44     n
 10 ADDI  0  0  1      INC(n);
 11 STW   0 15 -52
 12 RB    0  0  0      ReadByte(c)
 13 STW   0 15 -56
 14 BR   -8            END;
 15 LDW   0 15 -68     n
 16 WRD   0  0  0      Write(n);
 17 WRL   0  0  0      WriteLn
 18 POP  14 13  4      pop link register (return address) from stack
 19 RET  14            return (using return address in link register)
RISC OUTPUT BEGIN
 181                   size of CountBytes.Mod is 181 bytes
RISC OUTPUT END
```

## Makefile

The translation of a `.d.c` file is done in multiple steps and is automated by the Makefile.

| Target | Depends on | Tool to transform dependency | Remarks |
| --- | --- | --- | --- | 
| executable | o+ | linker (gcc) | + means one or more |
| o | c and h* | compiler (gcc -c) | * means zero or more |
| c and h | hy | headify | create header and implementation files |
| hy | d.c |  embrace | create C code file with braces {...} |

