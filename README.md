# Oberon0-DebracedC

## Debraced-C port of Oberon-0

This project is a translation of Niklaus Wirth's Oberon-0 compiler as presented in [1] into Debraced-C. Oberon-0 is a simplified variant of Oberon with just the primitive types INTEGER and BOOLEAN and without function procedures. It only compiles a single module. However, Oberon-0 has procedures, value and reference parameters, arrays, and records.

The source code is compiled for a hypothetical RISC processor as presented in [1]. The RISC processor is reduced to a bare minimum and mainly offers integer arithmetic, comparison, and brach instructions. Some details are given below.

Debraced C is C with optional braces and with automatic generation of header files. The programmer writes code with optional braces in a .d.c (debraced C) file. Public items are marked with a `*` at the beginning of the line. Public items are expored from the translation unit and included in the module's header file. The compilation process is automated in the Makefile.

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
../src/parser QuickSort.Mod
```

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

The hypotetical RISC target is a 32-bit processor, i.e, the word size is 4 bytes. All instructions are one word long. There are 16 general-purpose registers (R0 to R15). R15 is used as the program counter (PC) and R14 is used as the link register, containing the return address for the branch-to-subroutine (BSR) instruction. The comparison instructions (CMP, CMPI) implicitly compute the difference of their operands and set the N (negative) and Z (zero) flags according to the result. There are four instruction formats:

* F0: 00 op[4] a[4] b[4] unused[14] c[4] 
* F1: 01 op[4] a[4] b[4] im[18] 
* F2: 10 op[4] a[4] b[4] disp[18] 
* F3: 11 op[4] disp[26] 

where a, b, and c refer to registers (R0-R15) and im and disp are 18-bit
immediate/displacement values. They are given in two's complement format. The opcodes occupy the upper 6 bits of the instruction word. The opcodes are:

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
