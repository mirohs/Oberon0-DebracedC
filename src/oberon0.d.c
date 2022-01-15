/*
@author: Michael Rohs
@date: December 27, 2021
Debraced-C translation of: Oberon0 Compiler, Niklaus Wirth, Compiler Construction, 2005
https://people.inf.ethz.ch/wirth/CompilerConstruction/
*/

#include "util.h"
#include "risc.h"
#include "parser.h"
#include "oberon0.h"

int main(int argc, char* argv[])
    if argc != 2 do
        printf("Usage: %s <filename source file>\n", argv[0])
        exit(EXIT_FAILURE)
    char* filename = argv[1]

    String source_text = read_file(filename)
    INTEGER* code
    int code_length
    int entry
    if P_compile(source_text, &code, &code_length, &entry) do
        R_print_code(code, code_length)
        R_load(code, code_length)
        printf("RISC OUTPUT BEGIN\n")
        R_execute(entry * R_WORD_SIZE)
        printf("RISC OUTPUT END\n")
        // print memory:
        // R_print_memory(R_PROG_ORG / R_WORD_SIZE - 4 * R_WORD_SIZE, R_PROG_ORG / R_WORD_SIZE, 8)
    return 0
