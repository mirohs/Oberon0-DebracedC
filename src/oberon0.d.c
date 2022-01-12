/*
@author: Michael Rohs
@date: December 27, 2021
Debraced-C translation of: Oberon0 Compiler, Niklaus Wirth, Compiler Construction, 2005
https://people.inf.ethz.ch/wirth/CompilerConstruction/
*/

#include "util.h"
#include "risc.h"
#include "scanner.h"
#include "parser.h"
#include "generator.h"
#include "oberon0.h"

int main(int argc, char* argv[])
    if argc != 2 do
        printf("Usage: %s <filename source file>\n", argv[0])
        exit(EXIT_FAILURE)
    char* filename = argv[1]

    String s = read_file(filename)
    P_compile(s)
    if !S_error do
        R_print_code(G_code, G_pc)
        R_load(G_code, G_pc)
        printf("RISC OUTPUT BEGIN\n")
        R_execute(G_entry * R_WORD_SIZE)
        printf("RISC OUTPUT END\n")
        // print memory:
        // R_print_memory(R_PROG_ORG / R_WORD_SIZE - 4 * R_WORD_SIZE, R_PROG_ORG / R_WORD_SIZE, 8)
    return 0
