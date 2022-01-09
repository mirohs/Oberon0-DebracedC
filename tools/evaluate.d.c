/*
@author: Michael Rohs
@date: December 27, 2021
Debraced-C translation of: Oberon0 Compiler, Niklaus Wirth, Compiler Construction, 2005
https://people.inf.ethz.ch/wirth/CompilerConstruction/
*/

#include <ctype.h>
#include <limits.h>
#include "util.h"
#include "scanner.h"
#include "evaluate.h"

/*
expression = term {("+" | "-") term}.
term = factor {("*" | "/") factor}.
factor = id | "(" expression ")".

PROCEDURE expression(VAR val0: INTEGER);
    VAR val1, val2: INTEGER; op: CHAR;
BEGIN term(val1);
    WHILE (sym = "+") OR (sym = "-") DO
        op : = sym; GetSym; term(val2);
        IF op = "+" THEN val1 : = val1 + val2 ELSE val1 := val1 - val2 END
    END;
    val0 := val1
END expression
*/

S_Symbol sym = s_null

void put(S_Symbol s)
    switch s do
        case s_plus: printf(" +"); break
        case s_minus: printf(" -"); break
        case s_times: printf(" *"); break
        case s_div: printf(" DIV"); break
        case s_number: printf(" %d", S_value); break
        default: printf(" ?"); break
    // printf("\n")

/*
expression = term {("+" | "-") term}.
term = factor {("*" | "/") factor}.
factor = id | "(" expression ")".
*/
void expression(int* val0)
void term(int* val0)
void factor(int* val0)

void expression(int* val0)
    int val1, val2
    term(&val1)
    while sym == s_plus || sym == s_minus do
        S_Symbol op = sym
        S_get(&sym)
        term(&val2)
        if op == s_plus do
            val1 += val2
        else
            val1 -= val2
    *val0 = val1

void term(int* val0)
    int val1, val2
    factor(&val1)
    while sym == s_times || sym == s_div do
        S_Symbol op = sym
        S_get(&sym)
        factor(&val2)
        if op == s_times do
            val1 *= val2
        else
            val1 /= val2
    *val0 = val1

void factor(int* val0)
    // printf("factor: sym = %d, val = %d, id = %s\n", sym, S_value, S_identifier)
    if sym == s_number do
        *val0 = S_value
        S_get(&sym)
    else if sym == s_lparen do
        S_get(&sym)
        expression(val0)
        if sym == s_rparen do
            S_get(&sym)
        else
            S_mark(")?")
    else
        S_mark("number or (...)?")


void test_source_text(char* source_text)
    // printf("\n")
    String s = make_string(source_text)
    S_init(s, 0)
    S_get(&sym)
    // printf("sym = %d, val = %d, id = %s\n", sym, S_value, S_identifier)
    int val = 0;
    expression(&val)
    printf("val = %d\n", val)

void parser_test(void)
    test_source_text("123")
    test_source_text("123+2")
    test_source_text("64 DIV 2")
    test_source_text("1 + 64 DIV 2")
    test_source_text("2 * 3 + 4")
    test_source_text("2 + 3 * 4")
    test_source_text("2 * (3 + 4)")
    test_source_text("(2 * 3) + 4")
    test_source_text("(2 + 3) * 4")
    test_source_text("2 + (3 * 4)")
    test_source_text("2 + (3 * (7 - 2))")

int main(int argc, char* argv[])
    parser_test()
    return 0
