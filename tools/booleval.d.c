/*
@author: Michael Rohs
@date: January 1, 2022

Parses expessions into sequences of conditional branch instructions. Uses true
links and false links to avoid the unneccessary evaluation of subexpressions.

factor = id | "(" expression ")".
term = factor {"&" factor}.
expression = term {"OR" term}.

false links: i is for bf (branch if false)
true links: j is for bt (branch if true)

Example:

     T                   T
     ^-------v           ^
     |       |           |
(a  v  b)  ^  (c  ^  d  v  e  ^  f)
            |      |      |    |
            v      v------^    v
            F      F           F
*/

#include "util.h"
#include "scanner.h"
#include "booleval.h"

S_Symbol sym = s_null

typedef struct Op Op
struct Op
    char name[8]
    int address

Op code[100]
int pc = 0

void add(char* name, int address)
    Op* op = code + pc
    strcpy(op->name, name)
    op->address = address
    pc++

void print_code(void)
    for int i = 0; i < pc; i++ do
        Op* op = code + i
        if op->address < 0 do
            printf("[%d]%s ", i, op->name)
        else
            printf("[%d]%s%d ", i, op->name, op->address)
    printf("\n")

void print_code_lines(void)
    for int i = 0; i < pc; i++ do
        Op* op = code + i
        if op->address < 0 do
            printf("[%2d] %s\n", i, op->name)
        else
            printf("[%2d] %s %d\n", i, op->name, op->address)

// Emits branch-if-false instruction and set i to the index of the emitted instruction.
void bf(int* i)
    add("bf", *i)
    *i = pc - 1

// Emits branch-if-true instruction and set i to the index of the emitted instruction.
void bt(int* i)
    add("bt", *i)
    *i = pc - 1

// Sets the addresses of all linked branch instructions to pc.
void fix(int* i)
    int k = *i
    while k != 0 do
        int k_next  = code[k].address
        code[k].address = pc
        k = k_next 
    *i = 0

// Concatenates two lists of linked branch instructions.
// a = a1 ... an
// b = b1 ... bm
// result = a1 ... an b1 ... bm
// return result in a
void merge(int* a, int* b)
    int i = *a
    int j = *b
    while i != 0 do
        int i_next = code[i].address
        if i_next == 0 do
            code[i].address = j
            return
        i = i_next
    // a is empty, return b
    *a = *b

void factor(int* i, int* j)
void term(int* i, int* j)
void expression(int* i, int* j)

/*
factor = id | "(" expression ")".
term = factor {"&" factor}.
expression = term {"OR" term}.

i is for bf, j is for bt
*/
void factor(int* i, int* j)
    // printf("factor(%d, %d)\n", *i, *j)
    if sym == s_ident do
        // printf("ident(%s)\n", S_identifier)
        add(S_identifier, -1)
        S_get(&sym)
    else if sym == s_lparen do
        S_get(&sym)
        expression(i, j)
        if sym == s_rparen do
            S_get(&sym)
        else
            S_mark(")?")
    else
        S_mark("id or (...)?")

/*
factor = id | "(" expression ")".
term = factor {"&" factor}.
expression = term {"OR" term}.

Parses a term. Non resolved links (i for false, j for true) are merged with the
incoming links.
*/
void term(int* i, int* j)
    // printf("term(%d, %d)\n", *i, *j)
    int ti = 0, tj = 0
    factor(&ti, &tj)
    while sym == s_and do
        S_Symbol op = sym
        // printf("op=& ")
        bf(&ti) // emit branch-if-false, add to false-links
        fix(&tj) // let preceding true links point here
        S_get(&sym)
        factor(&ti, &tj)
    // merge non-resolved links with incoming links
    merge(i, &ti)
    merge(j, &tj)

/*
factor = id | "(" expression ")".
term = factor {"&" factor}.
expression = term {"OR" term}.

Parses an expression. Non resolved links (i for false, j for true) are merged
with the incoming links.
*/
void expression(int* i, int* j)
    // printf("exp(%d, %d)\n", *i, *j)
    int ei = 0, ej = 0
    term(&ei, &ej)
    while sym == s_or do
        S_Symbol op = sym
        // printf("op=OR ")
        bt(&ej) // emit branch-if-true, add to true-links
        fix(&ei) // let preceding false links point here
        S_get(&sym)
        term(&ei, &ej)
    // merge non-resolved links with incoming links
    merge(i, &ei)
    merge(j, &ej)

void test_source_text(char* source_text)
    pc = 0
    String s = make_string(source_text)
    S_init(s, 0)
    S_get(&sym)
    printf("%s\n", source_text)
    int i = 0, j = 0
    expression(&i, &j)
    print_code()
    printf("i = %d, j = %d\n", i, j)

void parser_test(void)
    test_source_text("a & b")
    test_source_text("a & b & c")
    test_source_text("a & b OR c & d")
    test_source_text("a & b & c OR d & e & f OR g & h & i")
    test_source_text("(a)")
    test_source_text("(a & b)")
    test_source_text("((a OR b))")
    test_source_text("a & (b OR c)")
    test_source_text("(a OR b) & (c OR d)")
    test_source_text("((a&b OR c&d) & (e OR f) OR g) & (h&i OR j)")
    test_source_text("(a & b & c OR d & e & f) & (g OR h)")
    test_source_text("a & (b OR c) & d")

int main(int argc, char* argv[])
    parser_test()
    return 0
