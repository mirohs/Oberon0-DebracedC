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

*#define S_IDENTIFIER_LENGTH 32

*typedef enum {
    s_null = 0,
    s_times = 1, s_div = 3, s_mod = 4, s_and = 5,
    s_plus = 6, s_minus = 7, s_or = 8,
    s_eql = 9, s_neq = 10, s_lss = 11, s_geq = 12, s_leq = 13, s_gtr = 14,
    s_period = 18, s_comma = 19, s_colon = 20,
    s_rparen = 22, s_rbrak = 23, s_of = 25, s_then = 26, s_do = 27,
    s_lparen = 29, s_lbrak = 30, s_not = 32, s_becomes = 33,
    s_number = 34, s_ident = 37, s_semicolon = 38,
    s_end = 40, s_else = 41, s_elsif = 42, s_if = 44, s_while = 46,
    s_array = 54, s_record = 55, s_const = 57, s_type = 58, s_var = 59,
    s_procedure = 60, s_begin = 61, s_module = 63,
    s_eof = 64
} S_Symbol

#define NAME(sym) {sym, #sym}

struct { S_Symbol sym; char* name; } symbol_names[] = {
    NAME(s_null),
    NAME(s_times), NAME(s_div), NAME(s_mod), NAME(s_and),
    NAME(s_plus), NAME(s_minus), NAME(s_or),
    NAME(s_eql), NAME(s_neq), NAME(s_lss), NAME(s_geq), NAME(s_leq), NAME(s_gtr),
    NAME(s_period), NAME(s_comma), NAME(s_colon),
    NAME(s_rparen), NAME(s_rbrak), NAME(s_of), NAME(s_then), NAME(s_do),
    NAME(s_lparen), NAME(s_lbrak), NAME(s_not), NAME(s_becomes),
    NAME(s_number), NAME(s_ident), NAME(s_semicolon),
    NAME(s_end), NAME(s_else), NAME(s_elsif), NAME(s_if), NAME(s_while),
    NAME(s_array), NAME(s_record), NAME(s_const), NAME(s_type), NAME(s_var),
    NAME(s_procedure), NAME(s_begin), NAME(s_module),
    NAME(s_eof)
}
#define S_SYMBOL_COUNT (sizeof(symbol_names) / sizeof(symbol_names[0]))

*typedef char S_Identifier[S_IDENTIFIER_LENGTH + 1]

struct {
    S_Symbol sym;
    char* id;
} keyword_table[] = {
    {s_null, "BY"},
    {s_do, "DO"},
    {s_if, "IF"},
    {s_null, "IN"},
    {s_null, "IS"},
    {s_of, "OF"},
    {s_or, "OR"},
    {s_null, "TO"},
    {s_end, "END"},
    {s_null, "FOR"},
    {s_mod, "MOD"},
    {s_null, "NIL"},
    {s_var, "VAR"},
    {s_null, "CASE"},
    {s_else, "ELSE"},
    {s_null, "EXIT"},
    {s_then, "THEN"},
    {s_type, "TYPE"},
    {s_null, "WITH"},
    {s_array, "ARRAY"},
    {s_begin, "BEGIN"},
    {s_const, "CONST"},
    {s_elsif, "ELSIF"},
    {s_null, "IMPORT"},
    {s_null, "UNTIL"},
    {s_while, "WHILE"},
    {s_record, "RECORD"},
    {s_null, "REPEAT"},
    {s_null, "RETURN"},
    {s_null, "POINTER"},
    {s_procedure, "PROCEDURE"},
    {s_div, "DIV"},
    {s_null, "LOOP"},
    {s_module, "MODULE"},
}
#define KEYWORD_COUNT (sizeof(keyword_table) / sizeof(keyword_table[0]))

*int S_value
*S_Identifier S_identifier
*bool S_error = true
char ch
int errpos
String source_text
int source_text_pos
int source_text_pos_token

#define ORD(ch) ((ch) & 0xff)

*void S_line_and_column(int* line, int* column)
    *line = 1
    *column = 1
    for int i = 0; i < source_text_pos_token && i < source_text.len; i++ do
        char c = source_text.s[i]
        if c == '\n' do
            *line += 1
            *column = 1
        else
            *column += 1

/*
Outputs a message referring to the current scanner position. The position is the
byte offset from the start of the source text. Only a single message per
position is output.
*/
*void S_mark(char* msg)
    int line, column
    S_line_and_column(&line, &column)
    if source_text_pos_token >= errpos do
        fprintf(stderr, "\t%d:%d: %s\n", line, column, msg)
    errpos = source_text_pos + 5
    S_error = true

// Prints the symbol and current state.
void print_symbol(S_Symbol sym)
    char* sym_name = "s_unknown"
    for int i = 0; i < S_SYMBOL_COUNT; i++ do
        if symbol_names[i].sym == sym do
            sym_name = symbol_names[i].name
            break
    printf("sym = %d %s, val = %d, id = %s\n", sym, sym_name + 2,
            S_value, S_identifier)

/*
Reads the next character from the source text, or '\0' if the end has been
reached.
*/
void read(char* c)
    //printf("read '%c'\n", source_text.s[source_text_pos])
    if source_text_pos < source_text.len do
        *c = source_text.s[source_text_pos]
    else
        *c = '\0'
    source_text_pos++

// Checks if the end of the source text has been reached.
bool read_eot()
    return source_text_pos > source_text.len

/*
Reads an identifier. Assumes that the first character has already been read.
Also checks if the identifier is a keyword and sets the symbol correspondingly
(or to s_ident if the identifier is not a keyword).
*/
void identifier(S_Symbol* sym)
    int i = 0
    do
        if i < S_IDENTIFIER_LENGTH do S_identifier[i++] = ch
        read(&ch)
    while ((ch >= '0' && ch <= '9') || (toupper(ch) >= 'A' && toupper(ch) <= 'Z'))
    S_identifier[i] = '\0'
    if i >= S_IDENTIFIER_LENGTH do S_mark("identifier too long")
    // Check if the identifier is a keyword.
    int k = 0
    while k < KEYWORD_COUNT && strcmp(S_identifier, keyword_table[k].id) != 0 do
        k++
    if k < KEYWORD_COUNT do *sym = keyword_table[k].sym
    else *sym = s_ident
    ensure("identifier is terminated", exists(i, S_IDENTIFIER_LENGTH + 1, S_identifier[i] == '\0'))
end. identifier

// Reads a number. Assumes that the first character has already been read.
void number(S_Symbol* sym)
    S_value = 0
    *sym = s_number
    do
        // 10 * val + ORD(ch) - ORD('0') < INT_MAX
        if S_value <= (INT_MAX - ORD(ch) + ORD('0')) / 10 do
            S_value = 10 * S_value + (ORD(ch) - ORD('0'))
        else
            S_mark("number too large")
            S_value = 0
        read(&ch)
    while (ch >= '0' && ch <= '9')
end. number

// Reads a (possibly nested) comment.
void comment(S_Symbol* sym)
    read(&ch)
    while (true)
        while (true)
            while ch == '(' do
                read(&ch)
                if ch == '*' do
                    comment(sym)
            if ch == '*' do
                read(&ch)
                break
            if read_eot() do
                break
            read(&ch)
        if ch == ')' do
            read(&ch)
            break
        if read_eot() do
            S_mark("comment not terminated")
            break
end. comment

*void S_get(S_Symbol* sym)
    // S_identifier[0] = '\0'
    // S_value = 0
    while !read_eot() && ch <= ' ' do read(&ch)
    source_text_pos_token = source_text_pos - 1
    if read_eot() do *sym = s_eof
    else
        switch ch do
            case '&': read(&ch); *sym = s_and; break
            case '*': read(&ch); *sym = s_times; break
            case '+': read(&ch); *sym = s_plus; break
            case '-': read(&ch); *sym = s_minus; break
            case '=': read(&ch); *sym = s_eql; break
            case '#': read(&ch); *sym = s_neq; break
            case '<':
                read(&ch)
                if ch == '=' do
                    read(&ch)
                    *sym = s_leq
                else
                    *sym = s_lss
                break
            case '>':
                read(&ch)
                if ch == '=' do
                    read(&ch)
                    *sym = s_geq
                else
                    *sym = s_gtr
                break
            case ';': read(&ch); *sym = s_semicolon; break
            case ',': read(&ch); *sym = s_comma; break
            case ':':
                read(&ch)
                if ch == '=' do
                    read(&ch)
                    *sym = s_becomes
                else
                    *sym = s_colon
                break
            case '.': read(&ch); *sym = s_period; break
            case '(':
                read(&ch)
                if ch == '*' do
                    comment(sym)
                    // do not return comment, but read next symbol
                    S_get(sym)
                else
                    *sym = s_lparen
                break
            case ')': read(&ch); *sym = s_rparen; break
            case '[': read(&ch); *sym = s_lbrak; break
            case ']': read(&ch); *sym = s_rbrak; break
            // https://gcc.gnu.org/onlinedocs/gcc/Case-Ranges.html
            case '0' ... '9': number(sym); break
            case 'A' ... 'Z': identifier(sym); break
            case 'a' ... 'z': identifier(sym); break
            case '~': read(&ch); *sym = s_not; break
            default: read(&ch); *sym = s_null; break
        end. switch
    // print_symbol(*sym)
end. S_get

/*
Initializes the scanner and prepares it to scan the source text starting at the
given position.
*/
*void S_init(String a_source_text, int pos)
    source_text = a_source_text
    source_text_pos = pos
    source_text_pos_token = pos
    S_value = 0
    S_identifier[0] = '\0'
    ch = '\0'
    S_error = false
    errpos = pos
    // read first character
    read(&ch)
end. S_init

void test_source_text(char* source_text)
    String s = make_string(source_text)
    S_Symbol sym = s_null
    S_init(s, 0)
    while sym != s_eof do
        S_get(&sym)
        print_symbol(sym)

void scanner_test(void)
    String s
    S_Symbol sym

    S_init(make_string("(*comment*)* DIV MOD&+-OR=#<>=<=>.,:)]OF THEN DO("
        "[~:=123 abc;END ELSE(*comment (*nested comment*)*)ELSIF IF WHILE ARRAY "
        "RECORD CONST TYPE VAR\tPROCEDURE BEGIN\nMODULE 123456789(*comment*) "), 0)
    S_get(&sym)
    test_equal_i(sym, s_times)
    S_get(&sym)
    test_equal_i(sym, s_div)
    S_get(&sym)
    test_equal_i(sym, s_mod)
    S_get(&sym)
    test_equal_i(sym, s_and)
    S_get(&sym)
    test_equal_i(sym, s_plus)
    S_get(&sym)
    test_equal_i(sym, s_minus)
    S_get(&sym)
    test_equal_i(sym, s_or)
    S_get(&sym)
    test_equal_i(sym, s_eql)
    S_get(&sym)
    test_equal_i(sym, s_neq)
    S_get(&sym)
    test_equal_i(sym, s_lss)
    S_get(&sym)
    test_equal_i(sym, s_geq)
    S_get(&sym)
    test_equal_i(sym, s_leq)
    S_get(&sym)
    test_equal_i(sym, s_gtr)
    S_get(&sym)
    test_equal_i(sym, s_period)
    S_get(&sym)
    test_equal_i(sym, s_comma)
    S_get(&sym)
    test_equal_i(sym, s_colon)
    S_get(&sym)
    test_equal_i(sym, s_rparen)
    S_get(&sym)
    test_equal_i(sym, s_rbrak)
    S_get(&sym)
    test_equal_i(sym, s_of)
    S_get(&sym)
    test_equal_i(sym, s_then)
    S_get(&sym)
    test_equal_i(sym, s_do)
    S_get(&sym)
    test_equal_i(sym, s_lparen)
    S_get(&sym)
    test_equal_i(sym, s_lbrak)
    S_get(&sym)
    test_equal_i(sym, s_not)
    S_get(&sym)
    test_equal_i(sym, s_becomes)
    S_get(&sym)
    test_equal_i(sym, s_number)
    S_get(&sym)
    test_equal_i(sym, s_ident)
    S_get(&sym)
    test_equal_i(sym, s_semicolon)
    S_get(&sym)
    test_equal_i(sym, s_end)
    S_get(&sym)
    test_equal_i(sym, s_else)
    S_get(&sym)
    test_equal_i(sym, s_elsif)
    S_get(&sym)
    test_equal_i(sym, s_if)
    S_get(&sym)
    test_equal_i(sym, s_while)
    S_get(&sym)
    test_equal_i(sym, s_array)
    S_get(&sym)
    test_equal_i(sym, s_record)
    S_get(&sym)
    test_equal_i(sym, s_const)
    S_get(&sym)
    test_equal_i(sym, s_type)
    S_get(&sym)
    test_equal_i(sym, s_var)
    S_get(&sym)
    test_equal_i(sym, s_procedure)
    S_get(&sym)
    test_equal_i(sym, s_begin)
    S_get(&sym)
    test_equal_i(sym, s_module)
    S_get(&sym)
    test_equal_i(sym, s_number)
    S_get(&sym)
    test_equal_i(sym, s_eof)
    S_get(&sym)
    test_equal_i(sym, s_eof)

    test_source_text("123 WHILE")
    test_source_text("(* comment (* nested *)*)test[123]IF")
    test_source_text("123\nBEGIN WHILE\n\nEND")
    test_source_text("2 * (3 + 4)")
    test_source_text("1234567890")
    // test_source_text("12345678901") // number too large
    test_source_text("123(*comment*)456")
    test_source_text("123(*co(*mm*)ent*)456")
end. scanner_test

int xmain(int argc, char* argv[])
    scanner_test()
    return 0
