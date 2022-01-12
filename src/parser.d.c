/*
@author: Michael Rohs
@date: December 27, 2021
Debraced-C translation of: Oberon0 Compiler, Niklaus Wirth, Compiler Construction, 2005
https://people.inf.ethz.ch/wirth/CompilerConstruction/
*/

#include "util.h"
#include "risc.h"
#include "scanner.h"
#include "generator.h"

/*
Oberon-0 EBNF Grammar

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
*/

// The most recently read symbol.
S_Symbol sym = s_null

/*
Linked lists of objects. All lists end with the guard object. Each scope starts
with a header object that links to the first object of this scope (next pointer)
and the header of the next scope below it (dsc pointer).
*/

// The current topmost scope.
G_Object* top_scope = NULL

// The outermost (global) scope.
G_Object* universe = NULL

// The guard object singleton marks the end of a list.
G_Object* guard = NULL

// List of forward calls (from nested procedures) that need to be fixed at the end.
G_Object* forward_calls = NULL

/*
Creates a new object of the given class and adds it to the topmost scope.
Assumes that the scanner has just read the identifier of the object. Only
creates the object if it does not already appear in the top scope. Only sets the
"name", "class", and "next" attributes of the new object.

An object can be the name of a variable, a value parameter, a variable
parameter, a constant, a field, a type, or a procedure. See G_ClassMode in
generator.
*/
void new_object(/*out*/G_Object** obj, G_ClassMode class)
    require("valid class", class == Const || class == Typ || class == Proc
                        || class == Fld || class == Var || class == Par)
    G_Object* x = top_scope
    // the scanner guarantees that S_identifier is terminated by '\0'
    strcpy(guard->name, S_identifier)
    while strcmp(x->next->name, S_identifier) != 0 do x = x->next
    if x->next == guard do
        // id not found in top scope
        G_Object* new = xcalloc(1, sizeof(G_Object))
        strcpy(new->name, S_identifier)
        new->class = class
        new->type = G_undefined_type
        new->next = guard
        x->next = new
        *obj = new
    else 
        // id found in top scope
        *obj = x->next
        S_mark("duplicate definition")
    ensure_not_null(*obj)
end. new_object

/*
G_Object variants:
Var (local and global variables)
Par (reference (VAR) parameters)
Const (constant value)
Fld (field of a record)
Typ (declared type)
Proc (procedure declaration)
StdProc (standard procedure)


Var (local and global variables):
name  = <identifier>
class = Var
type  = parsed type
value = negative offset relative to base, base is FP (local) or PC (global), 
        for int variables, first variable has value -4, second variable 
        has value -8, etc.)
level = current level (0 for global variables, 1 for local variables in global
        procedures, etc.)


Var (value parameters):
name  = <identifier>
class = Var
type  = parsed type
value = positive offset relative to base, base is FP, G_MARK_SIZE (for return
        address and old FP) is 2 words (8 bytes)
        for 2 int parameters, first parameter has value 12, second parameter 
        has value 8); for value parameters, the value of the actual parameter
        is located on the stack
level = current level (0 for global variables, 1 for local variables in global
        procedures, etc.)


Par (reference (VAR) parameters):
name  = <identifier>
class = Par
type  = stated type
value = positive offset relative to base, base is FP, G_MARK_SIZE (for return
        address and old FP) is 2 words (8 bytes)
        for 2 int parameters, first parameter has value 12, second parameter 
        has value 8); for variable parameters, the address of the actual parameter
        is located on the stack
level = current level (1 for local variables in global procedures, etc.)


Const (constant value):
name  = <identifier>
class = Const
type  = type of constant value
value = constant value
level = current level (0 for global constants, 1 for local constantsin global
        procedures, etc.)


Fld (field of a record):
name  = <identifier>
class = Fld
type  = parsed type
value = non-negative offset relative to start of record, 
        for 2 int fields, first field has value 0 and second field has value 4
level = 0


Typ (declared type):
name  = <identifier>
class = Typ
type  = parsed type
value = 0
level = 0


Proc (procedure declaration):
name  = <identifier>
class = Proc
type  = NULL
value = -1 of declared, but not defined; index of first instruction of procedure
        in code array, i.e. start of prologue
level = 0
dsc = formal parameter list


StdProc (standard procedure):
name  = <identifier>
class = StdProc
type  = NULL
value = <value used as argument to enter function>
level = 0
*/

/*
Tries to find an object in any scope. Assumes that the scanner has just read the
identifier of the object. Returns the guard object if no object with the given
identifier exists.
*/
void find(/*out*/G_Object** obj)
    G_Object* s = top_scope
    strcpy(guard->name, S_identifier)
    while true do
        G_Object* x = s->next
        // examine this scope
        while strcmp(x->name, S_identifier) != 0 do x = x->next
        if x != guard do
            // object found
            *obj = x
            break
        if s == universe do
            // not found and reached outermost scope, assign guard to object
            *obj = x
            S_mark("undef")
            break
        // go down to next lower scope
        s = s->dsc
    ensure_not_null(*obj)
end. find

/*
Tries to find an object in the list. Assumes that the scanner has just read the
identifier of the object. Returns the guard object if no object with the given
identifier exists.
*/
void find_field(/*out*/G_Object** obj, G_Object* list)
    strcpy(guard->name, S_identifier)
    while strcmp(list->name, S_identifier) != 0 do list = list->next
    *obj = list
    ensure_not_null(*obj)
end. find_field

/*
Checks if obj is a procedure parameter. Local variables have address offset
value < 0 (relative to FP). Global variables also have address offset < 0
(relative to PC). Value parameters have value > 0.
*/
BOOLEAN is_param(G_Object* obj)
    return obj->class == Par || (obj->class == Var && obj->value > 0)
end. is_param

// Opens a new variable scope.
void open_scope(void)
    G_Object* s = xcalloc(1, sizeof(G_Object))
    s->class = Head
    s->dsc = top_scope
    s->next = guard
    top_scope = s
end. open_scope

// Closes the topmost variable scope.
void close_scope(void)
    // todo: free dynamically allocated objects
    top_scope = top_scope->dsc
end. close_scope

void expression(G_Item* x)

// Parses a selector for array elements or record fields.
// selector = {"." ident | "[" expression "]"}.
void selector(/*inout*/G_Item* x) // x := x[y]  or  x := x.y
    G_Item y = G_make_item()
    G_Object* obj
    while sym == s_lbrak || sym == s_period do
        if sym == s_lbrak do
            S_get(&sym)
            expression(&y)
            if x->type->form == Array do G_index(x, &y)
            else S_mark("not an array")
            if sym == s_rbrak do S_get(&sym) 
            else S_mark("]?")
        else // s_period
            S_get(&sym)
            if sym == s_ident do
                if x->type->form == Record do
                    find_field(&obj, x->type->fields)
                    S_get(&sym)
                    if obj != guard do G_field(x, obj) 
                    else S_mark("undef")
                else S_mark("not a record")
            else S_mark("ident?")
end. selector

// Parses a factor.
// factor = ident selector | integer | "(" expression ")" | "~" factor.
void factor(/*out*/G_Item* x)
    G_Object* obj
    // synchronization point
    if sym < s_lparen do
        S_mark("ident?")
        do S_get(&sym); while (sym < s_lparen)
    if sym == s_ident do 
        find(&obj)
        S_get(&sym)
        G_item_from_object(x, obj)
        selector(x)
    else if sym == s_number do
        G_make_const_item(x, G_int_type, S_value)
        S_get(&sym)
    else if sym == s_lparen do
        S_get(&sym)
        expression(x)
        if sym == s_rparen do S_get(&sym)
        else S_mark(")?")
    else if sym == s_not do
        S_get(&sym)
        factor(x)
        G_op1(s_not, x)
    else
        S_mark("factor?")
        G_item_from_object(x, guard)
end. factor

// term = factor {("*" | "DIV" | "MOD" | "&") factor}.
void term(/*out*/G_Item* x)
    G_Item y = G_make_item()
    S_Symbol op
    factor(x)
    // s_times = 1, s_div = 3, s_mod = 4, s_and = 5 (see scanner symbols)
    while sym >= s_times && sym <= s_and do
        op = sym
        S_get(&sym)
        if op == s_and do G_op1(op, x)
        factor(&y)
        G_op2(op, x, &y)
end. term

// SimpleExpression = ["+"|"-"] term {("+"|"-" | "OR") term}.
void simple_expression(/*out*/G_Item* x)
    G_Item y = G_make_item()
    S_Symbol op
    if sym == s_plus do 
        S_get(&sym)
        term(x)
    else if sym == s_minus do 
        S_get(&sym)
        term(x)
        G_op1(s_minus, x)
    else
        term(x)
    // s_plus = 6, s_minus = 7, s_or = 8 (see scanner symbols)
    while sym >= s_plus && sym <= s_or do
        op = sym
        S_get(&sym)
        if op == s_or do G_op1(op, x)
        term(&y)
        G_op2(op, x, &y)
end. simple_expression

// expression = SimpleExpression [("=" | "#" | "<" | "<=" | ">" | ">=") SimpleExpression].
void expression(/*out*/G_Item* x)
    G_Item y = G_make_item()
    S_Symbol op
    simple_expression(x)
    // s_eql = 9, s_neq = 10, s_lss = 11, s_geq = 12, s_leq = 13, s_gtr = 14 (see scanner symbols)
    if sym >= s_eql && sym <= s_gtr do
        op = sym
        S_get(&sym)
        simple_expression(&y)
        G_relation(op, x, &y)
end. expression

/*
Parses a single parameter in an actual parameter list and checks whether it is
compatible with the given formal parameter. Then sets the argument to the next
parameter (if any).
ActualParameters = "(" [expression {"," expression}] ")" .
*/
void parameter(/*inout*/G_Object** p_formal_parameter)
    G_Object* formal_parameter = *p_formal_parameter
    G_Item x = G_make_item() // actual parameter
    expression(&x) // actual parameter expression
    if is_param(formal_parameter) do
        G_parameter(&x, formal_parameter->type, formal_parameter->class)
        formal_parameter = formal_parameter->next
    else
        S_mark("too many parameters")
    *p_formal_parameter = formal_parameter
end. parameter

// Parses an expression in a single-parameter call (e.g., IO calls).
void std_proc_call_param(/*out*/G_Item* x)
    if sym == s_lparen do S_get(&sym); else S_mark("(?")
    expression(x)
    if sym == s_rparen do S_get(&sym); else S_mark(")?")
end. std_proc_call_param

// Inline procedure calls.
void std_proc_call(/*in*/G_Item* x, /*out*/G_Item* y)
    require("x is standard procedure", x->mode == StdProc)
    G_Item delta = G_make_item()
    int std_proc_number = x->a
    switch std_proc_number do
        case 1: // Read
            std_proc_call_param(y)
            if y->type->form != Integer do S_mark("not integer")
            G_io_read(y)
            break
        case 2: // Write
            std_proc_call_param(y)
            if y->type->form != Integer do S_mark("not integer")
            G_io_write(y)
            break
        case 3: // WriteHex
            std_proc_call_param(y)
            if y->type->form != Integer do S_mark("not integer")
            G_io_write_hex(y)
            break
        case 4: // ReadByte
            std_proc_call_param(y)
            if y->type->form != Integer do S_mark("not integer")
            G_io_read_byte(y)
            break
        case 5: // WriteByte
            std_proc_call_param(y)
            if y->type->form != Integer do S_mark("not integer")
            G_io_write_byte(y)
            break
        case 6: // WriteLine
            if sym == s_lparen do
                S_get(&sym)
                if sym == s_rparen do S_get(&sym); else S_mark(")?")
            G_io_write_line()
            break
        case 7: case 8: // INC, DEC
            if sym == s_lparen do S_get(&sym); else S_mark("(?")
            expression(y) // var to increment
            if y->type->form != Integer do S_mark("not integer")
            if sym == s_comma do
                S_get(&sym)
                expression(&delta) // optional increment
            else
                G_make_const_item(&delta, G_int_type, 1)
            if delta.type->form != Integer do S_mark("not integer")
            if sym == s_rparen do S_get(&sym); else S_mark(")?")
            G_inc_dec(std_proc_number == 7, y, &delta)
            break
        case 9: // ASSERT
            std_proc_call_param(y)
            if y->type->form != Boolean do S_mark("not boolean")
            G_assert(y)
            break
        default: exit_if(true, "invalid standard procedure number %d", std_proc_number)
    end. switch
end. std_proc_call

// Adds a forward call.
void add_forward_call(G_Object* proc, int pc)
    G_Object* obj = xcalloc(1, sizeof(G_Object))
    obj->value = pc // where to do the fix
    obj->dsc = proc // the destination of the call
    obj->next = forward_calls
    forward_calls = obj
    // printf("added forward call to: %s\n", proc->name)

// Resolves all recorded forward calls.
void fix_foward_calls(void)
    G_Object* obj = forward_calls
    while obj != NULL do
        int at = obj->value
        int with = obj->dsc->value - at
        // printf("resolving forward call to %s at %d with %d\n", obj->dsc->name, at, with)
        G_fix(at, with)
        obj = obj->next

// statement = [assignment | ProcedureCall | IfStatement | WhileStatement].
// StatementSequence = statement {";" statement}.
void statement_sequence(void)
    G_Object *formal_parameters
    G_Object *obj
    G_Item x = G_make_item()
    G_Item y = G_make_item()
    INTEGER L
    while true do 
        // synchronization point
        obj = guard
        if sym < s_ident do
            S_mark("statement?")
            do S_get(&sym); while (sym < s_ident)
        // assignment | ProcedureCall
        if sym == s_ident do
            find(&obj)
            S_get(&sym)
            G_item_from_object(&x, obj)
            selector(&x)
            // assignment = ident selector ":=" expression.
            if sym == s_becomes do
                S_get(&sym)
                expression(&y)
                G_store(&x, &y)
            else if sym == s_eql do
                S_mark(":= ?")
                S_get(&sym)
                expression(&y)
                G_store(&x, &y)
            // ProcedureCall = ident [ActualParameters].
            else if x.mode == Proc do
                formal_parameters = obj->dsc; // parameter list
                if sym == s_lparen do 
                    S_get(&sym)
                    if sym == s_rparen do 
                        S_get(&sym)
                    else
                        while true do 
                            parameter(&formal_parameters)
                            if sym == s_comma do
                                S_get(&sym)
                            else if sym == s_rparen do
                                S_get(&sym)
                                break
                            else if sym >= s_semicolon do
                                break
                            else S_mark(") or , ?")
                end. sym == s_lparen
                if !is_param(formal_parameters) do
                    if obj->value < 0 do add_forward_call(obj, G_pc)
                    G_call(&x)
                else S_mark("too few parameters")
            // "standard" procedure
            else if x.mode == StdProc do
                std_proc_call(&x, &y)
            else if obj->class == Typ do S_mark("illegal assignment?")
            else S_mark("statement?")
        // IfStatement = "IF" expression "THEN" StatementSequence
        // {"ELSIF" expression "THEN" StatementSequence}
        // ["ELSE" StatementSequence] "END".
        else if sym == s_if do
            S_get(&sym)
            expression(&x)
            G_cond_forward_jump(&x)
            if sym == s_then do S_get(&sym); else S_mark("THEN?")
            statement_sequence()
            L = 0
            while sym == s_elsif do
                S_get(&sym)
                G_forward_jump(&L)
                G_fix_link(x.a)
                expression(&x)
                G_cond_forward_jump(&x)
                if sym == s_then do S_get(&sym); else S_mark("THEN?")
                statement_sequence()
            if sym == s_else do
                S_get(&sym)
                G_forward_jump(&L)
                G_fix_link(x.a)
                statement_sequence()
            else 
                G_fix_link(x.a)
            G_fix_link(L)
            if sym == s_end do S_get(&sym); else S_mark("END?")
        // WhileStatement = "WHILE" expression "DO" StatementSequence "END".
        else if sym == s_while do
            S_get(&sym)
            L = G_pc
            expression(&x)
            G_cond_forward_jump(&x)
            if sym == s_do do S_get(&sym); else S_mark("DO?")
            statement_sequence()
            G_backward_jump(L)
            G_fix_link(x.a)
            if sym == s_end do S_get(&sym); else S_mark("END?")
        if sym == s_semicolon do S_get(&sym)
        else if (sym >= s_semicolon && sym < s_if) || sym >= s_array do break
        else S_mark("; ?")
end. statement_sequence

/*
Parses a comma-separated list of identifiers. Creates an object for each one and
returns a pointer to the first created object.
IdentList = ident {"," ident}.
*/
void identifier_list(G_ClassMode class, /*out*/G_Object** first)
    G_Object* obj
    if sym == s_ident do
        new_object(first, class); S_get(&sym)
        while sym == s_comma do
            S_get(&sym)
            if sym == s_ident do 
                new_object(&obj, class); S_get(&sym)
            else S_mark("ident?")
        // IdentList is always followed by a colon
        if sym == s_colon do S_get(&sym)
        else S_mark(":?")
end. identifier_list

// type = ident | ArrayType | RecordType.
void P_type(/*out*/G_Type** type)
    G_Object* obj
    G_Object* first
    G_Item x = G_make_item()
    G_Type* tp
    *type = G_int_type 
    // synchronization point
    if sym != s_ident && sym < s_array do
        S_mark("type?")
        do S_get(&sym); while (sym != s_ident && sym < s_array)
    if sym == s_ident do
        find(&obj)
        S_get(&sym)
        if obj->class == Typ do *type = obj->type; else S_mark("type?")
    else if sym == s_array do
        S_get(&sym)
        expression(&x)
        if x.mode != Const || x.a < 0 do S_mark("bad index")
        if sym == s_of do S_get(&sym); else S_mark("OF?")
        P_type(&tp) // recursively parse element type
        *type = G_new_type(Array, x.a * tp->size)
        (*type)->base = tp
        (*type)->len = x.a
    else if sym == s_record do
        S_get(&sym)
        int size = 0
        open_scope()
        while true do
            if sym == s_ident do
                identifier_list(Fld, &first)
                P_type(&tp); // recursively parse field type
                obj = first
                while obj != guard do
                    obj->type = tp
                    obj->value = size
                    size += obj->type->size
                    obj = obj->next
            if sym == s_semicolon do S_get(&sym)
            else if sym == s_ident do S_mark("; ?")
            else break
        *type = G_new_type(Record, size)
        (*type)->fields = top_scope->next
        close_scope()
        if sym == s_end do S_get(&sym); else S_mark("END?")
    else S_mark("ident?")
end. P_type

// declarations = ["CONST" {ident "=" expression ";"}]
//     ["TYPE" {ident "=" type ";"}]
//     ["VAR" {IdentList ":" type ";"}]
//     {ProcedureDeclaration ";"}.
void declarations(INTEGER* varsize)
    G_Object* obj
    G_Object* first
    G_Item x
    G_Type* tp
    // sync
    if sym < s_const && sym != s_end do
        S_mark("declaration?")
        do S_get(&sym); while (sym < s_const && sym != s_end)
    while true do
        // ["CONST" {ident "=" expression ";"}]
        if sym == s_const do
            S_get(&sym)
            while sym == s_ident do
                new_object(&obj, Const)
                S_get(&sym)
                if sym == s_eql do S_get(&sym); else S_mark("=?")
                x = G_make_item()
                expression(&x)
                if x.mode == Const do 
                    obj->value = x.a
                    obj->type = x.type
                else S_mark("expression not constant")
                if sym == s_semicolon do S_get(&sym); else S_mark(";?")
        // ["TYPE" {ident "=" type ";"}]
        if sym == s_type do
            S_get(&sym)
            while sym == s_ident do
                new_object(&obj, Typ)
                S_get(&sym)
                if sym == s_eql do S_get(&sym); else S_mark("=?") 
                P_type(&obj->type)
                if sym == s_semicolon do S_get(&sym); else S_mark(";?")
        // ["VAR" {IdentList ":" type ";"}]
        if sym == s_var do
            S_get(&sym)
            while sym == s_ident do
                identifier_list(Var, &first)
                P_type(&tp)
                obj = first
                while obj != guard do
                    obj->type = tp
                    obj->level = G_current_level
                    *varsize += obj->type->size
                    obj->value = -*varsize
                    obj = obj->next
                if sym == s_semicolon do S_get(&sym); else S_mark("; ?")
        if sym >= s_const && sym <= s_var do
            S_mark("declaration?")
        else break
end. declarations

// FPSection = ["VAR"] IdentList ":" type.
void procedure_decl_fp_section(INTEGER* parblksize)
    G_Object* obj
    G_Object* first
    G_Type* tp
    INTEGER parsize
    if sym == s_var do 
        S_get(&sym)
        identifier_list(Par, &first)
    else
        identifier_list(Var, &first)
    if sym == s_ident do
        find(&obj)
        S_get(&sym)
        if obj->class == Typ do
            tp = obj->type
        else
            S_mark("ident?")
            tp = G_int_type
    else 
        S_mark("type-ident?") //? should work with ARRAY ... of...
        tp = G_int_type
    if first->class == Var do
        parsize = tp->size // size of value parameter
        if tp->form >= Array do S_mark("no struct params")
    else
        parsize = R_WORD_SIZE // size of reference parameter
    obj = first
    while obj != guard do
        obj->type = tp
        *parblksize += parsize
        obj = obj->next
end. procedure_decl_fp_section

// FormalParameters = "(" [FPSection {";" FPSection}] ")".
// ProcedureHeading = "PROCEDURE" ident [FormalParameters].
// ProcedureBody = declarations ["BEGIN" StatementSequence] "END".
// ProcedureDeclaration = ProcedureHeading ";" ProcedureBody ident.
void procedure_decl(void)
    G_Object *proc, *obj
    S_Identifier procid
    INTEGER locblksize, parblksize
    S_get(&sym)
    if sym == s_ident do
        strcpy(procid, S_identifier)
        new_object(&proc, Proc)
        S_get(&sym)
        parblksize = G_MARK_SIZE
        G_inc_level(1)
        open_scope()
        proc->value = -1
        if sym == s_lparen do
            S_get(&sym)
            if sym == s_rparen do S_get(&sym)
            else 
                procedure_decl_fp_section(&parblksize)
                while sym == s_semicolon do 
                    S_get(&sym)
                    procedure_decl_fp_section(&parblksize)
                if sym == s_rparen do S_get(&sym); else S_mark(")?")
        obj = top_scope->next // first parameter (after head), or guard
        locblksize = parblksize
        while obj != guard do
            obj->level = G_current_level
            if obj->class == Par do locblksize -= R_WORD_SIZE
            else locblksize -= obj->type->size
            obj->value = locblksize
            obj = obj->next
        proc->dsc = top_scope->next
        if sym == s_semicolon do S_get(&sym); else S_mark(";?")
        locblksize = 0
        declarations(&locblksize)
        while sym == s_procedure do
            procedure_decl()
            if sym == s_semicolon do S_get(&sym); else S_mark(";?")
        proc->value = G_pc // code index of procedure
        G_enter(locblksize) // procedure prologue
        if sym == s_begin do
            S_get(&sym)
            statement_sequence()
        if sym == s_end do S_get(&sym); else S_mark("END?")
        if sym == s_ident do
            if strcmp(procid, S_identifier) != 0 do S_mark("no match")
            S_get(&sym)
        G_return(parblksize - G_MARK_SIZE) // procedure epilogue
        close_scope()
        G_inc_level(-1)
        ensure_not_null(proc)
        ensure_not_null(proc->dsc) // the parameter lists consists at least of the guard object
    end. sym == s_ident
end. procedure_decl

// module = "MODULE" ident ";" declarations ["BEGIN" StatementSequence] "END" ident "." .
void module(void)
    S_Identifier module_identifier
    INTEGER varsize
    if sym == s_module do
        S_get(&sym)
        G_open()
        open_scope()
        varsize = 0
        if sym == s_ident do
            strcpy(module_identifier, S_identifier)
            S_get(&sym)
        else S_mark("ident?")
        if sym == s_semicolon do S_get(&sym); else S_mark(";?")
        declarations(&varsize)
        while sym == s_procedure do
            procedure_decl()
            if sym == s_semicolon do S_get(&sym); else S_mark(";?")
        G_header()
        if sym == s_begin do
            S_get(&sym)
            statement_sequence()
        if sym == s_end do S_get(&sym); else S_mark("END?") 
        if sym == s_ident do
            if strcmp(module_identifier, S_identifier) != 0 do S_mark("no match") 
            S_get(&sym)
        else S_mark("ident?")
        if sym != s_period do S_mark(". ?") 
        close_scope()
        if !S_error do
            G_close(varsize)
            printf("%d instructions generated\n", G_pc)
    else S_mark("MODULE?")
    fix_foward_calls()
end. module

/*
Creates a object of the given class, name, value, and type, and adds it to the
topmost scope.
*/
void enter(G_ClassMode class, INTEGER value, S_Identifier name, G_Type* type)
    G_Object* obj = xcalloc(1, sizeof(G_Object))
    obj->class = class
    obj->value = value
    strcpy(obj->name, name)
    obj->type = type
    obj->dsc = NULL
    obj->next = top_scope->next
    top_scope->next = obj
end. enter

void init(void)
    G_init()
    sym = s_null
    guard = xcalloc(1, sizeof(G_Object))
    guard->class = Var
    guard->type = G_int_type
    guard->value = 0
    top_scope = NULL
    open_scope()
    enter(Typ, 0, "UNDEFINED", G_undefined_type)
    enter(Typ, 1, "BOOLEAN", G_bool_type)
    enter(Typ, 2, "INTEGER", G_int_type)
    enter(Const, 1, "TRUE", G_bool_type)
    enter(Const, 0, "FALSE", G_bool_type)
    // "standard" procedures
    enter(StdProc, 1, "Read", NULL)
    enter(StdProc, 2, "Write", NULL)
    enter(StdProc, 3, "WriteHex", NULL)
    enter(StdProc, 4, "ReadByte", NULL)
    enter(StdProc, 5, "WriteByte", NULL)
    enter(StdProc, 6, "WriteLn", NULL)
    enter(StdProc, 7, "INC", NULL)
    enter(StdProc, 8, "DEC", NULL)
    enter(StdProc, 9, "ASSERT", NULL)
    universe = top_scope
    forward_calls = NULL
end. init

*void P_compile(String source_text)
    init()
    S_init(source_text, 0)
    S_get(&sym)
    module()
end. P_compile

void test_parser()
    String s
    s = make_string("MODULE M; END M.")
    // s = make_string("MODULE M; BEGIN END M.")
    // s = make_string("MODULE M; BEGIN WriteLn END M.")
    // s = make_string("MODULE M; CONST x = 123; BEGIN WriteLn END M.")
    // s = make_string("MODULE M; VAR x: INTEGER; BEGIN WriteLn END M.")
    // s = make_string("MODULE M; VAR x: INTEGER; BEGIN x := 123; WriteLn END M.")
    // s = make_string("MODULE M; VAR x: INTEGER; BEGIN x := 123; Write(x); WriteLn END M.")
    // s = make_string("MODULE M; VAR x: INTEGER; BEGIN Read(x); Write(x); WriteLn END M.")
    // s = make_string("MODULE M; VAR x: INTEGER; BEGIN Read(x); x := x - 1; Write(x); WriteLn END M.")
    // s = make_string("MODULE M; VAR x: INTEGER; BEGIN Read(x); x := x + 2; WriteHex(x); WriteLn END M.")
    // s = make_string("MODULE M; CONST c = 100; VAR x: INTEGER; BEGIN x := c; Write(x); WriteLn END M.")
    // s = make_string("MODULE M; CONST c = 12; VAR x: INTEGER; BEGIN x := 10 * c + c; Write(x); WriteLn END M.")
    // s = make_string("MODULE M; VAR x, y: INTEGER; BEGIN x := 1; y := 2 END M.")
    // s = make_string("MODULE M; CONST c = 12; VAR x: INTEGER; BEGIN x := 10 * c + c; Write(x); WriteLn END M.")
    // s = make_string("MODULE M; VAR x: INTEGER; BEGIN IF x > 5 THEN x := 3 ELSE x := 2 END END M.")
    //               0123456789012345678901234567890123456789012345678901234567890123456789
    // s = make_string("MODULE M; VAR x: INTEGER; BEGIN x := 10; IF x > 5 THEN x := 3 END; Write(x); WriteLn END M.")
    // s = make_string("MODULE M; VAR x: INTEGER; BEGIN x := 10; IF x > 5 THEN x := 1 ELSE x := 0 END; Write(x); WriteLn END M.")
    // s = make_string("MODULE M; VAR x: INTEGER; BEGIN x := 1; IF x > 10 THEN x := 2 ELSIF x > 5 THEN x := 1 ELSE x := 0 END; Write(x); WriteLn END M.")
    // s = make_string("MODULE M; VAR x, y: BOOLEAN; BEGIN x := x OR y END M.")
    // s = make_string("MODULE M; VAR x, y: BOOLEAN; BEGIN x := FALSE OR TRUE END M.")
    // s = make_string("MODULE M; VAR x, y: INTEGER; BEGIN x := 2 + 3 END M.")

    // s = read_file("if1.obn")
    P_compile(s)
    R_print_code(G_code, G_pc)
    R_load(G_code, G_pc)
    printf("RISC OUTPUT BEGIN\n")
    R_execute(0)
    printf("RISC OUTPUT END\n")

int mainx(int argc, char* argv[])
    if argc != 2 do
        printf("Usage: %s <filename source file>\n", argv[0])
        exit(EXIT_FAILURE)
    char* filename = argv[1]

    // test_parser()

    String s = read_file(filename)
    P_compile(s)
    if !S_error do
        R_print_code(G_code, G_pc)
        R_load(G_code, G_pc)
        printf("RISC OUTPUT BEGIN\n")
        R_execute(G_entry * R_WORD_SIZE)
        printf("RISC OUTPUT END\n")
        R_print_memory(R_PROG_ORG / R_WORD_SIZE - 4 * R_WORD_SIZE, R_PROG_ORG / R_WORD_SIZE, 8)
    return 0
