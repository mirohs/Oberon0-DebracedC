/*
@author: Michael Rohs
@date: December 27, 2021
Debraced-C translation of: Oberon0 Compiler, Niklaus Wirth, Compiler Construction, 2005
https://people.inf.ethz.ch/wirth/CompilerConstruction/
*/

#include "util.h"
#include "scanner.h"
#include "risc.h"
#include "generator.h"

// The maximum number of code words that can be generated.
*#define G_MAX_CODE 1000

// Space needed on runtime stack for return address and old frame pointer.
*#define G_MARK_SIZE (2 * R_WORD_SIZE)

#if 0
#define report_instruction(at) \
        printf("%3d (%-6s): ", at, __func__); R_print_instruction(G_code[at])
#else
#define report_instruction(at)
#endif

*typedef enum {
    Head = 0,
    Var = 1, Par = 2, Const = 3, Fld = 4, Typ = 5, Proc = 6, StdProc = 7,
    Reg = 10, Cond = 11
} G_ClassMode

#define NAME(x) {x, #x}

struct { G_ClassMode class; char* name; } class_mode_names[] = {
    NAME(Head),
    NAME(Var), NAME(Par), NAME(Const), NAME(Fld), NAME(Typ), NAME(Proc), NAME(StdProc),
    NAME(Reg), NAME(Cond)
}
#define CLASS_MODE_COUNT (sizeof(class_mode_names) / sizeof(class_mode_names[0]))

*typedef enum { Undefined, Boolean, Integer, Array, Record } G_Form

struct { G_Form class; char* name; } form_names[] = {
    NAME(Undefined), NAME(Boolean), NAME(Integer), NAME(Array), NAME(Record)
}
#define FORM_COUNT (sizeof(form_names) / sizeof(form_names[0]))

typedef enum { FP = 12, SP = 13, LNK = 14, PC = 15 } ReservedRegister

*typedef struct G_Item G_Item
*typedef struct G_Object G_Object
*typedef struct G_Type G_Type

/*
A (named) object in the source code. Example variants:
- G_Object variants (layout of objects if they are completely constructed):
- Var (local and global variables)
- Par (reference (VAR) parameters)
- Const (constant value)
- Fld (field of a record)
- Typ (declared type)
- Proc (procedure declaration)
- StdProc (standard procedure)
*/
*struct G_Object
    G_ClassMode class
    int level // level (global = 0, local var in global procedure = 1, etc.)
    G_Object* next // next object in current scope, list ends with guard object
    G_Object* dsc // scope stack for header, parameter list for procedures
    G_Type* type // type of this object (G_undefined_type used as guard object)
    S_Identifier name
    INTEGER value // meaning depends on class: constant value, address offset, instruction index

/*
Represents information about an object or an expression during code generation.
For example needed for delayed code generation.
*/
*struct G_Item
    G_ClassMode mode // initially copy of object class, then addressing mode, etc.
    int level  // level (global = 0, local variable in global procedure = 1, etc.)
    G_Type* type // reference to object type (G_undefined_type used as guard object)
    INTEGER a  // initially copy of object value, constant value, address offset, 
               // false-link, or address of incomplete branch instruction (G_cond_forward_jump)
    INTEGER b  // true-link for incomplete forward jumps
    INTEGER c  // condition: 0: =, 1: !=, 2: <, 3: >=, 4: <=, 5: >
    // BEQ = 48, BNE = 49, BLT = 50, BGE = 51, BLE = 52, BGT = 53
    // negate: +1 if even, -1 if odd
    INTEGER r // base register (e.g., FP or PC)

// Creates a zero-initialized item on the stack.
*G_Item G_make_item(void)
    // mode, level, type, a, b, c, r
    // G_undefined_type is used as a guard object
    // r == -1 means that this item does not use a register
    return (G_Item){Head, 0, G_undefined_type, 0, 0, 0, -1}

// Type information about an object.
*struct G_Type
    G_Form form // Boolean, Integer, Array, Record
    G_Object* fields // for records: list of fields
    G_Type* base // for arrays: element type
    int size // total size of an object of this type in bytes
    int len // for arrays: number of array elements

// Creates a new type object.
*G_Type* G_new_type(G_Form form, int size)
    require("valid size", size >= 0)
    G_Type* t = xcalloc(1, sizeof(G_Type))
    t->form = form
    t->size = size
    return t

*G_Type* G_undefined_type // guard, used to avoid NULL checks
*G_Type* G_bool_type // predefined type
*G_Type* G_int_type // predefined type

*int G_current_level // global = 0, local variable in global procedure = 1, etc.
*int G_pc // index of the next free slot in the code array ("program counter")
*int G_entry // entry point into the module (start of "main")
Set registers // currently occupied registers
*INTEGER G_code[G_MAX_CODE] // generated code

// Prints the item.
*void G_print_item(G_Item* i)
    char* form = "(null)"
    if i->type != NULL do
        form = form_names[i->type->form].name
    printf("G_Item(%s, %s, level=%d, a=%d, b=%d, c=%d, r=%d)\n", 
            class_mode_names[i->mode].name, 
            form, 
            i->level, i->a, i->b, i->c, i->r)

// Prints the object.
*void G_print_object(G_Object* o)
    char* form = "(null)"
    if o->type != NULL do
        form = form_names[o->type->form].name
    printf("G_Object(%s, %s, %s, level=%d, value=%d)\n", 
            o->name, 
            class_mode_names[o->class].name, 
            form, o->level, o->value)

// Prints the type.
*void G_print_type(G_Type* t)
    char* base = "(null)"
    if t->base != NULL do
        base = form_names[t->base->form].name
    printf("G_Type(%s, base=%s, size=%d, len=%d)\n",
            form_names[t->form].name, 
            base, t->size, t->len)

// Allocates a free register.
void get_register(/*inout*/INTEGER* r)
    require("register free", *r < 0 || *r >= FP)
    int i = 0
    while i < FP && in(i, registers) do i++
    panic_if(i >= FP, "no free register found")
    incl(&registers, i)
    *r = i
    // printf("get_register(%d):  %4lx\n", i, registers.s);

// Deallocates a register.
void free_register(/*inout*/INTEGER* r)
    INTEGER i = *r
    if i >= 0 && i < FP do
        excl(&registers, i)
    // printf("free_register(%d): %4lx\n", i, registers.s);
    *r = -1

// Emits an instruction.
void put(int op, int a, int b, int c) 
    require("code array not full", G_pc < G_MAX_CODE)
    G_code[G_pc++] = R_encode_instruction(op, a, b, c)
    report_instruction(G_pc - 1)

// Emits a branch instruction.
void put_BR(int op, int disp) 
    require("code array not full", G_pc < G_MAX_CODE)
    G_code[G_pc++] = R_encode_instruction(op, 0, 0, disp)
    report_instruction(G_pc - 1)

// Checks whether x is in the range of an 18-bit two's complement value.
void test_range(INTEGER x)
    if x >= 0x20000 || x < -0x20000 do 
        S_mark("value too large")

// Loads the value denoted by x into a register.
void load(G_Item* x)
    require("not in Reg mode", x->mode != Reg)
    INTEGER r = -1
    if x->mode == Var do
        // global variables are located at a fixed offset relative to the code
        // in the current implementation they are located just below the program origin
        // global variables use the PC register as the base address
        // G_pc is the index of the next instruction in the code array
        // x->a is byte offset relative to code origin
        // at runtime code_origin = PC_r - 4 * pc_c
        // effective address: code_origin + x->a = PC - 4 * G_pc + x->a
        if x->r == PC do x->a -= G_pc * 4
        get_register(&r) 
        put(LDW, r, x->r, x->a) // item in Var mode means: r = base register, a = offset
        free_register(&x->r)
        x->r = r
        x->mode = Reg
    else if x->mode == Const do
        test_range(x->a) 
        get_register(&x->r) 
        put(MOVI, x->r, 0, x->a) // item in Const mode mans: a = constant value
        x->mode = Reg
    else exit_if(true, "wrong mode %s", class_mode_names[x->mode].name)
end. load

/*
Loads the boolean value denoted by x into a register. Also issues a CMPI(x,0)
instruction, so the comparison result will be in the N and Z flags. If x is
false (x = 0) then Z = 1. If x is true (x != 0) then Z = 0. So a subsequent BEQ
would branch if x is true.
*/
void load_bool(G_Item* x)
    if x->type->form != Boolean do S_mark("Boolean?")
    load(x) // load boolean value into register, 0 represents false
    x->mode = Cond // the value represents a condition
    x->a = 0 // end of linked list of conditional branches
    x->b = 0 // end of linked list of conditional branches
    x->c = 1 // condition: 0: =, 1: !=, 2: <, 3: >=, 4: <=, 5: >
    // needs to be translated into conditional branches
    // Cond mode means:
    //   r = register (containing boolean value),
    //   a = false-links, b = true-links,
    //   c = condition code
    put(CMPI, 0, x->r, 0) 
    // The comparison result will be in the N and Z flags.

/*
Emits an operation of the form x := x OP y. The operations are:
MOV, MVN, ADD, SUB, MUL, Div, Mod, CMP.
*/
void put_op(INTEGER op, G_Item* x, G_Item* y)
    require("valid range", MOV <= op && op <= CMP)
    if x->mode != Reg do load(x)
    if y->mode == Const do
        test_range(y->a)
        // (MOVI - MOV) == 16: offset of immediate-mode instructions
        put(op + (MOVI - MOV), x->r, x->r, y->a)
    else
        if y->mode != Reg do load(y)
        put(op, x->r, x->r, y->r)
        free_register(&y->r);

/*
Negates the condition. Possible because of the arrangement of conditions in the
instructions: 0: =, 1: !=, 2: <, 3: >=, 4: <=, 5: >
*/
INTEGER negated(INTEGER cond)
    return ODD(cond) ? cond - 1 : cond + 1

/*
Concatenates two linked lists of incomplate forward jumps. Each branch
instruction refers to the preceding branch instruction in the list.
*/
INTEGER merged(INTEGER L0, INTEGER L1)
    INTEGER L2, L3
    if L0 != 0 do
        L2 = L0
        while true do 
            L3 = G_code[L2] & 0x03ffffff
            if L3 == 0 do break
            L2 = L3
        G_code[L2] = G_code[L2] - L3 + L1
        report_instruction(L2)
        return L0
    else
        return L1

// Fixes branch instruction with address.
*void G_fix(int at, INTEGER with)
    assert("branch instruction at destination", ((G_code[at] >> 26) & 0x3f) >= BEQ)
    G_code[at] = (G_code[at] & 0xfc000000) | (with & 0x03ffffff)
    report_instruction(at)

// Fixes link list with L1.
void fix_with(INTEGER L0, INTEGER L1)
    INTEGER L2
    while L0 != 0 do
        L2 = G_code[L0] & 0x03ffffff // 26 bits
        G_fix(L0, L1 - L0)
        L0 = L2

// Fixes link list with G_pc.
*void G_fix_link(INTEGER L)
    INTEGER L_prev
    while L != 0 do
        L_prev = G_code[L] & 0x03ffffff // 26 bits
        G_fix(L, G_pc - L)
        L = L_prev

// Increments the level by the given value.
*void G_inc_level(int n)
    require("valid offset", n == -1 || n == 1)
    G_current_level += n

/*
Creates a constant value. It is not directly emmited as code, but stored in the
item. Most likely, it will later be used in an immediate-mode instruction.
*/
*void G_make_const_item(/*out*/G_Item* x, G_Type* type, INTEGER value)
    require("register not in use", x->r == -1 || x->r >= FP)
    x->mode = Const
    x->type = type
    x->a = value

/*
Sets the attributes of item x according to object y. Copies class (to mode),
type, level, and value (to a). Sets r to the appropriate base register index.
Called from: factor and statement_sequence. From statement_sequence either
called as the left-hand side of an assignment or as a procedure name in a
procedure call.

Var (local and global variables)
Par (reference (VAR) parameters)
Const (constant value)
Fld (field of a record)
Typ (declared type)
Proc (procedure declaration)
StdProc (standard procedure)
*/
*void G_item_from_object(/*out*/G_Item* x, /*in*/G_Object* y)
    INTEGER r = -1
    x->mode = y->class
    x->type = y->type
    x->level = y->level
    x->a = y->value // object value: constant value, address, offset, or standard procedure number
    x->b = 0;
    if y->level == 0 do // global (module level)
        x->r = PC // base is PC register (R15)
    else if y->level == G_current_level do // local in current scope
        x->r = FP // base is FP register (R12)
    else
        S_mark("access to intermediary levels (surrounding procedures) is not allowed")
        x->r = -1
    if y->class == Par do // reference (VAR) parameter
        // printf("G_item_from_object: G_pc=%d ", G_pc); G_print_object(y)
        get_register(&r)
        // x->r: FP, x->a: y->value == positive offset relative to base (FP)
        put(LDW, r, x->r, x->a) // load the address of the actual variable
        x->mode = Var // using Var will trigger another load (dereferencing) to access the actual value
        x->r = r // register in which the effective address is stored
        x->a = 0 // special case, normally for local variables, a < 0 and for value parameters a > 0
        // printf("G_item_from_object: G_pc=%d ", G_pc); G_print_item(x)
end. G_item_from_object

// Replaces x by the address of the object referred to by x.y. Called from selector.
*void G_field(/*inout*/G_Item* x, /*in*/G_Object* y) // x := x.y
    require("x is a record", x->type->form == Record)
    x->a += y->value
    x->type = y->type
end. G_field

// Replaces x by the address of the object referred to by x[y]. Called from selector.
*void G_index(/*inout*/G_Item* x, /*in*/G_Item* y) // x := x[y]
    require("x is an array", x->type->form == Array)
    require("valid mode", x->mode == Var || x->mode == Const)
    if y->type != G_int_type do S_mark("index not integer")
    if y->mode == Const do
        if y->a < 0 || y->a >= x->type->len do S_mark("bad index")
        x->a += y->a * x->type->base->size // array offset + index * sizeof(element)
    else
        if y->mode != Reg do load(y)
        put(CHKI, y->r, 0, x->type->len) // index check
        put(MULI, y->r, y->r, x->type->base->size) // i*s
        // for PC-relative addressing (_r = run time, _c = compile time):
        // PC_r = global_base_r + 4 * pc_c (PC register contents at this point)
        // item in Var mode: addr_r = global_base_r + offset_c (offset_c is in item->a)
        // address of indexed item: global_base_r + offset_c + i_r * s_c
        //                       = (PC_r - 4 * pc_c) + offset_c + i_r * s_c
        if x->r == PC do
            put(ADD, y->r, x->r, y->r) // PC_r + i_r * s_c
            put(SUBI, y->r, y->r, 4 * G_pc - 4) // (PC_r - 4 * pc_c) + i_r * s_c = global_base_r + i_r * s_c
        else
            put(ADD, y->r, x->r, y->r) // base_r + scaled_index_r
        free_register(&x->r)
        x->r = y->r
    x->type = x->type->base
end. G_index

/*
factor = integer | "(" expression ")" | "~" factor.
term = factor {"&" factor}.
SimpleExpression = term {"OR" term}.
expression = SimpleExpression [("=" | "#" | "<" | "<=" | ">" | ">=") SimpleExpression].
*/

// Emits unary operators.
*void G_op1(S_Symbol op, /*inout*/G_Item* x) // x := op x
    /// require("valid mode", x->mode == Const || x->mode == Var || x->mode == Reg)
    INTEGER t
    if op == s_minus do // negation
        if x->type->form != Integer do 
            S_mark("bad type")
        else if x->mode == Const do 
            // no need to emit code if it is a constant, just update the constant
            x->a = -x->a
        else
            // need to emit code, because the value to negate is not constant
            if x->mode != Reg do load(x) // was: if x->mode == Var do load(x)
            put(MVN, x->r, 0, x->r)
    else if op == s_not do
        if x->mode != Cond do load_bool(x)
        x->c = negated(x->c)
        // need to swap false-links (a) and true-links (b) of incomplete forward jumps
        t = x->a
        x->a = x->b
        x->b = t
        free_register(&x->r)
    else if op == s_and do
        // term = factor {AND branch_if_false(next_term) fixup(true_links) factor}.
        if x->mode != Cond do load_bool(x)
        // flags are set such that BEQ would branch if x is true
        // but with "and" need to branch if condition is false (short-circuit and operator)
        // jump  to next term if factor is false, otherwise check next factor of this term
        // x->a is code index of previous incomplete branch instruction (false-link)
        put_BR(BEQ + negated(x->c), x->a)
        free_register(&x->r)
        x->a = G_pc - 1 // code index of just emitted branch instruction, for later fixup
        G_fix_link(x->b) // G_pc is now the index of the next factor of the conditional 
                         // expression, so can fix true-links that need to go here
        x->b = 0 // end of linked list of true-links
    else if op == s_or do
        // cond_exp = term {OR branch_if_true(next_factor) fixup(false_links) term}.
        // or: if term false, check next term in conditional expression, else jump to T
        // so, if term false, do not jump, else jump to T
        if x->mode != Cond do load_bool(x)
        // flags are set such that BEQ would branch if x is true
        // branch if condition is true (short-circuit or operator)
        // x->b is code index of previous incomplete branch instruction (true-link)
        put_BR(BEQ + x->c, x->b)
        free_register(&x->r)
        x->b = G_pc - 1 // code index of just emitted branch instruction, for later fixup
        G_fix_link(x->a) // G_pc is now the index of the next term of the conditional 
                         // expression, so can fix the false-links that need to go here
        x->a = 0 // end of linked list of false links
end. G_op1

// Emits binary operators.
*void G_op2(S_Symbol op, /*inout*/G_Item* x, /*in*/G_Item* y) // x := x op y
    if x->type->form == Integer && y->type->form == Integer do
        if x->mode == Const && y->mode == Const do
            // todo: overflow checks
            if op == s_plus do x->a += y->a
            else if op == s_minus do x->a -= y->a
            else if op == s_times do x->a = x->a * y->a
            else if op == s_div do x->a = x->a / y->a
            else if op == s_mod do x->a = R_mod(x->a, y->a)
            else S_mark("bad type")
        else
            if op == s_plus do put_op(ADD, x, y)
            else if op == s_minus do put_op(SUB, x, y)
            else if op == s_times do put_op(MUL, x, y)
            else if op == s_div do put_op(Div, x, y)
            else if op == s_mod do put_op(Mod, x, y)
            else S_mark("bad type")
    else if x->type->form == Boolean && y->type->form == Boolean do
        if y->mode != Cond do load_bool(y)
        if op == s_or do 
            x->a = y->a // only false-links of the second term y can propagate
            x->b = merged(y->b, x->b) // merge true-links of x and y
            x->c = y->c // the second condition code is relevant
        else if op == s_and do 
            x->a = merged(y->a, x->a) // merge false-links of x and y
            x->b = y->b // only true-links of the second factor y can propagate
            x->c = y->c // the second condition code is relevant
        else S_mark("bad type")
    else S_mark("bad type")
    free_register(&y->r)
end. G_op2

/*
Emits code to check the given relation. The relations are: s_eql = 9, s_neq =
10, s_lss = 11, s_geq = 12, s_leq = 13, s_gtr = 14 (see scanner symbols)
*/
*void G_relation(S_Symbol op, /*inout*/G_Item* x, /*in*/G_Item* y) // x := x ? y
    require("valid relation", s_eql <= op && op <= s_gtr)
    if x->type->form != Integer || y->type->form != Integer do 
        S_mark("bad type")
    else 
        put_op(CMP, x, y)
        x->c = op - s_eql
    x->mode = Cond
    x->type = G_bool_type
    x->a = 0
    x->b = 0
end. G_relation

/*
Checks if type1 is a legal actual parameter type for the formal parameter type
type2.
*/
bool parameter_compatible(G_Type* actual_type, G_Type* formal_type)
    if actual_type == G_undefined_type || formal_type == G_undefined_type do return false
    if actual_type->form != formal_type->form do return false
    if actual_type->form == Integer do return true
    if actual_type->form == Boolean do return true
    if actual_type->form == Array do
        return actual_type->len == formal_type->len && \
               parameter_compatible(actual_type->base, formal_type->base)
    if actual_type->form == Record do
        return actual_type == formal_type
    return false

// Checks whether source may be assigned to destination.
bool assignment_compatible(G_Type* destination, G_Type* source)
    return parameter_compatible(destination, source)

// Copies one array to another array. x := y
void store_array(G_Item* x, G_Item* y)
    require("destination is array", x->type->form == Array)
    require("source is array", y->type->form == Array)
    require("valid mode", x->mode == Var)
    require("valid mode", y->mode == Var)
    int size = x->type->size // number of bytes
    int cap = -1; get_register(&cap)
    int src = -1; get_register(&src)
    int dst = -1; get_register(&dst)
    int r = -1; get_register(&r)
    put(ADDI, src, y->r, y->a - G_pc * 4 * (y->r == PC))
    put(ADDI, dst, x->r, x->a - 4 - G_pc * 4 * (x->r == PC))
    put(ADDI, cap, src, size)
    put(CMP, 0, src, cap)
    put_BR(BGE, 4)
    put(POP, r, src, 4)
    put(PSH, r, dst, -4)
    put_BR(BR, -4)
    free_register(&cap)
    free_register(&src)
    free_register(&dst)
    free_register(&r)
end. store_array

// Copies one record to another record. x := y
void store_record(G_Item* x, G_Item* y)
    require("destination is record", x->type->form == Record)
    require("source is record", y->type->form == Record)
    int size = x->type->size // number of bytes
    int cap = -1; get_register(&cap)
    int src = -1; get_register(&src)
    int dst = -1; get_register(&dst)
    int r = -1; get_register(&r)
    put(ADDI, src, y->r, y->a - G_pc * 4 * (y->r == PC))
    put(ADDI, dst, x->r, x->a - 4 - G_pc * 4 * (x->r == PC))
    put(ADDI, cap, src, size)
    put(CMP, 0, src, cap)
    put_BR(BGE, 4)
    put(POP, r, src, 4)
    put(PSH, r, dst, -4)
    put_BR(BR, -4)
    free_register(&cap)
    free_register(&src)
    free_register(&dst)
    free_register(&r)
end. store_record

// Stores the value of expression y in the location denoted by x.
*void G_store(/*inout*/G_Item* x, /*in*/G_Item* y) // x := y
    if assignment_compatible(x->type, y->type) do
        G_Form x_form = x->type->form
        if x_form == Integer || x_form == Boolean do
            if y->mode == Cond do // transform N,Z flags into a storable value
                // generate a boolean value (1 for true, 0 for false)
                put(BEQ + negated(y->c), y->r, 0, y->a)
                free_register(&y->r)
                y->a = G_pc-1 // set location of last foward jump
                G_fix_link(y->b) // resolve true-links
                get_register(&y->r)
                put(MOVI, y->r, 0, 1) // emit true
                put_BR(BR, 2) // skip next instruction (MOVI false)
                G_fix_link(y->a) // resolve false-links
                put(MOVI, y->r, 0, 0) // emit false
            else if y->mode != Reg do
                load(y)
            if x->mode == Var do
                put(STW, y->r, x->r, x->a - G_pc * 4 * (x->r == PC))
            else
                S_mark("illegal assignment")
            free_register(&x->r)
            free_register(&y->r)
        else if x_form == Array do
            store_array(x, y)
            free_register(&x->r)
            free_register(&y->r)
        else if x_form == Record do
            store_record(x, y)
            free_register(&x->r)
            free_register(&y->r)
        else S_mark("incompatible assignment")
    else S_mark("incompatible assignment")
end. G_store

/*
Checks if x is a parameter that corresponds to the given type and class and, if
so, generates code to push the parameter onto the stack.
*/
*void G_parameter(G_Item* x, G_Type* fp_type, G_ClassMode fp_class)
    if parameter_compatible(x->type, fp_type) do
        if fp_class == Par do // Reference (VAR) parameter
            // push address of actual parameter onto stack
            if x->mode == Var do
                INTEGER a = x->a
                if x->r == PC do a -= 4 * G_pc
                if a != 0 do
                    int r = -1; get_register(&r) // current x->r may be PC or FP
                    put(ADDI, r, x->r, a)
                    free_register(&x->r)
                    x->r = r
            else S_mark("illegal parameter mode")
        else if fp_class == Var do // value param
            // push value of actual parameter onto stack
            if x->mode != Reg do load(x)
        else exit_if(true, "invalid parameter class %d", fp_class)
        put(PSH, x->r, SP, 4)
        free_register(&x->r)
    else
        S_mark("bad parameter type")
end. G_parameter

/*
Emits a conditional forward jump with unknown destination address. Will be fixed
once it is known.
*/
*void G_cond_forward_jump(G_Item* x)
    if x->type->form == Boolean do
        if x->mode != Cond do load_bool(x)
        put_BR(BEQ + negated(x->c), x->a) // contains address of previous G_cond_forward_jump for later fixup
        free_register(&x->r)
        G_fix_link(x->b)
        x->a = G_pc - 1 // index of emitted conditional jump
    else 
        S_mark("Boolean?")
        x->a = G_pc

// Emits a backward jump.
*void G_backward_jump(INTEGER L)
    put_BR(BR, L - G_pc)

// Emits a forward jump and sets L to the pc of the emitted instruction.
*void G_forward_jump(INTEGER* L)
    put_BR(BR, *L)
    *L = G_pc - 1 // index of branch instruction

// Emits a procedure call.
*void G_call(G_Item* x)
    put_BR(BSR, x->a - G_pc)

*void G_io_read(G_Item* y)
    G_Item z = G_make_item()
    get_register(&z.r)
    z.mode = Reg
    z.type = G_int_type
    put(RD, z.r, 0, 0)
    G_store(y, &z)

*void G_io_write(G_Item* y)
    if (y->mode != Reg) load(y)
    put(WRD, 0, 0, y->r)
    free_register(&y->r)

*void G_io_write_hex(G_Item* y)
    if y->mode != Reg do load(y)
    put(WRH, 0, 0, y->r)
    free_register(&y->r)

*void G_io_read_byte(G_Item* y)
    G_Item z = G_make_item()
    get_register(&z.r)
    z.mode = Reg
    z.type = G_int_type
    put(RB, z.r, 0, 0)
    G_store(y, &z)

*void G_io_write_byte(G_Item* y)
    if y->mode != Reg do load(y)
    put(WRB, 0, 0, y->r)
    free_register(&y->r)

*void G_io_write_line(void)
    put(WRL, 0, 0, 0)

*void G_inc_dec(bool inc, G_Item* y, G_Item* delta)
    if y->mode == Var do
        int pc_relative = 4 * (y->r == PC);
        int r = -1; get_register(&r)
        put(LDW, r, y->r, y->a - pc_relative * G_pc)
        if delta->type->form == Const do
            put(inc ? ADDI : SUBI, r, r, delta->a)
        else
            if delta->mode != Reg do load(delta)
            put(inc ? ADD : SUB, r, r, delta->r)
            free_register(&delta->r)
        put(STW, r, y->r, y->a - pc_relative * G_pc)
        free_register(&r)
        free_register(&y->r)
    else S_mark("illegal assignment")
end. G_inc_dec

void put_string(char* s)
    int r = -1; get_register(&r)
    while *s do
        put(MOVI, r, 0, *s)
        put(WRB, 0, 0, r)
        s++
    free_register(&r)

void put_number(int i)
    int r = -1; get_register(&r)
    put(MOVI, r, 0, i)
    put(WRD, 0, 0, r)
    free_register(&r)

*void G_assert(G_Item* y)
    if y->type->form == Boolean do
        // test
        if y->mode != Cond do load_bool(y)
        INTEGER branch_location = G_pc
        put_BR(BEQ + y->c, 0)
        free_register(&y->r)
        G_fix_link(y->b)

        // output line number of assertion
        int line = 0, column = 0
        S_line_and_column(&line, &column)
        if column == 0 do line--
        put_string("ASSERT failed, line:")
        put_number(line)
        put(WRL, 0, 0, 0)

        // terminate program
        put(MOVI, LNK, 0, 0)
        put_BR(RET, LNK)

        G_fix_link(branch_location)
    else 
        S_mark("Boolean?")
end. G_assert

/*
Sets module entry point and emit code to initialize stack pointer and push
initial link register value (0).
Memory layout:
- [above_code to R_MEM_SIZE): runtime stack
- [R_PROG_ORG to end_of_code): code
- [R_PROG_ORG - varsize, R_PROG_ORG): global variables of the module
*/
*void G_header(void)
    G_entry = G_pc
    printf("entry = %d\n", G_entry)
    put(MOVI, SP, 0, R_MEM_SIZE)
    put(PSH, LNK, SP, 4)

// Emits a function prologue.
*void G_enter(INTEGER size)
    put(PSH, LNK, SP, 4)
    put(PSH, FP, SP, 4)
    put(MOV, FP, 0, SP)
    put(SUBI, SP, SP, size)

// Emits a function epilogue with the given space for local variables.
*void G_return(INTEGER size)
    put(MOV, SP, 0, FP)
    put(POP, FP, SP, 4)
    put(POP, LNK, SP, size + 4)
    put_BR(RET, LNK)

// Opens a module.
*void G_open(void)
    G_current_level = 0
    G_pc = 0
    G_entry = 0
    registers = make_set()

// Closes a module.
*void G_close(INTEGER globals)
    put(POP, LNK, SP, 4)
    put_BR(RET, LNK)
    ensure("all registers free", registers.s == 0)

// Initializes the generator.
*void G_init_generator(void)
    G_undefined_type = G_new_type(Undefined, R_WORD_SIZE)
    G_undefined_type->base = G_undefined_type
    G_bool_type = G_new_type(Boolean, R_WORD_SIZE)
    G_int_type = G_new_type(Integer, R_WORD_SIZE)
