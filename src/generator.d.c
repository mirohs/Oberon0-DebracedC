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

*typedef enum {
    Head = 0,
    Var = 1, Par = 2, Const = 3, Fld = 4, Typ = 5, Proc = 6, StdProc = 7,
    Reg = 10, Cond = 11
} G_ClassMode

char* class_mode_names[] = {
    "Head",
    "Var", "Par", "Const", "Fld", "Typ", "Proc", "StdProc",
    "unused", "unused", 
    "Reg", "Cond"
}
#define CLASS_MODE_COUNT (sizeof(class_mode_names) / sizeof(class_mode_names[0]))

*typedef enum { Undefined, Boolean, Integer, Array, Record } G_Form
char* form_names[] = { "Undefined", "Boolean", "Integer", "Array", "Record" }
#define FORM_COUNT (sizeof(form_names) / sizeof(form_names[0]))

// use x4 as frame pointer (normally x8 is FP and x4 is TP)
enum ReservedRegister { ZERO = 0, LNK = 1, SP = 2, GP = 3, FP = 4 }

// Condition codes.
typedef enum {EQ = 0, NE = 1, LT = 2, GE = 3, LE = 4, GT = 5} ConditionCode
// s_eql = 9, s_neq = 10, s_lss = 11, s_geq = 12, s_leq = 13, s_gtr = 14 (see scanner symbols)

*typedef struct G_Item G_Item
*typedef struct G_Object G_Object
*typedef struct G_Type G_Type

/*
A (named) object in the source code. Example variants:
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
    INTEGER c  // condition: 0: =, 1: !=, 2: <, 3: >=, 4: <=, 5: >, see enum ConditionCode
    INTEGER r // base register (e.g., FP or GP)
    INTEGER r2 // comparison register (r RELATION r2)

// Creates a zero-initialized item on the stack.
*G_Item G_make_item(void)
    // mode, level, type, a, b, c, r, r2
    // G_undefined_type is used as a guard object
    // r == -1, r2 == -1 means no register
    return (G_Item){Head, 0, G_undefined_type, 0, 0, 0, -1, -1}

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
*int G_pc // index of the next free slot in the code array ("program counter"), word (not byte) index
*int G_entry // entry point into the module (start of "main"), word (not byte) index
Set registers // currently occupied registers
*INTEGER G_code[G_MAX_CODE] // generated code

// Prints the item.
*void G_print_item(G_Item* i)
    char* form = "(null)"
    if i->type != NULL do form = form_names[i->type->form]
    printf("G_Item(%s, %s, level=%d, a=%d, b=%d, c=%d, r=%d, r2=%d)\n", 
            class_mode_names[i->mode], 
            form, i->level, i->a, i->b, i->c, i->r, i->r2)

// Prints the object.
*void G_print_object(G_Object* o)
    char* form = "(null)"
    if o->type != NULL do form = form_names[o->type->form]
    printf("G_Object(%s, %s, %s, level=%d, value=%d)\n", 
            o->name, class_mode_names[o->class], 
            form, o->level, o->value)

// Prints the type.
*void G_print_type(G_Type* t)
    char* base = "(null)"
    if t->base != NULL do base = form_names[t->base->form]
    printf("G_Type(%s, base=%s, size=%d, len=%d)\n",
            form_names[t->form], 
            base, t->size, t->len)

// Allocates a free register.
void get_register(/*inout*/INTEGER* r)
    require("register free", *r <= FP)
    int i = FP + 1
    while i < R_REGISTER_COUNT && in(i, registers) do i++
    panic_if(i >= R_REGISTER_COUNT, "no free register found")
    incl(&registers, i)
    *r = i
    // printf("get_register(%d):  %4lx\n", i, registers.s);

// Deallocates a register.
void free_register(/*inout*/INTEGER* r)
    INTEGER i = *r
    if i > FP && i < R_REGISTER_COUNT do
        excl(&registers, i)
    // printf("free_register(%d): %4lx\n", i, registers.s);
    *r = -1

// Add the given instruction to the code array.
void put_instruction(INTEGER instruction)
    require("code array not full", G_pc < G_MAX_CODE)
    printf("add %3d: ", G_pc); R_print_instruction(instruction)
    G_code[G_pc++] = instruction

// Emits an R instruction.
void put_R(int inst, INTEGER rd, INTEGER rs1, INTEGER rs2)
    put_instruction(R_encode_R(inst, rd, rs1, rs2))

// Emits an I instruction.
void put_I(int inst, INTEGER rd, INTEGER rs1, INTEGER imm)
    // ADDI, XORI, ORI, ANDI, SLLI, SRLI, SRAI, SLTI, SLTIU 
    if ADDI <= inst && inst <= SLTIU do
        if -0x800 <= imm && imm < 0x800 do
            put_instruction(R_encode_I(inst, rd, rs1, imm))
        else
            INTEGER xl = (imm << 20) >> 20 // lower 12 bits, sign-extend
            INTEGER xu = (imm - xl) >> 12
            assert("valid xl and xu", imm == (xu << 12) + xl)
            put_instruction(R_encode_U(LUI, rd, xu))
            put_instruction(R_encode_I(ADDI, rd, rd, xl))
            put_instruction(R_encode_R(inst - ADDI + ADD, rd, rs1, rd))
    else
        put_instruction(R_encode_I(inst, rd, rs1, imm))
        // todo: could do the same as above for other immediates, e.g., LW, but would be more complicated
        // if in range, LW directly, otherwise load immediate in egister, add base, load

// Emits code to load an immediate into the destination register.
void put_I_load(INTEGER rd, INTEGER imm)
    if -0x800 <= imm && imm < 0x800 do
        put_instruction(R_encode_I(ADDI, rd, 0, imm))
    else
        INTEGER xl = (imm << 20) >> 20 // lower 12 bits, sign-extend
        INTEGER xu = (imm - xl) >> 12
        assert("valid xl and xu", imm == (xu << 12) + xl)
        put_instruction(R_encode_U(LUI, rd, xu))
        put_instruction(R_encode_I(ADDI, rd, rd, xl))

// Emits an S instruction.
void put_S(int inst, INTEGER rs2, INTEGER rs1, INTEGER imm)
    put_instruction(R_encode_S(inst, rs2, rs1, imm))

// Emits a B instruction.
void put_B(int inst, INTEGER rs1, INTEGER rs2, INTEGER imm)
    put_instruction(R_encode_B(inst, rs1, rs2, imm))

// Emits a B instruction using the given condition code.
void put_B_cc(ConditionCode cc, INTEGER rs1, INTEGER rs2, INTEGER imm)
    switch cc do
        case EQ: put_instruction(R_encode_B(BEQ, rs1, rs2, imm)); break
        case NE: put_instruction(R_encode_B(BNE, rs1, rs2, imm)); break
        case LT: put_instruction(R_encode_B(BLT, rs1, rs2, imm)); break
        case GE: put_instruction(R_encode_B(BGE, rs1, rs2, imm)); break
        case LE: put_instruction(R_encode_B(BGE, rs2, rs1, imm)); break
        case GT: put_instruction(R_encode_B(BLT, rs2, rs1, imm)); break
        default: exit_if(true, "invalid condition code %d", cc)

// Emits a U instruction.
void put_U(int inst, INTEGER rd, INTEGER imm)
    put_instruction(R_encode_U(inst, rd, imm))

// Emits a J instruction.
void put_J(int inst, INTEGER rd, INTEGER imm)
    put_instruction(R_encode_J(inst, rd, imm))

// Loads the value denoted by x into a register.
void load(G_Item* x)
    INTEGER r = -1
    if x->mode == Var do
        // global variables are located at a fixed offset relative to R[GP]
        // in the current implementation they are located just below the program origin
        // global variables use the GP register as the base address
        // x->a is a byte offset relative to the code origin
        // effective address: code_origin + x->a = R[GP] + x->a (globals, FP for locals)
        get_register(&r)
        put_I(LW, r, x->r, x->a) // item in Var mode means: r = base register, a = offset
        free_register(&x->r)
        x->r = r
        x->mode = Reg
    else if x->mode == Const do
        get_register(&x->r) 
        // item in Const mode mans: a = constant value
        put_I_load(x->r, x->a)
        x->mode = Reg
    else exit_if(true, "wrong mode %s", class_mode_names[x->mode])
end. load

/*
Loads the boolean value denoted by x into a register. The boolean value may
later be checked using a conditional branch instruction. Thus a condition code
is set here that represents a taken branch if the boolean value is true, i.e.:
BNE r, 0, target    (branch if r is not false)
*/
void load_bool(G_Item* x)
    if x->type->form != Boolean do S_mark("Boolean?")
    load(x) // load boolean value into register r, 0 represents false
    x->mode = Cond // the item represents a condition
    x->a = 0 // end of linked list of conditional branches (false-links)
    x->b = 0 // end of linked list of conditional branches (true-links)
    x->c = NE // condition, see enum ConditionCode
    // r = register (containing boolean value),
    // r2 = register (containing boolean comparison value),
    x->r2 = ZERO // second register for comparison (zero-register)

/*
Emits an operation of the form x := x OP y. The operations are:
ADD, SUB, MUL, DIV, REM.
language operations: s_plus, s_minus, etc. (scanner.d.c)
RISC instructions: ADD, ADDI, etc. (risc.d.c)
*/
void put_op(INTEGER op, /*inout*/G_Item* x, /*in*/G_Item* y) // x := x op y
    require("valid op", op == ADD || op == SUB || op == MUL || op == DIV || op == REM)
    if x->mode != Reg do load(x)
    if y->mode == Const && (op == ADD || op == SUB) do
        if op == SUB do y->a = -y->a
        put_I(ADDI, x->r, x->r, y->a)
    else
        if y->mode != Reg do load(y)
        put_R(op, x->r, x->r, y->r)
        free_register(&y->r)

/*
Negates the condition. Possible because of the arrangement of conditions in the
instructions: 0: =, 1: !=, 2: <, 3: >=, 4: <=, 5: >
*/
ConditionCode negated(ConditionCode cond)
    return ODD(cond) ? cond - 1 : cond + 1

/*
Concatenates two linked lists of incomplate forward jumps. Each branch
instruction refers to the preceding branch instruction in the list. The
immediate value of each branch instruction in the list contains the code index
of the preceding branch instruction in the list (or 0 to mark the end of the
list).
*/
INTEGER merged(INTEGER L0, INTEGER L1)
    INTEGER L2, L3
    R_Instruction i
    if L0 != 0 do
        L2 = L0
        while true do
            // divided by 2, because instruction stores word index times 2 (imm must be even)
            R_decode_instruction(G_code[L2], &i); L3 = i.imm / 2
            assert("is conditional branch", BEQ <= i.inst && i.inst <= BGEU)
            if L3 == 0 do break
            L2 = L3
        assert("end of list 0", L2 != 0 && L3 == 0)
        assert("is conditional branch", BEQ <= i.inst && i.inst <= BGEU)
        G_code[L2] = R_encode_B(i.inst, i.rs1, i.rs2, L1 * 2) // imm must be even
        printf("merged %3d: ", L2); R_print_instruction(G_code[L2])
        return L0
    else
        return L1

/*
Fixes branch instruction with jump offset. The with parameter is a word index
(index in code array) or difference between word indices.
*/
*void G_fix(int at, INTEGER with)
    R_Instruction i
    R_decode_instruction(G_code[at], &i)
    assert("is branch", (BEQ <= i.inst && i.inst <= BGEU) || i.inst == JAL)
    if BEQ <= i.inst && i.inst <= BGEU do
        G_code[at] = R_encode_B(i.inst, i.rs1, i.rs2, with * R_WORD_SIZE)
    else if i.inst == JAL do
        G_code[at] = R_encode_J(i.inst, i.rd, with * R_WORD_SIZE)
    printf("fix %3d: ", at); R_print_instruction(G_code[at])

/*
Fixes link list starting at code index L with jump offset relative to G_pc. L is
a word index (index in code array).
*/
*void G_fix_link(INTEGER L)
    R_Instruction i
    INTEGER L_prev
    while L != 0 do
        R_decode_instruction(G_code[L], &i)
        L_prev = i.imm / 2 // divided by 2, because instruction stores word index times 2 (imm must be even)
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
    x->level = 0
    x->b = 0; x->c = 0
    x->r = -1; x->r2 = -1

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
    x->level = y->level
    x->type = y->type
    x->a = y->value // object value: constant value, address, offset, or standard procedure number
    x->b = 0
    x->c = 0
    x->r2 = -1
    if y->level == 0 do // global (module level)
        x->r = GP // base is GP (global pointer) register (x3)
    else if y->level == G_current_level do // local in current scope
        x->r = FP // base is FP register (x4)
    else
        S_mark("access to intermediary levels (surrounding procedures) is not allowed")
        x->r = -1
    if y->class == Par do // reference (VAR) parameter
        // printf("G_item_from_object: G_pc=%d ", G_pc); G_print_object(y)
        get_register(&r)
        // x->r: FP, x->a: y->value == positive offset relative to base (FP)
        put_I(LW, r, x->r, x->a) // load the address of the actual variable
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

// Emits code to check whether 0 <= R[r_index] < R[r_len].
void check_index(int r_index, INTEGER len)
    INTEGER r_len = -1
    get_register(&r_len)
    put_I_load(r_len, len)
    INTEGER L = G_pc // index of BLTU instruction (to fix it later)
    put_B(BLTU, r_index, r_len, 0) // branch if index OK (skip ECALL)
    int line = 0, column = 0
    S_line_and_column(&line, &column)
    put_I_load(r_len, line * 1000 + column)
    put_I(ECALL, EXIDX, r_len, 0)
    G_fix_link(L)
    free_register(&r_len)
end. check_index

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
        check_index(y->r, x->type->len)
        INTEGER r_base_size = -1
        get_register(&r_base_size)
        put_I_load(r_base_size, x->type->base->size) // s
        put_R(MUL, y->r, y->r, r_base_size) // i * s
        free_register(&r_base_size)
        put_R(ADD, y->r, x->r, y->r) // base_r + scaled_index_r
        free_register(&x->r) // x->r may be GP or FP or another register
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
    if op == s_minus do // negation
        if x->type->form != Integer do 
            S_mark("bad type")
        else if x->mode == Const do 
            // no need to emit code if it is a constant, just update the constant
            x->a = -x->a
        else
            // need to emit code, because the value to negate is not constant
            if x->mode != Reg do load(x) // was: if x->mode == Var do load(x)
            put_R(SUB, x->r, 0, x->r)
    else if op == s_not do
        if x->mode != Cond do load_bool(x)
        x->c = negated(x->c)
        // need to swap false-links (a) and true-links (b) of incomplete forward jumps
        INTEGER t = x->a
        x->a = x->b
        x->b = t
        /// free_register(&x->r)
        // todo: not nice to have possibly: x->mode == Reg && x->r == -1, should be kept consistent
    else if op == s_and do
        // term = factor {AND branch_if_false(next_term) fixup(true_links) factor}.
        if x->mode != Cond do load_bool(x)
        // with "and" need to branch if condition is false (short-circuit and operator)
        // jump to next term if factor is false, otherwise check next factor of this term
        // x->a is code index of previous incomplete branch instruction (false-link)
        assert("", 0 <= x->r && x->r <= 31)
        put_B_cc(negated(x->c), x->r, x->r2, 2 * x->a)
        free_register(&x->r); free_register(&x->r2)
        x->a = G_pc - 1 // code index of just emitted branch instruction, for later fixup
        G_fix_link(x->b) // G_pc is now the index of the next factor of the conditional 
                         // term, so fix true-links that need to go here
        x->b = 0 // end of linked list of true-links
    else if op == s_or do
        // cond_exp = term {OR branch_if_true(next_factor) fixup(false_links) term}.
        if x->mode != Cond do load_bool(x)
        // with "or" need to branch if condition is true (short-circuit or operator)
        // x->b is code index of previous incomplete branch instruction (true-link)
        assert("", 0 <= x->r && x->r <= 31)
        put_B_cc(x->c, x->r, x->r2, 2 * x->b)
        free_register(&x->r); free_register(&x->r2)
        x->b = G_pc - 1 // code index of just emitted branch instruction, for later fixup
        G_fix_link(x->a) // G_pc is now the index of the next term of the conditional 
                         // expression, so fix the false-links that need to go here
        x->a = 0 // end of linked list of false links
end. G_op1

// Emits binary operators.
*void G_op2(S_Symbol op, /*inout*/G_Item* x, /*in*/G_Item* y) // x := x op y
    if x->type->form == Integer && y->type->form == Integer do
        if x->mode == Const && y->mode == Const do
            // todo: overflow checks
            if op == s_plus do x->a += y->a
            else if op == s_minus do x->a -= y->a
            else if op == s_times do x->a *= y->a
            else if op == s_div do x->a /= y->a
            else if op == s_mod do x->a %= y->a
            //else if op == s_mod do x->a = R_mod(x->a, y->a)
            else S_mark("bad type")
        else
            // todo: overflow checks
            if op == s_plus do put_op(ADD, x, y)
            else if op == s_minus do put_op(SUB, x, y)
            else if op == s_times do put_op(MUL, x, y)
            else if op == s_div do put_op(DIV, x, y)
            else if op == s_mod do put_op(REM, x, y)
            else S_mark("bad type")
    else if x->type->form == Boolean && y->type->form == Boolean do
        if y->mode != Cond do load_bool(y)
        if op == s_or do 
            x->a = y->a // only false-links of the second term y can propagate
            x->b = merged(y->b, x->b) // merge true-links of x and y
            x->c = y->c // the second condition code is relevant
            x->r = y->r
            x->r2 = y->r2
        else if op == s_and do 
            x->a = merged(y->a, x->a) // merge false-links of x and y
            x->b = y->b // only true-links of the second factor y can propagate
            x->c = y->c // the second condition code is relevant
            x->r = y->r
            x->r2 = y->r2
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
    if x->type->form != Integer || y->type->form != Integer do S_mark("bad type")
    if (x->mode != Reg) load(x)
    if (y->mode != Reg) load(y)
    x->c = op - s_eql // translate scanner symbol to enum ConditionCode
    x->mode = Cond
    x->type = G_bool_type
    x->a = 0
    x->b = 0
    x->r2 = y->r
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
    int src = -1; get_register(&src)
    int dst = -1; get_register(&dst)
    int r = -1; get_register(&r)
    int cap = -1; get_register(&cap)
    put_I(ADDI, src, y->r, y->a)
    put_I(ADDI, dst, x->r, x->a)
    put_I(ADDI, cap, src, size)
    assert("", 0 <= src && src <= 31)
    put_B(BGE, src, cap, 6 * R_WORD_SIZE) // jump if 0 >= R_i <=> jump if R_i <= 0
    // assert R_i > 0
    put_I(LW, r, src, 0)
    put_S(SW, r, dst, 0)
    put_I(ADDI, src, src, R_WORD_SIZE)
    put_I(ADDI, dst, dst, R_WORD_SIZE)
    put_J(JAL, 0, -5 * R_WORD_SIZE)
    free_register(&src)
    free_register(&dst)
    free_register(&r)
    free_register(&cap)
end. store_array

// Copies one record to another record. x := y
void store_record(G_Item* x, G_Item* y)
    require("destination is record", x->type->form == Record)
    require("source is record", y->type->form == Record)
    int size = x->type->size // number of bytes
    int src = -1; get_register(&src)
    int dst = -1; get_register(&dst)
    int r = -1; get_register(&r)
    int cap = -1; get_register(&cap)
    put_I(ADDI, src, y->r, y->a)
    put_I(ADDI, dst, x->r, x->a)
    put_I(ADDI, cap, src, size)
    assert("", 0 <= src && src <= 31)
    put_B(BGE, src, cap, 6 * R_WORD_SIZE) // jump if 0 >= R_i <=> jump if R_i <= 0
    // assert R_i > 0
    put_I(LW, r, src, 0)
    put_S(SW, r, dst, 0)
    put_I(ADDI, src, src, R_WORD_SIZE)
    put_I(ADDI, dst, dst, R_WORD_SIZE)
    put_J(JAL, 0, -5 * R_WORD_SIZE)
    free_register(&src)
    free_register(&dst)
    free_register(&r)
    free_register(&cap)
end. store_record

// Stores the value of expression y in the location denoted by x.
*void G_store(/*inout*/G_Item* x, /*in*/G_Item* y) // x := y
    if assignment_compatible(x->type, y->type) do
        G_Form x_form = x->type->form
        if x_form == Integer || x_form == Boolean do
            if y->mode == Cond do // transform condition into a storable value
                // generate a boolean value (1 for true, 0 for false)
                assert("", 0 <= y->r && y->r <= 31)
                put_B_cc(negated(y->c), y->r, y->r2, 2 * y->a)
                //free_register(&x->r); free_register(&x->r2)
                free_register(&y->r); free_register(&y->r2)
                y->a = G_pc-1 // set location of last foward jump
                G_fix_link(y->b) // resolve true-links
                get_register(&y->r)
                put_I(ADDI, y->r, 0, 1) // emit true
                put_J(JAL, 0, 2 * R_WORD_SIZE) // skip next instruction (emit false)
                G_fix_link(y->a) // resolve false-links
                put_I(ADDI, y->r, 0, 0) // emit false
            else if y->mode != Reg do
                load(y)
            if x->mode == Var do
                put_S(SW, y->r, x->r, x->a)
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
                int r = -1; get_register(&r)
                put_I(ADDI, r, x->r, x->a)
                free_register(&x->r)
                x->r = r
            else S_mark("illegal parameter mode")
        else if fp_class == Var do // value param
            // push value of actual parameter onto stack
            if x->mode != Reg do load(x)
        else exit_if(true, "invalid parameter class %d", fp_class)
        put_I(ADDI, SP, SP, -4) // todo: inefficient, should be done once and not for each parameter
        put_S(SW, x->r, SP, 0)
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
        assert("", 0 <= x->r && x->r <= 31)
        // branch instruction contains address of previous G_cond_forward_jump for later fixup
        // multiplied by two, because branch instruction can only store even values
        put_B_cc(negated(x->c), x->r, x->r2, 2 * x->a) 
        free_register(&x->r); free_register(&x->r2)
        G_fix_link(x->b)
        // todo: set x->b = 0 ?
        x->a = G_pc - 1 // index of emitted conditional jump
    else 
        S_mark("Boolean?")
        x->a = G_pc

/*
Emits a backward jump. L is a word index (index in code array). The L is final
and does not need to be fixed.
*/
*void G_backward_jump(INTEGER L)
    put_J(JAL, 0, R_WORD_SIZE * (L - G_pc))

/*
Emits a forward jump by L and then sets L to the pc of the emitted instruction.
*L is a word index (index in code array). Will be fixed once it is known.
*/
*void G_forward_jump(INTEGER* L)
    // *L is multiplied by two, because branch instruction can only store even values
    put_J(JAL, 0, *L * 2)
    *L = G_pc - 1 // index of branch instruction

// Emits a procedure call.
*void G_call(G_Item* x)
    put_J(JAL, LNK, R_WORD_SIZE * (x->a - G_pc))

*void G_io_read(G_Item* y)
    G_Item z = G_make_item()
    get_register(&z.r)
    z.mode = Reg
    z.type = G_int_type
    put_I(ECALL, RD, z.r, 0)
    G_store(y, &z)

*void G_io_write(G_Item* y)
    if (y->mode != Reg) load(y)
    put_I(ECALL, WRD, y->r, 0)
    free_register(&y->r)

*void G_io_write_hex(G_Item* y)
    if y->mode != Reg do load(y)
    put_I(ECALL, WRH, y->r, 0)
    free_register(&y->r)

*void G_io_read_byte(G_Item* y)
    G_Item z = G_make_item()
    get_register(&z.r)
    z.mode = Reg
    z.type = G_int_type
    put_I(ECALL, RB, z.r, 0)
    G_store(y, &z)

*void G_io_write_byte(G_Item* y)
    if y->mode != Reg do load(y)
    put_I(ECALL, WRB, y->r, 0)
    free_register(&y->r)

*void G_io_write_line(void)
    put_I(ECALL, WRL, 0, 0)

*void G_inc_dec(bool inc, G_Item* y, G_Item* delta)
    if y->mode == Var do
        int r = -1; get_register(&r)
        put_I(LW, r, y->r, y->a)
        if delta->mode == Const do
            put_I(ADDI, r, r, inc ? delta->a : -delta->a)
        else
            if delta->mode != Reg do load(delta)
            put_R(inc ? ADD : SUB, r, r, delta->r)
            free_register(&delta->r)
        put_S(SW, r, y->r, y->a)
        free_register(&r)
        free_register(&y->r)
    else S_mark("illegal assignment")
end. G_inc_dec

void put_string(char* s)
    int r = -1; get_register(&r)
    while *s do
        put_I(ADDI, r, 0, *s)
        put_I(ECALL, WRB, r, 0)
        s++
    free_register(&r)

void put_number(int i)
    int r = -1; get_register(&r)
    put_I(ADDI, r, 0, i)
    put_I(ECALL, WRD, r, 0)
    free_register(&r)

*void G_assert(G_Item* y)
    if y->type->form == Boolean do
        // test
        if y->mode != Cond do load_bool(y)
        INTEGER branch_location = G_pc
        assert("", 0 <= y->r && y->r <= 31)
        put_B_cc(y->c, y->r, y->r2, 0)
        free_register(&y->r); free_register(&y->r2)

        // emit code for failed assertion
        G_fix_link(y->a); y->a = 0 // fix false links
        // output line number of assertion
        int line = 0, column = 0
        S_line_and_column(&line, &column)
        if column == 0 do line--
        put_string("ASSERT failed, line:")
        put_number(line)
        put_I(ECALL, WRL, 0, 0)
        // terminate program
        put_I(ADDI, LNK, 0, 0)
        put_I(JALR, 0, LNK, 0)

        // continue here if assertion passed
        G_fix_link(branch_location)
        G_fix_link(y->b); y->b = 0 // fix true links
    else 
        S_mark("Boolean?")
end. G_assert

/*
Sets module entry point and emits code to initialize stack pointer and pushes
initial link register value (0).
Memory layout:
- [above_code to R_MEM_SIZE): runtime stack
- [R_PROG_ORG to end_of_code): code
- [R_PROG_ORG - varsize, R_PROG_ORG): global variables of the module
*/
*void G_header(void)
    G_entry = G_pc
    printf("entry = %d\n", G_entry)
    put_I_load(SP, R_MEM_SIZE - 4)
    put_S(SW, LNK, SP, 0)
    put_I_load(GP, R_PROG_ORG)

// Emits a function prologue.
*void G_enter(INTEGER size)
    put_S(SW, LNK, SP, -4)
    put_S(SW, FP, SP, -8)
    put_I(ADDI, FP, SP, -8)
    put_I(ADDI, SP, SP, -(size + 8))

// Emits a function epilogue with the given space for local variables.
*void G_return(INTEGER size)
    // pop FP, LNK, and parameters
    put_I(ADDI, SP, FP, 8 + size) // FP == SP_enter - 8, SP_return == FP + 8 = SP_enter
    put_I(LW, LNK, SP, -4 - size) // restore LNK
    put_I(LW, FP, SP, -8 - size)  // restore FP
    put_I(JALR, 0, LNK, 0) // return

// Opens a module.
*void G_open(void)
    G_current_level = 0
    G_pc = 0
    G_entry = 0
    registers = make_set()

// Closes a module.
*void G_close(INTEGER globals)
    put_I(LW, LNK, SP, 0)
    put_I(ADDI, SP, SP, 4)
    put_I(JALR, 0, LNK, 0)
    ensure("all registers free", registers.s == 0)

// Initializes the generator.
*void G_init(void)
    G_undefined_type = G_new_type(Undefined, R_WORD_SIZE)
    G_undefined_type->base = G_undefined_type
    G_bool_type = G_new_type(Boolean, R_WORD_SIZE)
    G_int_type = G_new_type(Integer, R_WORD_SIZE)
