/*
@author: Michael Rohs
@date: November 28, 2021
*/

#ifndef util_h_INCLUDED
#define util_h_INCLUDED

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

/*
A String points to some part of a C string, i.e., it does not have to end with
'\0', but has an explicit length.
*/
typedef struct String String;
struct String {
    char* s;
    int len;
    int cap;
};

String make_string(char* s);
String make_string2(char* s, int len);
String make_string3(char* s, int len, int cap);
String new_string(int cap);

void append_string(String* str, String t);
void append_cstring(String* str, char* t);
void append_cstring2(String* str, char* s, char* t);
void append_char(String* str, char c);
void append_test(void);

void xappend_string(String* str, String t);
void xappend_cstring(String* str, char* t);
void xappend_cstring2(String* str, char* s, char* t);
void xappend_char(String* str, char c);
void xappend_test(void);

void print_string(String str);
void println_string(String str);

String trim(String str);
String trim_left(String str);
String trim_right(String str);
void trim_test(void);
void trim_left_test(void);
void trim_right_test(void);
bool contains(String str, String part);
bool starts_with(String str, String part);
bool ends_with(String str, String part);
int index_of(String str, String part);
void index_of_test(void);
int index_of_char(String str, char c);
int last_index_of_char(String str, char c);

bool cstring_equal(String str, char* t);

typedef struct StringNode StringNode;
struct StringNode {
    String str;
    StringNode* next;
};

StringNode* new_string_node(String str, StringNode* next);

#define generate_list(ListType, ElementType, func_prefix, func_suffix)\
typedef struct ListType ListType;\
struct ListType {\
    ElementType* first;\
    ElementType* last;\
};\
void func_prefix##append##func_suffix(ListType* list, ElementType* element) {\
    require_not_null(list);\
    require_not_null(element);\
    if (list->first == NULL) {\
        list->first = element;\
        list->last = element;\
    } else {\
        list->last->next = element;\
        list->last = element;\
    }\
}\
void func_prefix##free##func_suffix(ListType* list) {\
    require_not_null(list);\
    ElementType* e_next;\
    for (ElementType* e = list->first; e != NULL; e = e_next) {\
        e_next = e->next;\
        free(e);\
    }\
}

typedef struct StringArray StringArray;
struct StringArray {
    int len;
    int cap;
    String a[]; // variable-sized array
};

StringArray* new_string_array(int cap);

StringArray* split(char* s, char sep);
void split_test(void);
StringArray* split_lines(char* s);
void split_lines_test(void);

String read_file(char* name);
void write_file(char* name, String data);



typedef struct Set Set;
struct Set {
    long int s;
};
Set make_set(void);
bool in(int x, Set s);
void incl(Set* s, int x);
void excl(Set* s, int x);


#define ODD(x) ((x) & 1)
#define EVEN(x) !ODD(x)


// #define NO_REQUIRE
// #define NO_ENSURE
// #define NO_ASSERT

#ifdef NO_REQUIRE
#define require_not_null(argument)
#else
/**
Checks that the given argument is not NULL. If so, does nothing. Otherwise
reports the location of the precondition and stops the program. A precondition
is a special type of assertion that has to be valid at the beginning of a
function.

Example use of a precondition:
@code{.c}
    int myfunction(char* s) {
        require_not_null(s);
        ...
    }
@endcode

Example output of failed preconditions:

    myfile.c, line 12: myfunction's precondition "not null" (s) violated

@param[in] argument pointer a pointer that must not be null
*/
#define require_not_null(argument) \
if (argument == NULL) {\
    fprintf(stderr, "%s, line %d: %s's precondition \"not null\" (" #argument ") violated\n", __FILE__, __LINE__, __func__);\
    exit(EXIT_FAILURE);\
}
#endif

#ifdef NO_REQUIRE
#define require(description, condition)
#else
/**
Checks the given precondition. If the condition is true, does nothing. If the
condition is false, reports the location of the precondition and stops the
program. A precondition is a special type of assertion that has to be valid at
the beginning of a function.

Example use of a precondition:
@code{.c}
    int myfunction(int x) {
        require("not too large", x < 100);
        ...
    }
@endcode

Example output of failed preconditions:

    myfile.c, line 12: myfunction's precondition "not too large" (x < 100) violated

@param[in] description char* a description of the condition that has to be valid
@param[in] condition boolean the condition to check
*/
#define require(description, condition) \
if (!(condition)) {\
    fprintf(stderr, "%s, line %d: %s's precondition \"%s\" (%s) violated\n", __FILE__, __LINE__, __func__, description, #condition);\
    exit(EXIT_FAILURE);\
}
#endif

#ifdef NO_ENSURE
#define ensure(description, condition)
#else
/**
Checks the given postcondition. If the condition is true, does nothing. If the
condition is false, reports the location of the postcondition and stops the
program. A postcondition is a special type of assertion that has to be valid
before returning from a function.

Example use of a postcondition:
@code{.c}
    int myfunction(...) {
        int result = 0;
        ...
        ensure("not negative", result >= 0);
        return result;
    }
@endcode

Example output of failed postconditions:

    myfile.c, line 12: myfunction's postcondition "not negative" (result >= 0) violated
    myfile.c, line 12: myfunction's postcondition "sorted" (forall(int i = 0, i < n-1, i++, a[i] <= a[i+1])) violated

@param[in] description char* a description of the condition that has to be valid
@param[in] condition boolean the condition to check
*/
#define ensure(description, condition) \
if (!(condition)) {\
    fprintf(stderr, "%s, line %d: %s's postcondition \"%s\" (%s) violated\n", __FILE__, __LINE__, __func__, description, #condition);\
    exit(EXIT_FAILURE);\
}
#endif

#ifdef NO_ASSERT
#define assert(description, condition)
#else
/**
Checks the given condition. If the condition is true, does nothing. If the
condition is false, reports the file and line of the assertion and stops the
program. Assertions are conditions that have to be valid at a particular point.
If an assertion is false this indicates a bug in the program.

Example use of an assertion:
@code{.c}
    ...
    assert("not too large", x < 3);
    ...
@endcode

Example output of failed assertions:

    myfile.c, line 12: assertion "not too large" (x < 3) violated
    myfile.c, line 12: assertion "sorted" (forall(int i = 0, i < n-1, i++, a[i] <= a[i+1])) violated

@param[in] description char* a description of the condition that has to be valid
@param[in] condition boolean the condition to check
*/
#define assert(description, condition) \
if (!(condition)) {\
    fprintf(stderr, "%s, line %d: assertion \"%s\" (%s) violated\n", __FILE__, __LINE__, description, #condition);\
    exit(EXIT_FAILURE);\
}
#endif

#ifdef NO_ASSERT
#define assert_not_null(pointer)
#else
/**
Checks that the given pointer is not NULL. If so, does nothing. Otherwise
reports the location of the assertion and stops the program. Assertions are
conditions that have to be valid at a particular point. If an assertion is false
this indicates a bug in the program.

Example use of an assertion:
@code{.c}
    ...
    assert_not_null(s);
    ...
@endcode

Example output of failed assertion:

    myfile.c, line 12: assertion "not null" (s) violated

@param[in] pointer a pointer that must not be null
*/
#define assert_not_null(pointer) \
if (pointer == NULL) {\
    fprintf(stderr, "%s, line %d: assertion \"not null\" (" #pointer ") violated\n", __FILE__, __LINE__);\
    exit(EXIT_FAILURE);\
}
#endif

/**
Allows writing code that is meant for use in a postcondition. The code is removed if NO_ENSURE is defined.

Example:
@code{.c}
    int inc(int x) {
        ensure_code(int old_x = x); // save old value for postcondition
        x = x + 1;
        ensure("incremented", x == old_x + 1); // check whether new value is as expected
        return x;
    }
@endcode
*/
#ifdef NO_ENSURE
#define ensure_code(x)
#else
#define ensure_code(x) x
#endif

#ifdef NO_ENSURE
#define ensure_not_null(pointer)
#else
/**
Checks that the given pointer is not NULL. If so, does nothing. Otherwise reports the location of the postcondition and stops the program. A postcondition is a special type of assertion that has to be valid before returning from a function.

Example use of a postcondition:
@code{.c}
    ...
    ensure_not_null(s);
    ...
@endcode

Example output of failed postcondition:

    myfile.c, line 12: myfunction's postcondition "not null" (s) violated

@param[in] pointer a pointer that must not be null
*/
#define ensure_not_null(pointer) \
if (pointer == NULL) {\
    fprintf(stderr, "%s, line %d: %s's postcondition \"not null\" (" #pointer ") violated\n", __FILE__, __LINE__, __func__);\
    exit(EXIT_FAILURE);\
}
#endif

/**
Checks whether the given condition is true for all steps of an iteration.
Primarily for use in assertions, preconditions, and postconditions.

Example: Checking whether an array is sorted:
@code{.c}
    bool is_sorted = forall(k, arr_length - 1, arr[k] <= arr[k+1]);
@endcode
 */
#define forall(i, length, condition) ({\
   bool _forall_result = true;\
   for (int i = 0; i < length; i++) { if (!(condition)) { _forall_result = false; break; } }\
   _forall_result;\
})

/**
Checks whether the given condition is true for all steps of an iteration.
Primarily for use in assertions, preconditions, and postconditions.

Example: Checking whether an array is sorted:
@code{.c}
    bool is_sorted = forall_x(int i = 0, i < arr_length - 1, i++, arr[i] <= arr[i+1]);
@endcode
 */
#define forall_x(init, has_more_steps, do_step, condition) ({\
   bool _forall_result = true;\
   for (init; has_more_steps; do_step) { if (!(condition)) { _forall_result = false; break; } }\
   _forall_result;\
})

/**
Checks whether the given condition is true for at least one step of an
iteration. Primarily for use in assertions, preconditions, and postconditions.

Example: Checking whether an array contains negative elements:
@code{.c}
    bool has_negative_elements = exists(k, arr_length, arr[k] < 0);
@endcode
 */
#define exists(i, length, condition) ({\
   bool _exists_result = false;\
   for (int i = 0; i < length; i++) { if (condition) { _exists_result = true; break; } }\
   _exists_result;\
})

/**
Checks whether the given condition is true for at least one step of an
iteration. Primarily for use in assertions, preconditions, and postconditions.

Example: Checking whether an array contains negative elements:
@code{.c}
    bool has_negative_elements = exists_x(int i = 0, i < arr_length, i++, arr[i] < 0);
@endcode
 */
#define exists_x(init, has_more_steps, do_step, condition) ({\
   bool _exists_result = false;\
   for (init; has_more_steps; do_step) { if (condition) { _exists_result = true; break; } }\
   _exists_result;\
})



#define panic(message) {\
    fprintf(stderr, "%s:%d, %s: %s\n", __FILE__, __LINE__, __func__, message);\
    exit(EXIT_FAILURE);\
}

#define panicf(...) {\
    fprintf(stderr, "%s:%d, %s: ", __FILE__, __LINE__, __func__);\
    fprintf(stderr, __VA_ARGS__);\
    fprintf(stderr, "\n");\
    exit(EXIT_FAILURE);\
}

#define panic_if(condition, message) \
if (condition) {\
    fprintf(stderr, "%s:%d, %s: %s\n", __FILE__, __LINE__, __func__, message);\
    exit(EXIT_FAILURE);\
}

#define panicf_if(condition, ...) \
if (condition) {\
    fprintf(stderr, "%s:%d, %s: ", __FILE__, __LINE__, __func__);\
    fprintf(stderr, __VA_ARGS__);\
    fprintf(stderr, "\n");\
    exit(EXIT_FAILURE);\
}

#define exit_if(condition, ...) \
if (condition) {\
    fprintf(stderr, __VA_ARGS__);\
    fprintf(stderr, "\n");\
    exit(EXIT_FAILURE);\
}



#define xcalloc(count, size) ({\
   void* result = calloc(count, size);\
    if (result == NULL) {\
        panic("Cannot allocate memory.");\
    }\
   result;\
})

#define xmalloc(size) ({\
   void* result = malloc(size);\
    if (result == NULL) {\
        panic("Cannot allocate memory.");\
    }\
   result;\
})



/** 
Checks whether the actual int (first argument) is equal to the expected int
(second argument).
*/
#define test_equal_i(a, e) base_test_equal_i(__FILE__, __LINE__, a, e)

/** 
Checks whether the actual value @c a is equal too the expected value @c e.
*/
bool base_test_equal_i(const char *file, int line, int a, int e);

/** 
Checks whether the actual String (first argument) is equal to the expected
char* (second argument).
*/
#define test_equal_s(a, e) base_test_equal_s(__FILE__, __LINE__, a, e)

/** Checks whether the actual value @c a is equal to the expected value @c e. */
bool base_test_equal_s(const char *file, int line, String a, char* e);

// Debugging

#define PL printf("%s:%d\n", __func__, __LINE__)
#define PLi(i) printf("%s:%d: %d\n", __func__, __LINE__, i)
#define PLs(s) printf("%s:%d: %s\n", __func__, __LINE__, s)

#endif // util_h_INCLUDED
