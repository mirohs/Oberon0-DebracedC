/*
@author: Michael Rohs
@date: November 28, 2021
*/

#include "util.h"

///////////////////////////////////////////////////////////////////////////////
// Strings

/*
String is shallow abstraction over C strings and C char arrays. String allows
keeping track of a starting point in memory, a length and a capacity (0 <=
length <= capacity). The explicit length allows dealing with parts of
('\0'-terminated) C strings.

The underlying buffer may or may not be dynamically
allocated. The xappend... ("extensible append") functions dynamically reallocate
memory if necessary. Therefore, when using these functinos, the buffer must
point to the beginning of a dynamically allocated memory block.
*/

String make_string(char* s) {
    require_not_null(s);
    int len = strlen(s);
    return (String) {s, len, len};
}

String make_string2(char* s, int len) {
    require_not_null(s);
    require("length not negative", len >= 0);
    return (String) {s, len, len};
}

String make_string3(char* s, int len, int cap) {
    require_not_null(s);
    require("length not negative", len >= 0);
    require("capacity not negative", cap >= 0);
    require("length does not exceed capacity", len <= cap);
    return (String) {s, len, cap};
}

String new_string(int cap) {
    require("capacity not negative", cap >= 0);
    return (String) {xmalloc(cap), 0, cap};
}

/*
Appends t to str. Fails if the result would not fit into str.
*/
void append_string(String* str, String t) {
    require_not_null(str);
    int n = str->len + t.len;
    panic_if(n > str->cap, "append_string overflow");
    memcpy(str->s + str->len, t.s, t.len);
    str->len = n;
}

/*
Appends t to str. Fails if the result would not fit into str.
*/
void append_cstring(String* str, char* t) {
    require_not_null(str);
    require_not_null(t);
    int t_len = strlen(t);
    int n = str->len + t_len;
    panic_if(n > str->cap, "append_cstring overflow");
    memcpy(str->s + str->len, t, t_len);
    str->len = n;
}

/*
Appends the chars from s (inclusive) to t (exclusive) to str. Fails if the
result would not fit into str.
*/
void append_cstring2(String* str, char* s, char* t) {
    require_not_null(str);
    require_not_null(s);
    require_not_null(t);
    require("t not before s", s <= t);
    int st_len = t - s;
    int n = str->len + st_len;
    panic_if(n > str->cap, "append_cstring overflow");
    memcpy(str->s + str->len, s, st_len);
    str->len = n;
}

/*
Appends c to str. Fails if the result would not fit into str.
*/
void append_char(String* str, char c) {
    require_not_null(str);
    panic_if(str->len >= str->cap, "append_char overflow");
    str->s[str->len] = c;
    str->len++;
}

/*
Appends t to str. Extends the underlying buffer if the capacity is exhausted.
Thus str->s must point to the beginning of a dynamically allocated memory block.
*/
void xappend_string(String* str, String t) {
    require_not_null(str);
    int n = str->len + t.len;
    if (n > str->cap) {
        char* s = xmalloc(2 * n);
        memcpy(s, str->s, str->len);
        free(str->s);
        str->s = s;
        str->cap = 2 * n;
    }
    memcpy(str->s + str->len, t.s, t.len);
    str->len = n;
}

/*
Appends t to str. Extends the underlying buffer if the capacity is exhausted.
Thus str->s must point to the beginning of a dynamically allocated memory block.
*/
void xappend_cstring(String* str, char* t) {
    require_not_null(str);
    require_not_null(t);
    int t_len = strlen(t);
    int n = str->len + t_len;
    if (n > str->cap) {
        char* s = xmalloc(2 * n);
        memcpy(s, str->s, str->len);
        free(str->s);
        str->s = s;
        str->cap = 2 * n;
    }
    memcpy(str->s + str->len, t, t_len);
    str->len = n;
}

/*
Appends the chars from s (incluisive) to t (exclusive) to str. Extends the
underlying buffer if the capacity is exhausted.  Thus str->s must point to the
beginning of a dynamically allocated memory block.
*/
void xappend_cstring2(String* str, char* s, char* t) {
    require_not_null(str);
    require_not_null(s);
    require_not_null(t);
    require("t not before s", s <= t);
    int st_len = t - s;
    int n = str->len + st_len;
    if (n > str->cap) {
        char* s_new = xmalloc(2 * n);
        memcpy(s_new, str->s, str->len);
        free(str->s);
        str->s = s_new;
        str->cap = 2 * n;
    }
    memcpy(str->s + str->len, s, st_len);
    str->len = n;
}

/*
Appends c to str. Extends the underlying buffer if the capacity is exhausted.
Thus str->s must point to the beginning of a dynamically allocated memory block.
*/
void xappend_char(String* str, char c) {
    require_not_null(str);
    if (str->len >= str->cap) {
        int n = 2 * (str->len + 1);
        char* s = xmalloc(n);
        memcpy(s, str->s, str->len);
        free(str->s);
        str->s = s;
        str->cap = n;
    }
    str->s[str->len] = c;
    str->len++;
}

void append_test(void) {
    String s = new_string(100);
    test_equal_s(s, "");
    append_char(&s, 'x');
    test_equal_s(s, "x");
    append_char(&s, 'y');
    test_equal_s(s, "xy");
    append_cstring(&s, "abc");
    test_equal_s(s, "xyabc");
    append_string(&s, make_string("hello"));
    test_equal_s(s, "xyabchello");
    test_equal_i(s.len, 10);
    test_equal_i(s.cap, 100);
    free(s.s);

    s = make_string("abc");
    // would oveflow: b = append_char(&s, 'x');
    test_equal_s(s, "abc");
}

void xappend_test(void) {
    String s = new_string(1);
    xappend_char(&s, 'x');
    printf("%d %d\n", s.len, s.cap);
    test_equal_s(s, "x");
    xappend_char(&s, 'y');
    printf("%d %d\n", s.len, s.cap);
    test_equal_s(s, "xy");
    xappend_cstring(&s, "abc");
    printf("%d %d\n", s.len, s.cap);
    test_equal_s(s, "xyabc");
    xappend_string(&s, make_string("hello"));
    printf("%d %d\n", s.len, s.cap);
    test_equal_s(s, "xyabchello");
    test_equal_i(s.len, 10);
    free(s.s);
}

void print_string(String str) {
    printf("%.*s", str.len, str.s);
}

void println_string(String str) {
    printf("%.*s\n", str.len, str.s);
}

/*
Removes spaces and tabs from the beginning and end of str. The underlying
content is not modified, but the String is appropriately shifted and the length
adapted.
*/
String trim(String str) {
    int left = 0, right = str.len - 1;
    for (; left < str.len && (str.s[left] == ' ' || str.s[left] == '\t' ); left++);
    // 0 <= left <= str.len
    for (; right >= left && (str.s[right] == ' ' || str.s[right] == '\t' ); right--);
    // -1 <= left - 1 <= right <= str.len - 1
    int len = right - left + 1;
    // 0 <= right - left + 1 <= str.len - left
    // 0 <= len <= str.len - left <= str.len
    assert("not negative", len >= 0);
    assert("not larger", len <= str.len);
    return (String){str.s + left, len, str.cap - left};
}

void trim_test(void) {
    test_equal_s(trim(make_string("")), "");
    test_equal_s(trim(make_string(" ")), "");
    test_equal_s(trim(make_string("  \t\t \t  ")), "");
    test_equal_s(trim(make_string("a")), "a");
    test_equal_s(trim(make_string("a ")), "a");
    test_equal_s(trim(make_string(" a")), "a");
    test_equal_s(trim(make_string("a \t")), "a");
    test_equal_s(trim(make_string("\t a")), "a");
    test_equal_s(trim(make_string("\t a \t")), "a");
    test_equal_s(trim(make_string("abc")), "abc");
    test_equal_s(trim(make_string("a b c")), "a b c");
    test_equal_s(trim(make_string("   a b c ")), "a b c");
}

/*
Removes spaces and tabs from the beginning of str. The underlying content is not
modified, but the String is appropriately shifted and the length adapted.
*/
String trim_left(String str) {
    int left = 0;
    for (; left < str.len && (str.s[left] == ' ' || str.s[left] == '\t' ); left++);
    // 0 <= left <= str.len
    int len = str.len - left;
    // 0 <= len <= str.len
    assert("not negative", len >= 0);
    assert("not larger", len <= str.len);
    // capacity only shrinks from left
    return (String){str.s + left, len, str.cap - left};
}

void trim_left_test(void) {
    test_equal_s(trim_left(make_string("")), "");
    test_equal_s(trim_left(make_string(" ")), "");
    test_equal_s(trim_left(make_string("  \t\t \t  ")), "");
    test_equal_s(trim_left(make_string("a")), "a");
    test_equal_s(trim_left(make_string("a ")), "a ");
    test_equal_s(trim_left(make_string(" a")), "a");
    test_equal_s(trim_left(make_string("a \t")), "a \t");
    test_equal_s(trim_left(make_string("\t a")), "a");
    test_equal_s(trim_left(make_string("\t a \t")), "a \t");
    test_equal_s(trim_left(make_string("abc")), "abc");
    test_equal_s(trim_left(make_string("a b c")), "a b c");
    test_equal_s(trim_left(make_string("   a b c ")), "a b c ");
}

/*
Removes spaces and tabs from the end of str. The underlying content is not
modified, but the String is appropriately shifted and the length adapted.
*/
String trim_right(String str) {
    int right = str.len - 1;
    for (; right >= 0 && (str.s[right] == ' ' || str.s[right] == '\t' ); right--);
    // -1 <= right <= str.len - 1
    int len = right + 1;
    // 0 <= right <= str.len
    assert("not negative", len >= 0);
    assert("not larger", len <= str.len);
    // capacity does not change when trimming right
    return (String){str.s, len, str.cap};
}

void trim_right_test(void) {
    test_equal_s(trim_right(make_string("")), "");
    test_equal_s(trim_right(make_string(" ")), "");
    test_equal_s(trim_right(make_string("  \t\t \t  ")), "");
    test_equal_s(trim_right(make_string("a")), "a");
    test_equal_s(trim_right(make_string("a ")), "a");
    test_equal_s(trim_right(make_string(" a")), " a");
    test_equal_s(trim_right(make_string("a \t")), "a");
    test_equal_s(trim_right(make_string("\t a")), "\t a");
    test_equal_s(trim_right(make_string("\t a \t")), "\t a");
    test_equal_s(trim_right(make_string("abc")), "abc");
    test_equal_s(trim_right(make_string("a b c")), "a b c");
    test_equal_s(trim_right(make_string("   a b c ")), "   a b c");
}

/*
Returns true iff str contains part.
*/
bool contains(String str, String part) {
    if (str.len < part.len) return false;
    // must use C strings to use strstr, on stack may overflow the stack
    char s[str.len + 1];
    char t[part.len + 1];
    memcpy(s, str.s, str.len);
    memcpy(t, part.s, part.len);
    s[str.len] = '\0';
    t[part.len] = '\0';
    return strstr(s, t) != NULL;
}

bool starts_with(String str, String part) {
    if (str.len < part.len) return false;
    return strncmp(str.s, part.s, part.len) == 0;
}

bool ends_with(String str, String part) {
    if (str.len < part.len) return false;
    return strncmp(str.s + str.len - part.len, part.s, part.len) == 0;
}

/*
Returns the index of part in str or -1 of part does not appear in s.
*/
int index_of(String str, String part) {
    if (str.len < part.len) return -1;
    // must use C strings to use strstr, on stack may overflow the stack
    char s[str.len + 1];
    char t[part.len + 1];
    memcpy(s, str.s, str.len);
    memcpy(t, part.s, part.len);
    s[str.len] = '\0';
    t[part.len] = '\0';
    char* p = strstr(s, t);
    if (p == NULL) return -1;
    return (int)(p - s);
}

void index_of_test(void) {
    test_equal_i(index_of(make_string("abcd"), make_string("ab")), 0);
    test_equal_i(index_of(make_string("abcd"), make_string("bc")), 1);
    test_equal_i(index_of(make_string("abcd"), make_string("cd")), 2);
    test_equal_i(index_of(make_string("abcd"), make_string("bd")), -1);
    test_equal_i(index_of(make_string("abcd"), make_string("x")), -1);
    test_equal_i(index_of(make_string("abcd"), make_string("")), 0);
    test_equal_i(index_of(make_string(""), make_string("a")), -1);
}

int index_of_char(String str, char c) {
    for (int i = 0; i < str.len; i++) {
        if (str.s[i] == c) return i;
    }
    return -1;
}

int last_index_of_char(String str, char c) {
    for (int i = str.len - 1; i >= 0; i--) {
        if (str.s[i] == c) return i;
    }
    return -1;
}

bool cstring_equal(String str, char* t) {
    return strncmp(str.s, t, str.len) == 0 && t[str.len] == '\0';
}

StringNode* new_string_node(String str, StringNode* next) {
    StringNode* node = xcalloc(1, sizeof(StringNode));
    node->str = str;
    node->next = next;
    return node;
}

StringArray* new_string_array(int cap) {
    require("not negative", cap >= 0);
    StringArray* arr = xcalloc(1, sizeof(StringArray) + cap * sizeof(String));
    arr->cap = cap;
    return arr;
}

/**
Reads the contents of a file into a string The function fails if the file does
not exist or cannot be read.
@param[in] name file name (including path)
@return a string that points to a newly allocated char* with data read from file
*/
String read_file(char* name) {
    require_not_null(name);

    // Opening in text mode should remove \r and only leave \n.
    // However, it does not do so on macOS.
    FILE *f = fopen(name, "r"); 
    panicf_if(f == NULL, "Cannot open %s", name);

    fseek (f, 0, SEEK_END);
    long size = ftell(f);
    rewind(f);
    
    char *s = xmalloc(size + 1);
    long sizeRead = fread(s, 1, size, f);
    // assert: size >= sizeRead (> if file contains \r characters)
    // printf("size = %lu, sizeRead = %lu, feof = %d\n", size, sizeRead, feof(f));
    panicf_if(sizeRead < size && feof(f) == 0, "Cannot read %s to end.\n", name); 
    s[sizeRead] = '\0';
    
    fclose(f);
    return make_string2(s, sizeRead);
}

void write_file(char* name, String data) {
    require_not_null(name);

    FILE *f = fopen(name, "w");
    panicf_if(f == NULL, "Cannot open %s", name); 

    size_t n_written = fwrite(data.s, 1, data.len, f);
    fclose(f);
    panicf_if(n_written != data.len, "Cannot write data to file %s.", name); 
}

/*
Splits the string using the given separator character. Does not modify the
content of the argument string.
*/
StringArray* split(char* s, char sep) {
    require_not_null(s);
    char* t = s;
    StringNode* lines = NULL;
    int line_count = 0;
    while (*t) {
        if (*t == sep) {
            lines = new_string_node(make_string2(s, (int)(t - s)), lines);
            s = t + 1;
            line_count++;
        }
        t++;
    }
    // last line
    if (lines != NULL || (lines == NULL && t > s)) {
        lines = new_string_node(make_string2(s, (int)(t - s)), lines);
        line_count++;
    }
    StringArray* arr = new_string_array(line_count);
    arr->len = line_count;
    for (int i = line_count - 1; i >= 0; i--) {
        assert_not_null(lines);
        arr->a[i] = lines->str;
        StringNode* node = lines;
        lines = lines->next;
        free(node);
    }
    assert("list empty", lines == NULL);
    return arr;
}

void split_test(void) {
    // empty string => empty array
    StringArray* a = split("", ' ');
    test_equal_i(a->len, 0);
    free(a);

    // separator => two empty strings
    a = split(" ", ' ');
    test_equal_i(a->len, 2);
    test_equal_i(a->a[0].len, 0);
    test_equal_i(a->a[1].len, 0);
    free(a);

    // a single non-empty line without line ending
    a = split("abc", '\n');
    test_equal_i(a->len, 1);
    test_equal_s(a->a[0], "abc");
    free(a);

    a = split("ab cde", ' ');
    test_equal_i(a->len, 2);
    test_equal_s(a->a[0], "ab");
    test_equal_s(a->a[1], "cde");
    free(a);

    a = split("ab cde ", ' ');
    test_equal_i(a->len, 3);
    test_equal_s(a->a[0], "ab");
    test_equal_s(a->a[1], "cde");
    test_equal_s(a->a[2], "");
    free(a);
}

/*
Splits the string into lines. Does not modify the content of the argument
string. Line separators may be "\n" or "\r\n".
*/
StringArray* split_lines(char* s) {
    require_not_null(s);
    char* t = s;
    StringNode* lines = NULL;
    int line_count = 0;
    while (*t) {
        char c = *t;
        if (c == '\n' || c == '\r') {
            lines = new_string_node(make_string2(s, (int)(t - s)), lines);
            if (c == '\r') t++; // skip carriage return, if needed
            s = t + 1;
            line_count++;
        }
        t++;
    }
    // last line
    if (lines != NULL || (lines == NULL && t > s)) {
        lines = new_string_node(make_string2(s, (int)(t - s)), lines);
        line_count++;
    }
    StringArray* arr = new_string_array(line_count);
    arr->len = line_count;
    for (int i = line_count - 1; i >= 0; i--) {
        assert_not_null(lines);
        arr->a[i] = lines->str;
        StringNode* node = lines;
        lines = lines->next;
        free(node);
    }
    assert("list empty", lines == NULL);
    return arr;
}

void split_lines_test(void) {
    // empty string => empty array
    StringArray* a = split_lines("");
    test_equal_i(a->len, 0);
    free(a);

    // separator => two empty strings
    a = split_lines("\n");
    test_equal_i(a->len, 2);
    test_equal_i(a->a[0].len, 0);
    test_equal_i(a->a[1].len, 0);
    free(a);

    // separator => two empty strings
    a = split_lines("\r\n");
    test_equal_i(a->len, 2);
    test_equal_i(a->a[0].len, 0);
    test_equal_i(a->a[1].len, 0);
    free(a);

    // a single non-empty line without line ending
    a = split_lines("abc");
    test_equal_i(a->len, 1);
    test_equal_s(a->a[0], "abc");
    free(a);

    a = split_lines("ab\ncde");
    test_equal_i(a->len, 2);
    test_equal_s(a->a[0], "ab");
    test_equal_s(a->a[1], "cde");
    free(a);

    a = split_lines("ab\r\ncde");
    test_equal_i(a->len, 2);
    test_equal_s(a->a[0], "ab");
    test_equal_s(a->a[1], "cde");
    free(a);

    a = split_lines("ab\ncde\n");
    test_equal_i(a->len, 3);
    test_equal_s(a->a[0], "ab");
    test_equal_s(a->a[1], "cde");
    test_equal_s(a->a[2], "");
    free(a);

    a = split_lines("ab\r\ncde\r\n");
    test_equal_i(a->len, 3);
    test_equal_s(a->a[0], "ab");
    test_equal_s(a->a[1], "cde");
    test_equal_s(a->a[2], "");
    free(a);
}

///////////////////////////////////////////////////////////////////////////////
// Sets

// Efficient set for integer elements 0..63.
Set make_set(void) {
    return (Set){0};
}

// Checks if x is in s.
bool in(int x, Set s) {
    require("valid range", 0 <= x && x < 64);
    return (s.s >> x) & 1;
}

// Adds x to s.
void incl(Set* s, int x) {
    require("valid range", 0 <= x && x < 64);
    s->s |= (1 << x);
}

// Remooves x from s.
void excl(Set* s, int x) {
    require("valid range", 0 <= x && x < 64);
    s->s &= ~(1 << x);
}

///////////////////////////////////////////////////////////////////////////////
// Testing

static int base_check_count = 0;
static int base_check_success_count = 0;
static bool base_atexit_registered = false;
static int exit_status = EXIT_SUCCESS;

// http://www.gnu.org/software/libc/manual/html_node/Cleanups-on-Exit.html#Cleanups-on-Exit
void base_atexit(void) {
    // if not a successful exit, supress further output
    if (exit_status == EXIT_SUCCESS) {
        // summary information about tests (if any)
        if (base_check_count > 0) {
            int fail_count = base_check_count - base_check_success_count;
            if (fail_count <= 0) {
                if (base_check_count == 1) {
                    fprintf(stderr, "The test passed!\n");
                } else if (base_check_count == 2) {
                    fprintf(stderr, "Both tests passed!\n");
                } else if (base_check_count >= 3) {
                    fprintf(stderr, "All %d tests passed!\n", base_check_count);
                }
            } else {
                if (base_check_count == 1) {
                    fprintf(stderr, "The test failed.\n");
                } else {
                    if (base_check_success_count == 0) {
                        fprintf(stderr, "0 of %d tests passed.\n", base_check_count);
                    } else {
                        fprintf(stderr, "%d of %d tests failed.\n", fail_count, base_check_count);
                    }
                }
            }
        }
    }
}

void base_init(void) {
    if (!base_atexit_registered) {
        atexit(base_atexit);
        base_atexit_registered = true;
    }
}

bool base_test_equal_i(const char *file, int line, int a, int e) {
    base_init();
    base_check_count++;
    if (a == e) {
        printf("%s, line %d: Test passed.\n", file, line);
        base_check_success_count++;
        return true;
    } else {
        printf("%s, line %d: Actual value %d differs from expected value %d.\n", file, line, a, e);
        return false;
    }
}

bool base_test_equal_s(const char *file, int line, String a, char* e) {
    base_init();
    base_check_count++;
    if ((a.s != NULL && e != NULL && strlen(e) == a.len && strncmp(a.s, e, a.len) == 0) || (a.s == NULL && e == NULL)) {
        printf("%s, line %d: Test passed.\n", file, line);
        base_check_success_count++;
        return true;
    } else {
        printf("%s, line %d: Actual value \"%.*s\" differs from expected value \"%s\".\n", file, line, a.len, a.s, e);
        return false;
    }
}

