CFLAGS = -std=c99 -Wall -Wno-unused-function -Wno-unused-variable -Werror -Wpointer-arith -Wfatal-errors
DEBUG = -g

# dependency chain:
# exe <-- o*       link
# o <-- c + h*     compile
# c <-- h*         preprocessor
# c + h <-- h.c     headify         create header and implementation files
# h.c <-- d.c       embrace         create C code with braces {...}

# disable default suffixes
.SUFFIXES:

oberon0: oberon0.o parser.o generator.o scanner.o risc.o util.o
	gcc $(CFLAGS) $(DEBUG) $^ -lm -o $@

parser: parser.o generator.o scanner.o risc.o util.o
	gcc $(CFLAGS) $(DEBUG) $^ -lm -o $@

risc: risc.o util.o
	gcc $(CFLAGS) $(DEBUG) $^ -lm -o $@

scanner: scanner.o util.o
	gcc $(CFLAGS) $(DEBUG) $^ -lm -o $@

to_postfix: to_postfix.o scanner.o util.o
	gcc $(CFLAGS) $(DEBUG) $^ -lm -o $@

evaluate: evaluate.o scanner.o util.o
	gcc $(CFLAGS) $(DEBUG) $^ -lm -o $@

booleval: booleval.o scanner.o util.o
	gcc $(CFLAGS) $(DEBUG) $^ -lm -o $@

# create C code file with braces {...}
%.h.c: %.d.c
	../../embrace/embrace $< > $@

# create header and implementation files
%.c %.h: %.h.c
	../../headify/headify $<

# pattern rule for compiling .c-file to executable
%: %.o util.o
	gcc $(CFLAGS) $(DEBUG) $^ -lm -o $@

# create object file from C file
%.o: %.c
	gcc -c $(CFLAGS) $(DEBUG) $<

# create dependency files (listing the header files that a C file depends on)
%.d: %.c
	@echo "$@ \\" >$@; \
	gcc -MM $(CFLAGS) $(DEBUG) $< >>$@

-include parser.d generator.d scanner.d risc.d util.d

# do not treat "clean" as a file name
.PHONY: clean 

# remove produced files, invoke as "make clean"
clean: 
	rm -f *.o
	rm -f *.d
	rm -rf .DS_Store
	rm -rf *.dSYM
	rm -f risc.[cho] risc
	rm -f scanner.[ch] scanner
	rm -f generator.[ch] generator
	rm -f parser.[ch] parser
	rm -f oberon0.[ch] oberon0
	rm -f booleval.[ch] booleval
