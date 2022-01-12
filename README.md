# Oberon0-DebracedC

## Debraced-C port of Oberon-0

This project is a translation of Niklaus Wirth's Oberon-0 compiler as presented in [1] into Debraced-C. Oberon-0 is a simplified variant of Oberon with just the primitive types INTEGER and BOOLEAN and without function procedures. It only compiles a single module. However, Oberon-0 has procedures, arrays and records.

Debraced C is C with optional braces and with automatic generation of header files. The programmer writes code with optional braces in a .d.c (debraced C) file. Public items are marked with a `*` at the beginning of the line. Public items are expored from the translation unit and included in the module's header file.

The tool [embrace](https://github.com/mirohs/embrace) reintroduces braces into the debraced C file and the tool [headify](https://github.com/mirohs/headify) automatically generates header files. Both tools have to be available to compile this project.

[1] [Oberon0 Compiler, Niklaus Wirth, Compiler Construction, 2005](https://people.inf.ethz.ch/wirth/CompilerConstruction/)

## Installation

```sh
git clone https://github.com/mirohs/Oberon0-DebracedC.git
git clone https://github.com/mirohs/embrace.git
git clone https://github.com/mirohs/headify.git
cd embrace
make
cd ../headify
make
cd ../Oberon0-DebracedC/src
make
./parser ../examples/QuickSort.Mod
```

