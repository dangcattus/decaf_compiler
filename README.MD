# Decaf Compiler

A decaf compiler written in C for my Principles of Compiler Design course.
http://anoopsarkar.github.io/compilers-class/

for the decaf specification
http://anoopsarkar.github.io/compilers-class/decafspec.html

## Assignment 3 - Code generation for decaf expressions

http://anoopsarkar.github.io/compilers-class/hw3.html

### Getting Started

You must have git and python (2.7) on your system to run the assignments.

This assignments

1. Implements a symbol table that can keep track of variables and methods in Decaf.

2. Code generation for expressions
   a. Arithmetic and Boolean expressions
   b. Function calls
   c. Function definitions
   d. Declaration of extern functions

### To build

```
make
```

will build the decafexpr executable.

### To Run against Testcases

To generate the test. ll provided

```
./decafcomp < test.decaf 2> test.ll
```

The output should be in LLVM assembly which can be compiled to x86 assembly using the LLVM tools and run as a binary. We will use the binary

```
llvm-run
```

in the answer directory to create and run the binary from the Decaf programs in the testcases directory.

The LLVM assembly and toolchain output is dumped into the llvm directory. You should examine your output to debug your compiler. Make sure you obey the following requirements:

If your program succeeds in parsing the input you should exit from your program using exit(EXIT_SUCCESS). And if your program finds an error in the input Decaf program you should exit using exit(EXIT_FAILURE). The definitions of EXIT_SUCCESS and EXIT_FAILURE are in cstdlib (for C++) and in stdlib.h (for C).
You must dump the LLVM assembly by calling TheModule->dump() where TheModule is of type llvm::Module\*.

Run your solution program on the testcases using the Python program zipout.py. Your solution must be compiled in the answer directory and must be called decafexpr. Run against

## Assignment 4 - Decaf compiler

Your task for this homework is to use LLVM for code generation in order to write a fully working compiler, including semantic checks, for the Decaf programming language.

see tasks for this compilers

### To build Decaf Compiler

In assign4 directory, from the command line terminal

```
make
```

will build all necessary binaries to run the decafcomp executable.

A test file, test.decaf is contained to test code.

to run the test program

```
./decafcomp < test.decaf 2> test.ll
```

This will generate test.ll the llvm file.
