# STACK_LANG
STACK_LANG is a dynamically typed imperative toy language whose syntax is inspired by C. It supports integers, booleans, variables, functions, and recursive functions. Sadly, it doesn't support strings, arrays, records, or mutually recursive functions since the purpose of making this language was to prove that it was possible to make a single-pass compiler that can convert any turing complete language into a single chunk of opcodes. The lexer(tokenizer) gets the source code as input and tokenizes it, creating a list of tokens. The compiler parses the list of tokens and converts it into a chunk of opcodes. The chunk is fed into the stack based virtual machine which produces the results. 

## Getting Started
1. Install g++. Make sure it supports c++20 or higher.
2. Downloa the Makefile and main.cpp file from the repo.
3. DEBUG_MODE is set to 1 by default so if you don't want to see the opcodes, stacktrace, and framepointer in action in runtime, change it to 0. You'll be able to find it in main.cpp fairly easly.
4. Execute the Makefile using `make`. This will create a file called 'main'.
5. Download src.st from the repo.
6. Execute `make run` to run the program.

## Alternatively,
- You can modify the Makefile for your needs.
- Or you can run the following in the command line after downloading main.cpp and src.st.
```
g++ -std=c++20 src.st -o main && ./main
```
- If you are in Windows do
```
g++ -std=c++20 src.st -o main >> ./main.exe
```
## Syntax
The syntax is for the most part identical to that of C, so I will only showcase the parts that deviate from it.
### Variable Declaration
Use the `var` keyword to declare variables. You can declare variables without the `var` keyword just like in Python, but this will make them global variables.
```c
var x = 5.2; // Created variable x.
```
### Function Declaration
Use the `func` keyword to declare and define functions.
```c
func add(x, y) // Created function add.
{
    return x + y;
}
```
### Scope Rules
Scope rules are identical to that of C, except that declaring variables without the `var` keyword will make them global.
```c
var x = 50; // x is a global variable
{
    var y = 20; // y is a local variable
    z = 10; // z is a global variable
}
print(z); // prints 10, because z is global.
```
## Resources that helped me
- https://craftinginterpreters.com/
- https://www.youtube.com/watch?v=2vBVvQTTdXg
