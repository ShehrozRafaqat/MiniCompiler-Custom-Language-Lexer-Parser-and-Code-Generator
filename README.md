<!-- PROJECT TITLE -->
<h1 align="center">MiniCompiler: Custom Language Lexer, Parser, and Code Generator</h1>

<!-- HEADER -->
<p align="center">
  <img src="Images/Compiler_Header.jpg"/>
</p>

<!-- PROJECT DESCRIPTION -->
## <br>**➒ Project Description**
**MiniCompiler** is a project designed to emulate the basic functionality of a compiler. It consists of three main components:
- A **lexer** to tokenize input code into meaningful units.
- A **parser** to analyze the syntax of tokenized input and build an abstract syntax tree (AST).
- A **code generator** to produce simplified assembly-like code.

This project supports:
- Keywords: `float`, `double`, `string`, `char`, `switch`, `case`, `print`, `loops` (`while`, `for`), `functions`.
- Logical operators: AND (`&&`), OR (`||`), NOT (`!`).
- Increment (`++`) and decrement (`--`) operations.
- Assembly code generation.

<!-- PREREQUISITES -->
## <br>**➒ Prerequisites**
To run this project, ensure the following dependencies are installed:

* <a href="https://gcc.gnu.org/" target="_blank">GCC</a> (for C++ compilation)
* A modern C++ compiler supporting C++11 or later

<!-- THE FILES -->
## <br>**➒ Project Files**
1. **lex.yy.c** - The output of the lexer, responsible for tokenizing the input.
2. **lexical.cpp** - Implements lexical analysis, defining tokens and keywords.
3. **comp.cpp** - Manages parsing and code generation based on tokens.

<!-- FEATURES -->
## <br>**➒ Features Implemented**
This project supports the following features:

1. **Keywords**
   - The lexer recognizes keywords such as `switch`, `case`, `float`, `print`, etc.
   - Example location in code: `lexical.cpp`, under `isalpha(current)` block for `switch` and `case`.

2. **Logical Operators**
   - Supports `&&`, `||`, and `!` operators.
   - Example location in code: `lexical.cpp`, under the `current == '&'` and `current == '|'` blocks.

3. **Increment/Decrement**
   - Handles `++` and `--` operations.
   - Example location in code: `lexical.cpp`, under the `current == '+'` and `current == '-'` blocks.

4. **Loops and Functions**
   - Recognizes `while`, `for`, and function-related keywords.
   - Example location in code: Defined under `isalpha(current)`.

5. **Assembly Code Generation**
   - Outputs simplified assembly-like code based on the parsed input.
   - Example location in code: `comp.cpp` under code generation logic.

<!-- INSTALLATION -->
## <br>**➒ Installation**
1. Clone the repo:
   ```sh
   git clone https://github.com/ShehrozRafaqat/MiniCompiler-Custom-Language-Lexer-Parser-and-Code-Generator.git
   ```
2. Compile the files:
   ```sh
   g++ -o mini_compiler lexical.cpp comp.cpp lex.yy.c
   ```
3. Run the compiler:
   ```sh
   ./mini_compiler
   ```

<!-- OUTPUT -->
## <br>**➒ Output**
After running the compiler, it processes input source code and produces:

1. **Tokenized Output**
   Displays tokens identified by the lexer.

2. **Parsed Syntax Tree**
   Constructs an abstract syntax tree for valid input.

3. **Assembly Code**
   Generates assembly-like instructions for valid code.

<!-- REFERENCES -->
## <br>**➒ References**
For further understanding of compilers and the concepts used:
1. Compiler Design Overview: https://www.geeksforgeeks.org/compiler-design-tutorials/
2. Lexical Analysis: https://en.wikipedia.org/wiki/Lexical_analysis
3. Parser Design: https://en.wikipedia.org/wiki/Parsing
4. Assembly Code Basics: https://en.wikipedia.org/wiki/Assembly_language

<!-- CONTACT -->
## <br>**➒ Contact**
- E-mail   : [shehrozrafaqat9@gmail.com](mailto:shehrozrafaqat9@gmail.com)
- LinkedIn : https://www.linkedin.com/in/shehroz-rafaqat-16a827265/
