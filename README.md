# Rust Calculator

This calculator project is a versatile expression evaluator that supports parsing and evaluating simple mathematical expressions, postfix expressions, and equations with a single variable.
The project is structured to handle different types of expressions with clarity and extendability in mind.

## Features

- **Expression Parsing**: Parses both infix (standard notation) and postfix (Reverse Polish Notation) mathematical expressions.
- **Equation Solving**: Solves simple linear equations with one variable.
- **Error Handling**: Comprehensive error handling for various scenarios like division by zero, parse errors, unexpected tokens, etc.
- **AST Generation**: Generates an Abstract Syntax Tree (AST) for more accurate evaluation of complex expressions.
- **Tokenization**: Lexical analysis to convert input strings into meaningful tokens for parsing.

## Components

- **Lexer (`lex`)**: Converts a string input into a sequence of tokens representing numbers, operators, parentheses, and variables.
- **Parser (`parse_expression`)**: Parses tokens to generate an AST based on the precedence and associativity of operators.
- **Evaluator (`evaluate_infix`, `evaluate_postfix`)**: Evaluates expressions represented as AST or in postfix notation.
- **Error Handling (`CalculatorError`)**: Enumerates possible errors that might occur during lexing, parsing, and evaluating expressions.

