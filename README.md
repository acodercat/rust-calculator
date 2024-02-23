
# Scientific Calculator

## Overview

This project is a scientific calculator designed to handle both simple arithmetic and complex mathematical functions, including logarithms, trigonometric operations, and solving linear equations. It supports calculations in both infix and postfix (Reverse Polish Notation, RPN) notations and is capable of understanding constants like Pi and Euler's number (e).

## Features

- **Infix and Postfix Notation:** Perform calculations using both standard and RPN formats.
- **Mathematical Functions:** Supports addition, subtraction, multiplication, division, logarithms (log, ln), and trigonometric functions (sin, cos, tan, ctan).
- **Constants Recognition:** Automatically recognizes and calculates expressions involving Pi and e.
- **Equation Solving:** Can solve simple linear equations with one variable.
- **Error Handling:** Gracefully handles errors, providing meaningful feedback to the user.

## Compile and Run

```bash
cargo run
```

## Running Tests

```bash
cargo test
```

## Examples

Here are a few examples of how to use the calculator:

* `(3+(4-1))*5` ➜ `30`
* `2 * x + 0.5 = 1` ➜ `x=0.25`
* `log(10)` ➜ `1`
* `log10` ➜ `1`
* `sin(pi)` ➜ `0`
* `sinpi` ➜ `0`
* `sin(1.5pi)` ➜ `-1`
* `sin(1.5*pi)` ➜ `-1`
* `pi` ➜ `3.14159265`
* `2pi` ➜ `6.28318531`
* `e` ➜ `2.71828183`
* `log100(10)` ➜ `0.5`