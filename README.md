# Lab12H

# bprog - A Stack-Based Concatenative Language Interpreter

bprog is a simple concatenative, stack-based programming language interpreter implemented in Haskell. It features:
- Reverse Polish Notation (postfix) syntax
- Interactive REPL and file interpretation modes
- Strong typing with type coercion
- First-class code blocks (quotations)
- Variable and function definitions

## Usage
### REPL Mode
stack run
bprog> 1 2 + print
3

### File Interpretation
stack run test.txt

## Language Features
### Basic Operations
1 2 +          3 (addition)
10 5 -         5 (subtraction)
3 4 *          12 (multiplication)
20 4 div       5 (integer division)
20 4 /         5.0 (float division)

### Stack Manipulation
10 dup         # [10, 10]
10 20 swap     # [20, 10]
10 20 pop      # [10]

Variables and Functions
bprog
x 10 :=        # Assign 10 to x
x              # 10 (retrieve value)

Function definition
square { dup * } fun
5 square       # 25

### Lists and Blocks
[1 2 3]        Create list
[1 2 3] head   1
[1 2 3] tail   [2, 3]

{ 10 + }       Create code block
5 { 10 + } exec # 15

## Error Handling
bprog provides detailed error messages:

* Parse errors (invalid syntax)
* Runtime errors (stack underflow, type mismatches)
* Program validation (empty/multiple stack results)

## Testing
### Run the test suite with:
stack test