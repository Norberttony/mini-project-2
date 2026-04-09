# Mini Project 2
High-level overview:
- S-Exp ---parser---> Exp (AST) ---desugarer---> Exp (core language) ---interpreter---> Value (runtime value)

- A parser takes an S-Exp (string expression) and converts it into the AST (Abstract Syntax Tree) representation
- A desugarer takes the AST (fancy sugar syntax form) and converts it into another AST without the sugar syntax.
- An interpreter takes the core language form (desugared language) and converts it into a runtime Value.

## Assignment Reminders
- We will not be dealing with REAL objects. REAL objects are resolved during runtime (dynamically). Instead, we'll be adding to the desugarer to convert our objects into functions and encapsulated variables.
- We will be representing method names using Symbols. This is by far the easiest.
