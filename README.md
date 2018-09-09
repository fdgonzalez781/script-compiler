# script-compiler

A Haskell compiler (interpreter?) for the untyped lambda calculus.

## Syntax

There are three different kinds of terms in the untyped lambda calculus: variables, lambda abstractions, and applications. A term enclosed in parentheses is also a valid term.

A statement is either a term by itself or a definition. All statements must end with a semicolon. 

### Variables

As is common, variables are denoted by a name, which is a string of characters. In this case, variable names can only be alphabetical characters. Numbers are not allowed in variable names.

Some examples:

```
x;
abc;
functionTwo;
```

### Lambda abstractions

Lambda expressions have the form `lambda [var]. [body]`, where `[var]` is a variable and `[body]` is any valid term.

Some examples:

```
lambda x. x;
lambda f. lambda x. f;
```

### Applications

An application is denoted by two terms separated by a space. Applications are not limited to two variables - in fact, applications can be any number of terms, and the terms can be as complex as desired. By default, applications are left-associative.

Some examples:

```
f x;
(lambda x. x) y;
(lambda f. lambda x. f x) (lambda x. x);
```

### Definitions

A definition is a statement that binds a term to an identifier. All definitions have the form `[name] := [term]`, where `[name]` is a string identifier and `[term]` is a valid lambda term. The rules for the validity of `[name]` are the same as the rules for variable names.

Some examples:

```
id := lambda x. x;
const := lambda x. lambda y. x;
returnThree := const 3; -- This would theoretically work as expected, however for the sake of purity, integer literals are unsupported. It may be possible (and interesting) to translate integer literals to Church numerals.
fix := lambda f. (lambda x. f (x x)) (lambda x. f (x x));
```

Interesting note: While `fix` passes all three stages of compilation, this expression fails type inference in Haskell. In fact, as written, `fix` is an instance of the Y combinator, which is not well-typed in Haskell. This is because we are translating the untyped lambda calculus to a strongly typed language, Haskell.

## Compilation

Lambda expressions are compiled using three processes: the lexer, the parser, and the code generator.
The lexer translates the input string to tokens, which comprise the terminals of the language.
The parser then translates those tokens to an abstract syntax tree based on rules of the grammar. The intended behavior of the grammar rules are as specified above.
The code generator then traverses the syntax tree provided by the parser and generates valid Haskell for each statement and expression.

One interesting note about this compiler is that each of the tree stages of the compiler are each a single function, each comprised of several smaller functions, and the compiler is simply a composition of those three functions. This structure allowed for incredibly simple and flexible testing. For example, if I needed to test how an expression was being parsed, I could create a function `test = runParser . runLexer` and pass a string to that function, which would immediately give me the generated syntax tree.

## Testing

To test the compiler with expressions, simply run the program and input a valid sequence of statements. The program will print the generated Haskell. To exit, simply input `quit`.

To test the compiler with a file, open the project in GHCi by running `ghci app/Main.hs`, then run `testCompilation` once the module is loaded in GHCi. This will compile the file located at `src/script.txt` to `src/script.hs`. Then, provided that compilation succeeded and the program is well-typed, run `ghci src/script.hs` and the compiled file should load in GHCi.
