# GLSL parser

This is a simple GLSL parser written in Rust.

The goal is to support any syntax that is valid according to the WebGL 1
specification.

## Usage

The lexer takes a source string as input, and returns a `Vec` of `Token`s as
output. The source string is represented as a `&[char]` rather than a `&str`.
This allows identifiers to be represented as spans in the source string, and
span lookup to be O(1). The parser takes a slice of `&[Token]` as input, and 
returns an AST as output.

```
    let chars = SOURCE.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.read_tokens().unwrap();
    let mut parser = Parser::new(&tokens);
    parser.parse().unwrap();
```

## Todo

* Maintain a symbol table during parsing
* Check left-hand-side in assignment expressions
* Support the `invariant` keyword