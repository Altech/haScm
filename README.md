## haScm
haScm is an Interpreter of Scheme written in Haskell.

The Implementation mainly (but loosely) follows "Revised5 Report on the Algorithmic Language Scheme".

### Settings
- It loads `<home>/.hascmrc` when REPL starts.
- `lib/core.scm` and `lib/lib.scm` includes functions provided r5rs.

### Important difference
- List is immutable(deriving Haskell) .
  - So set-car! and set-cdr! is limited.
  - So `eq?`, `eqv?`, `set!` has its own semantics.
- Macro is classic (like Common Lips's one).

#### Built-in Module System
- `(module <definition> ... (export <symbol> ...))`
  - A buit-in macro.
  - Write definitions and export some of them.
  - It is a list of pairs of name and value, actually.
- `(require '<symbol>)`
  - Search directories in 'path and load a file found.
- `(import <module>)`
  - define the exported definitons in the current environment.
