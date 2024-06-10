## Getting Started

### Interpreter Commands
The cauldron binary ships with several subcommands for working with cauldron code:
- `run` - Evaluates a `cld` file by passing it as the first argument to the `run` subcommand, e.g. `cauldron run <path_to_file>`
- `lex` - Lexes a `cld` file and prints the tokens, e.g. `cauldron lex <path_to_file>`
- `parse` - Parses a `cld` file and prints the AST, e.g. `cauldron parse <path_to_file>`
- `rlpl` - Reads a line from the prompt into the lexer, prints the corresponding tokens, and repeat, e.g. `cauldron rlpl`
- `rppl` - Reads a line from the prompt into the parser, prints the corresponding tokens, and repeat, e.g. `cauldron rppl`
- `repl` - Reads a line from the prompt into the evaluator, prints the result, and repeat, e.g. `cauldron repl`

For finer grained immediate feedback the interpreter comes with multiple read print loops:
1. Read Print Lex Loop - Print tokenized form of 


## Language Overview
Cauldron is a programming language focusing on simplicity and consistency

![Logo](https://raw.githubusercontent.com/cauldron-lang/cauldron/1c99a4ee3e606632d11bc084b1dea7d8656443bf/cauldron_logo.png)

### Comments
The `#` character is used to denote the start of a comment and ends at the end of the line.

### Variables
Labeled buckets that can be used to hold values of primitive types. Variables must only include the following characters `^[a-z0-9_]+$`.

#### Assignment
Variables are declared and assigned a value via the assignment operator: `:=`. This operator only works on variables that haven't already been declared. To re-assign a value to a variable use the re-assignment operator of `=`. This distinction helps to prevent accidental overwriting of variables.
```
# Allowed
foo := "bar" # "bar" can be assigned to foo since foo hasn't been assigned a variable yet
foo = "foobar" # foo can now be set to something else since it was previously declared 

# NOT Allowed
bar = "fail" # bar was never declared so this will fail
foo := "newvalue" # foo was declared earlier and cannot be re-declared
```

### Primitive Types
These are the atomic building blocks of Cauldron programs.

#### Collections
Collection types share some common interfaces:
- Collections can be accessed via the access operator similar to other languages
```
foo := "foo"
foo[0] == "f"

bar := %[fizz: "buzz"]
bar["fizz"] == "buzz"
```

The collection types are as follows:

##### Strings
- Strings are encapsulated within double quotes
```
foo := "bar";
```

##### Vectors
- Vectors are ordered collections of any number of elements of a homogenous type. They can be created using the vector literal square brackets.
```
integers := [1, 2, 3];
```

##### Maps
Maps are a labeled group of related data keyed by property. Properties are accessible via the `.` operator. Creating a map can be done using the following literal:
```
map := %[foo: "bar", fizz: "buzz"]
```

#### Functions
Functions are subroutines executed in their own scope. Functions can be called with arguments by suffixing them, or the variable they're assigned, within parentheses. Functions are closures enclosing over their parent scope(s). They are lexically scoped. The final expression or statement in a block determines the return value. An explicit return will cause the block to end early and return the specified value.

```
factorial := fn(a) {
  if (a <= 1) {
    return 1;
  } else {
    return factorial(a - 1);
  }
};

factorial(7);
```
- Can be passed as arguments to other functions
```
apply := fn(f, a) { f(a) };
apply(add5, 5) == 10;
```
- Functions can be passed anonymously
```
nums := map([1, 2, 3], fn(a) { a + 1 });
```

#### Algebraic Data Types
ADTs are constructed from one of two possible composite types: product and sum types. Product types are a set of labeled fields that have an AND relationship with each other, e.g. `Car(color, make, model)`. Sum types can be used to represent entities with an OR relationship, e.g. `Car(color, make, model) | Bicycle(color, manufacturer, model, type)`. The name of the type comes after the `adt` keyword and is primarily used for improved error messaging.

- ADTs are declared using the keyword `adt` followed by a scope where each line should start with a pipe `|` character followed by the structure declaration:
```
adt {
  | Car(color, make, model)
  | Bicycle(color, model, manufacturer, type)
  | Skateboard
}

courier = Bicycle("red", "Bike Maker LLC", "RB1", "road")
courier["color"] == "red"
courier["model"] == "RB1"
pedestrian := Skateboard()
```

Data constructors in ADTs are capitalized to disambiguate from function calls in the language. This is be done to make it easier to distinguish in a destructuring context. For example, destructuring into a variable like `color` could contain any primitive value since it's a variable whereas destructuring into a sum type variant like `Color` should match the literal `Color`.

### Control Flow
- Conditionals must use `if` and optionally `else` keywords
```
if(1 == 1) {
  print("Equality works!");
} else {
  print("Equality is broken :(");
}
```
- Matching on values with destructuring support
```
match(maybe_value) {
  Some(value) -> print(value)
  None        -> print("No value found!")
}
```
- Looping construct
```
i := 0

while (i < 2) {
  i = i + 1
}
```

### Modules
- The module system is namespace and directory based. Example program structure
```
- bin
-- main.cld
- lib
-- math.cld
```
- Exporting is performed by passing variables to the builtin `export` function. Calling `export` multiple times in a file will overwrite the exported value. Contents of `lib/math.cld`:
```
add := fn(a, b) { a + b};
subtract := fn(a, b) { a - b };

export({ add: add, subtract: subtract });
```
- The return value of the `import` function is always the argument passed to the corresponding files `export`. Relative import of `lib/math.cld` from within `bin/main.cld`:
```
math := import("../lib/math");

print(math.add(1, 1));
```
- Modules can also be imported and exported prefixed by namespaces. As an example, Cauldron ships with many built-in functions that can be accessed at the "bifs" namespace, e.g. `io = import("bifs:io");`. Currently only the `"bifs"` namespace exists.

### BIFs: Built-in Functions
Cauldron has several BIF modules at various levels of abstraction to provide utilities to the language. Modules imported from the BIF do not require a relative or absolute file path.

#### io
The `bifs:io` module provides a high level abstraction over program input and output. Some examples include:
- Import the `bifs:io` module. 
```
io := import("bifs:io");
```
- Write to stdout
```
io.print("Hello World!");
```

#### Formatting
- Cauldron comes with a builtin formatting tool called `cauldron_fmt`

## Resources
- [Pyret Programming Language](https://www.pyret.org/)