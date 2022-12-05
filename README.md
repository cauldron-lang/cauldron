## Overview
Cauldron is a programming language focusing on simplicity and consistency

### Features
- Minimal syntax
- Tooling included (formatter, repl, etc)
- No nulls!
- Strong static typing with inference
- Consistency

### Primitives
These are the atomic building blocks of Cauldron programs.

#### Variables
Labeled buckets that can be used to hold primitive values. Variables can only include the following characters `^[a-z0-9_]+$`.

#### Functions
Functions are subroutines executed in their own scope. Functions can be called with arguments by suffixing them, or the variable they're assigned, within parentheses. Functions are closures enclosing over their parent scope(s). They are lexically scoped. The final expression or statement in a block determines the return value. An explicit return will cause the block to end early and return the specified value.

```
factorial = fn(a) {
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
apply = fn(f, a) { f(a) };
apply(add5, 5) == 10;
```
- Functions can be passed anonymously
```
nums = map([1, 2, 3], fn(a) { a + 1 });
```

#### Strings
- Strings are encapsulated within double quotes
```
foo = "bar";
```

#### Vectors
- Vectors are ordered collections of any number of elements of a homogenous type. They can be created using the vector literal square brackets.
```
integers = [1, 2, 3];
```

#### Maps
Maps are a labeled group of related data keyed by property. Properties are accessible via the `.` operator. Creating a map can be done using the following literal:
```
{ foo: "bar", fizz: "buzz" }
```

### Control Flow
- Conditionals must use `if` and optionally `else` keywords
```
if(1 == 1) {
  print("Equality works!");
} else {
  print("Equality is broken :(");
}
```

### Modules
- The module system is simple and directory based. Example program structure
```
- bin
-- main.cld
- lib
-- math.cld
```
- Exporting is performed by passing exported variables to the builtin `export` function in a map. Calling `export` multiple times in a file is an error. Contents of `lib/math.cld`:
```
add = fn(a, b) { a + b};
subtract = fn(a, b) { a - b };

export({ add: add, subtract: subtract });
```
- The return value of the `import` function is always a map of the . Relative import of `lib/math.cld` from within `bin/main.cld`:
```
math = import("../lib/math.cld");

print(math.add(1, 1));
```

### BIFs: Built-in Functions
Cauldron has several BIF modules at various levels of abstraction to provide utilities to the language. Modules imported from the BIF do not require a relative or absolute file path.

#### io
The `io` module provides a high level abstraction over program input and output. Some examples include:
- Import the `io` module. 
```
io = import("io");
```
- Write to stdout
```
io.print("Hello World!");
```

#### Formatting
- Cauldron comes with a builtin formatting tool called `cauldron_fmt`
