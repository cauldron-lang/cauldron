## Overview
Cauldron is a programming language focusing on simplicity and consistency

### Planned Features
- Minimal syntax
- Tooling included (formatter, repl, etc)
- No nulls!
- Strong static typing with inference

### Primitives
These are the atomic building blocks of Cauldron programs

#### Variables
- Named identifiers for holding other primitives. Names must only include the following characters `a-zA-Z0-9_?`. They are immutable by default and cannot be rebound once created but can be shadowed in nested scopes.

#### Blocks
Blocks are subroutines executed in their own scope. Blocks can be called with arguments by suffixing them, or the variable they're assigned, with parentheses.

Blocks have the following features:
- Closures
- Coroutines
- Accept arguments
- Contain their own lexical scope
- Blocks optionally return a value when called the following ways
	- Can explicitly return with `return x`
	- Blocks containing a single expression will always return the result of that expression
- Once called all declared variables in scope as properties
- When the first parameter name of a block is `self` it always contains a reference to the  current running block. This can be used for recursive calls.
```
factorial = fn(self, a) {
  if (a <= 1) {
    return 1;
  } else {
    return self(a - 1);
  }
};

factorial(7);
```
- Can be passed as arguments to other blocks
```
apply = fn(f, a) { f(a) };
apply(add5, 5) == 10;
```
- blocks can be passed anonymously
```
nums = map([1, 2, 3], fn(a) { a + 1 });
```
- blocks are coroutines and are also the concurrency primitive of Cauldron. Similar to Wren's Fibers or Lua's Coroutines with the ability to jump to other coroutines by name. 
	- References 
		- [Coroutines in C](https://www.chiark.greenend.org.uk/~sgtatham/coroutines.html)
		- [Coroutines Wikipedia](https://en.m.wikipedia.org/wiki/Coroutine)
		- [Coroutines in Wren](https://wren.io/concurrency.html)
- Blocks can use `block.yield`  to yield execution back to the caller. This allows for creation of commonly used patterns like generators:
```
block = import("block");
fetch_item = fn(self) {
  pages = [
    "http://example.com/page1",
    "http://example.com/page2",
    "http://example.com/page3"
  ];
  
  each(pages, fn(page) {
    const items = parse(page);

    each(items, fn(item) {
      if (valid(item)) {
        block.yield(
          {
            name = clean(item.name);
            description = clean(item.description);
          }
        );
      }
    });
  });
};

items = [];

while(!finished?(fetch_item)) {
  push(items, fetch_item());
}
```
- blocks can also transfer control to other coroutines declared in the same scope
```
block = import("block");
pinger = fn(self, ponger) { 
  print("ping");
  block.yield_to(ponger);
};
ponger = fn(self) {
  print("pong");
  block.yield_to(pinger);
};

pinger(ponger);
```
- blocks can also be used to create structures for holding data in conjunction with `struct`. The block passed to `struct` will have all variables declared in scope set as properties on the returned struct.
```
coordinates = struct({
  x = 10;
  y = 100;
  z = 1000;
});

coordinates.x == 10;
coordinates.y == 100;
coordinates.z == 1000;
```

#### Data
- Strings are encapsulated within double quotes
```
foo = "bar";
```
- Lists are ordered collections of any number of elements of a homogenous type. They can be created using the list literal square brackets.
```
list = [1, 2, 3];
```
- Structs are a set of related data organized onto the same data structure. Struct properties are accessible via the `.` operator. They are created with `struct`.

**Modules**
- The module system is simple and file based. Example program structure
```
- bin
-- main.cld
- lib
-- math.cld
```
- Exporting is performed by passing a block to the builtin `export` procedure. Calling `export` multiple times in a file is an error. Contents of `lib/math.cld`:
```
export({
   add = fn(a, b) { a + b};
});
```
- The return value of the `import` function is always identical to the block passed to `export`. Relative import of `lib/math.cld` from within `bin/main.cld`:
```
math = import("../lib/math.cld");

print(math.add(1, 1));
```

**Control Flow**
- Conditionals must use `if` and optionally `else` keywords
```
if(1 == 1) {
  print("Equality works!");
} else {
  print("Equality is broken :(");
}
```

### Standard Library
Cauldron has several standard library modules at various levels of abstraction to provide utilities to the language. Modules imported from the standard library do not require a relative or absolute file path.

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

**Formatting**
- Cauldron comes with a builtin formatting tool called `cauldron_fmt`
