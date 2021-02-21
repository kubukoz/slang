# slang

A toy language loosely based on a mix of Scala and Haskell syntax. Built with Scala 3, Cats Effect 3 and Cats Parse.

## Features

None, don't use this!!!!!!! But for real though, if you're curious what I'm doing...

### Programs are expressions

Every Slang program, and source file, is an expression. This means every program and file has a value, and a file's value may be of any type.

Typechecking isn't implemented at the point of writing this, so Slang is basically a dynamic language.

### Top level expressions

A source file contains a list of expressions: function definitions, value assignments (not implemented yet), references, function calls.

This is still to be figured out, but so far the plan is to make the last expression define what the value of a program / file is,
similarly to blocks in Scala. Fun fact, every file's expressions are wrapped in an "implicit block" AST node.

A consequence of this, when time for figuring out how to do effects comes, is that only the last expression is allowed to have effects.
Running effects before the last expression, without assigning them to a value, will probably be a compilation error in the future.
At the very least, a warning (unused code).

### Function definitions are expressions

This is the fun part: function definitions are expressions of type Unit.

> Why bother then? What's the point of having functions as expressions if they're returning Unit?

So that's where the fun begins. A function definition will be syntactic sugar for adding a function to the local scope at compile time.

Metaprogramming in Slang shall be implemented as making a state transition on the expression's enclosing scope. This means that
a function definition is essentially a compile-time effect that modifies the state of the compiler in the region the function is defined,
in a way that creates a new symbol backed by the function's implementation.

This hasn't been tested yet, of course, but if it turns out to be possible, it would allow mixing compile-time code with normal code seamlessly.
First we need to have typechecking though.

### Full type inference

I want the language to have full type inference, think Haskell / Hindley-Milner. Might just implement the HM type system, might try something else.

An initial idea I have is to basically typecheck everything we possibly can until we find symbols of unknown types,
then stall the typechecking of the given branch (probably putting it on some stack-like structure, idk)
until everything is either fully typechecked or missing a type of a reference. This sounds dumb.

It's probably better to construct a graph of fully qualified names of the symbols we're looking at
(every scope / block should have a name, probably synthetic but maybe with an option of naming, like with the keyword `package`)
and traverse it starting from the one that doesn't have dependencies, trying to find nodes that can be fully resolved.

Anyway, I want to support things like (mutual) recursion, but I don't really have an idea on how to implement type inference there,
so it might be necessary to assign return types in these situations. A small sacrifice to pay :)

Reminder: I have no idea what I'm doing and this is just for fun.

### Integrated LSP support

I want precise diagnostics in Language Server Protol-based IDEs, that's it. The compiler will expose an LSP-compatible API.

Things like "go to definition", "find references", rename, inline/extract and call hierarchies would be very nice and I definitely want to have that.

### Compilation server

Since this is being written in Scala, the compiler will also either be optimized for running in watch mode,
or a compilation server will be made (e.g. with nailgun) to avoid the JVM startup overhead.
An alternative that could be considered is compiling to Scala Native or just GraalVM Native Image. This should be possible and the latter seems more realistic.

### Incremental compilation

Probably out of scope. I doubt anybody will ever write a program large enough that it'll take more than a couple seconds to compile.

### Target platform

For now, Slang is interpreted in real time. Obviously this has several downsides (discounting duplicate compilation overhead, which can be avoided by saving
fully resolved compiler outputs on disk in JSON or some other serialization format) - the most important one is performance and memory usage.
Then we have the tooling part (we'd need to implement our own debugger, and so on).

For these reasons, I would like to target the JVM using standard bytecode - the compiler would emit just that.

There is one significant possible blocker that I see for the time being: I want to use Cats Effect as the runtime for effects in the language.
I really don't want to implement IO myself.
I'm not sure if it makes sense, but possibly just treating CE as a standard library and including it in the classpath of Slang programs when they're being executed
would be good enough.

A future milestone could be targetting a platform closer to the metal, such as LLVM or a concrete architecture, like x86.
I don't know if I'll still be interested in this by then though.

## Run application

```shell
sbt run
```

## Run tests

```shell
sbt test
```
