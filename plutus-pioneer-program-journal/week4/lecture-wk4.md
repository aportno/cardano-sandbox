Tag: 2fbb7abb22138a434bb6c4f663a81e9b9dc51e98

# Imperative vs Function Programming

In an imperative language, like Java, we can create the below signature where the function will be variable:

```
public static int foo() {
    ...
}

... foo()
... foo()
```

The implementation of `foo()` might be different each time if for example the body of the function requires the users input from the command line and the input turns out to be two different integers.

Example:
* The body of the function might take a users input and multiply it by 2
* The first function call could be user input (2) * 2, and return 4
* The second function call could be user input (3) * 2, and return 6
* Thus, the output of foo() has changed. Once it returned 4, and the other time it return 6

This is very different compared to a functional language like Haskell where there iss **referential transparency**; the value of the function will never change. If the value of foo returned, say 13, then you can always expect the value of foo to return 13

Syntactically the same `foo()` signature looks like the below in Haskell:

```
foo :: Int
foo = ...

... foo
... foo
```

# IO

IO is a type constructor that takes one argument (similar to `Maybe` or `List`). It's a built-in primitive, and it shows that the computation involved will have side effects

```
foo :: IO Int
foo = ...
```

# >> operator
The `>>` operator executes two different `IO` actions:

```
putStrLn "Hello" >> putStrLn "World"
```
Will print the output:
```
Hello
World
```


