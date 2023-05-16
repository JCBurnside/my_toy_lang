*badges here*
samples could end up in the STD lib. 
# NAME TBD
this is a compiler for a functional programming language.  It has the following syntax
## basic syntax

Important note: number of spaces and/or tabs do not matter so long as they are consistent for each block.
- - -
### function and variable declaration
a simple function can be declared like so
```
let foo bar baz =
    ...
let quz = 3
```
this declares a function `foo` with two args and a constant `quz` using inferred typing (note if a type can not be deduced the function shall be treated as a generic declaration.  see the following section)

you can use explicit typing like so 
```
let foo bar baz : int32 -> int32 -> int32 =
    ...

let quz : int8 = 3
```

you can also declare a value as mutable with 
```
let mut foo = 3
```

### generic typing

you can explicitly declare a function as generic (for now only generic types are supported but eventually lifetimes and constant generics are planned in that order) with the following
```
for<T> let id x : T -> T = x

for<T,U> let map fun arg : (T -> U) -> &[T] -> &[U] = 
    ...
```


# TYPES
---
## built in

### numeric
*name here* supports man different widths of types
for integral types
- int8
- int16
- int32
- int64

for floating points
- float32
- float64

### characters and strings

there is the `char` type as well two string types

`str` which will need to behind a reference due to being a slice of chars and is always fixed length (but potentially internally mutable depending on lifetime) 
`string` which is a non fixed length string and can be taken by value or converted to a `&str`

### collection types

there a few basic collection types provided by the standard.

#### fixed length

there are arrays which take the form of `[T;N]` for typing and `[A,B,C]` for values. the lengths are fixed at compile time.
eg
```
let arr : [int8;3] = [0;1;2]
```

there are slices which take the form of `[T]` for typing.  they can not be manually constructed and can not be taken by value.  their lengths are fixed at runtime.
eg 
```
let max slc : &[int8] -> int8 =
    slc
    |> slice::fold (a b -> if a < b then b else a) int8::MIN
```

#### non-fixed length

there are lists defined `[[T]]` for typing and `[[A,B,C]]` for values. implemented as a linked list.

### Error handling and optionals

typically error handling should be done through `Result<T,E>` which has variants of `Ok T` and `Err E` 
eg
```
let some_fallible foo : int32 -> Result<int32,&'static str> = 
    ...

//to handle
match some_fallible foo ->
| Ok res -> ...
| Err _ -> ...
```

there is also `Option` which allows for optional parameters and return types (eg value not found) with variants of `Some T` and `None`
for typing it has both `T option` and `Option<T>` versions.
eg 
```
for<T,U> let map fun opt : (T->U)-> T option -> U option =
    match opt -> 
    | Some inner -> fun inner |> Some
    | None -> None
```

## User types

### Structure types

simple structure types can take the form of 
```
[for<Ident...> [where clause]] type <Ident> = 
    Ident : Type
    ...
```
eg
```
type Foo =
    start : int8
    end : int8

//it is recommended to make a constructing function
//eg
let new start end = 
```

### Union Types

A discriminated union type (referred to as a enum in this language) can be defined like such

```
[for<ident...> [where clause]] enum <Ident> =
    | <Variant Ident>[(Type[,Type...])]
    | <Variant Ident>[{ ident:Type, ...}]
    | <Variant Ident>
    ...
```
any variation can be mixed and matched. 

## Comments

comments come in two forms

```
//single line comments

/* 
    multi line.
    or inserted comments
*/
```