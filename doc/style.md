# Style Guide

- All source files should be UTF-8 encoded
- Tabs should be used for indentation
- Lines should not exceed 80 or 120 columns depending on which looks more readable

### Literal Definitions
##### Single
Single definitions should be defined on the same line as the `let` keyword.
**Good:**
```
let foo 123
let bar 456
```

**Bad:**
```
let foo
	123

let
bar 456
```

##### Multiple
Multiple definitions should be defined on _multiple_ lines with _no_ indentation.
**Good:**
```
let
foo 123
bar 456
```
**Bad:**
```
let foo 123
    bar 456

let x 1 y 2 z 3
```

### Operators
Operators should be surrounded by whitespace on both sides to increase readability.

**Good:**
```
4:16 ** 2 > 1
!..! & !!.. @ 120
```

**Bad:**
```
foo&bar=>baz
1+2*3
```


