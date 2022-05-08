> Note: this document is a work-in-progress and
> contributions are welcome.

# Reference
### Steps
A step is the basic unit of a sequence and is either a beat or
a skip.

Beats represent a note to be played and skips just pass time.

Beat `!`
Skip `.`

### Literals
Literals are just constant numberical values written as either decimal,
hexadecimal or binary.
```
42
0xFF
0b1001
```

### Sequences
Sequences are a collection of steps and the building blocks of Cane.
```
!.!!  # A literal sequence
3/4   # A euclidean sequence
```

### Operators
Sequence length and BPM are always inherited from the left-hand-side expression.

Prefix operators:
```
~
'
```

Infix operators (with literal):
```
<<
>>
*
@
```

Infix operators (with expression):
```
sync
=>
&
|
^
,
```

Postfix operators:
```
<
>
?
```

### Expressions

### Statements

### Sinks
Sinks are the Cane terminology for MIDI channels. They are a sort
of implicit sequence that is built up as a timeline of events
which will then be used when compilation is finished and playback
begins.

### File Extensions
`.cn` or `.cane` are accepted file extensions.

