# Reference
> Note: this document is a work-in-progress and
> contributions are welcome.

### Operator Overview

| Operator | Name | Applies To | Description | Example | Evaluation |
| --- | --- | --- | --- | --- | --- |
| <_seq_> `?` | Debug | Sequences | Print the sequence | `4:16?` | N/A |
| <_seq_> `=>` <_identifier_> | Chain | Sequences | Assigns the sequence to a name and evaluates to itself | `!..!..!. => x` | `!..!..!.` |
| <_seq_> `map` <_lit_> | Map | Sequences | Map a list of MIDI note values onto a sequence | `3:8 map note note+5 note+7` | `!..!..!.` |
| <_seq_> `car` | Car | Sequences | Evaluates to the first step of a sequence | `!... car` | `!` |
| <_seq_> `cdr` | Cdr | Sequences | Evaluates to a sequence without the first step | `.!!! cdr` | `!!!` |
| <_seq_> `,` <_seq_> | Concatenate | Sequences | Joins two sequences into a single sequence | `3:8, 2:8` | `!..!..!.!...!...` |
| <_seq_> `\|` <_seq_> | Disjunction (Logical OR) | Sequences | Performs the element-wise disjunction of two sequences | `!... \| .!..` | `!!..` |
| <_seq_> `&` <_seq_> | Conjunction (Logical AND) | Sequences | Performs the element-wise conjunction of two sequences | `!... & !.!.` | `!...` |
| <_seq_> `^` <_seq_> | Exclusive Disjunction (Logical XOR) | Sequences | Performs the element-wise exclusive disjunction of two sequences | `!... ^ !.!.` | `..!.` |
| <_seq_> `<` <_lit_> | Rotate Left | Sequences | Rotates the steps of a sequence to the left (with wrap-around) | `!..! < 1` | `..!!` |
| <_seq_> `>` <_lit_> | Rotate Right | Sequences | Rotates the steps of a sequence to the right (with wrap-around) | `!..! > 1` | `!!..` |
| <_seq_> `**` <_lit_> | Repeat | Sequences | Repeats the sequence a number of times | `!..! ** 2` | `!..!!..!` |
| <_seq_> `@` <_lit_> | BPM | Sequences | Sets the BPM for a sequence | `4:16 @ bpm` | N/A |
| `'` <_seq_> | Reverse | Sequences | Reverses the steps in a sequence | `!...!.!.` | `.!.!...!` |
| `~` <_seq_> | Invert (Logical NOT) | Sequences | Inverts the steps of a sequence | `!..!!.!!` | `.!!..!..` |
| <_lit_> `+` <_lit_> | Addition | Literals | Adds two literals together | `1 + 2 + 3` | `6` |
| <_lit_> `-` <_lit_> | Subtraction | Literals | Subtracts two literals from eachother | `6 - 2` | `4` |
| <_lit_> `*` <_lit_> | Multiplication | Literals | Multiplies two literals together | `6 * 7` | `42` |
| <_lit_> `/` <_lit_> | Division | Literals | Divides two literals | `12 / 4` | `3` |
| `len` <_seq_> | Length | Literals | Evaluates to the length of a sequence | `len 4:16` | `16` |
| `beats` <_seq_> | Beats | Literals | Evaluates to the number of beats in a sequence | `beats 4:16` | `4` |
| `skips` <_seq_> | Skips | Literals | Evaluates to the number of skips in a sequence | `skips 4:16` | `12` |

### Operator Precedence
Low to high precedence.

| Sequence Operators |
| --- |
| `?` `=>` `map` |
| `car` `cdr` |
| `,` `\|` `&` `^` `<` `>` `**` `@` |
| `'` `~` |

| Literal Operators |
| --- |
| `+` `-` |
| `*` `/` |
| `len` `beats` `skips` |

### Metadata
Every Cane file begins with a small meta-data section which contains
two values: `bpm` and `note`.

`bpm` defines a global tempo for the song and can be accessed in literal expression
contexts using the same name: `bpm`. This value dictates the default tempo of sequences
if you don't manually override it with the `@` operator. The global tempo also sets
the MIDI clock rate for timing messages.

`note` defines a global base note for sequences. By default, sequences will use
this value for all beats but you can define a new note mapping using the `map`
operator. You can choose to ignore this value entirely but it does provide an easy
way to change the key of your song. You can access this value in literal expressions
using `note`.

### Steps
A step is the basic unit of a sequence and is either a beat or
a skip.

A beat plays a note and a skip passes time (Skips do not sustain a note).

- Beat: `!`
- Skip: `.`

### Literals
Literals are scalar value expressions used as arguments to some
operators.

`x ** 1+2*3`

Constants can be defined like: `let foo 123 * 456`.
As the name implies, constants are immutable.

There are some implicitly defined constants like `bpm` and `note` which
evaluate to their respective global values.

### Sequences
Sequences are a collection of steps and the building blocks of Cane.

They can be created in two different ways:

- Euclidean Sequences: `4:16` where `4` is the number of beats and `16` is
the total number of steps.
- Explicit Sequences: `!..!..!.`

Sequences can be assigned to a name using the chaining operator (`=>`) like so:
`4:16 => x`. Assignments are immutable.

### Send
The `send` keyword sinks a sequence to a MIDI channel. In other words, without
`send`, your song will not send any MIDI.

`send` is _not_ an operator like most things in Cane. It is a statement and must
appear at the beginning of an expression.

As an example, you can send a sequence to MIDI channel 1 like so: `send 1 4:16`.

### Aliases
Aliases allow you to name a MIDI channel. This is usually a lot easier than having
to remember which channel your instruments are on manually.

You can define an alias like: `alias kick 1`.
Aliases are constant and cannot be re-defined.
You can use an alias in any context where a numeric channel value would be
expected.

`send kick 4:16`

### File Extensions
`.cn` or `.cane` are accepted file extensions.

