# Reference
> Note: this document is a work-in-progress and
> contributions are welcome.

### Operator Overview

| Operator | Name | Applies To | Description | Example | Evaluation |
| --- | --- | --- | --- | --- | --- |
| <_seq_> `?` | Debug | Sequences | Print the sequence | `4:16?` | N/A |
| <_seq_> `=>` <_identifier_> | Chain | Sequences | Assigns the sequence to a name and evaluates to itself | `!..!..!. => x` | `!..!..!.` |
| <_seq_> `map` <_lit_> | Map | Sequences | Map a list of MIDI note values onto a sequence | `3:8 map note note+5 note+7` | `!..!..!.` |
| <_seq_> `vel` <_lit_> | Map | Sequences | Map a list of velocity values onto a sequence | `3:8 vel 63 80 127` | `!..!..!.` |
| <_seq_> `car` | Car | Sequences | Evaluates to the first step of a sequence | `!... car` | `!` |
| <_seq_> `cdr` | Cdr | Sequences | Evaluates to a sequence without the first step | `.!!! cdr` | `!!!` |
| <_seq_> `,` <_seq_> | Concatenate | Sequences | Joins two sequences into a single sequence | `3:8, 2:8` | `!..!..!.!...!...` |
| <_seq_> `\|` <_seq_> | Disjunction (Logical OR) | Sequences | Performs the element-wise disjunction of two sequences | `!... \| .!..` | `!!..` |
| <_seq_> `&` <_seq_> | Conjunction (Logical AND) | Sequences | Performs the element-wise conjunction of two sequences | `!... & !.!.` | `!...` |
| <_seq_> `^` <_seq_> | Exclusive Disjunction (Logical XOR) | Sequences | Performs the element-wise exclusive disjunction of two sequences | `!... ^ !.!.` | `..!.` |
| <_seq_> `<` <_lit_> | Rotate Left | Sequences | Rotates the steps of a sequence to the left (with wrap-around) | `!..! < 1` | `..!!` |
| <_seq_> `>` <_lit_> | Rotate Right | Sequences | Rotates the steps of a sequence to the right (with wrap-around) | `!..! > 1` | `!!..` |
| <_seq_> `**` <_lit_> | Repeat | Sequences | Repeats the sequence a number of times | `!..! ** 2` | `!..!!..!` |
| <_seq_> `@` <_lit_> | BPM | Sequences | Maps step durations for a sequence | `4:16 @ 120` | N/A |
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
| `?` `=>` `map` `vel` `,` |
| `@` |
| `car` `cdr` |
| `\|` `&` `^` `<` `>` `**` |
| `'` `~` |

| Literal Operators |
| --- |
| `+` `-` |
| `*` `/` |
| `len` `beats` `skips` |

### Lexical Grammar
If you're working on a syntax highlighter for Cane, you might find this section
useful.

You may also want to check the `examples/` directory or any of the existing
highlighters in the `highlighters/` directory.

| Class | Token |
| --- | --- |
| Keywords | `let` `map` `vel` `car` `cdr` `len` `beats` `skips` `pat` |
| Operators | `=>` `@` `?` `<` `>` `**` `\|` `&` `^` `,` `~` `'` `+` `-` `*` `/` |
| Operators/Keywords | `$` `:` `~>` `\\` |
| Values | `!` `.` `=` |
| Comments | `#.+$` |
| Grouping | `(` `)` |
| Identifier | `\S+` |
| Literal | `\d+` |

### Steps
A step is the basic unit of a sequence and is either a beat,
a skip or a sustain.

A beat plays a note, a skip passes time and sustain continues to hold
a note down across multiple steps.

- Beat: `!`
- Skip: `.`
- Sustain: `=`

### Literals
Literals are scalar value expressions used as arguments to some
operators.

`x ** 1+2*3`

Constants can be defined like: `let foo 123 * 456`.
As the name implies, constants are immutable.

### Sequences
Sequences are a collection of steps and the building blocks of Cane.

They can be created in two different ways:

- Euclidean Sequences: `4:16` where `4` is the number of beats and `16` is
the total number of steps.
- Explicit Sequences: `!..!..!.`

Sequences can be assigned to a name using the chaining operator (`=>`) like so:
`4:16 => x`. Assignments are immutable.

### Notes
Notes in Cane are just simply their corresponding MIDI values. For example, Middle
C is MIDI value `60`.

To map notes to a sequence, we use the `map` operator like so:
```
4:16 map 60 65 67
```
In the above example, we can see that the sequence has four beats but we have
only mapped three notes. If there are too few notes, we map as many as we can
and if there are too few notes, we simply loop back around to the start and map
the first note.

### Send
The send (`~>`) statement sinks a sequence to a MIDI channel. In other words, without
send, your song will not send any MIDI.

send is _not_ an operator like most things in Cane. It is a statement and must
appear at the beginning of an expression.

As an example, you can send a sequence to MIDI channel 1 like so:
`4:16 @ 120 map 60 ~> 1`.

It is important to note that you should map step durations and notes before
trying to send a sequence otherwise you'll get an error.

### Timelines
Timelines are the result of sending a sequence to a MIDI channel. Timelines are
a vector of MIDI events and can be layered or defined as part of a pattern.

In an ideal Cane song, you'll define all of your patterns ahead of time and then
simply pick and choose which patterns to play in sequence at the end.

To define a pattern, simply use the `pat` keyword followed by an identifier and
timeline like so:
```
pat four_on_the_floor
	4:16 @ 120 map 60 ~> kick
```

To recall a pattern, you must prefix the name with `\\` to avoid ambiguity with
named sequences:
```
\four_on_the_floor
\four_on_the_floor
```

### Layering
Normally, sequences are played one after the other but you can use layering
to play two sequences at the same time to build up more complex rhythms.

If we start off with a simple beat like this:
```
2:16 @ 120 map 60 ~> kick
2:16 > 2 @ 120 map 60 ~> snare
8:16 @ 120 map 60 ~> hihat
```
When playing it you'll find that each sequence plays on its own after the other.
This obviously isn't what we want so to make the sequences play simultaneously,
we use the layer statement (`$`). Applying it to the above example, we get:
```
2:16 @ 120 map 60 ~> kick $
2:16 > 2 @ 120 map 60 ~> snare $
8:16 @ 120 map 60 ~> hihat
```
And voila, our sequences now play together as expected.

### Tuplets
In Cane, the BPM operator (`@`) allows you to map a duration to a sequence.
You can map a different BPM to two different sequences and then join them
together to have multiple tempos in the same sequence. This allows you to make
some rather complicated rhythms.

Let's build up a small rhythm using this idea. First, we will start off with just
our pattern and later subdivide the tempo:
```
let
	bpm 140
	qn bpm * 4

2:8, 4:32, 2:16
** 4 @qn map 60 ~> 1
```
If we take a listen to this beat, it sounds pretty boring and repetitive so
now lets add some rhythmic variation:
```
(
	2:8  @ qn / 8 * 8,
	4:32 @ qn / 4 * 32,
	2:16 @ qn / 4 * 16
) ** 4 @qn map 60 ~> 1
```
If you take a listen now I think you'll agree that it sounds a lot more
interesting than what we had before.

What we're doing here is taking this sequence with a combined total of 56 steps
and playing it in the same time as a sequence with 16 steps. We do this by
speeding up or slowing down the sequences. The first sequence simply plays at
a tempo of `qn`, `/ 8 * 8` does nothing here but it serves to keep things looking
consistent. Our second sequence has 32 total steps but we want it to play in the
same time as a 4 step sequence at a tempo of `qn`. To do this, we divide by the
number of steps we _want_ to take and the number of steps it contains. So plugging
that into the short formula, we get: `qn / 4 * 32`.

### Sustained Notes
Sustain steps allow you to hold down a note across multiple steps which makes it
easy to play varying note lengths while keeping sequences the same length and
tempo.

As an example, let's take two sequences that we want to play for the same
duration. One of the sequences will use quarter notes while the other uses
full notes.
```
!.!.!.!.!.!.!.!. @ 120 map 60 ~> 1
!===!===!===!=== @ 120 map 60 ~> 2
```
both of these sequences will take the same amount of time to play but the first
sequence will play eight notes sustained for a quarter note while the second
sequence will play four notes sustained for a full note each.

If you didn't want to use sustain steps here, you'd have to do something like this:
```
!.!.!.!.!.!.!.!. @ 120 map 60 ~> 1
!!!!             @ 30  map 60 ~> 2
```
Having to mess with the tempo and vary the sequence length to play the sequences
for the same duration very quickly gets unwieldy with more complex sequences.

### Debug
It is sometimes useful to visualise sequences instead of relying solely on your
ears to do the work. In Cane, you can visualise the steps in a sequence with
the debug operator (`?`).

Compiling `4:16?` will give us a short notice showing us the shortest repeating
sub-sequence. In this case it is: `!...`(x4).

### File Extensions
`.cn` or `.cane` are accepted file extensions.

