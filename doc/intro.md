> Note: this document is a work-in-progress and
> contributions are welcome.

# Introduction
Cane is a small language designed for generating rhythms
to be played over MIDI.

It takes some inspiration from
[Gwion](https://github.com/Gwion/Gwion) and
[Prop](https://paulbatchelor.github.io/proj/prop.html)
aswell as various other small live-coding tools I've
encountered on my adventures through the internet.

I made Cane in order to simplify the process of writing
beat patterns through a primarily textual representation.
I'm not a huge fan of the rigid structure of DAWs that
appeal primarily to mainstream and general use which makes
unconventional musical styles awkward to work with.
I wanted something that allowed me to experiment with
wacky rhythms without constraint in a way that felt
natural to me.

The language is intentionally designed to be turing
incomplete: all sequences should terminate. This
makes the implementation quite simple as all sequences
can be compiled to a linear timeline of events that fire
out all at once when compilation is complete.

There is a focus on euclidean rhythms, polyrhythms and
polymeters in Cane and experimentation is encouraged.

### Setting up JACK
JACK is required to facilitate MIDI communication between
hardware and/or software synthesizers/drum machines etc.
JACK is used for its low latency characteristics which
make it desirable for real-time use.

### Build & Install

### Your First Program

### Sequences & Steps

### Expressions & Statements

### Sinks

### The Timeline

### Where To Go From Here
