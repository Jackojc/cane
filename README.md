# cane
> The beatings will continue until a banger is made

Cane is a small vector language designed for making beats with realtime MIDI.

![c++](https://img.shields.io/badge/c%2B%2B-17-blue.svg?style=flat)
[![issues](https://img.shields.io/github/issues/Jackojc/cane.svg?style=flat)](https://github.com/Jackojc/cane/issues)
[![pull requests](https://img.shields.io/github/issues-pr/Jackojc/cane?style=flat)](https://github.com/Jackojc/cane/pulls)
[![license](https://img.shields.io/github/license/Jackojc/cane.svg?style=flat)](./LICENSE)
[![discord](https://img.shields.io/discord/997102432133849088.svg?label=discord&style=flat)](https://discord.gg/UVYAtpYYD2)

### Example
```
# Metadata
bpm 120
note 60 # Middle C

# Constants
let
	qn bpm * 4 # Quarter Note
	hn bpm * 2 # Half Note
	fn bpm     # Full Note

# MIDI Channels
let
	c_bd 1 # Bass Drum
	c_cl 2 # Clap
	c_sh 3 # Shaker
	c_ch 4 # Closed HiHat
	c_oh 6 # Open HiHat

# Notes
let
	bd 69 # c_bd: Bass Drum
	cl 69 # c_cl: Clap
	sh 58 # c_sh: Shaker
	ch 69 # c_ch: Closed HiHat
	oh 69 # c_oh: Open HiHat

# French House
!... !... !... !... map bd @ qn ~> c_bd $
.... !... .... !... map cl @ qn ~> c_cl $
!!!. !.!! !!!. !.!! map sh @ qn ~> c_sh $
!!!! !!!! !!!! !!!! map ch @ qn ~> c_ch $
.!.! .!.! .!.! .!.! map oh @ qn ~> c_oh
```

### What Can Cane Do?
- Control hardware and software synths through JACK MIDI
- Play chords and melodies
- Generate complex beats with relatively little effort
- Embeddable in larger projects
- Create polyrhythms and polymeters
- Live-coding (TODO)

### Introduction & Reference
See the introduction [here](doc/intro.md)
and see the reference [here](doc/ref.md).

The EBNF grammar is [here](doc/syntax.ebnf).

### Requirements
- [JACK](https://jackaudio.org/) _or_ [PipeWire](https://pipewire.org/)
- [a2jmidid](https://github.com/jackaudio/a2jmidid) (Optional for ALSA MIDI support)
- [conflict](https://github.com/qookei/conflict) (Included as a git submodule)

### Build & Run
Make sure to use a c++-17 compliant compiler.
```sh
git clone --recursive https://github.com/Jackojc/cane && cd cane
make dbg=no
./build/cane < foo.cn
```

### Rationale
Cane is a project born out of frustration with existing tools. DAWs and other
sequencing software generally don't favour a rapid iterative/experimental workflow.
I want to be able to edit all parts of my song in the same place while listening
to it play in realtime. This is generally just not possible with other tools or is
awkward to use. I also want to favour use of the keyboard since I can type faster
than I can hunt down context menus with the pointer which, again, allows for a very
quick iterative approach to writing music.

Most music software also tends to favour a very western influenced style of writing
and hinders more exotic compositions. Cane tries to stay fairly agnostic to any
particular style and allows for interesting rhythms and arrangements that would
otherwise not be possible in a traditional DAW. Tempo in Cane can be varied throughout
the song for example or sequences can easily play at many different tempos concurrently.
The important point to note here is that this is all _easy_ in Cane.

### Design
- Cane is intentionally designed to be turing incomplete: all sequences should
terminate. This makes the language deterministic and keeps the implementation
simple
- Cane is designed to enable an experimental and iterative workflow, one where you
more often spend time removing things than adding them
- JACK was chosen as the backend for MIDI transport for its low latency properties
which make it desirable for realtime use

### Acknowledgements
- [Gwion](https://github.com/Gwion/Gwion)
- [Prop](https://pbat.ch/proj/prop.html)
- [fennecdjay](https://github.com/fennecdjay) (for inspiring the project)
- [qookei](https://github.com/qookei) (for conflict)

### Resources
See the list of resources [here](doc/resources.md)

### License
This project uses the GPL-3.0 license. (check [LICENSE](LICENSE))

### Progress & Discussion
You can join the discord server in order to follow progress and/or contribute to discussion of the project: https://discord.gg/UVYAtpYYD2

