# cane
> The beatings will continue until a banger is made

A small MIDI sequencer DSL designed around polymeters, polyrhythms
and euclidean rhythms.

![c++](https://img.shields.io/badge/c%2B%2B-17-blue.svg?style=flat)
[![issues](https://img.shields.io/github/issues/Jackojc/cane.svg?style=flat)](https://github.com/Jackojc/cane/issues)
[![pull requests](https://img.shields.io/github/issues-pr/Jackojc/cane?style=flat)](https://github.com/Jackojc/cane/pulls)
[![license](https://img.shields.io/github/license/Jackojc/cane.svg?style=flat)](./LICENSE)
[![discord](https://img.shields.io/discord/537732103765229590.svg?label=discord&style=flat)](https://discord.gg/H2qKkSd9gC)

### Example
```
# Channels
alias kick    ~> 1
alias snare   ~> 4
alias hihat   ~> 5
alias hammer  ~> 7
alias pop     ~> 8
alias ride    ~> 9
alias crunchy ~> 10

# Sequences
8:16               @240 => kick
4:16 >> 2          @240 => pop
3:8 >> 3, (.**8)   @240 => ride
2:6 ** 2, 3:4 >> 3 @240 => crunchy
2:16               @240 => hammer

# 2:3 Polyrhythm
!**4 => a
!**6 => b

a fit b fit kick => hihat
b fit b fit kick => snare

# Send sequences to MIDI channels
kick **4    ~> kick
pop **4     ~> pop
ride **4    ~> ride
crunchy **4 ~> crunchy
hammer **4  ~> hammer
snare **4   ~> snare
hihat **4   ~> hihat
```

### Introduction & Reference
See the introduction [here](doc/intro.md)
and see the reference [here](doc/ref.md).

The EBNF grammar is [here](doc/syntax.ebnf).

### Requirements
- [JACK](https://jackaudio.org/) _or_ [PipeWire](https://pipewire.org/)
- [a2jmidid](https://github.com/jackaudio/a2jmidid) (optional for ALSA MIDI support)

### Build & Run
Make sure to use a c++-17 compliant compiler.
```sh
git clone https://github.com/Jackojc/cane && cd sane
make dbg=no
./build/cane < foo.cn
```

### Acknowledgements
- [Gwion](https://github.com/Gwion/Gwion)
- [Prop](https://pbat.ch/proj/prop.html)

### Resources
- [Euclidean Rhythms](http://cgm.cs.mcgill.ca/~godfried/publications/banff.pdf)
- [Euclidean Rhythms (Extended Paper)](http://cgm.cs.mcgill.ca/~godfried/publications/banff-extended.pdf)
- [Summary of MIDI 1.0 Messages](https://www.midi.org/specifications-old/item/table-1-summary-of-midi-message)
- [List of Euclidean Rhythms](http://www.iniitu.net/Euclidian_Erd%C3%B6s_Deep_Aksak_rhythms.html)
- [Polyrhythm](https://en.wikipedia.org/wiki/Polyrhythm)

### License
This project uses the GPL-3.0 license. (check [LICENSE](LICENSE))

### Progress & Discussion
You can join the discord server in order to follow progress and/or contribute to discussion of the project. (https://discord.gg/H2qKkSd9gC)
