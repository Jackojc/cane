# cane
> The beatings will continue until a banger is made
A small MIDI sequencer DSL designed around vectors and euclidean rhythms

![c++](https://img.shields.io/badge/c%2B%2B-%3E%3D17-blue.svg?style=flat)
[![license](https://img.shields.io/github/license/Jackojc/cane.svg?style=flat)](./LICENSE)
![code size](https://img.shields.io/github/languages/code-size/Jackojc/cane?style=flat-square)
[![issues](https://img.shields.io/github/issues/Jackojc/cane.svg?style=flat)](https://github.com/Jackojc/cane/issues)
[![discord](https://img.shields.io/discord/537732103765229590.svg?label=discord&style=flat)](https://discord.gg/H2qKkSd9gC)

### Example
```
4/8   => kick  => midi 0 @ 240
4/8+2 => snare => midi 0 @ 240
```

### Introduction & Reference
See the introduction [here](doc/introduction.md)
and see the reference [here](doc/reference.md).

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
- [Summary of MIDI 1.0 Messages](https://www.midi.org/specifications-old/item/table-1-summary-of-midi-message)

### License
This project uses the GPL-3.0 license. (check [LICENSE](LICENSE))

### Progress & Discussion
You can join the discord server in order to follow progress and/or contribute to discussion of the project. (https://discord.gg/H2qKkSd9gC)
