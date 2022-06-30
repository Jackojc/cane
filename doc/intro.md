# Introduction
> Note: this document is a work-in-progress and
> contributions are welcome.

Note: For installation and setup, this guide will assume you are running
Void Linux (because that's what I'm familiar with) but a lot of these
instructions should be applicable to most other distros.

### Setting Up An Audio Server
Cane depends on the JACK audio server in order to facilitate the routing
of MIDI.

There are two options you can use for setting up a JACK server.

1. JACK
2. PipeWire

PipeWire includes an implementation of a JACK server and is fairly easy to set up
so that's what we'll be using in this guide. Anyone wishing to use JACK is likely
already using it and knows what to do.

To download PipeWire, we run:
```sh
$ xbps-install -S pipewire libjack-pipewire
```

We're also going to need the JACK development libraries for building Cane.
```sh
$ xbps-install -S jack-devel
```

Next, we need to make sure to point the dynamic linker to the PipeWire JACK
libraries. There are two options we can use and the choice will really come down
to personal preference.

1. `pw-jack`
2. Override JACK libraries with PipeWire libraries

Overriding the JACK libraries may cause issues if you use JACK outside of PipeWire
so use it only if you're confident that you'll be sticking to PipeWire.

To use `pw-jack`, you just have to insert it in front of running `cane` like so:
```sh
$ pw-jack cane -f foo.cn -m bar
```

To override the JACK libraries, run:
```sh
$ echo "/usr/lib/pipewire-0.3/jack" > /etc/ld.so.conf.d/pipewire-jack.conf
$ ldconfig
```

Lastly, we need to launch the PipeWire server:
```sh
$ pipewire
```

And with that, we should now be good to go. PipeWire usually doesn't require any
complex configuration out of the box.

### Build & Install
Run the following commands to clone the repository and build Cane:
```sh
$ git clone --recursive https://github.com/Jackojc/cane && cd cane
$ make dbg=no  # Release build
```

This will place the Cane binary in `build/`. You can use Cane from here
or install it by running:
```sh
$ make install
```

You may wish to change the PREFIX directory:
```sh
$ PREFIX=/usr/bin make install
```

You should now be able to run Cane:
```sh
$ cane -f foo.cn -m bar
```

### Connecting To A MIDI Device

### Sequences

### Channels

### A Basic Beat

### Layering

### Note Mapping

### Four On The Floor

### Euclidean Rhythms

### Operators

### Something Weird

### Where To Go From Here
Now that you're familiar with the basics of Cane there are a few things you
can do to learn more:

- Check out the `examples/` directory for more complicated compositions
- Read the [reference](ref.md) for a more in-depth look at how Cane works
- Join the [Discord](https://discord.gg/Qqguu9SRvU) to talk with us
