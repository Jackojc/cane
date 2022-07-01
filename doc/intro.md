# Introduction
> This document is a work-in-progress and
> contributions are welcome.

For installation and setup, this guide will assume you are running
Void Linux (because that's what I'm familiar with) but a lot of these
instructions should be applicable to most other distros.

I am also going to use LMMS in order to turn our MIDI data from Cane
into sound but you can use whatever you like instead.

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
Cane cannot produce any sound by itself. That is the job of whatever is on the
receiving end of the MIDI messages produced by Cane.

Cane can talk to any MIDI aware device through JACK which includes most software
synthesisers or even hardware synthesisers.

In cases where you can't use JACK, you can use `j2amidi_bridge`
(from [a2jmidid](https://github.com/jackaudio/a2jmidid)) to translate from
JACK to ALSA.

I'm going to use LMMS here but feel free to use whatever you like.

##### Configuring LMMS
Make sure that you have "Jack-MIDI" selected in the LMMS MIDI settings dialog:

<img alt="LMMS MIDI Settings" src="img/lmms-midi.png" width=30%>

LMMS must be compiled with support for JACK in order to use JACK directly. If
your distribution of LMMS does not support JACK MIDI natively, you can instead
use the ALSA MIDI backend and route Cane through `j2amidi_bridge` and then to
LMMS.

Once you've got LMMS set up to work with JACK MIDI, you're going to want to set
up a synth to receive MIDI on a particular channel.

You just have to go to the MIDI tab, enable MIDI input and set a channel:

<img alt="LMMS Synth MIDI" src="img/lmms-synth.png" width=20%>

We're now ready to start making some beats with Cane!

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
