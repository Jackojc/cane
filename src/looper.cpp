/*

SENTINEL NOTES

We fill a buffer with recieved notes including time on information
until we find a sentinel note (commandline specified, usually note: 0,
velocity: 0). Once we have hit the sentinel note, we continue to loop
the entire sequence we recieved until we hit another sequence with
sentinel note. If we recieve new notes while the sequence is playing,
we replace the notes at the beginning of the currently playing sequence
so the next loop plays in the new one as soon as possible.

                    / sentinel note
                    v
[ A B C A B C A B C 0 ]

Once we receive this sequence, we start playing it in a loop until we
receive a new sequence.


FIXED BUFFER

Pass in a tempo + sequence length through the commandline arguments:
`looper --bpm 120 --steps 16`
If we convert this to an absolute duration, we get:
`60 / 120 * 16 = 8 seconds`

We store received notes in a fixed sized buffer with their timestamps
until we hit 8 seconds. Once we do, we start playing the sequence
repeatedly.

When we want to play something else, we simply play our MIDI notes
through the looper replacing what's in the buffer from the beginning.
When the looper moves back to the start, it starts playing our new
sequence instead and it will continue to loop once we've sent our new
sequence.

We don't strictly have to send 16 step sequences. We _could_ send a 32
step sequence which would play once and then loop on the last 16 notes.

You can think of this looper as a sort of glorified quantizer that aligns
sequences to start and end boundaries so they loop seamlessly. It also
happens to loop the last received sequence to prevent any silence.

Even if we don't play a complete sequence, once we hit 8s from first
arrival of a new note, we'll start looping the sequence _including_
silence.

*/

int main(int argc, const char* argv[]) {
	return 0;
}
