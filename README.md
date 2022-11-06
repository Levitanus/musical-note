# musical-note

Represents general musical note and allow to convert it,
currently only to MIDI byte and back.

Basically, this crate just makes very good work on alteration and resolving
enharmonical representation of midi notes, based on given key and scale.

At least, it works better, that built-in Reaper and MuseScore algorithms.

It uses Nederlanden language as string representation, as it is the most
consistent and easy to use (and hard to mistake) note representation.

## Basically:
- B is always B (there is no H)
- is — Sharp
- isis — DoubleSharp
- es (for E and A — Es and As respectively) — Flat
- eses — DoubleFlat

## Examples

```Rust
use musical_note::{midi_to_note, Accidental, Key, NoteName, Octave, ResolvedNote, Scale};
let as3 = ResolvedNote::new(NoteName::A, Accidental::Flat, Octave::new(5), 68);
assert_eq!(ResolvedNote::from_str("as3").unwrap(), as3);
assert_eq!(ResolvedNote::from_str("Aes3").unwrap(), as3);
let a_moll = Key::new(NoteName::A, Accidental::White, Scale::Minor);

// automatically unwraps
let a_dur = Key::from((&"a".to_string(), Scale::Major));

let d_moll = Key::from_str("d", Scale::Minor).unwrap();
let d_dur = Key::from_str("d", Scale::Major).unwrap();

let fis_moll = Key::from_str("fis", Scale::Minor).unwrap();
let fis_dur = Key::from_str("fis", Scale::Major).unwrap();

let ais_dur = Key::from_str("ais", Scale::Major).unwrap();
let des_moll = Key::from_str("des", Scale::Minor).unwrap();
// first test against idiot mistake:
assert_eq!(
    midi_to_note(60, Key::from_str("c", Scale::Major).unwrap(), None),
    ResolvedNote::new(NoteName::C, Accidental::White, Octave::new(60 / 12), 60)
);

// // test on Bb2
let midi = 58u8;
assert_eq!(
    midi_to_note(midi, d_moll, None),
    ResolvedNote::from_str("bes2").unwrap()
);
// // test against major alteration (VI♭)
assert_eq!(
    midi_to_note(midi, d_dur, None),
    ResolvedNote::from_str("bes2").unwrap()
);

// // test against minor alteration (II♭)
assert_eq!(
    midi_to_note(midi, a_moll, None),
    ResolvedNote::from_str("bes2").unwrap()
);
```
