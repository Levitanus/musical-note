//! Represents general musical note and allow to convert it,
//! currently only to MIDI byte and back.
//!
//! Basically, this crate just makes very good work on alteration and resolving
//! enharmonical representation of midi notes, based on given key and scale.
//!
//! At least, it works better, that built-in Reaper and MuseScore algorithms.
//!
//! It uses Nederlanden language as string representation, as it is the most
//! consistent and easy to use (and hard to mistake) note representation.
//!
//! Basically:
//! - B is always B (there is no H)
//! - is — Sharp
//! - isis — DoubleSharp
//! - es (for E and A — Es and As respectively) — Flat
//! - eses — DoubleFlat
//!
//! #Examples
//!
//! ```
//! use musical_note::{midi_to_note, Accidental, Key, NoteName, Octave, ResolvedNote, Scale};
//! let as3 = ResolvedNote::new(NoteName::A, Accidental::Flat, Octave::new(5), 68);
//! assert_eq!(ResolvedNote::from_str("as3").unwrap(), as3);
//! assert_eq!(ResolvedNote::from_str("Aes3").unwrap(), as3);
//! let a_moll = Key::new(NoteName::A, Accidental::White, Scale::Minor);
//!
//! // automatically unwraps
//! let a_dur = Key::from((&"a".to_string(), Scale::Major));
//!
//! let d_moll = Key::from_str("d", Scale::Minor).unwrap();
//! let d_dur = Key::from_str("d", Scale::Major).unwrap();
//!
//! let fis_moll = Key::from_str("fis", Scale::Minor).unwrap();
//! let fis_dur = Key::from_str("fis", Scale::Major).unwrap();
//!
//! let ais_dur = Key::from_str("ais", Scale::Major).unwrap();
//! let des_moll = Key::from_str("des", Scale::Minor).unwrap();
//! // first test against idiot mistake:
//! assert_eq!(
//!     midi_to_note(60, Key::from_str("c", Scale::Major).unwrap(), None),
//!     ResolvedNote::new(NoteName::C, Accidental::White, Octave::new(60 / 12), 60)
//! );
//!
//! // // test on Bb2
//! let midi = 58u8;
//! assert_eq!(
//!     midi_to_note(midi, d_moll, None),
//!     ResolvedNote::from_str("bes2").unwrap()
//! );
//! // // test against major alteration (VI♭)
//! assert_eq!(
//!     midi_to_note(midi, d_dur, None),
//!     ResolvedNote::from_str("bes2").unwrap()
//! );
//!
//! // // test against minor alteration (II♭)
//! assert_eq!(
//!     midi_to_note(midi, a_moll, None),
//!     ResolvedNote::from_str("bes2").unwrap()
//! );
//! assert_eq!(
//!     midi_to_note(midi, a_dur, None),
//!     ResolvedNote::from_str("ais2").unwrap()
//! );
//! assert_eq!(
//!     midi_to_note(midi, a_dur, Some(Accidental::Flat)),
//!     ResolvedNote::from_str("bes2").unwrap()
//! );
//!
//! assert_eq!(
//!     midi_to_note(midi, fis_moll, None),
//!     ResolvedNote::from_str("ais2").unwrap()
//! );
//! assert_eq!(
//!     midi_to_note(midi, fis_dur, None),
//!     ResolvedNote::from_str("ais2").unwrap()
//! );
//!
//! // // test against tonality doublesharps and doubleflats
//! assert_eq!(
//!     midi_to_note(62, ais_dur, None),
//!     ResolvedNote::from_str("cisis3").unwrap()
//! );
//! assert_eq!(
//!     midi_to_note(57, des_moll, None),
//!     ResolvedNote::from_str("beses2").unwrap()
//! );
//!
//! // // some church scales
//! let c_phrygian = Key::from_str("c", Scale::Phrygian).unwrap();
//! assert_eq!(
//!     midi_to_note(61, c_phrygian, None),
//!     ResolvedNote::from_str("des3").unwrap()
//! );
//! assert_eq!(
//!     midi_to_note(63, c_phrygian, None),
//!     ResolvedNote::from_str("es3").unwrap()
//! );
//!
//! let es3 = midi_to_note(63,
//!                         Key::new(
//!                             NoteName::C,
//!                             Accidental::Sharp,
//!                             Scale::Major,
//!                         ),
//!                         Some(Accidental::Flat));
//! assert_eq!(
//!     format!(
//!         "{}{}{}",
//!         es3.note.to_string(),
//!         es3.accidental.to_string_by_note(es3.note),
//!         es3.octave.as_midi()
//!     ),
//!     "es3".to_string()
//! );
//!
//! assert_eq!(
//!     midi_to_note(65, c_phrygian, None),
//!     ResolvedNote::from_str("f3").unwrap()
//! );
//!
//! let fis_dorian = Key::from_str("fis", Scale::Dorian).unwrap();
//! assert_eq!(
//!     midi_to_note(60, fis_dorian, None),
//!     ResolvedNote::from_str("bis2").unwrap()
//! );
//!
//! // // test against known bugs
//! assert_eq!(
//!     midi_to_note(64, fis_dur, None),
//!     ResolvedNote::from_str("e3").unwrap()
//! );
//! assert_eq!(
//!     midi_to_note(62, fis_dur, None),
//!     ResolvedNote::from_str("d3").unwrap()
//! );
//! ```
//!

use lazy_static::lazy_static;
use once_cell::sync::OnceCell;
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// static mut NOTES_MAP: NotesMap = Lazy::new(NotesMap::empty());
static _NOTES_MAP: OnceCell<NotesMap> = OnceCell::new();

// type Octave = u8;

/// Represents note, that can be written in score.
///
/// It has:
/// - precise note
/// - enharmonically correct accidental
/// - octave
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Serialize, Deserialize)]
pub struct ResolvedNote {
    pub note: NoteName,
    pub accidental: Accidental,
    pub octave: Octave,
    pub midi: u8,
}

impl ResolvedNote {
    pub fn new(note: NoteName, accidental: Accidental, octave: Octave, midi: u8) -> Self {
        Self {
            note,
            accidental,
            octave,
            midi,
        }
    }
    /// "cisis3" resolved to C## 3 (62 raw MIDI note) with raw Octave{octave:5}.
    pub fn from_str(name: &str) -> Option<ResolvedNote> {
        lazy_static! {
            static ref RESOLVED_NOTE_RE: Regex = Regex::new(r"(\w*)(-?\d)").unwrap();
        }
        for cap in RESOLVED_NOTE_RE.captures_iter(name) {
            let tonic = note_from_str(&cap[1])?;
            let (note, accidental) = tonic;
            let mut octave = Octave::from(&cap[2]);
            if note == NoteName::B
                && (accidental == Accidental::Sharp || accidental == Accidental::DoubleSharp)
            {
                octave = Octave::new(octave.raw() + 1);
            } else if note == NoteName::C
                && (accidental == Accidental::Flat || accidental == Accidental::DoubleFlat)
            {
                octave = Octave::new(octave.raw() - 1);
            }
            return Some(Self {
                note,
                accidental,
                octave,
                midi: octave.apply_to_midi_note(NotesMap::get().get_by_note(note, accidental)),
            });
        }
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Serialize, Deserialize)]
pub struct Octave {
    octave: u8,
}
impl Octave {
    /// octave number should be raw: like
    /// ```
    /// # use musical_note::Octave;
    /// let middle_c = 60u8;
    /// let octave = Octave::new(middle_c/12);
    /// ```
    pub fn new(octave: u8) -> Self {
        Self { octave }
    }
    pub fn raw(&self) -> u8 {
        self.octave
    }
    /// can be negative. Here middle-c considered as C3.
    /// So, function return would be in range of -2..9
    pub fn as_midi(&self) -> i8 {
        self.octave as i8 - 2
    }
    pub fn from_midi(midi: u8) -> Self {
        Self { octave: midi / 12 }
    }
    /// Get note index `(C=0, D=2, D#=3, B=11)` and octave.
    pub fn split_midi(midi: u8) -> (u8, Self) {
        (midi % 12, Self::from_midi(midi))
    }
    /// Return raw midi byte.
    pub fn apply_to_midi_note(&self, note_idx: u8) -> u8 {
        self.octave * 12 + (note_idx % 12)
    }
}
impl From<&str> for Octave {
    fn from(value: &str) -> Self {
        let nr = value.parse::<i8>().unwrap();
        return Self {
            octave: (nr + 2) as u8,
        };
    }
}

/// Main function to convert midi to ResolvedNote.
///
/// If accidental is given — it tries to return note with this accidental.
/// Otherwise — it uses Key (and, mainly, Scale) rules for alteration.
pub fn midi_to_note(midi: u8, key: Key, accidental: Option<Accidental>) -> ResolvedNote {
    Note::from_midi(midi, accidental).resolve(key)
}

/// Not yet resolved Note, constructed by MIDI.
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Serialize, Deserialize)]
pub struct Note {
    midi: u8,
    accidental: Option<Accidental>,
    octave: Octave,
}
impl Note {
    pub fn from_midi(midi: u8, accidental: Option<Accidental>) -> Self {
        Self {
            midi,
            accidental,
            octave: Octave::from_midi(midi),
        }
    }
    pub fn resolve(&self, key: Key) -> ResolvedNote {
        let (midi_note, octave) = Octave::split_midi(self.midi);
        // let midi_note = self.midi % 12;
        let notes_map = NotesMap::get();
        let res = self.resolve_by_accident(&notes_map, midi_note, octave);
        if res.is_some() {
            return res.unwrap();
        }
        let scale = key.resolve_scale(&notes_map);

        scale.resolve_pitch(notes_map, midi_note, octave)
    }
    fn resolve_by_accident(
        &self,
        notes_map: &NotesMap,
        midi_note: u8,
        octave: Octave,
    ) -> Option<ResolvedNote> {
        if self.accidental.is_some() {
            let acc = self.accidental.as_ref().unwrap();
            let note = notes_map.get_by_midi(&midi_note).get(&acc);
            if note.is_some() {
                return Some(ResolvedNote::new(
                    note.unwrap().clone(),
                    acc.clone(),
                    octave,
                    octave.apply_to_midi_note(midi_note),
                ));
            }
        }
        None
    }
    pub fn midi(&self) -> u8 {
        self.midi
    }
    pub fn set_midi(&mut self, midi: u8) {
        self.midi = midi;
    }
    pub fn accidental(&self) -> Option<Accidental> {
        self.accidental.clone()
    }
    pub fn set_accidental(&mut self, accidental: Option<Accidental>) {
        self.accidental = accidental;
    }
}
impl From<u8> for Note {
    /// from midi, but without boilerplate.
    fn from(midi: u8) -> Self {
        Self::from_midi(midi, None)
    }
}
impl From<ResolvedNote> for Note {
    fn from(value: ResolvedNote) -> Self {
        Self {
            midi: value.midi,
            accidental: Some(value.accidental),
            octave: value.octave,
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Hash, Eq, Clone, Copy, Serialize, Deserialize)]
pub enum Accidental {
    White,
    Sharp,
    DoubleSharp,
    Flat,
    DoubleFlat,
}
impl Accidental {
    pub fn to_string_by_note(&self, note: NoteName) -> String {
        match self {
            Self::DoubleFlat | Self::Flat => {
                if note.need_trunk() {
                    let mut s = self.to_string();
                    s.remove(0);
                    s
                } else {
                    self.to_string()
                }
            }
            _ => self.to_string(),
        }
    }
    /// "es" is Flat, "is" is Sharp, "white" is White.
    pub fn from_str(name: &str) -> Option<Self> {
        match name.to_lowercase().as_str() {
            "s" | "es" => Some(Self::Flat),
            "eses" => Some(Self::DoubleFlat),
            "is" => Some(Self::Sharp),
            "isis" => Some(Self::DoubleSharp),
            "white" => Some(Self::White),
            _ => None,
        }
    }
}
impl Default for Accidental {
    fn default() -> Self {
        Self::White
    }
}
impl ToString for Accidental {
    fn to_string(&self) -> String {
        match self {
            Accidental::White => "white".to_string(),
            Accidental::Sharp => "is".to_string(),
            Accidental::DoubleSharp => "isis".to_string(),
            Accidental::Flat => "es".to_string(),
            Accidental::DoubleFlat => "eses".to_string(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Serialize, Deserialize)]
pub struct Key {
    pub tonic: (NoteName, Accidental),
    pub scale: Scale,
}
impl Key {
    pub fn new(note: NoteName, accidental: Accidental, scale: Scale) -> Self {
        Self {
            tonic: (note, accidental),
            scale,
        }
    }
    /// `[root_midi, root_index]`
    ///
    /// `root_midi` is `u8` in rage 0..12, based on note name and accidental.
    /// `root_index` is `u8` in rage 0..7, based only on note name.
    pub fn get_root(&self) -> [u8; 2] {
        let (tonic_note, tonic_acc) = self.tonic.clone();
        let root_midi = NotesMap::get().get_by_note(tonic_note.clone(), tonic_acc);
        let root_idx = tonic_note.index();
        [root_midi, root_idx]
    }
    /// Returns concrete Scale representation (grades) for the key.
    fn resolve_scale(&self, notes_map: &NotesMap) -> ResolvedScale {
        self.scale.resolve_for_key(notes_map, self)
    }
    /// "as" or "eis" — without octave.
    pub fn from_str(name: &str, scale: Scale) -> Option<Self> {
        Some(Self {
            tonic: note_from_str(name)?,
            scale,
        })
    }
}
impl Default for Key {
    fn default() -> Self {
        Self::new(Default::default(), Default::default(), Default::default())
    }
}
impl From<(&String, Scale)> for Key {
    fn from(value: (&String, Scale)) -> Self {
        let (name, scale) = value;
        Self {
            tonic: note_from_str(name).unwrap(),
            scale,
        }
    }
}

fn note_from_str(name: &str) -> Option<(NoteName, Accidental)> {
    let note = &name[0..1];
    match NoteName::from_str(note) {
        Some(note) => {
            if name.len() <= 1 {
                return Some((note, Accidental::White));
            } else {
                match Accidental::from_str(&name[1..]) {
                    Some(acc) => Some((note, acc)),
                    None => None,
                }
            }
        }
        None => None,
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy, PartialOrd, Serialize, Deserialize)]
pub enum NoteName {
    C,
    D,
    E,
    F,
    G,
    A,
    B,
}
impl NoteName {
    fn need_trunk(&self) -> bool {
        match self {
            Self::A | Self::E => true,
            _ => false,
        }
    }
    fn index(self) -> u8 {
        match self {
            Self::C => 0,
            Self::D => 1,
            Self::E => 2,
            Self::F => 3,
            Self::G => 4,
            Self::A => 5,
            Self::B => 6,
        }
    }
    pub fn from_str(name: &str) -> Option<Self> {
        match name.to_uppercase().as_str() {
            "C" => Some(Self::C),
            "D" => Some(Self::D),
            "E" => Some(Self::E),
            "F" => Some(Self::F),
            "G" => Some(Self::G),
            "A" => Some(Self::A),
            "B" => Some(Self::B),
            _ => None,
        }
    }
    fn by_index(mut index: u8) -> Self {
        let names = [
            Self::C,
            Self::D,
            Self::E,
            Self::F,
            Self::G,
            Self::A,
            Self::B,
        ];
        if index > 6 {
            index -= 7;
        }
        names[index as usize].clone()
    }
}
impl Default for NoteName {
    fn default() -> Self {
        Self::C
    }
}
impl ToString for NoteName {
    fn to_string(&self) -> String {
        match self {
            Self::C => String::from("c"),
            Self::D => String::from("d"),
            Self::E => String::from("e"),
            Self::F => String::from("f"),
            Self::G => String::from("g"),
            Self::A => String::from("a"),
            Self::B => String::from("b"),
        }
    }
}

/// Concrete Scale representation, based on the given root note.
///
/// Used to estimate alteration for notes, can be alterated by scale rules.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ResolvedScale {
    pub key: Key,
    /// 0..12 midi bytes, representing every grade of the scale.
    /// E.g. for C-Dur it would be: `[0, 2, 4, 5, 7, 9, 11]`,
    /// and for A-moll: `[9, 11, 0, 2, 4, 5, 7]`
    pub degree_midi: [u8; 7],
    /// the same, but, holding Tuples of note names and accidentals.
    pub degree_notes: [(NoteName, Accidental); 7],
    /// Collects accidentals, used for degrees to estimate, which
    /// alteration of non-degree notes is preferable.
    /// TODO: think on using this as weight.
    pub used_accidentals: Vec<Accidental>,
}
impl ResolvedScale {
    pub fn resolve_pitch(
        &self,
        notes_map: &NotesMap,
        midi_note: u8,
        octave: Octave,
    ) -> ResolvedNote {
        let note: (NoteName, Accidental);
        let key_root_midi = self.key.get_root()[0];
        match self.degree_midi.binary_search(&midi_note) {
            Ok(note_index) => {
                note = self.degree_notes[note_index];
                // println!("found note from scale {:?} at index {:?}", note, note_index);
            }
            Err(_err) => {
                if midi_note == key_root_midi + 1 && self.key.scale == Scale::Minor {
                    // println!("that is minor II♭");
                    note = notes_map
                        .resolve_note_for_midi(self.degree_notes[1 as usize], midi_note)
                        .unwrap();
                } else if midi_note == (key_root_midi + 8) % 12 && self.key.scale == Scale::Major {
                    // println!("that is major VI♭");
                    let candidate_note = self.degree_notes[5 as usize];
                    note = notes_map
                        .resolve_note_for_midi(candidate_note, midi_note)
                        .unwrap();
                } else {
                    // println!("just search for anything for midi: {:?}", midi_note);
                    note = notes_map
                        .resolve_enharmonic(self.used_accidentals.last().copied(), midi_note);
                }
            }
        }
        ResolvedNote::new(note.0, note.1, octave, octave.apply_to_midi_note(midi_note))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Serialize, Deserialize)]
pub enum Scale {
    Major,
    Minor,
    Dorian,
    Phrygian,
    Lydian,
    Mixolidyan,
    Locrian,
}
impl Scale {
    /// Get interval structure of the scale.
    pub fn structure(&self) -> [u8; 6] {
        match self {
            Self::Major => [2, 2, 1, 2, 2, 2],
            Self::Minor => [2, 1, 2, 2, 1, 2],
            Self::Dorian => [2, 1, 2, 2, 2, 1],
            Self::Phrygian => [1, 2, 2, 2, 1, 2],
            Self::Lydian => [2, 2, 2, 1, 2, 2],
            Self::Mixolidyan => [2, 2, 1, 2, 2, 1],
            Self::Locrian => [1, 2, 2, 1, 2, 2],
        }
    }
    /// Make concrete representation, based on given Key.
    fn resolve_for_key(&self, notes_map: &NotesMap, key: &Key) -> ResolvedScale {
        let root = key.get_root();
        let (root_midi, _root_idx) = (root[0], root[1]);
        let mut degree_midi = [root_midi; 7];
        let mut degree_notes = [key.tonic; 7];
        let mut used_accidentals = Vec::with_capacity(10);
        for (idx, interval) in self.structure().iter().enumerate() {
            let mut next_midi = degree_midi[idx];
            next_midi += interval;
            if next_midi > 11 {
                next_midi %= 12;
            }
            let index = degree_notes[idx].0.index() + 1;
            let candidate_note = NoteName::by_index(index);
            // println!(
            //     "candidate_note: {:?}, next_midi: {:?}",
            //     candidate_note, next_midi
            // );
            let next_note = notes_map
                .resolve_note_for_midi((candidate_note, Accidental::White), next_midi)
                .unwrap();
            degree_notes[idx + 1] = next_note;
            degree_midi[idx + 1] = next_midi;
            if next_note.1 != Accidental::White {
                used_accidentals.push(next_note.1);
            }
            // println!(
            //     "next_note: {:?}, degree_notes: {:?}, degree_midi: {:?}, used_accidentals: {:?}",
            //     next_note, degree_notes, degree_midi, used_accidentals
            // )
        }
        ResolvedScale {
            key: *key,
            degree_midi,
            degree_notes,
            used_accidentals,
        }
    }
}
impl Default for Scale {
    fn default() -> Self {
        Self::Major
    }
}
impl ToString for Scale {
    fn to_string(&self) -> String {
        match self {
            Self::Major => "major".to_string(),
            Self::Minor => "minor".to_string(),
            Self::Dorian => "dorian".to_string(),
            Self::Phrygian => "phrygian".to_string(),
            Self::Lydian => "lydian".to_string(),
            Self::Mixolidyan => "mixolidyan".to_string(),
            Self::Locrian => "locrian".to_string(),
        }
    }
}

/// Effectively maps notes to midi and backwards.
///
/// Example
/// ```
/// use musical_note::{NotesMap, NoteName, Accidental};
/// let map = NotesMap::get();
/// assert_eq!(map.get_by_note(NoteName::F, Accidental::DoubleFlat), 3);
/// ```
#[derive(Debug)]
pub struct NotesMap {
    note_index: HashMap<(NoteName, Accidental), u8>,
    midi_index: HashMap<u8, HashMap<Accidental, NoteName>>,
}
impl NotesMap {
    pub fn get() -> &'static NotesMap {
        if _NOTES_MAP.get().is_none() {
            _NOTES_MAP.set(Self::new()).unwrap();
        }
        _NOTES_MAP.get().expect("logger is not initialized")
    }
    // fn empty() -> Self {}
    fn new() -> Self {
        let mut obj = Self {
            note_index: HashMap::with_capacity(35),
            midi_index: HashMap::with_capacity(35),
        };
        obj.add(NoteName::C, Accidental::White, 0);
        obj.add(NoteName::B, Accidental::Sharp, 0);
        obj.add(NoteName::D, Accidental::DoubleFlat, 0);
        //
        obj.add(NoteName::C, Accidental::Sharp, 1);
        obj.add(NoteName::B, Accidental::DoubleSharp, 1);
        obj.add(NoteName::D, Accidental::Flat, 1);
        //
        obj.add(NoteName::D, Accidental::White, 2);
        obj.add(NoteName::C, Accidental::DoubleSharp, 2);
        obj.add(NoteName::E, Accidental::DoubleFlat, 2);
        //
        obj.add(NoteName::D, Accidental::Sharp, 3);
        obj.add(NoteName::E, Accidental::Flat, 3);
        obj.add(NoteName::F, Accidental::DoubleFlat, 3);
        //
        obj.add(NoteName::E, Accidental::White, 4);
        obj.add(NoteName::D, Accidental::DoubleSharp, 4);
        obj.add(NoteName::F, Accidental::Flat, 4);
        //
        obj.add(NoteName::F, Accidental::White, 5);
        obj.add(NoteName::E, Accidental::Sharp, 5);
        obj.add(NoteName::G, Accidental::DoubleFlat, 5);
        //
        obj.add(NoteName::F, Accidental::Sharp, 6);
        obj.add(NoteName::E, Accidental::DoubleSharp, 6);
        obj.add(NoteName::G, Accidental::Flat, 6);
        //
        obj.add(NoteName::G, Accidental::White, 7);
        obj.add(NoteName::F, Accidental::DoubleSharp, 7);
        obj.add(NoteName::A, Accidental::DoubleFlat, 7);
        //
        obj.add(NoteName::G, Accidental::Sharp, 8);
        obj.add(NoteName::A, Accidental::Flat, 8);
        //
        obj.add(NoteName::A, Accidental::White, 9);
        obj.add(NoteName::G, Accidental::DoubleSharp, 9);
        obj.add(NoteName::B, Accidental::DoubleFlat, 9);
        //
        obj.add(NoteName::A, Accidental::Sharp, 10);
        obj.add(NoteName::B, Accidental::Flat, 10);
        obj.add(NoteName::C, Accidental::DoubleFlat, 10);
        //
        obj.add(NoteName::B, Accidental::White, 11);
        obj.add(NoteName::A, Accidental::DoubleSharp, 11);
        obj.add(NoteName::C, Accidental::Flat, 11);
        //
        obj
    }
    fn add(&mut self, note: NoteName, acc: Accidental, midi: u8) {
        self.note_index.insert((note.clone(), acc.clone()), midi);
        match self.midi_index.get_mut(&midi) {
            Some(map) => {
                map.insert(acc, note);
            }
            None => {
                let mut map = HashMap::with_capacity(4);
                map.insert(acc, note);
                self.midi_index.insert(midi, map);
            }
        }
    }
    pub fn get_by_midi(&self, midi: &u8) -> &HashMap<Accidental, NoteName> {
        let midi = midi % 12;
        self.midi_index.get(&midi).unwrap()
    }
    pub fn get_by_note(&self, note: NoteName, acc: Accidental) -> u8 {
        *self.note_index.get(&(note, acc)).unwrap()
    }
    /// Accidental of "root" is not counted.
    ///
    /// For the given note and MIDI-byte finds only possible enharmonic variant,
    /// or returns None.
    ///
    /// For example: with the given `NoteName::C` and midi `2`, only possible
    /// enharmonic variant is `(NoteName::C, Accidental::DoubleSharp)`, which
    /// will be returned.
    ///
    /// If None is returned — something goes very wrong and it is better to panic.
    pub fn resolve_note_for_midi(
        &self,
        note: (NoteName, Accidental),
        midi: u8,
    ) -> Option<(NoteName, Accidental)> {
        let notes = self.get_by_midi(&midi);
        for (acc, candidate) in notes.iter() {
            if candidate == &note.0 {
                return Some((*candidate, *acc));
            }
        }
        None
    }
    pub fn resolve_enharmonic(
        &self,
        accidental: Option<Accidental>,
        midi: u8,
    ) -> (NoteName, Accidental) {
        let notes = self.get_by_midi(&(midi % 12));
        // println!(
        //     "resolve_enharmonic:(accidental: {:?}, midi: {:?}, notes: {:?})",
        //     accidental, midi, notes
        // );
        let acc_final: Accidental;
        match accidental {
            Some(acc) => {
                if notes.contains_key(&acc) {
                    acc_final = acc;
                } else {
                    acc_final = Accidental::White;
                }
            }
            None => {
                acc_final = Accidental::White;
            }
        }
        (notes[&acc_final], acc_final)
    }
}
