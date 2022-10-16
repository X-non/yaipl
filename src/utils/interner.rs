use std::{
    cell::Cell, collections::HashMap, mem::MaybeUninit, slice::from_raw_parts,
    str::from_utf8_unchecked,
};

// https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct UnsafeValidStr(&'static str);

impl UnsafeValidStr {
    ///SAFTEY: The string must be valid as long as Self exists
    unsafe fn new(text: &str) -> Self {
        Self(&*(text as *const str))
    }
}

pub struct Interner {
    map: HashMap<UnsafeValidStr, Ident>,
    idents: Vec<UnsafeValidStr>,
    buffer: StrArena,
}

impl Interner {
    pub fn intern<'a>(&'a mut self, text: &'a str) -> Ident {
        //SAFTEY text is valid as long as self and therefore valid under the hash
        if let Some(&id) = self.map.get(&unsafe { UnsafeValidStr::new(text) }) {
            return id;
        }

        let id = Ident(
            self.map
                .len()
                .try_into()
                .expect("can't have more than u32 idents"),
        );

        //SAFTEY: the allocated str is valid as long as the buffer and we only use it while self is alive
        let interned_text = unsafe { UnsafeValidStr::new(self.buffer.alloc(text)) };
        self.map.insert(interned_text, id);
        self.idents.push(interned_text);

        debug_assert!(self.lookup(id) == interned_text.0);
        debug_assert!(self.intern(interned_text.0) == id);

        id
    }

    pub fn lookup(&self, ident: Ident) -> &str {
        self.idents[ident.0 as usize].0
    }
}

//modeled after the dropless arena in rustc
struct StrArena {
    ///points to unwritten byte in self.chunks.last()
    start: *mut u8,
    ///points to byte after last valid byte self.chunks.last()
    end: *mut u8,
    chunks: Vec<ArenaChunk>,
}

struct ArenaChunk {
    elements: Box<[MaybeUninit<u8>]>,
}

impl ArenaChunk {
    fn new(capacity: usize) -> Self {
        Self {
            elements: Box::new_uninit_slice(capacity),
        }
    }
    fn as_ptr_pair(&mut self) -> (*mut u8, *mut u8) {
        let a = self.elements.as_mut_ptr_range();
        (a.start as *mut _, a.end as *mut _)
    }

    fn capacity(&self) -> usize {
        self.elements.len()
    }
}

impl StrArena {
    fn with_capacity(capacity: usize) -> Self {
        let mut chunk = ArenaChunk::new(capacity);
        let (start, end) = chunk.as_ptr_pair();
        Self {
            start,
            end,
            chunks: vec![chunk],
        }
    }

    //SAFTEY: Can't allocate if text is in the arena
    unsafe fn alloc<'arena>(&'arena mut self, text: &'_ str) -> &'arena str {
        let len = text.len();

        if !self.has_space(len) {
            self.grow(len);
        }

        let start_of_alloc = self.start;

        text.as_ptr().copy_to_nonoverlapping(start_of_alloc, len);

        // the grow ensures there is space for str and as such safe to offset
        self.start = start_of_alloc.offset(len as isize);

        std::str::from_utf8_unchecked(from_raw_parts(start_of_alloc, len))
    }

    fn has_space(&self, needed: usize) -> bool {
        let capacity = self.avalible_cap();
        needed <= capacity
    }

    fn grow(&mut self, additional: usize) {
        let old_cap = self.current_chunk().capacity();
        let new_cap = (old_cap.max(additional) + 1).next_power_of_two();

        let mut new_chunk = ArenaChunk::new(new_cap);

        let (start, end) = new_chunk.as_ptr_pair();

        self.chunks.push(new_chunk);
        self.start = start;
        self.end = end;
    }

    fn current_chunk(&self) -> &ArenaChunk {
        self.chunks.last().unwrap()
    }

    fn avalible_cap(&self) -> usize {
        unsafe { self.end.offset_from(self.start).try_into().unwrap() }
    }
}
