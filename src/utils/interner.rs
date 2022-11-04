use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt::Debug,
    mem::MaybeUninit,
    slice::from_raw_parts,
    str,
};
pub mod branded;
mod interned;

use crate::utils::interner::branded::Identifier;

use self::unsafe_str::UnsafeValidStr;

pub use interned::Interned;

mod unsafe_str {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub(crate) struct UnsafeValidStr(&'static str);

    impl UnsafeValidStr {
        ///SAFTEY: The string must be valid as long as Self exists
        pub(crate) unsafe fn new(text: &str) -> Self {
            Self(&*(text as *const str))
        }
        ///SAFTEY: Must make sure the unbound lifetime doesnt outlive the storage of the str
        pub(crate) unsafe fn get<'unbound>(self) -> &'unbound str {
            self.0
        }
    }
}

// https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html
pub struct Interner<Brand> {
    map: RefCell<HashMap<UnsafeValidStr, Interned<Brand>>>,
    idents: RefCell<Vec<UnsafeValidStr>>,
    arena: StrArena,
}

impl<Brand: Debug + Default> std::fmt::Debug for Interner<Brand> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map()
            .entries(
                self.idents
                    .borrow()
                    .iter()
                    .enumerate()
                    .map(|(i, s)| (Interned::<Brand>::new(i as u32), unsafe { s.get() })),
            )
            .finish()
    }
}
unsafe impl Send for StrArena {}

impl<Brand> Interner<Brand> {
    pub fn new() -> Self {
        Interner {
            map: Default::default(),
            idents: Default::default(),
            arena: StrArena::with_capacity(4096),
        }
    }
    pub fn debug_dump_strs(&self) {
        let idents: &[_] = &self.idents.borrow();
        for (ident, &text) in idents.iter().enumerate() {
            println!("{:?}, {:?}", Identifier::new(ident as u32), unsafe {
                text.get()
            });
        }
    }

    pub fn intern(&self, text: &str) -> Interned<Brand> {
        //SAFTEY text is valid as long as self and therefore valid under the hash
        if let Some(id) = self.get_ident(text) {
            return id;
        }

        let id = Interned::new(
            self.map
                .borrow()
                .len()
                .try_into()
                .expect("can't have more than u32 idents"),
        );

        //SAFTEY: the allocated str is valid as long as the buffer and we only use it while self is alive
        let interned_text = unsafe { UnsafeValidStr::new(self.arena.alloc(text)) };
        self.map.borrow_mut().insert(interned_text, id);
        self.idents.borrow_mut().push(interned_text);

        //SAFTEY the arena lives
        unsafe {
            debug_assert!(self.lookup(id) == interned_text.get());
            debug_assert!(self.intern(interned_text.get()) == id);
        }

        id
    }

    pub fn get_ident(&self, text: &str) -> Option<Interned<Brand>> {
        self.map
            .borrow()
            .get(&unsafe { UnsafeValidStr::new(text) })
            .copied()
    }

    pub fn lookup(&self, ident: Interned<Brand>) -> &str {
        // SAFTEY: the lifetime of the arena is the same as &self
        // and the UnsafeValidStrs are valid as long as the arena lives
        unsafe { self.idents.borrow()[ident.value() as usize].get() }
    }
    pub fn arena_space_capacity(&self) -> (usize, usize) {
        (
            self.arena.avalible_cap(),
            self.arena.chunks.borrow().last().unwrap().elements.len(),
        )
    }
}

//modeled after the dropless arena in rustc
struct StrArena {
    ///points to unwritten byte in self.chunks.last()
    start: Cell<*mut u8>,
    ///points to byte after last valid byte self.chunks.last()
    end: Cell<*mut u8>,
    chunks: RefCell<Vec<ArenaChunk>>,
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
            start: Cell::new(start),
            end: Cell::new(end),
            chunks: RefCell::new(vec![chunk]),
        }
    }

    //SAFTEY: Can't allocate if text is in the arena
    unsafe fn alloc<'arena>(&'arena self, text: &'_ str) -> &'arena str {
        let len = text.len();

        if !self.has_space(len) {
            self.grow(len);
        }

        let start_of_alloc = self.start.get();

        text.as_ptr().copy_to_nonoverlapping(start_of_alloc, len);

        // the grow ensures there is space for str and as such safe to offset
        self.start.set(start_of_alloc.offset(len as isize));

        str::from_utf8_unchecked(from_raw_parts(start_of_alloc, len))
    }

    fn has_space(&self, needed: usize) -> bool {
        needed <= self.avalible_cap()
    }

    fn grow(&self, additional: usize) {
        let old_cap = self.chunks.borrow().last().unwrap().capacity();

        let new_cap = (old_cap.max(additional) + 1).next_power_of_two();

        let mut new_chunk = ArenaChunk::new(new_cap);

        let (start, end) = new_chunk.as_ptr_pair();

        self.chunks.borrow_mut().push(new_chunk);
        self.start.set(start);
        self.end.set(end);
    }

    fn avalible_cap(&self) -> usize {
        unsafe {
            self.end
                .get()
                .offset_from(self.start.get())
                .try_into()
                .unwrap()
        }
    }
}
