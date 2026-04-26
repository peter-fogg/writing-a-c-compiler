use std::collections::HashMap;

#[derive(Debug)]
pub struct Interner(HashMap<String, Symbol>, Vec<String>);

pub type Symbol = usize;

impl Interner {
    pub fn new() -> Self {
        Self(HashMap::new(), Vec::new())
    }

    pub fn intern(&mut self, s: String) -> Symbol {
        match self.0.get(&s) {
            Some(idx) => *idx,
            None => {
                let idx = self.1.len();
                self.0.insert(s.clone(), idx);
                self.1.push(s);
                idx
            }
        }
    }

    pub fn get_symbol(&self, idx: Symbol) -> &str {
        self.1.get(idx).unwrap()
    }

    pub fn get_str(&self, s: &str) -> Symbol {
        *self.0.get(s).unwrap()
    }
}
