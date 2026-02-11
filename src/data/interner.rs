use super::ordered_map::OrderedMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(pub u32);

#[derive(Debug, Clone, Default)]
pub struct StringInterner {
    symbol_by_text: OrderedMap<String, Symbol>,
    text_by_symbol: Vec<String>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        self.text_by_symbol.len()
    }

    pub fn is_empty(&self) -> bool {
        self.text_by_symbol.is_empty()
    }

    pub fn get_or_intern(&mut self, text: &str) -> Symbol {
        if let Some(symbol) = self.symbol_by_text.get(&text.to_string()) {
            return *symbol;
        }
        let symbol = Symbol(self.text_by_symbol.len() as u32);
        let owned = text.to_string();
        self.symbol_by_text.insert(owned.clone(), symbol);
        self.text_by_symbol.push(owned);
        symbol
    }

    pub fn resolve(&self, symbol: Symbol) -> Option<&str> {
        self.text_by_symbol
            .get(symbol.0 as usize)
            .map(String::as_str)
    }

    pub fn get(&self, text: &str) -> Option<Symbol> {
        self.symbol_by_text.get(&text.to_string()).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::StringInterner;

    #[test]
    fn interner_round_trips_and_deduplicates() {
        let mut interner = StringInterner::new();
        let a = interner.get_or_intern("alpha");
        let b = interner.get_or_intern("beta");
        let again = interner.get_or_intern("alpha");
        assert_eq!(a, again);
        assert_ne!(a, b);
        assert_eq!(interner.resolve(a), Some("alpha"));
        assert_eq!(interner.resolve(b), Some("beta"));
        assert_eq!(interner.len(), 2);
    }
}
