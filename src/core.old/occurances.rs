pub enum Occurances {
    Zero,
    One,
    Many,
    Ambiguous,
}

impl Add for Occurances {
    type Output = Occurances;

    fn add(self, other: Occurances) -> Occurances {
        match (self, other) {
            (Occurances::Zero, x) => x,
            (x, Occurances::Zero) => x,
            (Occurances::Many, _) => Occurances::Many,
            (_, Occurances::Many) => Occurances::Many,
            (Occurances::One, Occurances::One) => Occurances::Many,
            (Occurances::Ambiguous, Occurances::Ambiguous) => Occurances::Ambiguous,
            (Occurances::One, Occurances::Ambiguous) => Occurances::One,
            (Occurances::Ambiguous, Occurances::One) => Occurances::One,
        }
    }
}


