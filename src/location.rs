#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl Location {
    pub fn new(line: usize, column: usize) -> Location {
        Location { line, column }
    }

    pub fn from_chars_and_index(chars: &[char], index: usize) -> Location {
        let mut previous_was_cr = false;
        let mut line = 1;
        let mut column = 1;
        for &ch in &chars[..index] {
            match ch {
                '\n' => {
                    if !previous_was_cr {
                        line += 1;
                        column = 1;
                    }
                    previous_was_cr = false;
                }
                '\r' => {
                    previous_was_cr = true;
                    line += 1;
                    column = 1;
                }
                _ => {
                    previous_was_cr = false;
                    column += 1
                }
            }
        }
        Location { line, column }
    }
}
