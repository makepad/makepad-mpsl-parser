pub trait CharExt {
    fn is_identifier_start(self) -> bool;
    fn is_identifier_part(self) -> bool;
    fn is_octal_digit(self) -> bool;
    fn is_decimal_digit(self) -> bool;
    fn is_hex_digit(self) -> bool;
}

impl CharExt for char {
    fn is_identifier_start(self) -> bool {
        match self {
            'A'..='Z' | '_' | 'a'..='z' => true,
            _ => false,
        }
    }

    fn is_identifier_part(self) -> bool {
        match self {
            '0'..='9' | 'A'..='Z' | '_' | 'a'..='z' => true,
            _ => false,
        }
    }

    fn is_octal_digit(self) -> bool {
        match self {
            '0'..='7' => true,
            _ => false,
        }
    }

    fn is_decimal_digit(self) -> bool {
        match self {
            '0'..='9' => true,
            _ => false,
        }
    }

    fn is_hex_digit(self) -> bool {
        match self {
            '0'..='7' | 'A'..='F' | 'a'..='f' => true,
            _ => false,
        }
    }
}
