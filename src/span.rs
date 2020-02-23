pub trait FromInnerAndSpan {
    type Inner;

    fn from_inner_and_span(inner: Self::Inner, span: Span) -> Self;
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }
}
