mod keyword;
mod kind;
mod scanner;
#[cfg(test)]
mod test;
mod token;

type Span = std::ops::Range<usize>;
pub use crate::keyword::KeyWord;
pub use crate::kind::TokenKind;
pub use crate::scanner::Scanner;
pub use crate::token::Token;
