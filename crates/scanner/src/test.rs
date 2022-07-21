// use super::{KeyWord, Scanner, TokenType};
use super::Scanner;

// #[cfg(test)]
// use pretty_assertions::assert_eq;

#[test]
fn scanner_main() {
    let src = r#""#;
    let _scanner = Scanner::new(src);
    // assert_eq!(
    //     scanner.next().ok().map(|t| t.tok_type().clone()),
    //     Some(TokenType::KeyWord(KeyWord::Fn))
    // );
}

#[test]
fn scanner_add_func() {
    let src = r#""#;
    let _scanner = Scanner::new(src);
    // assert_eq!(
    //     scanner.next().ok().map(|t| t.tok_type().clone()),
    //     Some(TokenType::Ctrl('}'))
    // );
}
