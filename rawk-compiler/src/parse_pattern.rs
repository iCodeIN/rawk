use crate::lex::*;
use crate::parse::ParseError;
use std::collections::VecDeque;
use crate::parse::Parse;

struct BlockLex<'t, 'a: 't> {
	tokens: &'t mut Tokens<'a>,
}

impl<'t, 'a: 't> Iterator for BlockLex<'t, 'a> {
	type Item = Token;
	fn next(&mut self) -> Option<Token> {
		let t = self.tokens.next()?;
		if t.data == TokenData::CloseCurley && self.tokens.curley_count() == 0 {
			return None;
		}
		Some(t)
	}
}

pub struct PatternParser<'a> {
	tokens: Tokens<'a>,
}

impl<'a> PatternParser<'a> {
	pub fn new(code: &'a [u8], personality: Option<Personality>) -> PatternParser<'a> {
		PatternParser {
			tokens: Tokens::new(code, personality)
		}
	}
	fn parse_pattern(&mut self, pattern: &[Token]) -> Pattern {
		match pattern {
			&[token] if token.data == TokenData::Regex => {
				Pattern::Regex(token)
			}
			expr => {
				let parser = Parse::new(expr.iter().cloned());
				Pattern::ParsedExpr(parser.collect())
			}
		}
	}
	pub fn next<'b>(&'b mut self) -> Option<PatternBlock<'b, 'a>> {
		let mut pattern_tokens: Vec<Token> = Vec::new();
		let mut patterns = Vec::new();
		assert_eq!(self.tokens.curley_count(), 0);
		while let Some(token) = self.tokens.next() {
			if token.data == TokenData::OpenCurley {
				let this = self.parse_pattern(&std::mem::replace(&mut pattern_tokens, Vec::new()));
				patterns.push(this);
				break;
			} else if token.data == TokenData::Comma {
				let this = self.parse_pattern(&std::mem::replace(&mut pattern_tokens, Vec::new()));
				patterns.push(this);
			} else if token.data == TokenData::LineBreak && self.tokens.round_count() == 0 && self.tokens.square_count() == 0 {
				break;
			} else {
				pattern_tokens.push(token);
			}
		}
		if patterns.is_empty() {
			if pattern_tokens.is_empty() {
				return None;
			} else {
				let token = pattern_tokens.last().unwrap().clone();
				let this = self.parse_pattern(&std::mem::replace(&mut pattern_tokens, Vec::new()));
				patterns.push(this);
				return Some(PatternBlock {
					patterns: patterns.into_iter(),
					block: PatternBlockInner::Print0(token),
				})
			}
		}
		Some(PatternBlock {
			patterns: patterns.into_iter(),
			block: PatternBlockInner::Block(
				Parse::new((BlockLex { tokens: &mut self.tokens }).fuse())
			),
		})
	}
}

enum PatternBlockInner<'t, 'a: 't> {
	Block(Parse<std::iter::Fuse<BlockLex<'t, 'a>>>),
	Print0(Token),
	None,
}

pub struct PatternBlock<'t, 'a: 't> {
	patterns: std::vec::IntoIter<Pattern>,
	block: PatternBlockInner<'t, 'a>
}

impl<'t, 'a: 't> PatternBlock<'t, 'a> {
	pub fn next_pattern(&mut self) -> Option<Pattern> {
		self.patterns.next()
	}
}

impl<'t, 'a: 't> Iterator for PatternBlock<'t, 'a> {
	type Item = Token;
	fn next(&mut self) -> Option<Token> {
		match self.block {
			PatternBlockInner::Block(ref mut block) => block.next(),
			PatternBlockInner::Print0(token) => {
				self.block = PatternBlockInner::None;
				Some(Token {
					data: TokenData::Print0,
					..token
				})
			}
			PatternBlockInner::None => None
		}
	}
}

#[derive(Debug)]
pub enum Pattern {
	Regex(Token),
	ParsedExpr(Vec<Token>),
}

impl Pattern {
	pub fn is_regex(&self) -> bool {
		match self {
			&Pattern::Regex(_) => true,
			_ => false,
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::lex::*;
	#[test]
	fn basic_regex() {
		let mut pp = PatternParser::new(b"/test/ { print }", None);
		let mut pb = pp.next().expect("at least one pattern block");
		assert!(pb.next_pattern().unwrap().is_regex());
		assert_eq!(pb.next().unwrap().data, TokenData::Print);
	}
	#[test]
	fn basic_expr() {
		let mut pp = PatternParser::new(b"$1 == 1 { print }", None);
		let mut pb = pp.next().expect("at least one pattern block");
		assert!(!pb.next_pattern().unwrap().is_regex());
		assert_eq!(pb.next().unwrap().data, TokenData::Print);
	}
	#[test]
	fn compound_expr() {
		let mut pp = PatternParser::new(b"/test/, $1 == 1 { print }", None);
		let mut pb = pp.next().expect("at least one pattern block");
		assert!(pb.next_pattern().unwrap().is_regex());
		assert!(!pb.next_pattern().unwrap().is_regex());
		assert_eq!(pb.next().unwrap().data, TokenData::Print);
	}
	#[test]
	fn nested_action() {
		let mut pp = PatternParser::new(b"/test/ { if 1 { print } }", None);
		let mut pb = pp.next().expect("at least one pattern block");
		assert_eq!(pb.next().unwrap().data, TokenData::If);
	}
	#[test]
	fn double_pattern_block() {
		let mut pp = PatternParser::new(b"/test/ { print } 1 { 1 }", None);
		let mut pb = pp.next().expect("at least one pattern block");
		assert!(pb.next_pattern().unwrap().is_regex());
		assert_eq!(pb.next().unwrap().data, TokenData::Print);
		let mut pb = pp.next().expect("at least two pattern blocks");
		assert!(!pb.next_pattern().unwrap().is_regex());
		assert_eq!(pb.next().unwrap().data, TokenData::Word);
	}
}
