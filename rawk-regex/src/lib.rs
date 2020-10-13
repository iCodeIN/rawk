
#![allow(unused)]

use regex::Regex;
use regex_syntax::{ast::Ast, hir::Hir};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Personality {
	Gnu,
	Posix,
	Traditional,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ParseError;

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
	Literal(regex_syntax::ast::Literal),
	StartBrace(bool),
	RangeHyphen,
	EndBrace,
	Terminator(u8),
	BeginAnchor,
	EndAnchor,
	AlternationPipe,
	StartParen,
	EndParen,
	Dot,
	Star,
	Plus,
	Question,
	FixedInterval(u32),
	RangeInterval(u32, u32),
	OpenInterval(u32),
	PosixClass(String), // for example, ":alpha:" corresponds to the regex "[[:alpha:]]"
}

impl Token {
	pub fn is_begin_anchor(&self) -> bool {
		match self {
			&Token::BeginAnchor => true,
			_ => false,
		}
	}
	pub fn is_end_anchor(&self) -> bool {
		match self {
			&Token::EndAnchor => true,
			_ => false,
		}
	}
	pub fn is_terminator(&self) -> bool {
		match self {
			&Token::Terminator(_) => true,
			_ => false,
		}
	}
	pub fn c(&self) -> char {
		match self {
			&Token::Literal(ref lit) => lit.c,
			_ => panic!(),
		}
	}
	pub fn is_start_paren(&self) -> bool {
		match self {
			&Token::StartParen => true,
			_ => false,
		}
	}
	pub fn is_end_paren(&self) -> bool {
		match self {
			&Token::EndParen => true,
			_ => false,
		}
	}
	pub fn is_start_brace(&self) -> bool {
		match self {
			&Token::StartBrace(_) => true,
			_ => false,
		}
	}
	pub fn is_complement(&self) -> bool {
		match self {
			&Token::StartBrace(complement) => complement,
			_ => panic!(),
		}
	}
	pub fn is_range_hyphen(&self) -> bool {
		match self {
			&Token::RangeHyphen => true,
			_ => false,
		}
	}
	pub fn is_alternation_pipe(&self) -> bool {
		match self {
			&Token::AlternationPipe => true,
			_ => false,
		}
	}
	pub fn is_end_brace(&self) -> bool {
		match self {
			&Token::EndBrace => true,
			_ => false,
		}
	}
}

pub struct ExtendedRegexTokens<I> {
	inner: I,
	peek: Vec<u8>,
	push_punct: Option<u8>,
	is_in_brace: bool,
	is_beginning: bool,
	paren_level: u32,
	terminator: Option<u8>,
	personality: Personality,
	pub offset: usize,
}

impl<I: Iterator<Item=u8>> ExtendedRegexTokens<I> {
	pub fn new(inner: I, personality: Personality, terminator: Option<u8>) -> ExtendedRegexTokens<I> {
		ExtendedRegexTokens {
			inner, personality, terminator,
			peek: Vec::new(),
			push_punct: None,
			is_in_brace: false,
			is_beginning: true,
			paren_level: 0,
			offset: 0,
		}
	}
	fn next_byte(&mut self) -> Option<u8> {
		if let Some(c) = self.peek.pop() {
			Some(c)
		} else {
			self.offset += 1;
			self.inner.next()
		}
	}
}

impl<I: Iterator<Item=u8>> Iterator for ExtendedRegexTokens<I> {
	type Item = Token;
	fn next(&mut self) -> Option<Token> {
		use regex_syntax::*;
		use regex_syntax::ast::*;
		// Hack.
		let span = Span {
			start: Position { offset: 0, line: 0, column: 0 },
			end: Position { offset: 0, line: 0, column: 0 },
		};
		if let Some(c) = self.push_punct.take() {
			self.is_beginning = false;
			return Some(Token::Literal(Literal {
				span,
				c: c as char,
				kind: LiteralKind::Punctuation,
			}));
		}
		let mut c = self.next_byte()?;
		if c == b'\\' {
			c = if let Some(c) = self.next_byte() {
				c
			} else {
				self.is_beginning = false;
				return Some(Token::Literal(Literal {
					span,
					c: '\\',
					kind: LiteralKind::Punctuation,
				}))
			};
			// https://www.gnu.org/software/gawk/manual/gawk.html#Escape-Sequences
			//
			// Unlike rust-regex, GNU AWK, and thus RAWK, treat unknown
			// backslash escapes as literals. It also follows POSIX's lead
			// with octal and metacharacters.
			let (c_, kind) = match c as char {
				'a' => (7, LiteralKind::Special(SpecialLiteralKind::Bell)),
				'b' => (8, LiteralKind::HexFixed(HexLiteralKind::X)), // not in rust-regex
				'f' => (12, LiteralKind::Special(SpecialLiteralKind::FormFeed)),
				'n' => (10, LiteralKind::Special(SpecialLiteralKind::LineFeed)),
				'r' => (13, LiteralKind::Special(SpecialLiteralKind::CarriageReturn)),
				't' => (9, LiteralKind::Special(SpecialLiteralKind::Tab)),
				'v' => (11, LiteralKind::Special(SpecialLiteralKind::VerticalTab)),
				'\\' | '$' | '^' | '.' | '[' | ']' | '-' => (c, LiteralKind::Punctuation),
				'x' if self.personality == Personality::Gnu => {
					// first character required
					let x = match self.next_byte() {
						Some(x) if x >= b'0' && x <= b'9' => x - b'0',
						Some(x) if x >= b'a' && x <= b'f' => x - b'a' + 10,
						Some(x) if x >= b'A' && x <= b'F' => x - b'A' + 10,
						x => {
							if let Some(x) = x {
								self.peek.push(x);
							}
							return None;
						},
					};
					// second character may or may not be hex
					let y = match self.next_byte() {
						Some(y) if y >= b'0' && y <= b'9' => (y - b'0') | (x << 4),
						Some(y) if y >= b'a' && y <= b'f' => (y - b'a' + 10) | (x << 4),
						Some(y) if y >= b'A' && y <= b'F' => (y - b'A' + 10) | (x << 4),
						y => {
							if let Some(y) = y {
								self.peek.push(y);
							}
							x
						},
					};
					(y, LiteralKind::HexFixed(HexLiteralKind::X))
				}
				'0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' => {
					let x = match c {
						x if x >= b'0' && x <= b'7' => x - b'0',
						_ => unreachable!(),
					};
					let y = self.next_byte();
					let y = match y {
						Some(y) if y >= b'0' && y <= b'7' => {
							let y = y - b'0';
							let z = self.next_byte();
							match z {
								Some(z) if z >= b'0' && z <= b'7' => {
									(((x << 3) | y) << 3) | (z - b'0')
								}
								z => {
									if let Some(z) = z {
										self.peek.push(z);
									}
									(x << 3) | y
								}
							}
						},
						y => {
							if let Some(y) = y {
								self.peek.push(y);
							}
							x
						},
					};
					(y, LiteralKind::Octal)
				}
				_ => (c, LiteralKind::Verbatim),
			};
			if self.personality == Personality::Traditional || c_ <= b' ' || (kind != LiteralKind::Octal && c != b'x') {
				self.is_beginning = false;
				return Some(Token::Literal(Literal {
					span,
					c: c_ as char,
					kind: kind,
				}));
			} else {
				c = c_;
			}
		}
		if c == b'[' {
			if self.is_in_brace {
				let mut local_peek = Vec::new();
				let mut matches_class_name = false;
				if let Some(c) = self.next_byte() {
					local_peek.push(c);
					if c == b':' {
						while let Some(c) = self.next_byte() {
							local_peek.push(c);
							if !(c >= b'a' && c <= b'z') {
								break;
							}
						}
						if local_peek.last() == Some(&b':') {
							if let Some(c) = self.next_byte() {
								local_peek.push(c);
							}
							if local_peek.last() == Some(&b']') {
								matches_class_name = true;
							}
						}
					}
				}
				if matches_class_name {
					assert_eq!(local_peek.pop(), Some(b']'));
					assert_eq!(local_peek.pop(), Some(b':'));
					assert_eq!(local_peek.remove(0), b':');
					return Some(Token::PosixClass(String::from_utf8(local_peek).expect("character class is valid utf-8")));
				}
				local_peek.reverse();
				self.peek.append(&mut local_peek);
				return Some(Token::Literal(Literal {
					span,
					c: '[',
					kind: LiteralKind::Punctuation,
				}))
			} else {
				let c = if let Some(c) = self.next_byte() {
					c
				} else {
					self.is_beginning = false;
					return Some(Token::Literal(Literal {
						span,
						c: '[',
						kind: LiteralKind::Punctuation,
					}))
				};
				let complement = match c {
					b']' | b'-' => {
						self.push_punct = Some(c);
						false
					}
					b'^' => {
						if let Some(peek) = self.next_byte() {
							if peek == b']' || peek == b'-' {
								self.push_punct = Some(peek);
							} else {
								self.peek.push(peek);
							}
						}
						true
					}
					_ => {
						self.peek.push(c);
						false
					}
				};
				self.is_in_brace = true;
				self.is_beginning = false;
				return Some(Token::StartBrace(complement));
			}
		}
		if c == b']' && self.is_in_brace {
			self.is_in_brace = false;
			self.is_beginning = false;
			return Some(Token::EndBrace);
		}
		if c == b'-' && self.is_in_brace {
			let peek = self.next_byte();
			if let Some(peek) = peek {
				self.peek.push(peek);
			}
			if peek == Some(b']') {
				self.is_beginning = false;
				return Some(Token::Literal(Literal {
					span,
					c: '-',
					kind: LiteralKind::Punctuation,
				}));
			} else {
				self.is_beginning = false;
				return Some(Token::RangeHyphen);
			}
		}
		if c == b'^' && !self.is_in_brace {
			self.is_beginning = true;
			return Some(Token::BeginAnchor);
		}
		if c == b'$' && !self.is_in_brace {
			self.is_beginning = true;
			return Some(Token::EndAnchor);
		}
		if c == b'|' && !self.is_in_brace {
			self.is_beginning = true;
			return Some(Token::AlternationPipe);
		}
		if c == b'.' && !self.is_in_brace {
			self.is_beginning = false;
			return Some(Token::Dot);
		}
		if c == b'*' && !self.is_in_brace {
			if self.is_beginning {
				self.is_beginning = false;
				return Some(Token::Literal(Literal {
					span,
					c: '*',
					kind: LiteralKind::Punctuation,
				}))
			} else {
				return Some(Token::Star);
			}
		}
		if c == b'+' && !self.is_in_brace {
			if self.is_beginning {
				self.is_beginning = false;
				return Some(Token::Literal(Literal {
					span,
					c: '+',
					kind: LiteralKind::Punctuation,
				}))
			} else {
				return Some(Token::Plus);
			}
		}
		if c == b'?' && !self.is_in_brace {
			if self.is_beginning {
				self.is_beginning = false;
				return Some(Token::Literal(Literal {
					span,
					c: '?',
					kind: LiteralKind::Punctuation,
				}))
			} else {
				return Some(Token::Question);
			}
		}
		if c == b'(' && !self.is_in_brace {
			self.paren_level += 1;
			self.is_beginning = true;
			return Some(Token::StartParen);
		}
		if c == b')' && !self.is_in_brace {
			self.is_beginning = false;
			if self.paren_level > 0 {
				self.paren_level -= 1;
				return Some(Token::EndParen);
			} else {
				return Some(Token::Literal(Literal {
					span,
					c: ')',
					kind: LiteralKind::Punctuation,
				}));
			}
		}
		if c == b'{' && !self.is_in_brace {
			if self.is_beginning || self.personality == Personality::Traditional {
				self.is_beginning = false;
				return Some(Token::Literal(Literal {
					span,
					c: '{',
					kind: LiteralKind::Punctuation,
				}));
			}
			let mut has_seen_comma = false;
			let mut local_peek = Vec::new();
			while let Some(c) = self.next_byte() {
				if c == b'}' {
					let parts = String::from_utf8(local_peek.clone()).expect(r"\d+,\d to be valid UTF-8");
					let mut parts = parts.split(',');
					let start = parts.next();
					let end = parts.next();
					match (start, end) {
						(Some(""), _) => local_peek.push(c),
						(Some(start), Some("")) => {
							let start: u32 = start.parse().expect("already checked valid int");
							return Some(Token::OpenInterval(start));
						}
						(Some(start), Some(end)) => {
							let start: u32 = start.parse().expect("already checked valid int");
							let end: u32 = end.parse().expect("already checked valid int");
							return Some(Token::RangeInterval(start, end));
						}
						(Some(start), None) => {
							let start: u32 = start.parse().expect("already checked valid int");
							return Some(Token::FixedInterval(start));
						}
						_ => local_peek.push(c),
					}
				} else if c >= b'0' && c <= b'9' {
					local_peek.push(c);
				} else if c == b',' && !has_seen_comma {
					has_seen_comma = true;
					local_peek.push(c);
				} else {
					local_peek.push(c);
					break;
				}
			}
			local_peek.reverse();
			self.peek.append(&mut local_peek);
			return Some(Token::Literal(Literal {
				span,
				c: '{',
				kind: LiteralKind::Punctuation,
			}));
		}
		if Some(c) == self.terminator {
			return Some(Token::Terminator(c));
		}
		self.is_beginning = false;
		Some(Token::Literal(Literal {
			span,
			c: c as char,
			kind: LiteralKind::Verbatim,
		}))
	}
}

pub fn convert(syntax: &[u8], personality: Personality) -> Result<String, ParseError> {
	let (ast, _) = extended_regex(syntax, personality, None)?;
	Ok(ast.to_string())
}

pub fn convert_terminated(syntax: &[u8], personality: Personality, terminator: u8) -> Result<(String, usize), ParseError> {
	let (ast, offset) = extended_regex(syntax, personality, Some(terminator))?;
	Ok((ast.to_string(), offset))
}

/// Convert [GNU AWK] regular expressions to Rust Regex Ast.
///
/// [GNU GAWK]: https://www.gnu.org/software/gawk/manual/gawk.html#Regexp
fn extended_regex(syntax: &[u8], personality: Personality, terminator: Option<u8>) -> Result<(Ast, usize), ParseError> {
	use regex_syntax::*;
	use regex_syntax::ast::*;
	let mut tok = ExtendedRegexTokens {
		inner: syntax.iter().cloned(),
		peek: Vec::new(),
		push_punct: None,
		is_in_brace: false,
		is_beginning: true,
		paren_level: 0,
		terminator,
		personality: personality,
		offset: 0,
	};
	let span = Span {
		start: Position { offset: 0, line: 0, column: 0 },
		end: Position { offset: 0, line: 0, column: 0 },
	};
	let mut ast = Ast::Empty(span);
	enum State {
		Bracket(bool, ClassSetItem, Vec<Literal>, bool),
		Alternation(Ast),
		Paren(Ast),
	}
	let mut state: Vec<State> = vec![];
	let mut capture_index = 0;
	let mut is_terminated = false;
	for t in &mut tok {
		match state.last_mut() {
			Some(&mut State::Bracket(negated, ref mut item, ref mut literals, ref mut after_hyphen)) => {
				match t {
					Token::Terminator(_) | Token::StartBrace(_) => return Err(ParseError),
					Token::EndBrace => {
						let item = std::mem::replace(item, ClassSetItem::Empty(span));
						let item = if literals.is_empty() {
							item
						} else if literals.len() == 1 && item == ClassSetItem::Empty(span) {
							ClassSetItem::Literal(literals.pop().expect("one item"))
						} else {
							let mut previous = std::mem::replace(literals, Vec::new())
								.into_iter()
								.map(ClassSetItem::Literal)
								.collect();
							let mut items = if let ClassSetItem::Empty(_) = item {
								previous
							} else {
								let mut items = vec![item];
								items.append(&mut previous);
								items
							};
							ClassSetItem::Union(ClassSetUnion {
								span, items
							})
						};
						let new_ast = Ast::Class(Class::Bracketed(ClassBracketed {
							span, negated,
							kind: ClassSet::Item(item)
						}));
						if ast.is_empty() {
							ast = new_ast;
						} else {
							ast = Concat {
								span,
								asts: vec![ast, new_ast],
							}.into_ast();
						}
						state.pop();
					},
					Token::Literal(lit) => {
						if *after_hyphen {
							let start = literals.pop().expect("range hyphen always follows at least one literal");
							let end = lit;
							let mut previous = std::mem::replace(literals, Vec::new())
								.into_iter()
								.map(ClassSetItem::Literal)
								.collect();
							let mut items = if let ClassSetItem::Empty(_) = item {
								previous
							} else {
								let mut items = vec![std::mem::replace(item, ClassSetItem::Empty(span))];
								items.append(&mut previous);
								items
							};
							let new_range = ClassSetItem::Range(ClassSetRange {
								span, start, end
							});
							if items.is_empty() {
								*item = new_range;
							} else {
								items.push(new_range);
								*item = ClassSetItem::Union(ClassSetUnion {
									span, items
								});
							}
						} else {
							literals.push(lit);
						}
						*after_hyphen = false;
					}
					Token::RangeHyphen => {
						*after_hyphen = true;
					}
					Token::PosixClass(name) => {
						let kind = match &name[..] {
							"alnum" => ClassAsciiKind::Alnum,
							"alpha" => ClassAsciiKind::Alpha,
							"blank" => ClassAsciiKind::Blank,
							"cntrl" => ClassAsciiKind::Cntrl,
							"digit" => ClassAsciiKind::Digit,
							"graph" => ClassAsciiKind::Graph,
							"lower" => ClassAsciiKind::Lower,
							"print" => ClassAsciiKind::Print,
							"punct" => ClassAsciiKind::Punct,
							"space" => ClassAsciiKind::Space,
							"upper" => ClassAsciiKind::Upper,
							"xdigit" => ClassAsciiKind::Xdigit,
							_ => return Err(ParseError),
						};
						let mut previous = std::mem::replace(literals, Vec::new())
							.into_iter()
							.map(ClassSetItem::Literal)
							.collect();
						let mut items = if let ClassSetItem::Empty(_) = item {
							previous
						} else {
							let mut items = vec![std::mem::replace(item, ClassSetItem::Empty(span))];
							items.append(&mut previous);
							items
						};
						let new_item = ClassSetItem::Ascii(ClassAscii {
							span, kind,
							negated: false,
						});
						if items.is_empty() {
							*item = new_item;
						} else {
							items.push(new_item);
							*item = ClassSetItem::Union(ClassSetUnion {
								span, items
							});
						}
					}
					Token::BeginAnchor | Token::EndAnchor => panic!("anchors cannot appear in character lists"),
					_ => unimplemented!("regular AST"),
				}
			}
			_ => {
				match t {
					Token::Terminator(_) => {
						is_terminated = true;
						break;
					},
					Token::StartBrace(negated) => {
						state.push(State::Bracket(negated, ClassSetItem::Empty(span), Vec::new(), false));
					}
					Token::EndBrace => return Err(ParseError),
					Token::EndParen => {
						while let Some(&mut State::Alternation(ref mut old_ast)) = state.last_mut() {
							let old_ast = std::mem::replace(old_ast, Ast::Empty(span));
							if let Ast::Alternation(Alternation { ref mut asts, .. }) = ast {
								asts.insert(0, old_ast);
							} else {
								ast = Ast::Alternation(Alternation {
									span,
									asts: vec![old_ast, ast],
								});
							}
							state.pop();
						}
						if let Some(State::Paren(old_ast)) = state.pop() {
							capture_index += 1;
							ast = Ast::Group(Group {
								span,
								kind: GroupKind::CaptureIndex(capture_index),
								ast: Box::new(ast),
							});
							if !old_ast.is_empty() {
								ast = Ast::Concat(Concat {
									span,
									asts: vec![old_ast, ast],
								});
							}
						} else {
							return Err(ParseError);
						}
					},
					Token::Literal(lit) => {
						if let Ast::Concat(Concat { ref mut asts, .. }) = ast {
							asts.push(Ast::Literal(lit));
						} else if let Ast::Empty(_) = ast {
							ast = Ast::Literal(lit);
						} else {
							ast = Concat {
								span,
								asts: vec![ast, Ast::Literal(lit)],
							}.into_ast();
						}
					}
					Token::BeginAnchor => {
						let new = Ast::Assertion(Assertion { span, kind: AssertionKind::StartLine });
						if let Ast::Concat(Concat { ref mut asts, .. }) = ast {
							asts.push(new);
						} else if ast.is_empty() {
							ast = new;
						} else {
							ast = Concat {
								span,
								asts: vec![ast, new],
							}.into_ast();
						}
					}
					Token::EndAnchor => {
						let new = Ast::Assertion(Assertion { span, kind: AssertionKind::EndLine });
						if let Ast::Concat(Concat { ref mut asts, .. }) = ast {
							asts.push(new);
						} else if ast.is_empty() {
							ast = new;
						} else {
							ast = Concat {
								span,
								asts: vec![ast, new],
							}.into_ast();
						}
					}
					Token::AlternationPipe => {
						let old_ast = std::mem::replace(&mut ast, Ast::Empty(span));
						state.push(State::Alternation(old_ast));
					}
					Token::StartParen => {
						let old_ast = std::mem::replace(&mut ast, Ast::Empty(span));
						state.push(State::Paren(old_ast));
					}
					Token::Dot => {
						if let Ast::Empty(_) = ast {
							ast = Ast::Dot(span);
						} else if let Ast::Concat(Concat { ref mut asts, .. }) = ast {
							asts.push(Ast::Dot(span));
						} else {
							ast = Concat {
								span,
								asts: vec![ast, Ast::Dot(span)],
							}.into_ast();
						}
					}
					Token::Star | Token::Plus | Token::Question | Token::OpenInterval(..) | Token::FixedInterval(..) | Token::RangeInterval(..) => {
						let kind = if let Token::Star = t {
							RepetitionKind::ZeroOrMore
						} else if let Token::Plus = t {
							RepetitionKind::OneOrMore
						} else if let Token::Question = t {
							RepetitionKind::ZeroOrOne
						} else if let Token::OpenInterval(start) = t {
							RepetitionKind::Range(RepetitionRange::AtLeast(start))
						} else if let Token::FixedInterval(start) = t {
							RepetitionKind::Range(RepetitionRange::Exactly(start))
						} else if let Token::RangeInterval(start, end) = t {
							RepetitionKind::Range(RepetitionRange::Bounded(start, end))
						} else {
							panic!();
						};
						if let Ast::Empty(_) = ast {
							return Err(ParseError);
						} else if let Ast::Concat(Concat { ref mut asts, .. }) = ast {
							let old_ast = asts.pop().expect("concat should never be empty");
							let old_ast = Ast::Repetition(Repetition {
								span,
								op: RepetitionOp {
									span, kind
								},
								greedy: true,
								ast: Box::new(old_ast),
							});
							asts.push(old_ast);
						} else {
							ast = Ast::Repetition(Repetition {
								span,
								op: RepetitionOp {
									span, kind
								},
								greedy: true,
								ast: Box::new(ast),
							});
						}
					}
					_ => unimplemented!("regular AST"),
				}
			}
		}
	}
	while let Some(state) = state.pop() {
		match state {
			State::Bracket(..) => return Err(ParseError),
			State::Paren(..) => return Err(ParseError),
			State::Alternation(old_ast) => {
				if let Ast::Alternation(Alternation { ref mut asts, .. }) = ast {
					asts.insert(0, old_ast);
				} else {
					ast = Ast::Alternation(Alternation {
						span,
						asts: vec![old_ast, ast],
					});
				}
			}
		}
	}
	if !is_terminated && terminator.is_some() {
		return Err(ParseError);
	}
	Ok((ast, tok.offset))
}

#[cfg(test)]
mod tests {
	use super::Personality::*;
	#[test]
	fn tokens() {
		let t = br#"a\x1g\0c[]]]"#;
		let t = super::ExtendedRegexTokens {
			inner: t.iter().cloned(),
			peek: Vec::new(),
			push_punct: None,
			is_in_brace: false,
			is_beginning: true,
			paren_level: 0,
			terminator: None,
			personality: Gnu,
			offset: 0,
		};
		let t: Vec<_> = t.collect();
		assert_eq!(t[0].c(), 'a');
		assert_eq!(t[1].c(), '\x01');
		assert_eq!(t[2].c(), 'g');
		assert_eq!(t[3].c(), '\0');
		assert_eq!(t[4].c(), 'c');
		assert!(t[5].is_start_brace());
		assert!(!t[5].is_complement());
		assert_eq!(t[6].c(), ']');
		assert!(t[7].is_end_brace());
		assert_eq!(t[8].c(), ']');
	}
	#[test]
	fn tokens_paren() {
		let t = br#"())[()]"#;
		let t = super::ExtendedRegexTokens {
			inner: t.iter().cloned(),
			peek: Vec::new(),
			push_punct: None,
			is_in_brace: false,
			is_beginning: true,
			paren_level: 0,
			terminator: None,
			personality: Gnu,
			offset: 0,
		};
		let t: Vec<_> = t.collect();
		assert!(t[0].is_start_paren());
		assert!(t[1].is_end_paren());
		assert_eq!(t[2].c(), ')');
		assert!(t[3].is_start_brace());
		assert_eq!(t[4].c(), '(');
		assert_eq!(t[5].c(), ')');
		assert!(t[6].is_end_brace());
	}
	#[test]
	fn tokens_range_hyphen() {
		let t = br#"[-a][a-][a-b]"#;
		let t = super::ExtendedRegexTokens {
			inner: t.iter().cloned(),
			peek: Vec::new(),
			push_punct: None,
			is_in_brace: false,
			is_beginning: true,
			paren_level: 0,
			terminator: None,
			personality: Gnu,
			offset: 0,
		};
		let t: Vec<_> = t.collect();
		assert!(t[0].is_start_brace());
		assert_eq!(t[1].c(), '-');
		assert_eq!(t[2].c(), 'a');
		assert!(t[3].is_end_brace());
		assert!(t[4].is_start_brace());
		assert_eq!(t[5].c(), 'a');
		assert_eq!(t[6].c(), '-');
		assert!(t[7].is_end_brace());
		assert!(t[8].is_start_brace());
		assert_eq!(t[9].c(), 'a');
		assert!(t[10].is_range_hyphen());
		assert_eq!(t[11].c(), 'b');
		assert!(t[12].is_end_brace());
	}
	#[test]
	fn tokens_range_hyphen_complement() {
		let t = br#"[^-a][^a-][^a-b]"#;
		let t = super::ExtendedRegexTokens {
			inner: t.iter().cloned(),
			peek: Vec::new(),
			push_punct: None,
			is_in_brace: false,
			is_beginning: true,
			paren_level: 0,
			terminator: None,
			personality: Gnu,
			offset: 0,
		};
		let t: Vec<_> = t.collect();
		assert!(t[0].is_start_brace());
		assert_eq!(t[1].c(), '-');
		assert_eq!(t[2].c(), 'a');
		assert!(t[3].is_end_brace());
		assert!(t[4].is_start_brace());
		assert_eq!(t[5].c(), 'a');
		assert_eq!(t[6].c(), '-');
		assert!(t[7].is_end_brace());
		assert!(t[8].is_start_brace());
		assert_eq!(t[9].c(), 'a');
		assert!(t[10].is_range_hyphen());
		assert_eq!(t[11].c(), 'b');
		assert!(t[12].is_end_brace());
	}
	fn zero_span(span: &mut regex_syntax::ast::Span) {
		use regex_syntax::ast::{Span, Position};
		*span = Span {
			start: Position { offset: 0, line: 0, column: 0 },
			end: Position { offset: 0, line: 0, column: 0 },
		};
	}
	fn zero_class_item(item: &mut regex_syntax::ast::ClassSetItem) {
		use regex_syntax::ast::*;
		match item {
			&mut ClassSetItem::Empty(ref mut span) => zero_span(span),
			&mut ClassSetItem::Literal(ref mut literal) => zero_span(&mut literal.span),
			&mut ClassSetItem::Range(ref mut range) => {
				zero_span(&mut range.start.span);
				zero_span(&mut range.end.span);
				zero_span(&mut range.span);
			}
			&mut ClassSetItem::Ascii(ref mut ascii) => zero_span(&mut ascii.span),
			&mut ClassSetItem::Unicode(ref mut unicode) => zero_span(&mut unicode.span),
			&mut ClassSetItem::Perl(ref mut perl) => zero_span(&mut perl.span),
			&mut ClassSetItem::Bracketed(ref mut bracketed) => zero_class_kind(&mut bracketed.kind),
			&mut ClassSetItem::Union(ref mut union) => {
				zero_span(&mut union.span);
				for item in union.items.iter_mut() {
					zero_class_item(item);
				}
			}
		}
	}
	fn zero_class_kind(kind: &mut regex_syntax::ast::ClassSet) {
		use regex_syntax::ast::*;
		match kind {
			&mut ClassSet::BinaryOp(ref mut op) => {
				zero_span(&mut op.span);
				zero_class_kind(&mut op.lhs);
				zero_class_kind(&mut op.rhs);
			}
			&mut ClassSet::Item(ref mut item) => {
				zero_class_item(item);
			}
		}
	}
	fn zero_class(class: &mut regex_syntax::ast::Class) {
		use regex_syntax::ast::*;
		match class {
			&mut Class::Unicode(ref mut unicode) => zero_span(&mut unicode.span),
			&mut Class::Perl(ref mut perl) => zero_span(&mut perl.span),
			&mut Class::Bracketed(ref mut bracketed) => {
				zero_span(&mut bracketed.span);
				zero_class_kind(&mut bracketed.kind);
			},
		}
	}
	fn zero_ast(ast: &mut regex_syntax::ast::Ast) {
		use regex_syntax::ast::*;
		match ast {
			&mut Ast::Empty(ref mut span) => zero_span(span),
			&mut Ast::Flags(ref mut flags) => zero_span(&mut flags.span),
			&mut Ast::Literal(ref mut literal) => zero_span(&mut literal.span),
			&mut Ast::Dot(ref mut span) => zero_span(span),
			&mut Ast::Assertion(ref mut assertion) => zero_span(&mut assertion.span),
			&mut Ast::Class(ref mut class) => zero_class(class),
			&mut Ast::Repetition(ref mut repetition) => {
				zero_span(&mut repetition.span);
				zero_span(&mut repetition.op.span);
				zero_ast(&mut repetition.ast);
			},
			&mut Ast::Group(ref mut group) => {
				zero_span(&mut group.span);
				zero_ast(&mut group.ast);
			},
			&mut Ast::Alternation(ref mut alternation) => {
				zero_span(&mut alternation.span);
				for item in alternation.asts.iter_mut() {
					zero_ast(item);
				}
			},
			&mut Ast::Concat(ref mut concat) => {
				zero_span(&mut concat.span);
				for item in concat.asts.iter_mut() {
					zero_ast(item);
				}
			},
		}
	}
	fn equiv_regex(posix: &[u8], rust: &str) {
		let mut builder = regex::bytes::RegexBuilder::new(rust);
		builder.octal(true);
		builder.unicode(false);
		let _r = builder.build().expect("valid rust regex");
		let mut posix_ast = super::extended_regex(posix, Gnu, None).expect("valid posix regex").0;
		assert_eq!(posix_ast.to_string(), rust);
		let mut parser = regex_syntax::ast::parse::ParserBuilder::new();
		parser.octal(true);
		let mut parser = parser.build();
		let mut rust_ast = parser.parse(rust).expect("valid rust regex ast");
		zero_ast(&mut rust_ast);
		zero_ast(&mut posix_ast);
		assert_eq!(posix_ast, rust_ast);
	}
	#[test]
	fn basic_equiv_regex() {
		equiv_regex(br#"."#, r".");
		equiv_regex(br#"a"#, r"a");
		equiv_regex(br#"ab"#, r"ab");
		equiv_regex(br#"\a"#, r"\a");
		equiv_regex(br#"\x1"#, r"\x01");
		equiv_regex(br#"\x01"#, r"\x01");
		equiv_regex(br#"\x11"#, r"\x11");
		equiv_regex(br#"\x1g"#, r"\x01g");
		equiv_regex(br#"\x111"#, r"\x111");
		equiv_regex(br#"\1"#, r"\1");
		equiv_regex(br#"\1g"#, r"\1g");
		equiv_regex(br#"\11"#, r"\11");
		equiv_regex(br#"\111"#, r"I");
		equiv_regex(br#"\$"#, r#"\$"#);
		equiv_regex(br#"\^"#, r#"\^"#);
		equiv_regex(br#"\."#, r#"\."#);
		equiv_regex(br#"\["#, r#"\["#);
		equiv_regex(br#"\]"#, r#"\]"#);
		equiv_regex(br#"[a]"#, r#"[a]"#);
		equiv_regex(br#"[ab]"#, r#"[ab]"#);
		equiv_regex(br#"[^ab]"#, r#"[^ab]"#);
		equiv_regex(br#"[]ab]"#, r#"[\]ab]"#);
		equiv_regex(br#"[[ab]"#, r#"[\[ab]"#);
		equiv_regex(br#"[a^b]"#, r#"[a^b]"#);
		equiv_regex(br#"[a-b]"#, r#"[a-b]"#);
		equiv_regex(br#"[xa-b]"#, r#"[xa-b]"#);
		equiv_regex(br#"[a-bx]"#, r#"[a-bx]"#);
		equiv_regex(br#"[-bx]"#, r#"[\-bx]"#);
		equiv_regex(br#"[bx-]"#, r#"[bx\-]"#);
		equiv_regex(br#"[]-x]"#, r#"[\]-x]"#);
		equiv_regex(br#"\/"#, r"/");
		equiv_regex(br#"\""#, r#"""#);
		equiv_regex(br#"a\qc"#, r#"aqc"#);
		equiv_regex(br#"\b"#, r"\x08");
		equiv_regex(br#"\a"#, r"\a");
		equiv_regex(br#"^a$"#, r"^a$");
		equiv_regex(br#"a|b|c"#, r"a|b|c");
		equiv_regex(br#"a(b)c"#, r"a(b)c");
		equiv_regex(br#"(a|b|c)"#, r"(a|b|c)");
		equiv_regex(br#"a+"#, r"a+");
		equiv_regex(br#"ab+"#, r"ab+");
		equiv_regex(br#"(ab)+"#, r"(ab)+");
		equiv_regex(br#"a*"#, r"a*");
		equiv_regex(br#"ab*"#, r"ab*");
		equiv_regex(br#"(ab)*"#, r"(ab)*");
		equiv_regex(br#"a?"#, r"a?");
		equiv_regex(br#"ab?"#, r"ab?");
		equiv_regex(br#"(ab)?"#, r"(ab)?");
		equiv_regex(br#"+a"#, r"\+a");
		equiv_regex(br#"+ab"#, r"\+ab");
		equiv_regex(br#"+(ab)"#, r"\+(ab)");
		equiv_regex(br#"(+ab)"#, r"(\+ab)");
		equiv_regex(br#"*a"#, r"\*a");
		equiv_regex(br#"*ab"#, r"\*ab");
		equiv_regex(br#"*(ab)"#, r"\*(ab)");
		equiv_regex(br#"(*ab)"#, r"(\*ab)");
		equiv_regex(br#"?a"#, r"\?a");
		equiv_regex(br#"?ab"#, r"\?ab");
		equiv_regex(br#"?(ab)"#, r"\?(ab)");
		equiv_regex(br#"(?ab)"#, r"(\?ab)");
		equiv_regex(br#"[[:alpha:]]"#, r"[[:alpha:]]");
		equiv_regex(br#"[^[:alpha:]]"#, r"[^[:alpha:]]");
	}
	fn do_must_fail(posix: &[u8]) {
		assert!(super::extended_regex(posix, Gnu, None).is_err());
	}
	#[test]
	fn must_fail() {
		do_must_fail(br#"[[:ascii:]]"#);
	}
	#[test]
	fn must_fail_unterminated() {
		assert!(super::extended_regex(br#"t"#, Gnu, Some(b'/')).is_err());
	}
	fn equiv_regex_posix(posix: &[u8], rust: &str) {
		let mut builder = regex::bytes::RegexBuilder::new(rust);
		builder.octal(true);
		builder.unicode(false);
		let _r = builder.build().expect("valid rust regex");
		let mut posix_ast = super::extended_regex(posix, Posix, None).expect("valid posix regex").0;
		assert_eq!(posix_ast.to_string(), rust);
		let mut parser = regex_syntax::ast::parse::ParserBuilder::new();
		parser.octal(true);
		let mut parser = parser.build();
		let mut rust_ast = parser.parse(rust).expect("valid rust regex ast");
		zero_ast(&mut rust_ast);
		zero_ast(&mut posix_ast);
		assert_eq!(posix_ast, rust_ast);
	}
	fn equiv_regex_traditional(posix: &[u8], rust: &str) {
		let mut builder = regex::bytes::RegexBuilder::new(rust);
		builder.octal(true);
		builder.unicode(false);
		let _r = builder.build().expect("valid rust regex");
		let mut posix_ast = super::extended_regex(posix, Traditional, None).expect("valid posix regex").0;
		assert_eq!(posix_ast.to_string(), rust);
		let mut parser = regex_syntax::ast::parse::ParserBuilder::new();
		parser.octal(true);
		let mut parser = parser.build();
		let mut rust_ast = parser.parse(rust).expect("valid rust regex ast");
		zero_ast(&mut rust_ast);
		zero_ast(&mut posix_ast);
		assert_eq!(posix_ast, rust_ast);
	}
	#[test]
	fn regex_interval() {
		// Traditional mode does not have interval expressions
		equiv_regex_traditional(br#"a{1,2}"#, r"a\{1,2}");
		equiv_regex_traditional(br#"a{1,}"#, r"a\{1,}");
		equiv_regex_traditional(br#"a{1}"#, r"a\{1}");
		// GNU mode and posix mode do
		equiv_regex_posix(br#"a{1,2}"#, r"a{1,2}");
		equiv_regex_posix(br#"a{1,}"#, r"a{1,}");
		equiv_regex_posix(br#"a{1}"#, r"a{1}");
		equiv_regex(br#"a{1,2}"#, r"a{1,2}");
		equiv_regex(br#"a{1,}"#, r"a{1,}");
		equiv_regex(br#"a{1}"#, r"a{1}");
		// test error correction
		equiv_regex(br#"a{ 1}"#, r"a\{ 1}");
		equiv_regex(br#"a{a}"#, r"a\{a}");
		equiv_regex(br#"a{"#, r"a\{");
		equiv_regex(br#"{"#, r"\{");
		equiv_regex(br#"a{1,a}"#, r"a\{1,a}");
		equiv_regex(br#"a{1,a}{alphanumeric}"#, r"a\{1,a}\{alphanumeric}");
		equiv_regex(br#"ab{1,2}c"#, r"ab{1,2}c");
		equiv_regex(br#"({1,2})"#, r"(\{1,2})");
	}
	#[test]
	fn hex_expressions_not_in_posix() {
		equiv_regex_posix(br#"\x1"#, r"x1");
		equiv_regex_traditional(br#"\x1"#, r"x1");
		equiv_regex(br#"\x1"#, r"\x01");
	}
	#[test]
	fn posix_metacharacters() {
		// \x2a and \052 are ascii for '*'
		equiv_regex(br#"a\x2a"#, r"a*");
		equiv_regex(br#"a\052"#, r"a*");
		equiv_regex_posix(br#"a\052"#, r"a*");
		equiv_regex_traditional(br#"a\052"#, r"a\52");
	}
}
