use rawk_regex;
pub use rawk_regex::{ExtendedRegexTokens, Personality};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenData {
	String,
	Regex,
	InvalidRegex,
	UnterminatedRegex,
	UnterminatedString,
	Word,
	LineBreak,
	OpenRound,
	CloseRound,
	OpenSquare,
	CloseSquare,
	OpenCurley,
	CloseCurley,
	Dollar,
	Plus,
	PlusAssign,
	Minus,
	MinusAssign,
	Times,
	TimesAssign,
	Divide,
	DivideAssign,
	Assign,
	Equals,
	NotEquals,
	Comma,
	Semicolon,
	Matches,
	NotMatches,
	Print,
	Print0,
	In,
	If,
	Then,
	ThenTail,
	EndIf,
	Else,
	While,
	StartWhile,
	EndWhile,
	Do,
	DoWhile,
	DoTail,
	For,
	Concat,
	FnCall,
	Question,
	Colon,
	Less,
	Greater,
	StartFor,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Token {
	pub data: TokenData,
	pub start: usize,
	pub end: usize,
}

pub struct Tokens<'a> {
	data: &'a [u8],
	offset: usize,
	personality: Option<Personality>,
	curley_count: u32,
	round_count: u32,
	square_count: u32,
	start: bool,
}

impl<'a> Tokens<'a> {
	pub fn new(data: &'a [u8], personality: Option<Personality>) -> Tokens<'a> {
		Tokens {
			data,
			personality,
			offset: 0,
			curley_count: 0,
			round_count: 0,
			square_count: 0,
			start: true,
		}
	}
	pub fn round_count(&mut self) -> u32 {
		self.round_count
	}
	pub fn square_count(&mut self) -> u32 {
		self.square_count
	}
	pub fn curley_count(&mut self) -> u32 {
		self.curley_count
	}
}

impl<'a> std::iter::FusedIterator for Tokens<'a> {}

impl<'a> Iterator for Tokens<'a> {
	type Item = Token;
	fn next(&mut self) -> Option<Token> {
		let c = self.data.get(self.offset)?;
		match *c {
			b'"' => {
				self.start = false;
				let old_offset = self.offset;
				self.offset += 1;
				let mut terminated = false;
				let mut escaped = false;
				while let Some(&c) = self.data.get(self.offset) {
					self.offset += 1;
					if c == b'"' && !escaped {
						terminated = true;
						break;
					}
					escaped = c == b'\\' && !escaped;
				}
				if terminated {
					return Some(Token {
						data: TokenData::String,
						start: old_offset,
						end: self.offset,
					});
				} else {
					// unterminated string is probably part of a function call or something
					// use this as our end hint
					let mut round_count = 0;
					let mut curley_count = 0;
					let mut square_count = 0;
					self.offset = old_offset;
					while let Some(&c) = self.data.get(self.offset) {
						if c == b'(' && self.round_count != 0 {
							round_count += 1;
						}
						if c == b')' && self.round_count != 0 {
							if round_count > 0 {
								round_count -= 1;
							} else {
								break;
							}
						}
						if c == b'[' {
							square_count += 1;
						}
						if c == b']' {
							if square_count > 0 {
								square_count -= 1;
							} else {
								break;
							}
						}
						if c == b'{' {
							curley_count += 1;
						}
						if c == b'}' {
							if curley_count > 0 {
								curley_count -= 1;
							} else {
								break;
							}
						}
						self.offset += 1;
					}
					return Some(Token {
						data: TokenData::UnterminatedString,
						start: old_offset,
						end: self.offset,
					});
				}
			}
			b'/' if self.personality.is_some() && self.start => {
				self.start = false;
				let mut ert = rawk_regex::ExtendedRegexTokens::new(self.data[self.offset+1..].iter().cloned(), self.personality.unwrap(), Some(b'/'));
				let mut terminated = false;
				while let Some(n) = ert.next() {
					if n.is_terminator() {
						terminated = true;
						break;
					}
				}
				if terminated {
					let old_offset = self.offset;
					self.offset += ert.offset + 1;
					return Some(Token {
						data: TokenData::Regex,
						start: old_offset,
						end: self.offset,
					});
				} else {
					// the regex is probably intended to end at the line, or at the brace
					let brace = if self.curley_count == 0 { b'{' } else { b'}' };
					let old_offset = self.offset;
					let mut round_count = 0;
					self.offset += 1;
					let mut escaped = false;
					let mut data = TokenData::UnterminatedRegex;
					while let Some(&c) = self.data.get(self.offset) {
						if c == b'(' && !escaped && self.round_count != 0 {
							round_count += 1;
						}
						if c == b')' && !escaped && self.round_count != 0 {
							if round_count == 0 {
								break;
							}
							round_count -= 1;
						}
						let next = self.data.get(self.offset + 1);
						if c == b'{' && next <= Some(&b'9') && next >= Some(&b'0') {
							// looks like `{2}`, so it's probably still part of the regex
							self.offset += 2;
							continue;
						}
						if c == b'\n' || c == b'\r' || (c == brace && !escaped) {
							break;
						}
						if c == b'/' && !escaped {
							self.offset += 1;
							data = TokenData::InvalidRegex;
							break;
						}
						escaped = c == b'\\' && !escaped;
						self.offset += 1;
					}
					return Some(Token {
						data,
						start: old_offset,
						end: self.offset,
					});
				}
			}
			b'/' if self.personality.is_none() && self.start => {
				self.start = false;
				let old_offset = self.offset;
				self.offset += 1;
				let mut terminated = false;
				let mut escaped = false;
				// Rust regexes always end at the first unescaped `/`
				while let Some(&c) = self.data.get(self.offset) {
					if c == b'/' && !escaped {
						self.offset += 1;
						terminated = true;
						break;
					}
					escaped = c == b'\\' && !escaped;
					self.offset += 1;
				}
				if terminated {
					// check Rust regexes for invalid UTF-8.
					match String::from_utf8(Vec::from(&self.data[(old_offset+1)..(self.offset-1)])) {
						Ok(_) => {},
						Err(_) => {
							return Some(Token {
								data: TokenData::InvalidRegex,
								start: old_offset,
								end: self.offset,
							})
						}
					};
					return Some(Token {
						data: TokenData::Regex,
						start: old_offset,
						end: self.offset,
					});
				} else {
					// the regex is probably intended to end at the line, or at the brace
					self.offset = old_offset + 1;
					let brace = if self.curley_count == 0 { b'{' } else { b'}' };
					let mut round_count = 0;
					while let Some(&c) = self.data.get(self.offset) {
						if c == b'(' && self.round_count != 0 {
							round_count += 1;
						}
						if c == b')' && self.round_count != 0 {
							if round_count == 0 {
								break;
							}
							round_count -= 1;
						}
						let next = self.data.get(self.offset + 1);
						if c == b'{' && next <= Some(&b'9') && next >= Some(&b'0') {
							// looks like `{2}`, so it's probably still part of the regex
							self.offset += 2;
							continue;
						}
						if c == b'\n' || c == b'\r' || c == brace {
							break;
						}
						self.offset += 1;
					}
					return Some(Token {
						data: TokenData::UnterminatedRegex,
						start: old_offset,
						end: self.offset,
					});
				}
			}
			b'\n' | b'\r' | b' ' | b'\t' => {
				if *c == b'\n' && self.round_count == 0 {
					self.start = true;
				}
				// numbers, variable names, stuff like that
				let old_offset = self.offset;
				let mut linebreak = false;
				while let Some(&c) = self.data.get(self.offset) {
					match c {
						b'\n' if self.round_count == 0 && self.square_count == 0 => {
							linebreak = true;
							self.offset += 1;
						}
						b'\n' | b'\r' | b' ' | b'\t' => {
							self.offset += 1;
						}
						_ => {
							break;
						}
					}
				}
				if !linebreak {
					return self.next();
				}
				return Some(Token {
					data: TokenData::LineBreak,
					start: old_offset,
					end: self.offset,
				});
			}
			b'(' => {
				self.offset += 1;
				self.round_count += 1;
				self.start = true;
				return Some(Token {
					data: TokenData::OpenRound,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b')' => {
				self.offset += 1;
				if self.round_count > 0 {
					self.round_count -= 1;
				}
				self.start = false;
				return Some(Token {
					data: TokenData::CloseRound,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b'[' => {
				self.offset += 1;
				self.square_count += 1;
				self.start = true;
				return Some(Token {
					data: TokenData::OpenSquare,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b']' => {
				self.offset += 1;
				if self.square_count > 0 {
					self.square_count -= 1;
				}
				self.start = false;
				return Some(Token {
					data: TokenData::CloseSquare,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b'$' => {
				self.offset += 1;
				self.start = true;
				return Some(Token {
					data: TokenData::Dollar,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b';' => {
				self.offset += 1;
				self.start = true;
				return Some(Token {
					data: TokenData::Semicolon,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b'~' => {
				self.offset += 1;
				self.start = true;
				return Some(Token {
					data: TokenData::Matches,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b'!' if self.data.get(self.offset+1) == Some(&b'~') => {
				self.offset += 2;
				self.start = true;
				return Some(Token {
					data: TokenData::NotMatches,
					start: self.offset - 2,
					end: self.offset,
				});
			}
			b'+' if self.data.get(self.offset+1) == Some(&b'=') => {
				self.start = true;
				self.offset += 2;
				return Some(Token {
					data: TokenData::PlusAssign,
					start: self.offset - 2,
					end: self.offset,
				});
			}
			b'-' if self.data.get(self.offset+1) == Some(&b'=') => {
				self.start = true;
				self.offset += 2;
				return Some(Token {
					data: TokenData::MinusAssign,
					start: self.offset - 2,
					end: self.offset,
				});
			}
			b'*' if self.data.get(self.offset+1) == Some(&b'=') => {
				self.start = true;
				self.offset += 2;
				return Some(Token {
					data: TokenData::TimesAssign,
					start: self.offset - 2,
					end: self.offset,
				});
			}
			b'/' if self.data.get(self.offset+1) == Some(&b'=') => {
				self.start = true;
				self.offset += 2;
				return Some(Token {
					data: TokenData::DivideAssign,
					start: self.offset - 2,
					end: self.offset,
				});
			}
			b'+' => {
				self.start = true;
				self.offset += 1;
				return Some(Token {
					data: TokenData::Plus,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b'-' => {
				self.start = true;
				self.offset += 1;
				return Some(Token {
					data: TokenData::Minus,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b'*' => {
				self.start = true;
				self.offset += 1;
				return Some(Token {
					data: TokenData::Times,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b'/' => {
				self.start = true;
				self.offset += 1;
				return Some(Token {
					data: TokenData::Divide,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b'<' => {
				self.start = true;
				self.offset += 1;
				return Some(Token {
					data: TokenData::Less,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b'>' => {
				self.start = true;
				self.offset += 1;
				return Some(Token {
					data: TokenData::Greater,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b'=' if self.data.get(self.offset+1) == Some(&b'=') => {
				self.start = true;
				self.offset += 2;
				return Some(Token {
					data: TokenData::Equals,
					start: self.offset - 2,
					end: self.offset,
				});
			}
			b'!' if self.data.get(self.offset+1) == Some(&b'=') => {
				self.start = true;
				self.offset += 2;
				return Some(Token {
					data: TokenData::NotEquals,
					start: self.offset - 2,
					end: self.offset,
				});
			}
			b'=' => {
				self.start = true;
				self.offset += 1;
				return Some(Token {
					data: TokenData::Assign,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b',' => {
				self.start = true;
				self.offset += 1;
				return Some(Token {
					data: TokenData::Comma,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b'?' => {
				self.start = true;
				self.offset += 1;
				return Some(Token {
					data: TokenData::Question,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b':' => {
				self.start = true;
				self.offset += 1;
				return Some(Token {
					data: TokenData::Colon,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b'{' => {
				self.start = true;
				self.offset += 1;
				self.curley_count += 1;
				return Some(Token {
					data: TokenData::OpenCurley,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			b'}' => {
				self.start = true;
				self.offset += 1;
				if self.curley_count > 0 {
					self.curley_count -= 1;
				}
				return Some(Token {
					data: TokenData::CloseCurley,
					start: self.offset - 1,
					end: self.offset,
				});
			}
			_ => {
				self.start = false;
				// numbers, variable names, stuff like that
				let old_offset = self.offset;
				while let Some(&c) = self.data.get(self.offset) {
					match c {
						b'<' | b'>' | b'~' | b';' | b',' | b'"' | b'\n' | b'\r' | b' ' | b'\t' | b'{' | b'}' | b')' | b'[' | b']' | b'+' | b'-' | b'*' | b'/' | b'=' | b'?' | b':' | b'$' => {
							break;
						}
						b'(' => {
							self.start = true;
							self.round_count += 1;
							self.offset += 1;
							return Some(Token {
								data: TokenData::FnCall,
								start: old_offset,
								end: self.offset - 1,
							})
						}
						_ => {
							self.offset += 1;
						}
					}
				}
				let data = match &self.data[old_offset..self.offset] {
					b"print" => TokenData::Print,
					b"in" => TokenData::In,
					b"if" => TokenData::If,
					b"else" => TokenData::Else,
					b"while" => TokenData::While,
					b"do" => TokenData::Do,
					b"for" => TokenData::For,
					_ => TokenData::Word,
				};
				return Some(Token {
					data,
					start: old_offset,
					end: self.offset,
				});
			}
		}
	}
}

pub fn stringify(tokens: impl Iterator<Item=Token>, data: &[u8]) -> String {
	use std::fmt::Write;
	use std::str;
	let mut out = String::new();
	let mut first = true;
	for token in tokens {
		if first {
			first = false;
		} else {
			out += " ";
		}
		match token.data {
			TokenData::String => {
				write!(&mut out, "String({})", str::from_utf8(&data[token.start..token.end]).unwrap()).unwrap();
			},
			TokenData::Regex => {
				write!(&mut out, "Regex({})", str::from_utf8(&data[token.start..token.end]).unwrap()).unwrap();
			}
			TokenData::InvalidRegex => {
				write!(&mut out, "InvalidRegex").unwrap();
			}
			TokenData::UnterminatedRegex => {
				write!(&mut out, "UnterminatedRegex").unwrap();
			}
			TokenData::UnterminatedString => {
				write!(&mut out, "UnterminatedString").unwrap();
			}
			TokenData::Word => {
				write!(&mut out, "Word({})", str::from_utf8(&data[token.start..token.end]).unwrap()).unwrap();
			}
			TokenData::FnCall => {
				write!(&mut out, "FnCall({})", str::from_utf8(&data[token.start..token.end]).unwrap()).unwrap();
			}
			TokenData::LineBreak => {
				write!(&mut out, "LineBreak").unwrap();
			}
			TokenData::OpenRound => {
				write!(&mut out, "(").unwrap();
			}
			TokenData::CloseRound => {
				write!(&mut out, ")").unwrap();
			}
			TokenData::OpenSquare => {
				write!(&mut out, "[").unwrap();
			}
			TokenData::CloseSquare => {
				write!(&mut out, "]").unwrap();
			}
			TokenData::OpenCurley => {
				write!(&mut out, r"{{").unwrap();
			}
			TokenData::CloseCurley => {
				write!(&mut out, r"}}").unwrap();
			}
			TokenData::Dollar => {
				write!(&mut out, "$").unwrap();
			}
			TokenData::Plus => {
				write!(&mut out, "+").unwrap();
			}
			TokenData::PlusAssign => {
				write!(&mut out, "+=").unwrap();
			}
			TokenData::Minus => {
				write!(&mut out, "-").unwrap();
			}
			TokenData::MinusAssign => {
				write!(&mut out, "-=").unwrap();
			}
			TokenData::Times => {
				write!(&mut out, "*").unwrap();
			}
			TokenData::TimesAssign => {
				write!(&mut out, "*=").unwrap();
			}
			TokenData::Divide => {
				write!(&mut out, "/").unwrap();
			}
			TokenData::Less => {
				write!(&mut out, "<").unwrap();
			}
			TokenData::Greater => {
				write!(&mut out, ">").unwrap();
			}
			TokenData::DivideAssign => {
				write!(&mut out, "/=").unwrap();
			}
			TokenData::Assign => {
				write!(&mut out, "ASSIGN").unwrap();
			}
			TokenData::Equals => {
				write!(&mut out, "==").unwrap();
			}
			TokenData::NotEquals => {
				write!(&mut out, "~=").unwrap();
			}
			TokenData::Comma => {
				write!(&mut out, ",").unwrap();
			}
			TokenData::Question => {
				write!(&mut out, "?").unwrap();
			}
			TokenData::Colon => {
				write!(&mut out, ":").unwrap();
			}
			TokenData::Semicolon => {
				write!(&mut out, ";").unwrap();
			}
			TokenData::Matches => {
				write!(&mut out, "~").unwrap();
			}
			TokenData::NotMatches => {
				write!(&mut out, "!~").unwrap();
			}
			TokenData::Print => {
				write!(&mut out, "print").unwrap();
			}
			TokenData::Print0 => {
				write!(&mut out, "print0").unwrap();
			}
			TokenData::In => {
				write!(&mut out, "in").unwrap();
			}
			TokenData::If => {
				write!(&mut out, "if").unwrap();
			}
			TokenData::Then => {
				write!(&mut out, "then").unwrap();
			}
			TokenData::ThenTail => {
				write!(&mut out, "neht").unwrap();
			}
			TokenData::Else => {
				write!(&mut out, "else").unwrap();
			}
			TokenData::EndIf => {
				write!(&mut out, "fi").unwrap();
			}
			TokenData::While => {
				write!(&mut out, "while").unwrap();
			}
			TokenData::DoWhile => {
				write!(&mut out, "WHILE").unwrap();
			}
			TokenData::StartWhile => {
				write!(&mut out, "DO").unwrap();
			}
			TokenData::EndWhile => {
				write!(&mut out, "DONE").unwrap();
			}
			TokenData::Do => {
				write!(&mut out, "do").unwrap();
			}
			TokenData::DoTail => {
				write!(&mut out, "od").unwrap();
			}
			TokenData::For => {
				write!(&mut out, "for").unwrap();
			}
			TokenData::StartFor => {
				write!(&mut out, "STARTFOR").unwrap();
			}
			TokenData::Concat => {
				write!(&mut out, "CONCAT").unwrap();
			}
		}
	}
	out
}

#[cfg(test)]
mod tests {
	use super::*;
	fn tok(data: &[u8]) -> Vec<Token> {
		Tokens::new(data, None).collect()
	}
	fn tok_gnu(data: &[u8]) -> Vec<Token> {
		Tokens::new(data, Some(Personality::Gnu)).collect()
	}
	#[test]
	fn tokenize_basic() {
		assert_eq!(tok(b"/a/ {a}"), vec![
			Token { data: TokenData::Regex, start: 0, end: 3 },
			Token { data: TokenData::OpenCurley, start: 4, end: 5 },
			Token { data: TokenData::Word, start: 5, end: 6 },
			Token { data: TokenData::CloseCurley, start: 6, end: 7 },
		]);
	}
	#[test]
	fn tokenize_unterminated_regex() {
		let pt = vec![
			Token { data: TokenData::UnterminatedRegex, start: 0, end: 3 },
			Token { data: TokenData::OpenCurley, start: 3, end: 4 },
			Token { data: TokenData::Word, start: 4, end: 5 },
			Token { data: TokenData::CloseCurley, start: 5, end: 6 },
		];
		assert_eq!(tok(b"/a {a}"), pt);
		assert_eq!(tok_gnu(b"/a {a}"), pt);
	}
	#[test]
	fn tokenize_unterminated_regex_repetition() {
		let pt = vec![
			Token { data: TokenData::UnterminatedRegex, start: 0, end: 6 },
			Token { data: TokenData::OpenCurley, start: 6, end: 7 },
			Token { data: TokenData::Word, start: 7, end: 8 },
			Token { data: TokenData::CloseCurley, start: 8, end: 9 },
		];
		assert_eq!(tok(b"/a{2} {a}"), pt);
		assert_eq!(tok_gnu(b"/a{2} {a}"), pt);
	}
	#[test]
	fn tokenize_unterminated_close_regex() {
		let pt = vec![
			Token { data: TokenData::UnterminatedRegex, start: 0, end: 4 },
			Token { data: TokenData::OpenCurley, start: 4, end: 5 },
			Token { data: TokenData::Word, start: 5, end: 6 },
			Token { data: TokenData::CloseCurley, start: 6, end: 7 },
		];
		assert_eq!(tok(b"/a) {a}"), pt);
		assert_eq!(tok_gnu(b"/a) {a}"), pt);
	}
	#[test]
	fn tokenize_unterminated_regex_inside() {
		let pt = vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::UnterminatedRegex, start: 1, end: 3 },
			Token { data: TokenData::CloseCurley, start: 3, end: 4 },
		];
		assert_eq!(tok(b"{/a}"), pt);
		assert_eq!(tok_gnu(b"{/a}"), pt);
	}
	#[test]
	fn tokenize_divide() {
		assert_eq!(tok(b"{a/a}"), vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::Divide, start: 2, end: 3 },
			Token { data: TokenData::Word, start: 3, end: 4 },
			Token { data: TokenData::CloseCurley, start: 4, end: 5 },
		]);
	}
	#[test]
	fn tokenize_times() {
		assert_eq!(tok(b"{a*a}"), vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::Times, start: 2, end: 3 },
			Token { data: TokenData::Word, start: 3, end: 4 },
			Token { data: TokenData::CloseCurley, start: 4, end: 5 },
		]);
	}
	#[test]
	fn tokenize_plus() {
		assert_eq!(tok(b"{a+a}"), vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::Plus, start: 2, end: 3 },
			Token { data: TokenData::Word, start: 3, end: 4 },
			Token { data: TokenData::CloseCurley, start: 4, end: 5 },
		]);
	}
	#[test]
	fn tokenize_plus_assign() {
		assert_eq!(tok(b"{a+=a}"), vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::PlusAssign, start: 2, end: 4 },
			Token { data: TokenData::Word, start: 4, end: 5 },
			Token { data: TokenData::CloseCurley, start: 5, end: 6 },
		]);
	}
	#[test]
	fn tokenize_minus_assign() {
		assert_eq!(tok(b"{a-=a}"), vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::MinusAssign, start: 2, end: 4 },
			Token { data: TokenData::Word, start: 4, end: 5 },
			Token { data: TokenData::CloseCurley, start: 5, end: 6 },
		]);
	}
	#[test]
	fn tokenize_times_assign() {
		assert_eq!(tok(b"{a*=a}"), vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::TimesAssign, start: 2, end: 4 },
			Token { data: TokenData::Word, start: 4, end: 5 },
			Token { data: TokenData::CloseCurley, start: 5, end: 6 },
		]);
	}
	#[test]
	fn tokenize_divide_assign() {
		assert_eq!(tok(b"{a/=a}"), vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::DivideAssign, start: 2, end: 4 },
			Token { data: TokenData::Word, start: 4, end: 5 },
			Token { data: TokenData::CloseCurley, start: 5, end: 6 },
		]);
	}
	#[test]
	fn tokenize_assign_divide() {
		assert_eq!(tok(b"{a=a/a}"), vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::Assign, start: 2, end: 3 },
			Token { data: TokenData::Word, start: 3, end: 4 },
			Token { data: TokenData::Divide, start: 4, end: 5 },
			Token { data: TokenData::Word, start: 5, end: 6 },
			Token { data: TokenData::CloseCurley, start: 6, end: 7 },
		]);
	}
	#[test]
	fn tokenize_assign_unterminated_regex() {
		let pt = vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::Assign, start: 2, end: 3 },
			Token { data: TokenData::UnterminatedRegex, start: 3, end: 5 },
			Token { data: TokenData::CloseCurley, start: 5, end: 6 },
		];
		assert_eq!(tok(b"{a=/a}"), pt);
		assert_eq!(tok_gnu(b"{a=/a}"), pt);
	}
	#[test]
	fn tokenize_match() {
		let pt = vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::Assign, start: 2, end: 3 },
			Token { data: TokenData::Regex, start: 3, end: 6 },
			Token { data: TokenData::Matches, start: 6, end: 7 },
			Token { data: TokenData::Word, start: 7, end: 8 },
			Token { data: TokenData::CloseCurley, start: 8, end: 9 },
		];
		assert_eq!(tok(b"{a=/a/~a}"), pt);
		assert_eq!(tok_gnu(b"{a=/a/~a}"), pt);
	}
	#[test]
	fn tokenize_not_match() {
		let pt = vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::Assign, start: 2, end: 3 },
			Token { data: TokenData::Regex, start: 3, end: 6 },
			Token { data: TokenData::NotMatches, start: 6, end: 8 },
			Token { data: TokenData::Word, start: 8, end: 9 },
			Token { data: TokenData::CloseCurley, start: 9, end: 10 },
		];
		assert_eq!(tok(b"{a=/a/!~a}"), pt);
		assert_eq!(tok_gnu(b"{a=/a/!~a}"), pt);
	}
	#[test]
	fn tokenize_equals() {
		let pt = vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::Assign, start: 2, end: 3 },
			Token { data: TokenData::Regex, start: 3, end: 6 },
			Token { data: TokenData::Equals, start: 6, end: 8 },
			Token { data: TokenData::Word, start: 8, end: 9 },
			Token { data: TokenData::CloseCurley, start: 9, end: 10 },
		];
		assert_eq!(tok(b"{a=/a/==a}"), pt);
		assert_eq!(tok_gnu(b"{a=/a/==a}"), pt);
	}
	#[test]
	fn tokenize_not_equals() {
		let pt = vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::Assign, start: 2, end: 3 },
			Token { data: TokenData::Regex, start: 3, end: 6 },
			Token { data: TokenData::NotEquals, start: 6, end: 8 },
			Token { data: TokenData::Word, start: 8, end: 9 },
			Token { data: TokenData::CloseCurley, start: 9, end: 10 },
		];
		assert_eq!(tok(b"{a=/a/!=a}"), pt);
		assert_eq!(tok_gnu(b"{a=/a/!=a}"), pt);
	}
	#[test]
	fn tokenize_call_regex() {
		let pt = vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::FnCall, start: 1, end: 2 },
			Token { data: TokenData::UnterminatedRegex, start: 3, end: 5 },
			Token { data: TokenData::CloseRound, start: 5, end: 6 },
			Token { data: TokenData::CloseCurley, start: 6, end: 7 },
		];
		assert_eq!(tok(b"{a(/a)}"), pt);
		assert_eq!(tok_gnu(b"{a(/a)}"), pt);
	}
	#[test]
	fn tokenize_call_regex_2() {
		let pt = vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::FnCall, start: 1, end: 2 },
			Token { data: TokenData::Word, start: 3, end: 4 },
			Token { data: TokenData::Comma, start: 4, end: 5 },
			Token { data: TokenData::UnterminatedRegex, start: 5, end: 7 },
			Token { data: TokenData::CloseRound, start: 7, end: 8 },
			Token { data: TokenData::CloseCurley, start: 8, end: 9 },
		];
		assert_eq!(tok(b"{a(a,/a)}"), pt);
		assert_eq!(tok_gnu(b"{a(a,/a)}"), pt);
	}
	#[test]
	fn tokenize_basic_parens() {
		assert_eq!(tok(b"{a(b)}"), vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::FnCall, start: 1, end: 2 },
			Token { data: TokenData::Word, start: 3, end: 4 },
			Token { data: TokenData::CloseRound, start: 4, end: 5 },
			Token { data: TokenData::CloseCurley, start: 5, end: 6 },
		]);
	}
	#[test]
	fn tokenize_basic_quotes() {
		assert_eq!(tok(br#"{a("b")}"#), vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::FnCall, start: 1, end: 2 },
			Token { data: TokenData::String, start: 3, end: 6 },
			Token { data: TokenData::CloseRound, start: 6, end: 7 },
			Token { data: TokenData::CloseCurley, start: 7, end: 8 },
		]);
	}
	#[test]
	fn tokenize_unterminated_quotes() {
		assert_eq!(tok(br#"{a("b)}"#), vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::FnCall, start: 1, end: 2 },
			Token { data: TokenData::UnterminatedString, start: 3, end: 5 },
			Token { data: TokenData::CloseRound, start: 5, end: 6 },
			Token { data: TokenData::CloseCurley, start: 6, end: 7 },
		]);
	}
	#[test]
	fn tokenize_unterminated_quotes_assign() {
		assert_eq!(tok(br#"{a="b)}"#), vec![
			Token { data: TokenData::OpenCurley, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::Assign, start: 2, end: 3 },
			Token { data: TokenData::UnterminatedString, start: 3, end: 6 },
			Token { data: TokenData::CloseCurley, start: 6, end: 7 },
		]);
	}
	#[test]
	fn tokenize_dangling_delimiters() {
		assert_eq!(tok(br#"})]"#), vec![
			Token { data: TokenData::CloseCurley, start: 0, end: 1 },
			Token { data: TokenData::CloseRound, start: 1, end: 2 },
			Token { data: TokenData::CloseSquare, start: 2, end: 3 },
		]);
	}
	#[test]
	fn tokenize_keywords() {
		assert_eq!(tok(br#"print in"#), vec![
			Token { data: TokenData::Print, start: 0, end: 5 },
			Token { data: TokenData::In, start: 6, end: 8 },
		]);
	}
	#[test]
	fn tokenize_keywords_b2b() {
		assert_eq!(tok(br#"printin"#), vec![
			Token { data: TokenData::Word, start: 0, end: 7 },
		]);
	}
	#[test]
	fn tokenize_toplevel_newline() {
		assert_eq!(tok(b"1\n2"), vec![
			Token { data: TokenData::Word, start: 0, end: 1 },
			Token { data: TokenData::LineBreak, start: 1, end: 2 },
			Token { data: TokenData::Word, start: 2, end: 3 },
		]);
	}
	#[test]
	fn tokenize_inside_newline() {
		assert_eq!(tok(b"(1\n2)"), vec![
			Token { data: TokenData::OpenRound, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::Word, start: 3, end: 4 },
			Token { data: TokenData::CloseRound, start: 4, end: 5 },
		]);
	}
	#[test]
	fn tokenize_inside_semicolon() {
		assert_eq!(tok(b"(1;2)"), vec![
			Token { data: TokenData::OpenRound, start: 0, end: 1 },
			Token { data: TokenData::Word, start: 1, end: 2 },
			Token { data: TokenData::Semicolon, start: 2, end: 3 },
			Token { data: TokenData::Word, start: 3, end: 4 },
			Token { data: TokenData::CloseRound, start: 4, end: 5 },
		]);
	}
}
