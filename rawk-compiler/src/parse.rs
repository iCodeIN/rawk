use crate::lex::*;
use std::collections::VecDeque;
use std::iter::FusedIterator;

#[derive(Debug, Eq, PartialEq)]
pub enum ParseError {
	Mismatched(Token),
	Unexpected(Token),
}

fn precedence(token: Token) -> u32 {
	match token.data {
		TokenData::Assign => 1,
		TokenData::PlusAssign => 1,
		TokenData::MinusAssign => 1,
		TokenData::TimesAssign => 1,
		TokenData::DivideAssign => 1,
		TokenData::Colon => 2,
		TokenData::Question => 3,
		TokenData::Matches => 4,
		TokenData::NotMatches => 4,
		TokenData::Equals => 5,
		TokenData::Less => 5,
		TokenData::Greater => 5,
		TokenData::Concat => 6,
		TokenData::Plus => 7,
		TokenData::Minus => 7,
		TokenData::Times => 8,
		TokenData::Divide => 8,
		TokenData::Dollar => 9,
		// it is important that parens are never pushed into the output queue,
		// so if the algorithm ever asks whether they're higher precedence,
		// the answer is always "no, leave them on the stack"
		TokenData::FnCall => 0,
		TokenData::OpenRound => 0,
		TokenData::CloseRound => 0,
		// curley braces do wind up in the output queue,
		// but not through normal mechanisms
		TokenData::OpenCurley => 0,
		TokenData::CloseCurley => 0,
		// print, similarly, should always appear at the top of the operator stack
		TokenData::Print => 0,
		TokenData::If => 0,
		TokenData::Then => 0,
		TokenData::ThenTail => 0,
		TokenData::Else => 0,
		TokenData::EndIf => 0,
		TokenData::StartWhile => 0,
		TokenData::EndWhile => 0,
		TokenData::Do => 0,
		TokenData::DoTail => 0,
		TokenData::DoWhile => 0,
		TokenData::While => 0,
		TokenData::For => 0,
		// literals and variables are never on the operator stack,
		// so they shouldn't ever be queried for precedence
		// semicolons / newlines are treated specially, so they also won't
		_ => unimplemented!("{:?}", token.data),
	}
}

fn is_left_associative(token: Token) -> bool {
	match token.data {
		TokenData::Divide => true,
		TokenData::Minus => true,
		TokenData::Equals => true,
		TokenData::Colon => true,
		// notriddle:~$ gawk 'BEGIN { print 4 ~ /4/ ~ /1/ }'
		// 1
		// notriddle:~$ gawk 'BEGIN { print (4 ~ /4/) ~ /1/ }'
		// 1
		// notriddle:~$ gawk 'BEGIN { print 4 ~ (/4/ ~ /1/) }'
		// 0
		TokenData::Matches => true,
		TokenData::NotMatches => true,
		_ => false,
	}
}

pub struct Parse<T> {
	// a modified shunting yard parser
	tokens: T,
	operator_stack: Vec<Token>,
	output_queue: VecDeque<Token>,
	errors: VecDeque<ParseError>,
	// detect if we're right next to a symbol
	// if so, awk inserts implicit CONCAT operators
	touching: bool,
	// detect if we are on a semicolon or applicable newline
	// if so, push everything into the output queue
	flushing: Option<Token>,
	// absolutely horrid hack to parse for loops
	for_preamble: VecDeque<Token>,
	for_iterate: Vec<Vec<Token>>,
}

impl<T> Parse<T> where T: Iterator<Item=Token> + FusedIterator {
	pub fn new(tokens: T) -> Parse<T> {
		Parse {
			tokens,
			operator_stack: Vec::new(),
			output_queue: VecDeque::new(),
			errors: VecDeque::new(),
			touching: false,
			flushing: None,
			for_preamble: VecDeque::new(),
			for_iterate: Vec::new(),
		}
	}
	fn handle_operator(&mut self, token: Token) {
		while let Some(operator) = self.operator_stack.pop() {
			if (precedence(operator) > precedence(token)) ||
				(precedence(operator) == precedence(token) && is_left_associative(token)) {
				if operator.data == TokenData::Question {
					self.errors.push_back(ParseError::Unexpected(token));
				} else {
					self.output_queue.push_back(operator);
				}
			} else {
				self.operator_stack.push(operator);
				break;
			}
		}
		self.operator_stack.push(token);
	}
	fn is_statement(&self) -> bool {
		let top_type = self.operator_stack.last().map(|x| x.data);
		self.operator_stack.is_empty() || top_type == Some(TokenData::OpenCurley) || top_type == Some(TokenData::ThenTail) || top_type == Some(TokenData::DoTail)
	}
	fn next_token(&mut self) -> Option<Token> {
		if let Some(token) = self.flushing {
			if !self.output_queue.is_empty() || !self.is_statement() {
				return None;
			}
			self.output_queue.push_back(token);
			if token.data == TokenData::If || token.data == TokenData::While || token.data == TokenData::For || token.data == TokenData::Do {
				self.operator_stack.push(token);
			}
			self.flushing = None;
		}
		if let Some(token) = self.for_preamble.pop_front() {
			return Some(token);
		}
		self.tokens.next()
	}
}

impl<T> Iterator for Parse<T> where T: Iterator<Item=Token> + FusedIterator {
	type Item = Token;
	fn next(&mut self) -> Option<Token> {
		'outer: while let Some(token) = self.next_token() {
			let operator = self.operator_stack.last().map(|d| d.data);
			if operator == Some(TokenData::ThenTail) && token.data != TokenData::Else {
				self.operator_stack.pop();
				self.output_queue.push_back(Token {
					data: TokenData::Else,
					..token
				});
				self.output_queue.push_back(Token {
					data: TokenData::EndIf,
					..token
				});
			}
			match token.data {
				TokenData::Word | TokenData::String | TokenData::Regex => {
					if self.touching {
						self.handle_operator(Token {
							data: TokenData::Concat,
							start: token.start,
							end: token.start,
						});
					}
					self.touching = true;
					self.output_queue.push_back(token);
				}
				TokenData::FnCall => {
					if self.touching {
						self.handle_operator(Token {
							data: TokenData::Concat,
							start: token.start,
							end: token.start,
						});
					}
					self.touching = false;
					self.operator_stack.push(token);
				}
				TokenData::Dollar => {
					if self.touching {
						self.handle_operator(Token {
							data: TokenData::Concat,
							start: token.start,
							end: token.start,
						});
					}
					self.touching = false;
					self.operator_stack.push(token);
				}
				TokenData::OpenRound => {
					if self.touching {
						self.handle_operator(Token {
							data: TokenData::Concat,
							start: token.start,
							end: token.start,
						});
					}
					self.touching = false;
					self.operator_stack.push(token);
				}
				TokenData::CloseRound => {
					self.touching = true;
					while let Some(operator) = self.operator_stack.pop() {
						if operator.data == TokenData::OpenRound {
							if self.operator_stack.last().map(|t| t.data) == Some(TokenData::If) {
								// C-style conditionals allow any statement to immediately follow
								// an `if`, given that the conditional is wrapped in parens.
								//
								// Do that here.
								self.operator_stack.pop();
								let then = Token {
									data: TokenData::Then,
									..token
								};
								self.operator_stack.push(then);
								self.output_queue.push_back(then);
								self.touching = false;
							} else if self.operator_stack.last().map(|t| t.data) == Some(TokenData::While) {
								// C-style conditionals allow any statement to immediately follow
								// a `while`, given that the conditional is wrapped in parens.
								//
								// Do that here.
								self.operator_stack.pop();
								let then = Token {
									data: TokenData::StartWhile,
									..token
								};
								self.operator_stack.push(then);
								self.output_queue.push_back(then);
								self.touching = false;
							} else if self.operator_stack.last().map(|t| t.data) == Some(TokenData::Do) {
								// do-while loops mandate that the block be immediately followed by
								// the while preamble.
								self.operator_stack.pop();
								let then = Token {
									data: TokenData::DoTail,
									..token
								};
								self.operator_stack.push(then);
								self.touching = false;
							}
							continue 'outer;
						} else if operator.data == TokenData::FnCall {
							self.output_queue.push_back(operator);
							continue 'outer;
						} else if operator.data == TokenData::Print || operator.data == TokenData::Question {
							self.errors.push_back(ParseError::Unexpected(operator));
						} else if operator.data == TokenData::OpenCurley {
							self.errors.push_back(ParseError::Mismatched(operator));
						} else {
							self.output_queue.push_back(operator);
						}
					}
					// if the stack is exhausted without finding an `(`, then it's busted.
					self.errors.push_back(ParseError::Mismatched(token));
				}
				TokenData::OpenCurley => {
					self.touching = false;
					// curley braces cannot be combined with anything else
					// after all, they don't evaluate to anything...
					while let Some(operator) = self.operator_stack.pop() {
						if operator.data == TokenData::OpenCurley || operator.data == TokenData::Then || operator.data == TokenData::Else || operator.data == TokenData::StartWhile || operator.data == TokenData::Do {
							self.operator_stack.push(operator);
							break;
						} else if operator.data == TokenData::If {
							// Rust-style conditionals allow an if statement to have any
							// expression in the condition spot, if the rest is wrapped
							// in curley braces.
							let then = Token {
								data: TokenData::Then,
								..token
							};
							self.operator_stack.push(then);
							self.output_queue.push_back(then);
						} else if operator.data == TokenData::While {
							// Rust-style conditionals allow a while statement to have any
							// expression in the condition spot, if the rest is wrapped
							// in curley braces.
							//
							// Note that Rust-style `for` loops aren't implemented, yet.
							let then = Token {
								data: TokenData::StartWhile,
								..token
							};
							self.operator_stack.push(then);
							self.output_queue.push_back(then);
						} else {
							self.output_queue.push_back(operator);
						}
					}
					self.output_queue.push_back(token);
					self.operator_stack.push(token);
				}
				TokenData::CloseCurley => {
					self.touching = false;
					while let Some(operator) = self.operator_stack.pop() {
						if operator.data == TokenData::OpenCurley {
							self.output_queue.push_back(token);
							if self.operator_stack.last().map(|t| t.data) == Some(TokenData::Then) {
								self.operator_stack.pop();
								self.operator_stack.push(Token {
									data: TokenData::ThenTail,
									..token
								});
							} else if self.operator_stack.last().map(|t| t.data) == Some(TokenData::Else) {
								self.operator_stack.pop();
								self.output_queue.push_back(Token {
									data: TokenData::EndIf,
									..token
								});
							} else if self.operator_stack.last().map(|t| t.data) == Some(TokenData::StartWhile) {
								self.operator_stack.pop();
								if let Some(for_iterate) = self.for_iterate.pop() {
									self.for_preamble = for_iterate.into();
									self.for_preamble.push_back(Token {
										data: TokenData::EndWhile,
										..token
									});
								} else {
									self.output_queue.push_back(Token {
										data: TokenData::EndWhile,
										..token
									});
								}
							} else if self.operator_stack.last().map(|t| t.data) == Some(TokenData::Do) {
								self.operator_stack.pop();
								self.output_queue.push_back(Token {
									data: TokenData::DoTail,
									..token
								});
							}
							continue 'outer;
						} else if operator.data == TokenData::OpenRound || operator.data == TokenData::FnCall {
							self.errors.push_back(ParseError::Mismatched(operator));
						} else if operator.data == TokenData::Question {
							self.errors.push_back(ParseError::Unexpected(operator));
						} else {
							self.output_queue.push_back(operator);
						}
					}
					// if the stack is exhausted without finding an `{`, then it's busted.
					self.errors.push_back(ParseError::Mismatched(token));
				}
				TokenData::Print => {
					// print, by fiat, cannot be combined with anything else
					if self.touching || !self.is_statement() {
						self.errors.push_back(ParseError::Unexpected(token));
					}
					self.touching = false;
					self.operator_stack.push(token);
				}
				TokenData::Semicolon | TokenData::LineBreak => {
					// semicolon is not a real operator
					// it's just a separator
					self.touching = false;
					self.flushing = Some(token);
				}
				TokenData::If => {
					// if, by fiat, cannot be combined with anything else
					if self.touching {
						self.errors.push_back(ParseError::Unexpected(token));
					}
					// like the semicolon, the conditional should mark the end
					// of a statement, and, in the backend, of a basic block
					self.touching = false;
					self.flushing = Some(token);
				}
				TokenData::While | TokenData::StartFor => {
					if self.operator_stack.last().map(|t| t.data) == Some(TokenData::DoTail) {
						self.operator_stack.pop();
						self.operator_stack.push(Token {
							data: TokenData::DoWhile,
							..token
						});
					} else {
						// while, by fiat, cannot be combined with anything else
						if self.touching {
							self.errors.push_back(ParseError::Unexpected(token));
						}
						if token.data != TokenData::StartFor {
							self.for_iterate.push(Vec::new());
						}
						// like the semicolon, the conditional should mark the end
						// of a statement, and, in the backend, of a basic block
						self.touching = false;
						self.flushing = Some(Token {
							data: TokenData::While,
							..token
						});
					}
				}
				TokenData::EndWhile => {
					while !self.is_statement() {
						self.output_queue.push_back(self.operator_stack.pop().unwrap());
					}
					self.touching = false;
					self.output_queue.push_back(token);
				}
				TokenData::Do => {
					// do, by fiat, cannot be combined with anything else
					if self.touching {
						self.errors.push_back(ParseError::Unexpected(token));
					}
					// like the semicolon, the "do" loop block should mark the end
					// of a statement, and, in the backend, of a basic block
					self.touching = false;
					self.flushing = Some(token);
				}
				TokenData::Else => {
					if self.touching {
						self.errors.push_back(ParseError::Unexpected(token));
						self.touching = false;
					}
					let then = if let Some(&operator) = self.operator_stack.last() {
						operator.data == TokenData::ThenTail
					} else {
						false
					};
					if then {
						self.operator_stack.pop();
						self.operator_stack.push(token);
						self.output_queue.push_back(token);
					} else {
						self.errors.push_back(ParseError::Unexpected(token));
					}
				}
				TokenData::Colon => {
					let mut found_question = false;
					while let Some(operator) = self.operator_stack.pop() {
						if (precedence(operator) > precedence(token)) ||
							(precedence(operator) == precedence(token) && is_left_associative(token)) {
							if operator.data == TokenData::Question {
								found_question = true;
								break;
							} else {
								self.output_queue.push_back(operator);
							}
						} else {
							self.operator_stack.push(operator);
							break;
						}
					}
					if found_question {
						self.operator_stack.push(token);
					} else {
						self.errors.push_back(ParseError::Unexpected(token));
					}
					self.output_queue.push_back(Token {
						data: TokenData::Else,
						..token
					});
					self.touching = false;
				}
				TokenData::For => {
					if self.touching {
						self.errors.push_back(ParseError::Unexpected(token));
						self.touching = false;
					}
					// for loops are way too irregular
					// use recursive descent
					assert!(!self.flushing.is_some());
					let token_c = self.next_token();
					let mut round_count = 0;
					if token_c.map(|t| t.data) != Some(TokenData::OpenRound) {
						self.errors.push_back(ParseError::Unexpected(token));
						continue 'outer;
					}
					// first group, run before any loop
					while let Some(token) = self.tokens.next() {
						if token.data == TokenData::OpenRound {
							round_count += 1;
						}
						if token.data == TokenData::CloseRound {
							round_count -= 1;
						}
						if token.data == TokenData::OpenCurley {
							self.errors.push_back(ParseError::Unexpected(token));
							continue 'outer;
						}
						self.for_preamble.push_back(token);
						if token.data == TokenData::Semicolon {
							if round_count != 0 {
								self.errors.push_back(ParseError::Unexpected(token));
								continue 'outer;
							}
							break;
						}
					}
					self.for_preamble.push_back(Token {
						data: TokenData::StartFor,
						..token
					});
					// second group, run as the conditional
					self.for_preamble.push_back(Token {
						data: TokenData::OpenRound,
						..token
					});
					let mut for_iterate = vec![];
					while let Some(token) = self.tokens.next() {
						if token.data == TokenData::Semicolon {
							for_iterate.push(token);
							if round_count != 0 {
								self.errors.push_back(ParseError::Unexpected(token));
								continue 'outer;
							}
							break;
						}
						if token.data == TokenData::OpenRound {
							round_count += 1;
						}
						if token.data == TokenData::CloseRound {
							round_count -= 1;
						}
						if token.data == TokenData::OpenCurley {
							self.errors.push_back(ParseError::Unexpected(token));
							continue 'outer;
						}
						self.for_preamble.push_back(token);
					}
					self.for_preamble.push_back(Token {
						data: TokenData::CloseRound,
						..token
					});
					while let Some(token) = self.tokens.next() {
						if token.data == TokenData::OpenRound {
							round_count += 1;
						}
						if token.data == TokenData::CloseRound {
							if round_count == 0 {
								break;
							}
							round_count -= 1;
						}
						if token.data == TokenData::OpenCurley {
							self.errors.push_back(ParseError::Unexpected(token));
							continue 'outer;
						}
						for_iterate.push(token);
					}
					self.for_iterate.push(for_iterate);
				}
				TokenData::Question => {
					self.handle_operator(token);
					self.output_queue.push_back(Token {
						data: TokenData::Then,
						..token
					});
					self.touching = false;
				}
				_ => {
					self.handle_operator(token);
					self.touching = false;
				}
			}
			if let Some(output) = self.output_queue.pop_front() {
				return Some(output);
			}
		}
		if let Some(output) = self.output_queue.pop_front() {
			Some(output)
		} else if let Some(operator) = self.operator_stack.pop() {
			if operator.data == TokenData::OpenRound || operator.data == TokenData::CloseRound || operator.data == TokenData::FnCall {
				self.errors.push_back(ParseError::Mismatched(operator));
			}
			if operator.data == TokenData::Question {
				self.errors.push_back(ParseError::Unexpected(operator));
			}
			if operator.data == TokenData::Then {
				self.operator_stack.push(Token {
					data: TokenData::ThenTail,
					..operator
				});
				self.next()
			} else if operator.data == TokenData::StartWhile {
				self.operator_stack.push(Token {
					data: TokenData::EndWhile,
					..operator
				});
				self.next()
			} else if operator.data == TokenData::ThenTail {
				self.output_queue.push_back(Token {
					data: TokenData::Else,
					..operator
				});
				self.output_queue.push_back(Token {
					data: TokenData::EndIf,
					..operator
				});
				self.next()
			} else if operator.data == TokenData::Else {
				self.output_queue.push_back(Token {
					data: TokenData::EndIf,
					..operator
				});
				self.next()
			} else {
				Some(operator)
			}
		} else {
			None
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::lex::*;
	fn p(data: &[u8]) -> String {
		let mut t = Tokens::new(data, None);
		let mut p = Parse::new(&mut t);
		let result = stringify(&mut p, data);
		assert!(p.errors.is_empty(), "Expected no errors {:?}", p.errors);
		result
	}
	fn err(data: &[u8]) -> Vec<ParseError> {
		let mut t = Tokens::new(data, None);
		let mut p = Parse::new(&mut t);
		let _ = stringify(&mut p, data);
		p.errors.into()
	}
	#[test]
	fn conditional_statement() {
		assert_eq!(p(b"a = 1 + 3; if a { b = 1; }"), "Word(a) Word(1) Word(3) + ASSIGN ; if Word(a) then { Word(b) Word(1) ASSIGN ; } else fi");
		assert_eq!(p(b"a = 1 + 3; if a { b = 1; } else { 2 }"), "Word(a) Word(1) Word(3) + ASSIGN ; if Word(a) then { Word(b) Word(1) ASSIGN ; } else { Word(2) } fi");
		assert_eq!(p(b"a = 1 + 3; if (a) { b = 1; }"), "Word(a) Word(1) Word(3) + ASSIGN ; if Word(a) then { Word(b) Word(1) ASSIGN ; } else fi");
		assert_eq!(p(b"a = 1 + 3; if (a) { b = 1; } else { 2 }"), "Word(a) Word(1) Word(3) + ASSIGN ; if Word(a) then { Word(b) Word(1) ASSIGN ; } else { Word(2) } fi");
		assert_eq!(p(b"a = 1 + 3; if (a) b = 1;"), "Word(a) Word(1) Word(3) + ASSIGN ; if Word(a) then Word(b) Word(1) ASSIGN ; else fi");
		assert_eq!(p(b"a = 1 + 3; if (a) b = 1; else { 2 }"), "Word(a) Word(1) Word(3) + ASSIGN ; if Word(a) then Word(b) Word(1) ASSIGN ; else { Word(2) } fi");

		assert_eq!(p(b"if a { b = 1; }"), "if Word(a) then { Word(b) Word(1) ASSIGN ; } else fi");
		assert_eq!(p(b"if a { b = 1; } else { 2 }"), "if Word(a) then { Word(b) Word(1) ASSIGN ; } else { Word(2) } fi");
		assert_eq!(p(b"if (a) { b = 1; }"), "if Word(a) then { Word(b) Word(1) ASSIGN ; } else fi");
		assert_eq!(p(b"if (a) { b = 1; } else { 2 }"), "if Word(a) then { Word(b) Word(1) ASSIGN ; } else { Word(2) } fi");
		assert_eq!(p(b"if (a) b = 1;"), "if Word(a) then Word(b) Word(1) ASSIGN ; else fi");
		assert_eq!(p(b"if (a) b = 1; else { 2 }"), "if Word(a) then Word(b) Word(1) ASSIGN ; else { Word(2) } fi");

		assert_eq!(p(b"if a { b = 1; } a = 1 + 3"), "if Word(a) then { Word(b) Word(1) ASSIGN ; } else fi Word(a) Word(1) Word(3) + ASSIGN");
		assert_eq!(p(b"if a { b = 1; } else { 2 }  a = 1 + 3"), "if Word(a) then { Word(b) Word(1) ASSIGN ; } else { Word(2) } fi Word(a) Word(1) Word(3) + ASSIGN");
		assert_eq!(p(b"if (a) { b = 1; }  a = 1 + 3"), "if Word(a) then { Word(b) Word(1) ASSIGN ; } else fi Word(a) Word(1) Word(3) + ASSIGN");
		assert_eq!(p(b"if (a) { b = 1; } else { 2 }  a = 1 + 3"), "if Word(a) then { Word(b) Word(1) ASSIGN ; } else { Word(2) } fi Word(a) Word(1) Word(3) + ASSIGN");
		assert_eq!(p(b"if (a) b = 1; a = 1 + 3"), "if Word(a) then Word(b) Word(1) ASSIGN ; else fi Word(a) Word(1) Word(3) + ASSIGN");
		assert_eq!(p(b"if (a) b = 1; else { 2 } a = 1 + 3"), "if Word(a) then Word(b) Word(1) ASSIGN ; else { Word(2) } fi Word(a) Word(1) Word(3) + ASSIGN");
	}
	#[test]
	fn loop_statement() {
		assert_eq!(p(b"a = 1 + 3; while a { b = 1; }"), "Word(a) Word(1) Word(3) + ASSIGN ; while Word(a) DO { Word(b) Word(1) ASSIGN ; } DONE");
		assert_eq!(p(b"a = 1 + 3; while (a) { b = 1; }"), "Word(a) Word(1) Word(3) + ASSIGN ; while Word(a) DO { Word(b) Word(1) ASSIGN ; } DONE");
		assert_eq!(p(b"a = 1 + 3; while (a) b = 1;"), "Word(a) Word(1) Word(3) + ASSIGN ; while Word(a) DO Word(b) Word(1) ASSIGN DONE ;");

		assert_eq!(p(b"while a { b = 1; }"), "while Word(a) DO { Word(b) Word(1) ASSIGN ; } DONE");
		assert_eq!(p(b"while a { print 1; }"), "while Word(a) DO { Word(1) print ; } DONE");
		assert_eq!(p(b"while (a) { b = 1; }"), "while Word(a) DO { Word(b) Word(1) ASSIGN ; } DONE");
		assert_eq!(p(b"while (a) b = 1;"), "while Word(a) DO Word(b) Word(1) ASSIGN DONE ;");

		assert_eq!(p(b"while a { b = 1; } a = 1 + 3"), "while Word(a) DO { Word(b) Word(1) ASSIGN ; } DONE Word(a) Word(1) Word(3) + ASSIGN");
		assert_eq!(p(b"while (a) { b = 1; } a = 1 + 3"), "while Word(a) DO { Word(b) Word(1) ASSIGN ; } DONE Word(a) Word(1) Word(3) + ASSIGN");
		assert_eq!(p(b"while (a) b = 1; a = 1 + 3"), "while Word(a) DO Word(b) Word(1) ASSIGN DONE ; Word(a) Word(1) Word(3) + ASSIGN");

		assert_eq!(p(b"for (a = 1; a < 10; a = 10) { a }"), "Word(a) Word(1) ASSIGN ; while Word(a) Word(10) < DO { Word(a) } ; Word(a) Word(10) ASSIGN DONE");
		assert_eq!(p(b"for (a = 1; a < 10; a = 10) { print a }"), "Word(a) Word(1) ASSIGN ; while Word(a) Word(10) < DO { Word(a) print } ; Word(a) Word(10) ASSIGN DONE");
	}
	#[test]
	fn one_plus_one() {
		assert_eq!(p(b"1+1"), "Word(1) Word(1) +");
		assert_eq!(p(b"1 + 1"), "Word(1) Word(1) +");
	}
	#[test]
	fn precedence() {
		assert_eq!(p(b"1 + 2 * 3"), "Word(1) Word(2) Word(3) * +");
		assert_eq!(p(b"1 * 2 + 3"), "Word(1) Word(2) * Word(3) +");
	}
	#[test]
	fn parens() {
		assert_eq!(p(b"(1 + 2) * 3"), "Word(1) Word(2) + Word(3) *");
		assert_eq!(p(b"1 * (2 + 3)"), "Word(1) Word(2) Word(3) + *");
	}
	#[test]
	fn touch_concat() {
		assert_eq!(p(b"1 2"), "Word(1) Word(2) CONCAT");
	}
	#[test]
	fn touch_concat_precedence() {
		assert_eq!(p(b"1 + 2 3"), "Word(1) Word(2) + Word(3) CONCAT");
		assert_eq!(p(b"1 2 + 3"), "Word(1) Word(2) Word(3) + CONCAT");
	}
	#[test]
	fn touch_concat_parens() {
		assert_eq!(p(b"1 + (2 3)"), "Word(1) Word(2) Word(3) CONCAT +");
	}
	#[test]
	fn assign() {
		assert_eq!(p(b"a = 1"), "Word(a) Word(1) ASSIGN");
		assert_eq!(p(b"a = 1 + 2"), "Word(a) Word(1) Word(2) + ASSIGN");
		assert_eq!(p(b"a = 1 + 2 3"), "Word(a) Word(1) Word(2) + Word(3) CONCAT ASSIGN");
		assert_eq!(p(b"a = (1 + 2) 3"), "Word(a) Word(1) Word(2) + Word(3) CONCAT ASSIGN");
		assert_eq!(p(b"a = 1 + (2 3)"), "Word(a) Word(1) Word(2) Word(3) CONCAT + ASSIGN");
	}
	#[test]
	fn print() {
		assert_eq!(p(b"print 1"), "Word(1) print");
		assert_eq!(p(b"print 1 + 2"), "Word(1) Word(2) + print");
		assert_eq!(p(b"print 1 + 2 3"), "Word(1) Word(2) + Word(3) CONCAT print");
		assert_eq!(p(b"print (1 + 2) 3"), "Word(1) Word(2) + Word(3) CONCAT print");
		assert_eq!(p(b"print 1 + (2 3)"), "Word(1) Word(2) Word(3) CONCAT + print");
		assert_eq!(p(b"print a = 1 1"), "Word(a) Word(1) Word(1) CONCAT ASSIGN print");
	}
	#[test]
	fn call() {
		assert_eq!(p(b"abc(1)"), "Word(1) FnCall(abc)");
		assert_eq!(p(b"abc(1 + 2)"), "Word(1) Word(2) + FnCall(abc)");
		assert_eq!(p(b"abc(1 + 2 3)"), "Word(1) Word(2) + Word(3) CONCAT FnCall(abc)");
		assert_eq!(p(b"abc((1 + 2) 3)"), "Word(1) Word(2) + Word(3) CONCAT FnCall(abc)");
		assert_eq!(p(b"abc(1 + (2 3))"), "Word(1) Word(2) Word(3) CONCAT + FnCall(abc)");
		assert_eq!(p(b"abc(a = 1 1)"), "Word(a) Word(1) Word(1) CONCAT ASSIGN FnCall(abc)");
	}
	#[test]
	fn call_nested() {
		assert_eq!(p(b"abc(a(1))"), "Word(1) FnCall(a) FnCall(abc)");
		assert_eq!(p(b"abc(a(1) + a(2))"), "Word(1) FnCall(a) Word(2) FnCall(a) + FnCall(abc)");
		assert_eq!(p(b"abc(a(1 + 2) 3)"), "Word(1) Word(2) + FnCall(a) Word(3) CONCAT FnCall(abc)");
		assert_eq!(p(b"abc((1 + 2) a(3))"), "Word(1) Word(2) + Word(3) FnCall(a) CONCAT FnCall(abc)");
		assert_eq!(p(b"abc(fn(a) = 1 a(1))"), "Word(a) FnCall(fn) Word(1) Word(1) FnCall(a) CONCAT ASSIGN FnCall(abc)");
	}
	#[test]
	fn print_call() {
		assert_eq!(p(b"print a(1)"), "Word(1) FnCall(a) print");
		assert_eq!(p(b"print a(1 + 2)"), "Word(1) Word(2) + FnCall(a) print");
		assert_eq!(p(b"print a(1 + 2 3)"), "Word(1) Word(2) + Word(3) CONCAT FnCall(a) print");
		assert_eq!(p(b"print a((1 + 2) 3)"), "Word(1) Word(2) + Word(3) CONCAT FnCall(a) print");
		assert_eq!(p(b"print a(1 + (2 3))"), "Word(1) Word(2) Word(3) CONCAT + FnCall(a) print");
		assert_eq!(p(b"print a(a = 1 1)"), "Word(a) Word(1) Word(1) CONCAT ASSIGN FnCall(a) print");
	}
	#[test]
	fn statement_separators() {
		assert_eq!(p(b"a;b"), "Word(a) ; Word(b)");
		assert_eq!(p(b"a + b ; c"), "Word(a) Word(b) + ; Word(c)");
		assert_eq!(p(b"a + b ; c + d"), "Word(a) Word(b) + ; Word(c) Word(d) +");
	}
	// notriddle:~$ gawk 'BEGIN { print 8 / 2 / 2 }'
	// 2
	// notriddle:~$ gawk 'BEGIN { print 8 / (2 / 2) }'
	// 8
	#[test]
	fn divide_is_left_associative() {
		assert_eq!(p(b"8 / 2 / 2"), "Word(8) Word(2) / Word(2) /");
		assert_eq!(p(b"8 / (2 / 2)"), "Word(8) Word(2) Word(2) / /");
	}
	// notriddle:~$ gawk 'BEGIN { print 8 - 2 - 2 }'
	// 4
	// notriddle:~$ gawk 'BEGIN { print 8 - (2 - 2) }'
	// 8
	#[test]
	fn minus_is_left_associative() {
		assert_eq!(p(b"8 - 2 - 2"), "Word(8) Word(2) - Word(2) -");
		assert_eq!(p(b"8 - (2 - 2)"), "Word(8) Word(2) Word(2) - -");
	}
	// gawk and onetrueawk both produce a syntax error on this code
	// for maximum compatibility, let's follow what mawk does here
	//
	// notriddle:~$ mawk 'BEGIN { print 3 == 2 == 0 }'
	// 1
	// notriddle:~$ mawk 'BEGIN { print 3 == (2 == 0) }'
	// 0
	#[test]
	fn equals_is_left_associative() {
		assert_eq!(p(b"3 == 2 == 0"), "Word(3) Word(2) == Word(0) ==");
		assert_eq!(p(b"3 == (2 == 0)"), "Word(3) Word(2) Word(0) == ==");
	}
	// notriddle:~$ gawk 'BEGIN { print 1 2 == "12" }'
	// 1
	// notriddle:~$ gawk 'BEGIN { print 1 (2 == "12") }'
	// 0
	// notriddle:~$ gawk 'BEGIN { print "12" == 1 2 }'
	// 1
	#[test]
	fn equals_is_weaker_than_concat() {
		assert_eq!(p(br#" 1 2 == "12" "#), r#"Word(1) Word(2) CONCAT String("12") =="#);
		assert_eq!(p(br#" 1 (2 == "12") "#), r#"Word(1) Word(2) String("12") == CONCAT"#);
		assert_eq!(p(br#" "12" == 1 2 "#), r#"String("12") Word(1) Word(2) CONCAT =="#);
	}
	// notriddle:~$ echo "4 8" | gawk '/4/ { print $2/2 }'
	// 4
	#[test]
	fn dollar_is_stronger_than_divide() {
		assert_eq!(p(br#" $2/2 "#), r#"Word(2) $ Word(2) /"#);
	}
	// notriddle:~$ gawk 'BEGIN { print 1 2 == "12" }'
	// 1
	// notriddle:~$ gawk 'BEGIN { print 1 (2 == "12") }'
	// 0
	// notriddle:~$ gawk 'BEGIN { print "12" == 1 2 }'
	// 1
	#[test]
	fn equals_is_stronger_than_assign() {
		assert_eq!(p(br"a = 1 == 2"), r#"Word(a) Word(1) Word(2) == ASSIGN"#);
	}
	#[test]
	fn bad_ternary() {
		assert_eq!(err(br"a ?"), vec![
			ParseError::Unexpected(Token {
				data: TokenData::Question,
				start: 2,
				end: 3,
			})
		]);
		assert_eq!(err(br"a :"), vec![
			ParseError::Unexpected(Token {
				data: TokenData::Colon,
				start: 2,
				end: 3,
			})
		]);
		assert_eq!(err(br"a ? b = c : d"), vec![
			ParseError::Unexpected(Token {
				data: TokenData::Assign,
				start: 6,
				end: 7,
			}),
			ParseError::Unexpected(Token {
				data: TokenData::Colon,
				start: 10,
				end: 11,
			}),
		]);
		assert_eq!(err(br"a ? b : print d"), vec![
			ParseError::Unexpected(Token {
				data: TokenData::Print,
				start: 8,
				end: 13,
			}),
		]);
		assert_eq!(err(br"a ? (b : c)"), vec![
			ParseError::Unexpected(Token {
				data: TokenData::Colon,
				start: 7,
				end: 8,
			}),
			ParseError::Unexpected(Token {
				data: TokenData::Question,
				start: 2,
				end: 3,
			}),
		]);
		assert_eq!(err(br"(a ? b) : c"), vec![
			ParseError::Unexpected(Token {
				data: TokenData::Question,
				start: 3,
				end: 4,
			}),
			ParseError::Unexpected(Token {
				data: TokenData::Colon,
				start: 8,
				end: 9,
			}),
		]);
		assert_eq!(err(br"(a ? b)"), vec![
			ParseError::Unexpected(Token {
				data: TokenData::Question,
				start: 3,
				end: 4,
			}),
		]);
	}
	// The ternary operator gets compiled to a ":" operand which,
	// in the logical machine of the parser, consumes three stack items.
	#[test]
	fn good_ternary() {
		assert_eq!(p(br"a = 1 ? 2 : 3"), r#"Word(a) Word(1) then Word(2) else Word(3) : ASSIGN"#);
		assert_eq!(p(br"a = 1 ? 2 : 3 ? 4 : 5"), r#"Word(a) Word(1) then Word(2) else Word(3) then Word(4) else Word(5) : : ASSIGN"#);
		assert_eq!(p(br"a = 1 ? 2 : (3 ? 4 : 5)"), r#"Word(a) Word(1) then Word(2) else Word(3) then Word(4) else Word(5) : : ASSIGN"#);
		assert_eq!(p(br"a = (1 ? 2 : 3) ? 4 : 5"), r#"Word(a) Word(1) then Word(2) else Word(3) : then Word(4) else Word(5) : ASSIGN"#);
		assert_eq!(p(br"(1 ? 2 : 3) ? 4 : 5"), r#"Word(1) then Word(2) else Word(3) : then Word(4) else Word(5) :"#);
		assert_eq!(p(br"a(1 ? 2 : 3)"), r#"Word(1) then Word(2) else Word(3) : FnCall(a)"#);
		assert_eq!(p(br"1 ? (2 ? 3 : 4) : 5"), r#"Word(1) then Word(2) then Word(3) else Word(4) : else Word(5) :"#);
		assert_eq!(p(br"1 ? 2 ? 3 : 4 : 5"), r#"Word(1) then Word(2) then Word(3) else Word(4) : else Word(5) :"#);
	}
}
