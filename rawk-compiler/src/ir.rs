use crate::lex::{Personality, Tokens, Token, TokenData};
use crate::parse::Parse;
use crate::parse_pattern::{Pattern, PatternParser};
use std::str;
use std::str::FromStr;
use regex::Regex;
use std::collections::HashMap;

const LIMIT: usize = 1 << 31;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct InstructionId(u32);

impl InstructionId {
	pub fn null() -> InstructionId {
		InstructionId(!0)
	}
	pub fn is_null(self) -> bool {
		self.0 == !0
	}
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct BlockId(u32);

impl BlockId {
	pub fn null() -> BlockId {
		BlockId(!0)
	}
	pub fn is_null(self) -> bool {
		self.0 == !0
	}
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct VariableId(u32);

impl VariableId {
	pub fn null() -> VariableId {
		VariableId(!0)
	}
	pub fn is_null(self) -> bool {
		self.0 == !0
	}
}

#[derive(Debug)]
pub struct Block {
	instructions: Vec<Instruction>,
	terminal: Terminal,
}

impl Block {
	fn new() -> Block {
		Block {
			instructions: Vec::new(),
			terminal: Terminal::Return,
		}
	}
	fn next_instruction_id(&self) -> InstructionId {
		assert!(self.instructions.len() < LIMIT);
		InstructionId(self.instructions.len() as u32)
	}
	fn push(&mut self, i: Instruction) -> InstructionId {
		let n = self.next_instruction_id();
		self.instructions.push(i);
		n
	}
	pub fn slow_interpret(&self, captures: &[String], init: Literal, variables: &mut HashMap<VariableId, Literal>, output: &mut SlowInterpretOutput) -> Literal {
		let mut results = vec![];
		for instruction in self.instructions.iter() {
			match instruction {
				&Instruction::Load(variable_id) => {
					results.push(variables.get(&variable_id).cloned().unwrap_or(Literal::String(String::new())));
				}
				&Instruction::Assign(variable_id, a) => {
					let a = results[a.0 as usize].clone();
					variables.insert(variable_id, a.clone());
					results.push(a);
				}
				&Instruction::Literal(ref f) => {
					results.push(f.clone());
				}
				&Instruction::Capture(a) => {
					let a = &results[a.0 as usize];
					let r = match a {
						&Literal::Int(a) => Literal::String(captures[a as usize].clone()),
						&Literal::Float(a) => Literal::String(captures[a as usize].clone()),
						&Literal::String(ref a) => Literal::String(captures[usize::from_str(a).expect("valid int for capture")].clone()),
						&Literal::Regex(_) => unimplemented!(),
					};
					results.push(r);
				}
				&Instruction::Less(a, b) => {
					let a = &results[a.0 as usize];
					let b = &results[b.0 as usize];
					let r = match (a, b) {
						(&Literal::Int(a), &Literal::Int(b)) => Literal::Int(if a < b { 1 } else { 0 }),
						(&Literal::Float(a), &Literal::Float(b)) => Literal::Int(if a < b { 1 } else { 0 }),
						(&Literal::Int(a), &Literal::Float(b)) => Literal::Int(if (a as f64) < b { 1 } else { 0 }),
						(&Literal::Float(a), &Literal::Int(b)) => Literal::Int(if a < b as f64 { 1 } else { 0 }),
						(&Literal::Int(a), &Literal::String(ref b)) => {
							// intentionally casting everything to float
							let b = f64::from_str(b).unwrap_or(0.);
							Literal::Int(if (a as f64) < b { 1 } else { 0 })
						}
						(&Literal::String(ref a), &Literal::Int(b)) => {
							// intentionally casting everything to float
							let a = f64::from_str(a).unwrap_or(0.);
							Literal::Int(if a < b as f64 { 1 } else { 0 })
						}
						(&Literal::Float(a), &Literal::String(ref b)) => {
							let b = f64::from_str(b).unwrap_or(0.);
							Literal::Int(if a < b { 1 } else { 0 })
						}
						(&Literal::String(ref a), &Literal::Float(b)) => {
							let a = f64::from_str(a).unwrap_or(0.);
							Literal::Int(if a < b { 1 } else { 0 })
						}
						_ => unimplemented!(),
					};
					results.push(r);
				}
				&Instruction::Greater(a, b) => {
					let a = &results[a.0 as usize];
					let b = &results[b.0 as usize];
					let r = match (a, b) {
						(&Literal::Int(a), &Literal::Int(b)) => Literal::Int(if a > b { 1 } else { 0 }),
						(&Literal::Float(a), &Literal::Float(b)) => Literal::Int(if a > b { 1 } else { 0 }),
						(&Literal::Int(a), &Literal::Float(b)) => Literal::Int(if a as f64 > b { 1 } else { 0 }),
						(&Literal::Float(a), &Literal::Int(b)) => Literal::Int(if a > b as f64 { 1 } else { 0 }),
						(&Literal::Int(a), &Literal::String(ref b)) => {
							// intentionally casting everything to float
							let b = f64::from_str(b).unwrap_or(0.);
							Literal::Int(if a as f64 > b { 1 } else { 0 })
						}
						(&Literal::String(ref a), &Literal::Int(b)) => {
							// intentionally casting everything to float
							let a = f64::from_str(a).unwrap_or(0.);
							Literal::Int(if a > b as f64 { 1 } else { 0 })
						}
						(&Literal::Float(a), &Literal::String(ref b)) => {
							let b = f64::from_str(b).unwrap_or(0.);
							Literal::Int(if a > b as f64 { 1 } else { 0 })
						}
						(&Literal::String(ref a), &Literal::Float(b)) => {
							let a = f64::from_str(a).unwrap_or(0.);
							Literal::Int(if a > b { 1 } else { 0 })
						}
						_ => unimplemented!(),
					};
					results.push(r);
				}
				&Instruction::Add(a, b) => {
					let a = &results[a.0 as usize];
					let b = &results[b.0 as usize];
					let r = match (a, b) {
						(&Literal::Int(a), &Literal::Int(b)) => Literal::Int(a + b),
						(&Literal::Float(a), &Literal::Float(b)) => Literal::Float(a + b),
						(&Literal::Int(a), &Literal::Float(b)) => Literal::Float(a as f64 + b),
						(&Literal::Float(a), &Literal::Int(b)) => Literal::Float(a + b as f64),
						(&Literal::Int(a), &Literal::String(ref b)) => {
							// intentionally casting everything to float
							let b = f64::from_str(b).unwrap_or(0.);
							Literal::Float(a as f64 + b)
						}
						(&Literal::String(ref a), &Literal::Int(b)) => {
							// intentionally casting everything to float
							let a = f64::from_str(a).unwrap_or(0.);
							Literal::Float(a + b as f64)
						}
						(&Literal::Float(a), &Literal::String(ref b)) => {
							let b = f64::from_str(b).unwrap_or(0.);
							Literal::Float(a + b)
						}
						(&Literal::String(ref a), &Literal::Float(b)) => {
							let a = f64::from_str(a).unwrap_or(0.);
							Literal::Float(a + b)
						}
						_ => unimplemented!(),
					};
					results.push(r);
				}
				&Instruction::Subtract(a, b) => {
					let a = &results[a.0 as usize];
					let b = &results[b.0 as usize];
					let r = match (a, b) {
						(&Literal::Int(a), &Literal::Int(b)) => Literal::Int(a - b),
						(&Literal::Float(a), &Literal::Float(b)) => Literal::Float(a - b),
						(&Literal::Int(a), &Literal::Float(b)) => Literal::Float(a as f64 - b),
						(&Literal::Float(a), &Literal::Int(b)) => Literal::Float(a - b as f64),
						(&Literal::Int(a), &Literal::String(ref b)) => {
							// intentionally casting everything to float
							let b = f64::from_str(b).unwrap_or(0.);
							Literal::Float(a as f64 - b)
						}
						(&Literal::String(ref a), &Literal::Int(b)) => {
							// intentionally casting everything to float
							let a = f64::from_str(a).unwrap_or(0.);
							Literal::Float(a - b as f64)
						}
						(&Literal::Float(a), &Literal::String(ref b)) => {
							let b = f64::from_str(b).unwrap_or(0.);
							Literal::Float(a - b)
						}
						(&Literal::String(ref a), &Literal::Float(b)) => {
							let a = f64::from_str(a).unwrap_or(0.);
							Literal::Float(a - b)
						}
						_ => unimplemented!(),
					};
					results.push(r);
				}
				&Instruction::Multiply(a, b) => {
					let a = &results[a.0 as usize];
					let b = &results[b.0 as usize];
					let r = match (a, b) {
						(&Literal::Int(a), &Literal::Int(b)) => Literal::Int(a * b),
						(&Literal::Float(a), &Literal::Float(b)) => Literal::Float(a * b),
						(&Literal::Int(a), &Literal::Float(b)) => Literal::Float(a as f64 * b),
						(&Literal::Float(a), &Literal::Int(b)) => Literal::Float(a * b as f64),
						(&Literal::Int(a), &Literal::String(ref b)) => {
							// intentionally casting everything to float
							let b = f64::from_str(b).unwrap_or(0.);
							Literal::Float(a as f64 * b)
						}
						(&Literal::String(ref a), &Literal::Int(b)) => {
							// intentionally casting everything to float
							let a = f64::from_str(a).unwrap_or(0.);
							Literal::Float(a * b as f64)
						}
						(&Literal::Float(a), &Literal::String(ref b)) => {
							let b = f64::from_str(b).unwrap_or(0.);
							Literal::Float(a * b)
						}
						(&Literal::String(ref a), &Literal::Float(b)) => {
							let a = f64::from_str(a).unwrap_or(0.);
							Literal::Float(a * b)
						}
						_ => unimplemented!(),
					};
					results.push(r);
				}
				&Instruction::Divide(a, b) => {
					let a = &results[a.0 as usize];
					let b = &results[b.0 as usize];
					let r = match (a, b) {
						(&Literal::Int(a), &Literal::Int(b)) => Literal::Int(a / b),
						(&Literal::Float(a), &Literal::Float(b)) => Literal::Float(a / b),
						(&Literal::Int(a), &Literal::Float(b)) => Literal::Float(a as f64 / b),
						(&Literal::Float(a), &Literal::Int(b)) => Literal::Float(a / b as f64),
						(&Literal::Int(a), &Literal::String(ref b)) => {
							// intentionally casting everything to float
							let b = f64::from_str(b).unwrap_or(0.);
							Literal::Float(a as f64 / b)
						}
						(&Literal::String(ref a), &Literal::Int(b)) => {
							// intentionally casting everything to float
							let a = f64::from_str(a).unwrap_or(0.);
							Literal::Float(a / b as f64)
						}
						(&Literal::Float(a), &Literal::String(ref b)) => {
							let b = f64::from_str(b).unwrap_or(0.);
							Literal::Float(a / b)
						}
						(&Literal::String(ref a), &Literal::Float(b)) => {
							let a = f64::from_str(a).unwrap_or(0.);
							Literal::Float(a / b)
						}
						_ => unimplemented!(),
					};
					results.push(r);
				}
				&Instruction::Concat(a, b) => {
					let a = &results[a.0 as usize];
					let b = &results[b.0 as usize];
					let r = match (a, b) {
						(&Literal::Int(a), &Literal::Int(b)) => Literal::String(format!("{}{}", a, b)),
						(&Literal::Float(a), &Literal::Float(b)) => Literal::String(format!("{}{}", a, b)),
						(&Literal::Int(a), &Literal::Float(b)) => Literal::String(format!("{}{}", a, b)),
						(&Literal::Float(a), &Literal::Int(b)) => Literal::String(format!("{}{}", a, b)),
						(&Literal::Int(a), &Literal::String(ref b)) => Literal::String(format!("{}{}", a, b)),
						(&Literal::String(ref a), &Literal::Int(b)) => Literal::String(format!("{}{}", a, b)),
						(&Literal::Float(a), &Literal::String(ref b)) => Literal::String(format!("{}{}", a, b)),
						(&Literal::String(ref a), &Literal::Float(b)) => Literal::String(format!("{}{}", a, b)),
						(&Literal::String(ref a), &Literal::String(ref b)) => Literal::String(format!("{}{}", a, b)),
						_ => unimplemented!(),
					};
					results.push(r);
				}
				&Instruction::Equals(a, b) | &Instruction::NotEquals(a, b) => {
					let a = &results[a.0 as usize];
					let b = &results[b.0 as usize];
					let (t, f) = if let &Instruction::NotEquals(..) = instruction {
						(0, 1)
					} else {
						(1, 0)
					};
					let lit = Literal::Int(if a.to_string() == b.to_string() { t } else { f });
					results.push(lit);
				}
				&Instruction::Matches(a, b) | &Instruction::NotMatches(a, b) => {
					let a = &results[a.0 as usize];
					let b = &results[b.0 as usize];
					let r = match (a, b) {
						(a, &Literal::Regex(ref b)) => {
							b.is_match(&a.to_string())
						}
						(a, b) => {
							Regex::new(&b.to_string()).unwrap().is_match(&a.to_string())
						}
					};
					let (t, f) = if let &Instruction::NotMatches(..) = instruction {
						(0, 1)
					} else {
						(1, 0)
					};
					results.push(Literal::Int(if r { t } else { f }));
				}
				&Instruction::Conditional(a, b, c) => {
					let a = &results[a.0 as usize];
					let b = results[b.0 as usize].clone();
					let c = results[c.0 as usize].clone();
					let truthy = a.is_truthy();
					results.push(if truthy { b } else { c });
				}
			}
		}
		results.pop().unwrap_or(init)
	}
}

#[derive(Clone, Debug)]
pub enum Literal {
	Int(i64),
	Float(f64),
	String(String),
	Regex(Regex),
}

impl Literal {
	pub fn is_truthy(&self) -> bool {
		match self {
			&Literal::Int(a) => a != 0,
			&Literal::Float(a) => a != 0.,
			&Literal::String(ref a) => a != "",
			&Literal::Regex(_) => true,
		}
	}
	pub fn to_string(&self) -> String {
		match self {
			&Literal::Int(i) => i.to_string(),
			&Literal::Float(f) => f.to_string(),
			&Literal::String(ref s) => s.clone(),
			&Literal::Regex(_) => String::from("0"),
		}
	}
}

#[derive(Debug)]
pub enum Instruction {
	Capture(InstructionId),
	Less(InstructionId, InstructionId),
	Greater(InstructionId, InstructionId),
	Add(InstructionId, InstructionId),
	Subtract(InstructionId, InstructionId),
	Multiply(InstructionId, InstructionId),
	Divide(InstructionId, InstructionId),
	Concat(InstructionId, InstructionId),
	Matches(InstructionId, InstructionId),
	NotMatches(InstructionId, InstructionId),
	Equals(InstructionId, InstructionId),
	NotEquals(InstructionId, InstructionId),
	Conditional(InstructionId, InstructionId, InstructionId),
	Literal(Literal),
	Assign(VariableId, InstructionId),
	Load(VariableId),
}

#[derive(Clone, Copy, Debug)]
pub enum Terminal {
	Discard,
	Return,
	ReturnIfTrue,
	Print,
	// if this block's last instruction evaluates to a falsy value,
	// then the branch is taken
	// otherwise, it's not
	Branch { target: BlockId },
	// this is the unconditional jump
	Jump { target: BlockId },
}

impl Terminal {
	fn target(&self) -> BlockId {
		match *self {
			Terminal::Branch { target } => target,
			Terminal::Jump { target } => target,
			_ => panic!("attempted to get branch target of non-control block"),
		}
	}
}

#[derive(Debug)]
pub struct BlockSet {
	blocks: Vec<Block>,
}

impl BlockSet {
	pub fn new() -> BlockSet {
		BlockSet {
			blocks: Vec::new(),
		}
	}
	fn next_block_id(&self) -> BlockId {
		assert!(self.blocks.len() < LIMIT);
		BlockId(self.blocks.len() as u32)
	}
	fn push(&mut self, i: Block) -> BlockId {
		let n = self.next_block_id();
		self.blocks.push(i);
		n
	}
	pub fn slow_interpret(&self, captures: &[String], output: &mut SlowInterpretOutput) -> Literal {
		let mut block_id = BlockId(0);
		let mut result = Literal::String(String::new());
		let mut variables = HashMap::new();
		loop {
			let block = &self.blocks[block_id.0 as usize];
			result = block.slow_interpret(captures, result, &mut variables, output);
			match block.terminal {
				Terminal::Branch { target } => {
					let truthy = result.is_truthy();
					if !truthy {
						block_id = target;
						continue;
					}
				}
				Terminal::Jump { target } => {
					block_id = target;
					continue;
				}
				Terminal::Return => break,
				Terminal::ReturnIfTrue => {
					let truthy = result.is_truthy();
					if truthy {
						break;
					}
				}
				Terminal::Print => {
					output.output.push(result.to_string());
				}
				_ => {},
			}
			block_id.0 += 1;
		}
		result
	}
}

pub struct Program {
	pattern_action: Vec<(BlockSet, BlockSet)>,
}

impl Program {
	fn new() -> Program {
		Program {
			pattern_action: Vec::new(),
		}
	}
	pub fn slow_interpret(&self, captures: &[String], output: &mut SlowInterpretOutput) {
		for (ref pattern, ref action) in &self.pattern_action {
			let matches = pattern.slow_interpret(captures, output).is_truthy();
			if matches {
				action.slow_interpret(captures, output);
			}
		}
	}
}

pub fn compile_program(code: &[u8], personality: Option<Personality>) -> Program {
	let mut p = PatternParser::new(code, personality);
	let mut program = Program::new();
	while let Some(mut pb) = p.next() {
		let mut pattern = BlockSet::new();
		let mut action = BlockSet::new();
		while let Some(p) = pb.next_pattern() {
			match p {
				Pattern::Regex(token) => {
					let mut block = Block::new();
					// the +1 and -1 strips the / delimiters
					let string = if let Some(personality) = personality {
						rawk_regex::convert(&code[token.start+1..token.end-1], personality).unwrap()
					} else {
						str::from_utf8(&code[token.start+1..token.end-1]).unwrap_or("INVALID").to_owned()
					};
					let regex = Regex::new(&string).unwrap();
					let zero = block.push(Instruction::Literal(Literal::Int(0)));
					let line = block.push(Instruction::Capture(zero));
					let regex = block.push(Instruction::Literal(Literal::Regex(regex)));
					let _matches = block.push(Instruction::Matches(line, regex));
					pattern.push(block);
				}
				Pattern::ParsedExpr(tokens) => {
					compile_action(&mut pattern, tokens.into_iter(), code, personality);
				}
			}
		}
		// the pattern should short-circuit if one of the subpatterns is true
		for block in &mut pattern.blocks {
			if let Terminal::Return = block.terminal {
				block.terminal = Terminal::ReturnIfTrue;
			}
		}
		pattern.push(Block::new());
		compile_action(&mut action, pb, code, personality);
		program.pattern_action.push((pattern, action));
	}
	program
}

pub fn compile_action(block_set: &mut BlockSet, parse: impl Iterator<Item=Token>, code: &[u8], personality: Option<Personality>) {
	let mut block = Block::new();
	let mut stack = Vec::new();
	let mut control_stack = Vec::new();
	let mut if_cond = false;
	let mut while_cond = false;
	let mut variables: HashMap<Vec<u8>, VariableId> = HashMap::new();
	let mut next_variable_id = 1u32;
	for token in parse {
		match token.data {
			TokenData::If => {}
			TokenData::Then => {
				control_stack.push(block_set.push(std::mem::replace(&mut block, Block::new())));
			}
			TokenData::Else => {
				let control_block = control_stack.last().expect("else must be after if then");
				let target = block_set.push(std::mem::replace(&mut block, Block::new()));
				block_set.blocks[control_block.0 as usize].terminal = Terminal::Branch { target };
			}
			TokenData::EndIf | TokenData::Colon => {
				let control_block = control_stack.pop().expect("endif must be after else");
				let then_block = block_set.blocks[control_block.0 as usize].terminal.target();
				let else_block = block_set.push(std::mem::replace(&mut block, Block::new()));
				let target = block_set.next_block_id();
				block_set.blocks[control_block.0 as usize].terminal = Terminal::Branch { target: else_block };
				block_set.blocks[then_block.0 as usize].terminal = Terminal::Jump { target };
				block_set.blocks[else_block.0 as usize].terminal = Terminal::Jump { target };
			}
			TokenData::While => {
				while_cond = true;
			}
			TokenData::StartWhile => {
				if !while_cond {
					unimplemented!();
				}
				while_cond = false;
				control_stack.push(block_set.push(std::mem::replace(&mut block, Block::new())));
			}
			TokenData::EndWhile => {
				let control_block = control_stack.pop().expect("endwhile must be after while");
				block.terminal = Terminal::Jump { target: control_block };
				let do_block = block_set.push(std::mem::replace(&mut block, Block::new()));
				let target = block_set.next_block_id();
				block_set.blocks[control_block.0 as usize].terminal = Terminal::Branch { target };
			}
			TokenData::Dollar => {
				let capture = stack.pop().expect("capture");
				stack.push(block.push(Instruction::Capture(capture)));
			}
			TokenData::Assign => {
				let value = stack.pop().expect("value");
				let place = stack.pop().expect("place");
				let variable_id = match &block.instructions[place.0 as usize] {
					Instruction::Load(variable_id) => variable_id.clone(),
					_ => unimplemented!(),
				};
				stack.push(block.push(Instruction::Assign(variable_id, value)));
			}
			TokenData::PlusAssign => {
				let value = stack.pop().expect("value");
				let place = stack.pop().expect("place");
				let variable_id = match &block.instructions[place.0 as usize] {
					Instruction::Load(variable_id) => variable_id.clone(),
					_ => unimplemented!(),
				};
				let add = block.push(Instruction::Add(place, value));
				stack.push(block.push(Instruction::Assign(variable_id, add)));
			}
			TokenData::MinusAssign => {
				let value = stack.pop().expect("value");
				let place = stack.pop().expect("place");
				let variable_id = match &block.instructions[place.0 as usize] {
					Instruction::Load(variable_id) => variable_id.clone(),
					_ => unimplemented!(),
				};
				let add = block.push(Instruction::Subtract(place, value));
				stack.push(block.push(Instruction::Assign(variable_id, add)));
			}
			TokenData::TimesAssign => {
				let value = stack.pop().expect("value");
				let place = stack.pop().expect("place");
				let variable_id = match &block.instructions[place.0 as usize] {
					Instruction::Load(variable_id) => variable_id.clone(),
					_ => unimplemented!(),
				};
				let add = block.push(Instruction::Multiply(place, value));
				stack.push(block.push(Instruction::Assign(variable_id, add)));
			}
			TokenData::DivideAssign => {
				let value = stack.pop().expect("value");
				let place = stack.pop().expect("place");
				let variable_id = match &block.instructions[place.0 as usize] {
					Instruction::Load(variable_id) => variable_id.clone(),
					_ => unimplemented!(),
				};
				let add = block.push(Instruction::Divide(place, value));
				stack.push(block.push(Instruction::Assign(variable_id, add)));
			}
			TokenData::Word => {
				let inst = if let Ok(f) = i64::from_str(str::from_utf8(&code[token.start..token.end]).unwrap_or("INVALID")) {
					Instruction::Literal(Literal::Int(f))
				} else if let Ok(f) = f64::from_str(str::from_utf8(&code[token.start..token.end]).unwrap_or("INVALID")) {
					Instruction::Literal(Literal::Float(f))
				} else {
					let name = &code[token.start..token.end];
					let variable = if let Some(variable) = variables.get(name) {
						variable.clone()
					} else {
						let variable = VariableId(next_variable_id);
						next_variable_id += 1;
						variables.insert(name.to_owned(), variable);
						variable
					};
					Instruction::Load(variable)
				};
				stack.push(block.push(inst));
			}
			TokenData::String => {
				// +1 and -1 strip the quote marks
				let string = str::from_utf8(&code[token.start+1..token.end-1]).unwrap_or("INVALID").to_owned();
				stack.push(block.push(Instruction::Literal(Literal::String(string))));
			}
			TokenData::Regex => {
				// the +1 and -1 strips the / delimiters
				let string = if let Some(personality) = personality {
					rawk_regex::convert(&code[token.start+1..token.end-1], personality).unwrap()
				} else {
					str::from_utf8(&code[token.start+1..token.end-1]).unwrap_or("INVALID").to_owned()
				};
				let regex = Regex::new(&string).unwrap();
				stack.push(block.push(Instruction::Literal(Literal::Regex(regex))));
			}
			TokenData::Colon => {
				let then_else = stack.pop().expect("after the ':' token");
				let then_do = stack.pop().expect("after the '?' token, before the ':' token");
				let if_cond = stack.pop().expect("before the '?' token");
				stack.push(block.push(Instruction::Conditional(if_cond, then_do, then_else)));
			}
			TokenData::Less => {
				let right = stack.pop().expect("right");
				let left = stack.pop().expect("left");
				stack.push(block.push(Instruction::Less(left, right)));
			}
			TokenData::Greater => {
				let right = stack.pop().expect("right");
				let left = stack.pop().expect("left");
				stack.push(block.push(Instruction::Greater(left, right)));
			}
			TokenData::Plus => {
				let right = stack.pop().expect("right");
				let left = stack.pop().expect("left");
				stack.push(block.push(Instruction::Add(left, right)));
			}
			TokenData::Minus => {
				let right = stack.pop().expect("right");
				let left = stack.pop().expect("left");
				stack.push(block.push(Instruction::Subtract(left, right)));
			}
			TokenData::Times => {
				let right = stack.pop().expect("right");
				let left = stack.pop().expect("left");
				stack.push(block.push(Instruction::Multiply(left, right)));
			}
			TokenData::Divide => {
				let right = stack.pop().expect("right");
				let left = stack.pop().expect("left");
				stack.push(block.push(Instruction::Divide(left, right)));
			}
			TokenData::Equals => {
				let right = stack.pop().expect("right");
				let left = stack.pop().expect("left");
				stack.push(block.push(Instruction::Equals(left, right)));
			}
			TokenData::NotEquals => {
				let right = stack.pop().expect("right");
				let left = stack.pop().expect("left");
				stack.push(block.push(Instruction::NotEquals(left, right)));
			}
			TokenData::Matches => {
				let right = stack.pop().expect("right");
				let left = stack.pop().expect("left");
				stack.push(block.push(Instruction::Matches(left, right)));
			}
			TokenData::NotMatches => {
				let right = stack.pop().expect("right");
				let left = stack.pop().expect("left");
				stack.push(block.push(Instruction::NotMatches(left, right)));
			}
			TokenData::Concat => {
				let right = stack.pop().expect("right");
				let left = stack.pop().expect("left");
				stack.push(block.push(Instruction::Concat(left, right)));
			}
			TokenData::Semicolon | TokenData::LineBreak => {
				let mut old_block = std::mem::replace(&mut block, Block::new());
				old_block.terminal = Terminal::Discard;
				block_set.push(old_block);
			}
			TokenData::Print => {
				let mut old_block = std::mem::replace(&mut block, Block::new());
				old_block.terminal = Terminal::Print;
				block_set.push(old_block);
			}
			TokenData::Print0 => {
				let mut old_block = std::mem::replace(&mut block, Block::new());
				let zero = old_block.push(Instruction::Literal(Literal::Int(0)));
				old_block.push(Instruction::Capture(zero));
				old_block.terminal = Terminal::Print;
				block_set.push(old_block);
			}
			TokenData::OpenCurley | TokenData::CloseCurley => {}
			_ => unimplemented!("{:?}", token.data),
		}
	}
	block_set.push(std::mem::replace(&mut block, Block::new()));
}

#[derive(Debug)]
pub struct SlowInterpretOutput {
	pub output: Vec<String>,
}

impl SlowInterpretOutput {
	fn new() -> SlowInterpretOutput {
		SlowInterpretOutput { output: Vec::new() }
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	fn exec(code: &str) -> String {
		let tokens = crate::lex::Tokens::new(code.as_bytes(), None);
		let parse = crate::parse::Parse::new(tokens);
		let mut blocks = BlockSet::new();
		compile_action(&mut blocks, parse, code.as_bytes(), None);
		println!("{:?}", blocks);
		let mut output = SlowInterpretOutput::new();
		blocks.slow_interpret(&["666".to_owned()], &mut output).to_string()
	}
	fn run(code: &str, input: &str) -> Vec<String> {
		let program = compile_program(code.as_bytes(), None);
		println!("{:?}", program.pattern_action);
		let mut output = SlowInterpretOutput::new();
		program.slow_interpret(&[input.to_owned()], &mut output);
		output.output
	}
	#[test]
	fn run_implicit_print() {
		assert_eq!(run("/T/", "TV SET"), vec!["TV SET"]);
	}
	#[test]
	fn run_one_plus_one() {
		assert_eq!(run("/T/ { print 1 + 1 }", "T"), vec!["2"]);
	}
	#[test]
	fn run_complex_pattern() {
		assert_eq!(run("$0 == \"Z\", /T/ { print 1 + 1 }", "T"), vec!["2"]);
		assert_eq!(run("$0 == \"T\", /Z/ { print 1 + 1 }", "T"), vec!["2"]);
		assert_eq!(run("$0 == \"Z\", /Z/ { print 1 + 1 }", "T"), Vec::<String>::new());
	}
	#[test]
	fn run_nested() {
		assert_eq!(run("/T/ { if $0 == \"T\" { print $0 \"A\" } }", "T"), vec!["TA"]);
		assert_eq!(run("/T/ { if $0 == \"U\" { print $0 \"A\" } }", "T"), Vec::<String>::new());
	}
	#[test]
	fn one_plus_one() {
		assert_eq!(exec("1 + 1"), "2");
	}
	#[test]
	fn ternary() {
		assert_eq!(exec("1 ? 1 + 1 : 3"), "2");
		assert_eq!(exec("0 ? 1 + 1 : 3"), "3");
		assert_eq!(exec("1 ? 2 ? 3 : 4 : 5"), "3");
	}
	#[test]
	fn conditional() {
		assert_eq!(exec("if 1 { 1 } else { 2 }"), "1");
		assert_eq!(exec("if 0 { 1 } else { 2 }"), "2");
		assert_eq!(exec("if 1 { 1 } else { 2 }; 12"), "12");
		assert_eq!(exec("if 0 { 1 } else { 2 }; 12"), "12");
		assert_eq!(exec("if 1 { 1 }; 12"), "12");

		assert_eq!(exec(r#"if "foo" ~ /f/ { 1 } else { 2 }"#), "1");
		assert_eq!(exec(r#"if "foo" ~ /a/ { 1 } else { 2 }"#), "2");
		assert_eq!(exec(r#"if "foo" ~ /f/ { 1 } else { 2 }; 12"#), "12");
		assert_eq!(exec(r#"if "foo" ~ /a/ { 1 } else { 2 }; 12"#), "12");
		assert_eq!(exec(r#"if "foo" ~ /f/ { 1 }; 12"#), "12");
	}
	#[test]
	fn while_loop() {
		assert_eq!(exec("while 0 { 1 } 2"), "2");
		assert_eq!(exec("a = 1; while a < 10 { a = a + 1 } a"), "10");
		assert_eq!(exec("for (a = 0; a < 10; a = a + 1) { b = a * 2 } a"), "10");
		assert_eq!(exec("for (a = 0; a < 10; a = a + 1) { b = a * 2 } a = a + 1; a"), "11");
		assert_eq!(exec("for (a = 0; a < 10; a += 1) { b = a * 2 } a += 1; a"), "11");
	}
	#[test]
	fn concat() {
		assert_eq!(exec("1 1"), "11");
	}
	#[test]
	fn capture() {
		assert_eq!(exec("$0"), "666");
		assert_eq!(exec("$ ( 1 - 1)"), "666");
	}
	#[test]
	fn matches() {
		assert_eq!(exec("2 ~ /2/"), "1");
		assert_eq!(exec("1 + 1 ~ /2/"), "1");
		assert_eq!(exec("2 + 2 ~ /2/"), "0");
		assert_eq!(exec("1 ? 2 : 3 ~ /2/"), "2");
	}
	#[test]
	fn semicolon() {
		assert_eq!(exec("2; 1"), "1");
		assert_eq!(exec("2\n1"), "1");
	}
	#[test]
	fn assignment() {
		assert_eq!(exec("a = 1"), "1");
		assert_eq!(exec("a = 1; 2"), "2");
		assert_eq!(exec("a = 1; a"), "1");
	}
}