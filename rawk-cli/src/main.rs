use regex::Regex;
use rawk_regex::{convert_terminated, Personality};
use std::env;

pub fn main() {
	let mut args = env::args();
	args.next(); // discard program name
	let code = args.next().unwrap();
	println!("{}", code);
	assert_eq!(code.as_bytes()[0], b'/');
	let regex = convert_terminated(&code.as_bytes()[1..], Personality::Gnu, b'/').unwrap().0;
	println!("{}", regex);
}