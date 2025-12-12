My initial inspiration for this project was from reading responses to an article about Python vs Rust. The Pythonistas made a point about Python being faster to get to production than Rust projects (although nothing was said about what happens post-deployment). 
I thought to myself, given that this may be a potential pain point for Rust developers, why not develop a simple Python-like language that transpiles to Rust in order to improve productivity in the initial stages. It would be like the Blade of the two languages - all of Rust and Python's strengths, and none of their weaknesses.    

So while starting on a lexer and parser for "py-rs" - It occurred to me that such a language would be easier to maintain if their were tools to make it easy to change and edit syntax without needing to rewrite large amounts of code. 

Hence this project where I am developing utilities to take a context-free grammar (CFG) as input and produce language-specific scanners, parsers and code generators as outputs. 

Thus far, I have created a procedural macro that parses the CFG into a production rules data structure (P) and (for debug purposes) returns a string reproducing the production rules generated from the P structs.
