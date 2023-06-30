test: c_grammar
	tests/test_compiler.sh ../target/release/c_grammar

c_grammar:
	cargo build --release

run: 
	cargo build --release && ./tests/test_compiler.sh ./target/release/c_grammar