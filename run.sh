cargo build --release > /dev/null  2>&1


gcc source.c -o expected
./target/release/rsc source.c 

cat program.s

gcc program.s -o result


echo "-------"

./expected; echo "expected: $?"
./result; echo "result: $?"

rm expected
rm result
rm program.s