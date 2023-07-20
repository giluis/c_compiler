#!/bin/bash

cargo build --release > /dev/null  2>&1

stages_to_run=("4")
break_loop="false"
for stage in "${stages_to_run[@]}"
do
    dirname="./tests/stage_$stage"
    if [ -d "$dirname" ]; then
        echo -e "--- Stage $stage\n"
        for file in $dirname/valid/*.c
        do
            filename=$(basename $file) 
            filename="${filename%.*}"
            gcc "$file" -o expected
            ./target/release/rsc "$file"
            gcc program.s -o result

            expected_output=$(./expected)
            actual_output=$(./result)

            if [ "$expected_output" != "$actual_output" ]; then
                echo -e "$filename: failed\n Expected: $expected_output\n Actual output: $actual_output\n ------ \n $(cat program.s)"
                break_loop="true"
            fi

            rm expected result program.s
            if [ break_loop = "true" ]; then
                break 2
            fi
            echo "$filename: passed"
        done

        for file in $dirname/invalid/*.c
        do
            filename=$(basename $file) 
            filename="${filename%.*}"

            if [ "$filename" == "no_space" ]; then
                # no spaces are ignored in lexing, for now.
                # a custom might be necessary
                echo "no_space: ignored"
                continue
            fi

            ./target/release/rsc "$file"

            if [ -f "program.s" ]; then
                cat program.s
                echo "$filename: failed. Invalid file was compiled"
                rm program.s
                break 2
            fi
        done
    fi
done