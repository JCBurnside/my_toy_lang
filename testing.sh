#!/bin/zsh
for i in {1..1000}
do
    if cargo test -p compiler -- typed_ast::tests > out.txt 2>&1
    then
        echo "$i passed"
    else
        echo "$i failed"
        break
    fi
done