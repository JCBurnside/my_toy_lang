for file in /usr/lib/llvm-20/lib
    
    if nm -aC $file | grep "$1"; then 
        echo $file
    fi
done 