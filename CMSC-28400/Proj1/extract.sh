for filename in ctxts/*.txt; do
    echo "$filename:"
    echo " "
    cat "$filename"
    echo " "
    echo " "
    # ... rest of the loop body
done

