#!/bin/sh

make_target="${1:-build}"
return_value="0"

while read -r test_dir; do
    env CC="$(readlink -f ../build/bompiler)" \
        make -f "$(readlink -f ./Makefile)" \
        -C "$test_dir" --no-print-directory "$make_target" \
        1>"$test_dir/build.out" 2>"$test_dir/error.out"
    make_status="$?"
    if [ $make_status -ne 0 ]; then
        echo "Error making target \"$make_target\" for test \"$(basename "$test_dir")\""
        return_value="$make_status"
    fi
done << EOF
$(find ./ -maxdepth 1 -mindepth 1 -type d)
EOF

exit $return_value
