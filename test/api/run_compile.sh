#!/bin/bash
set -e

# Recursively find all run.sh files under current directory
find . -name "run.sh" | grep -v "^./run.sh$" | while read -r file; do
    # Get the directory where run.sh is located
    dir=$(dirname "$file")
    cd "$dir"
    echo -e "\n\nENTER $dir"

    # Execute the script
    echo -e "EXEC:$dir/run.sh"
    bash ./run.sh
    ret=$?

    # Check execution result
    if [ "$ret" -ne 0 ]; then
        echo "FAILED!!"
        exit "$ret"
    fi

    # Switch back to previous directory, suppress output
    cd - >/dev/null
done

echo "TEST FINISH!"
exit 0
