#!/bin/bash

# Define the string to remove, including the quotes
string_to_remove='"multi_stim"'

# Use find to search for all .R files in the current directory and subdirectories
find . -type f -name "*.R" | while read file; do
  # Check if the file exists
  if [ -f "$file" ]; then
    # Use sed to remove the string with quotes and overwrite the file
    sed -i '' "s/$string_to_remove//g" "$file"
    echo "Removed string from $file"
  fi
done

echo "String removal completed for all .R files in current directory and subdirectories."
