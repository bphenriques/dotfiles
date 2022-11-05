cd "$WORKSPACE/$1" 2>/dev/null || cd "$WORKSPACE" 2>/dev/null || echo "WORKSPACE is not set or not a directory!"
