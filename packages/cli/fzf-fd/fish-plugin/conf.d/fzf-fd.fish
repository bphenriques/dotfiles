if not set -q FZF_FD_CMD
    set -g FZF_FD_CMD ffd
end

function $FZF_FD_CMD -d "go to directory"
    __ffd $argv
end
