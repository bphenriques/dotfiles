if not set -q FZF_RG_CMD
    set -g FZF_RG_CMD frg
end

function $FZF_RG_CMD -d "find file"
    __frg $argv
end
