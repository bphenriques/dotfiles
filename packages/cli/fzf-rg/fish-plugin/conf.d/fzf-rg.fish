if test -z "$FZF_RG_CMD"
  set -U FZF_RG_CMD frg
end

if test ! -z $FZF_RG_CMD
  function $FZF_RG_CMD -d "find file"
    __frg $argv
  end
end