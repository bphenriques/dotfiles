if test -z "$FZF_FD_CMD"
  set -U FZF_FD_CMD ffd
end

if test ! -z $FZF_FD_CMD
  function $FZF_FD_CMD -d "go to directory"
    __ffd $argv
  end
end