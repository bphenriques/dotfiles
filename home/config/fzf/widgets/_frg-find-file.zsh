LBUFFER="${LBUFFER}$(frg)"
local ret=$?
zle reset-prompt
return $ret
