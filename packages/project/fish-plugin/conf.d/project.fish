if test -z "$PROJECT_CMD"
  set -U PROJECT_CMD p
end

if test ! -z $PROJECT_CMD
  function $PROJECT_CMD -d "go to project"
    __project $argv
  end
end