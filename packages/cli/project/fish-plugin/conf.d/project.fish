if not set -q PROJECT_CMD
    set -g PROJECT_CMD p
end

function $PROJECT_CMD -d "go to project"
    __project $argv
end
