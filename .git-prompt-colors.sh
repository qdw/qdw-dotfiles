override_git_prompt_colors() {
  GIT_PROMPT_THEME_NAME="qdw"
  GIT_PROMPT_MASTER_BRANCHES='@(master|main)'
  GIT_PROMPT_REMOTE=""
  GIT_PROMPT_MASTER_BRANCH_COLOR="${Red}"
  GIT_PROMPT_BRANCH="${Cyan}"
  GIT_PROMPT_SEPARATOR=""
  GIT_PROMPT_PREFIX="["
  GIT_PROMPT_SUFFIX="]"
  
  GIT_PROMPT_LEADING_SPACE=0
  GIT_PROMPT_COMMAND_OK=""
  GIT_PROMPT_COMMAND_FAIL=""
  GIT_PROMPT_REMOTE=""
  GIT_PROMPT_UPSTREAM="${Cyan}_UPSTREAM_${ResetColor}"
  GIT_PROMPT_START=""
  GIT_PROMPT_SHOW_CHANGED_FILES_COUNT=0
  GIT_PROMPT_START_USER=""
  GIT_PROMPT_END_USER=" ${Blue}${PathShort}\\$ ${ResetColor}"
  GIT_PROMPT_START_ROOT=""
  
  GIT_PROMPT_END=""

  GIT_PROMPT_STAGED="${Cyan}"           # the number of staged files/directories
  GIT_PROMPT_CONFLICTS="${Magenta}"     # the number of files in conflict
  GIT_PROMPT_CHANGED="${Green}"         # the number of changed files
  GIT_PROMPT_UNTRACKED="${Yellow}"      # the number of untracked files/dirs
  GIT_PROMPT_STASHED="${Blue}"          # the number of stashed files/dir
  GIT_PROMPT_CLEAN="${White}"           # a colored flag indicating a "clean" repo
  GIT_PROMPT_SYMBOLS_NO_REMOTE_TRACKING=""
}

reload_git_prompt_colors "qdw"
