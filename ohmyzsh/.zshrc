# Helper to find executables
exists_executable()
{
    command -v ${1} > /dev/null 2>&1
}

# git clone a repo in user/reponame form to optional dir
clone-repo() {
    local repo="${1}"
    local dir="${2:-}"

    repo="https://github.com/${repo%.git}.git"

    [[ ! -z ${dir} ]] && mkdir -p "${dir}"

    echo "++ bootstrap: cloning ${repo} in ${dir}"

    git clone --depth=1 --recursive "${repo}" "${dir}"
}

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Install oh-my-zsh if not found
if [[ ! -d "${ZSH}" ]]; then
    clone-repo ohmyzsh/ohmyzsh "${ZSH}"
    clone-repo zsh-users/zsh-autosuggestions ${ZSH}/custom/plugins/zsh-autosuggestions
    clone-repo zsh-users/zsh-syntax-highlighting ${ZSH}/custom/plugins/zsh-syntax-highlighting
    clone-repo supercrabtree/k ${ZSH}/custom/plugins/k
    # fix permissions or ohmyzsh does not load plugins
    chmod -R g-w,o-w "${ZSH}"
fi

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="avit"

# Set list of themes to load
# Setting this variable when ZSH_THEME=random
# cause zsh load theme from this variable instead of
# looking in ~/.oh-my-zsh/themes/
# An empty array have no effect
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="dd.mm.yyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  asdf
  docker
  git
  python
  extract
  command-not-found
  zsh-autosuggestions
  zsh-syntax-highlighting
  k
)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='vim'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
alias zshconfig="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
alias ssh256='TERM=xterm-256color ssh'

# echo "Adding user's bin/ to path"
export PATH=$PATH:$HOME/bin

export WORK=/mnt/work/$(id -un)

# Some locale settings, perhaps they should go in
# ~/.pam_environment

# export LANG=de_DE.UTF-8
# export LC_ALL=de_DE.UTF-8
# export LC_MESSAGES=POSIX

if exists_executable nvim; then
    EDITOR=nvim
else
    EDITOR=vim
fi

# less setup to use highlight
if exists_executable highlight; then
    export LESSOPEN='| $(which highlight) --out-format=xterm256 --line-numbers --quiet --force --style="base16/one-light" %s'
    export LESS=" -R "
    export PAGER="less"
    # alternative to cat using highligh
    alias hicat='highlight --out-format=xterm256 --line-numbers --quiet --force --style="base16/one-light" $1'
fi

# personal notes, inspired by um notes
# new note
nnote() {
    mkdir -p ~/Documents/notes
    local notefile=~/Documents/notes/$(date +"%Y-%m-%d").md
    echo "# $(date +'%Y.%m.%d')" > ${notefile}
    $EDITOR ${notefile}
}
# edit daily note
enote() {
    # allow input filename with or without .md
    local notefile=${1%.md}.md
    if [[ -f ${notefile} ]]; then
        ${EDITOR} ~/Documents/notes/${notefile}
    else
        echo "$notefile not found. Nothing to do."
    fi
}
noteless() {
    pandoc -s -f markdown -t man ${1} | groff -T utf8 -man | less
}
notehtml() {
    local htmlfile=/tmp/${${1##*/}%.md}.html
    pandoc -s -f markdown -t html5 ${1} -o ${htmlfile} && xdg-open ${htmlfile}
}
# show note
snote() { noteless ~/Documents/notes/${1}.md }
# html note
hnote() { notehtml ~/Documents/notes/${1}.md }
# liss notes
lnote() { ls -l ~/Documents/notes | column -t }

# FZF
# github.com/junegunn/fzf
fzv () {
    # fuzzy vim: use fzf to call vim/nvim
    local prevops
    prevops='[[ $(file --mime {}) =~ binary ]] &&
        echo {} is a binary file ||
        (highlight -O xterm256 --line-numbers --style="base16/one-light" {} ||
         cat{})2> /dev/null |head -500'
    $EDITOR $(fzf --preview "$prevops")
}
# Use fd instead of find if available
# In Ubuntu the binary is called `fdfind`
export FDBINARY=fd
if exists_executable ${FDBINARY}; then
    export FZF_DEFAULT_COMMAND="${FDBINARY} --type f --hidden --exclude .git --exclude '.*cache*' --exclude '.ipynb_*'"
    export FZF_CTRL_T_COMMAND=${FZF_DEFAULT_COMMAND}
    export FZF_ALT_C_COMMAND='${FDBINARY} --type d --follow'
fi
# some default options
export FZF_DEFAULT_OPTS='--height 60% --reverse --preview-window up'
# CTRL-R options: add preview
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:wrap --bind '?:toggle-preview'"
# ALT-C options: show tree preview
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -100'"
# python envs, from https://seb.jambor.dev/posts/improving-shell-workflows-with-fzf/
activate-venv() {
    local selected_env
    selected_env=$(ls ~/envs/ | fzf)
    [[ -n "${selected_env}" ]] && source "$HOME/envs/${selected_env}/bin/activate"
}
# choose and change branch with preview
change-branch() {
    git branch |
        grep --invert-match '\*' |
        cut -c 3- |
        fzf --preview="git log {}" |
        xargs git checkout
}

[[ -f ~/.fzf.zsh ]] && . ~/.fzf.zsh

# Load local config if exists
[[ -f ~/.local.zsh ]] && . ~/.local.zsh
