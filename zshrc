# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
# ZSH_THEME="avit"
ZSH_THEME="spaceship"

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
  docker
  git
  python
  autojump
  extract
  zsh-syntax-highlighting
  command-not-found
)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

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
alias ssh='TERM=xterm-256color ssh'

# echo "Adding ASHS (fastashs) to the path:"
# export ASHS_ROOT=$HOME/Source/external/ashs

# echo "Adding user's bin/ to path"
export PATH=$PATH:$HOME/bin
export PATH=$PATH:$HOME/abin
export PATH=$PATH:/opt/bin

# Conda
if [[ -d /opt/conda ]]; then
    # should that be source /opt/conda/bin/activate
    # instead of just export the conda bin path?
    . "/opt/conda/etc/profile.d/conda.sh"
    conda activate base
else
    export PATH="$HOME/conda/bin:$PATH"
fi

export WORK=/mnt/work/$(id -un)

# Some locale settings, perhaps they should go in
# ~/.pam_environment

# export LANG=de_DE.UTF-8
# export LC_ALL=de_DE.UTF-8
# export LC_MESSAGES=POSIX

if [[ ${HOST##*-} == "3013497" ]]; then
    # singularity keeps a cache directory at $HOME/.singularity by default
    # Bad idea for network drives
    export SINGULARITY_CACHEDIR=/opt/cache/singularity
    # Define the binds as env var; avoids having to write in command line
    export SINGULARITY_BINDPATH=/dzne,/mnt/work/$(id -un)
    # vagrant home (do not use $HOME/.vagrant.d)
    export VAGRANT_HOME=$WORK/vagrant-home
fi

# less setup to use highlight
if [[ -n $(command -v highlight) ]]; then
    export LESSOPEN="| $(which highlight) --out-format=xterm256 --line-numbers --quiet --force --style=molokai %s"
    export LESS=" -R "
    export PAGER="less"
    # alternative to cat using highligh
    alias hicat="highlight --out-format=xterm256 --line-numbers --quiet --force --style=molokai $1"
fi

# Poor man's um, personal notes
function mdless() {
    pandoc -s -f markdown -t man ${1} | groff -T utf8 -man | less
}
umedit() { mkdir -p ~/.notes; ${EDITOR} ~/.notes/${1}; }
um() { mdless ~/.notes/${1} }
umls() { ls ~/.notes }

# github.com/junegunn/fzf
fzv () {
    # fuzzy vim: use fzf to call vim/nvim
    local prevops
    prevops='[[ $(file --mime {}) =~ binary ]] &&
        echo {} is a binary file ||
        (highlight -O xterm256 --line-numbers --style=molokai {} ||
         cat{})2> /dev/null |head -500'
    nvim $(fzf --preview "$prevops")
}
# Use fd instead of find if available
if [[ -n $(command -v fd) ]]; then
    export FZF_DEFAULT_COMMAND='fd --type f'
    export FZF_CTRL_T_COMMAND=${FZF_DEFAULT_COMMAND}
    export FZF_ALT_C_COMMAND='fd --type d --follow'
fi
# some default options
export FZF_DEFAULT_OPTS='--height 60% --reverse --preview-window up'
# CTRL-R options: add preview
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:wrap --bind '?:toggle-preview'"
# ALT-C options: show tree preview
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -100'"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
