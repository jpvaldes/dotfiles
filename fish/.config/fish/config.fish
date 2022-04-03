if status is-interactive
    # Commands to run in interactive sessions can go here
    abbr --add --global gco git checkout
    abbr --add --global gcount git shortlog -sn
    abbr --add --global glg git log --stat
    abbr --add --global glgp git log --stat -p
    abbr --add --global glog git log --oneline --decorate --graph
    abbr --add --global gsb git status -sb
    abbr --add --global gst git status
    abbr --add --global gstl git stash list
    abbr --add --global gsts git stash show --text
end

fish_add_path -p "$HOME/bin" "$HOME/.fzf/bin/"

set EDITOR vim
if test -n (exists_executable fd)
    set -g FINDEXE fd
else
    set -g FINDEXE find
end
set --export FZF_DEFAULT_OPTS '--height 60% --reverse --preview-window up'
set --export FZF_DEFAULT_COMMAND "$FINDEXE --type f --hidden --exclude .git --exclude '.*cache*' --exclude '.ipynb_*'"

if test -n (exists_executable highlight)
    set -l HIGHLIGHT_EXE (which highlight)
    set --export LESSOPEN "| $HIGHLIGHT_EXE --out-format=xterm256 --line-numbers --quiet --force --style='base16/one-light' %s"
    set --export LESS " -R "
    set --export PAGER "less"
end

source ~/.asdf/asdf.fish

if test -e ~/local.fish
    source ~/local.fish
end

# The next line updates PATH for the Google Cloud SDK.
if [ -f "$HOME/google-cloud-sdk/path.fish.inc" ]; . "$HOME/google-cloud-sdk/path.fish.inc"; end
