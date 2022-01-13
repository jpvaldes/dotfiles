function activate-env
    set -l selected_env (ls "$HOME"/envs | fzf)
    if test -n "$selected_env"
       source "$HOME"/envs/"$selected_env"/bin/activate.fish
    end
end
