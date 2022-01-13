function fish_prompt
    if not set -q VIRTUAL_ENV_DISABLE_PROMPT
        set -g VIRTUAL_ENV_DISABLE_PROMPT true
    end
    set -g __fish_git_prompt_char_dirtystate '✚'
    if not set -q __fish_git_prompt_showdirtystate
        set -g __fish_git_prompt_showdirtystate
    end
    if not set -q __fish_git_prompt_showcolorhints
        set -g __fish_git_prompt_showcolorhints
    end
    set_color yellow
    printf '%s' $USER
    set_color normal
    printf ' at '

    set_color magenta
    echo -n (prompt_hostname)
    set_color normal
    printf ' in '

    set_color $fish_color_cwd
    printf '%s' (prompt_pwd)
    set_color normal

    printf '%s' (fish_git_prompt)

    # Line 2
    echo
    if test -n "$VIRTUAL_ENV"
        printf "(%s) " (set_color blue)(basename $VIRTUAL_ENV)(set_color normal)
    end
    printf '↪ '
    set_color normal
end
