alias br="broot"
alias ls="exa --color=auto"
alias ll='ls -alF'
alias la='ls -A'
alias l='exa -lah -S  --color=auto --group-directories-first'
alias e="emacsclient"
alias gm="git commit -am"
alias gt="git status"
alias cat="batcat"
alias copy='xclip -selection clipboard'
alias openchat="python -m ochat.serving.openai_api_server --model openchat/openchat_3.5 --engine-use-ray --worker-use-ray --tensor-parallel-size 2"
alias antlr4="java -jar /usr/local/lib/antlr-4.13.1-complete.jar"
alias grun="java org.antlr.v4.gui.TestRig"
# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
alias cl="clear"
alias cd..="cd .."
alias nlp="conda activate nlp"
ec() {
    emacsclient "$(find . -type f | fzf)"
}
