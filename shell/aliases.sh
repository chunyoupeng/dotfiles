alias br="broot"
alias shut="sudo shutdown -h now"
alias ls="exa --color=auto"
alias ll='ls -alF'
alias la='ls -A'
alias l='exa -lah -S  --color=auto --group-directories-first'
alias e="emacsclient"
alias gm="git commit -am"
alias gam="gaa; gcm"
alias gd="git diff"
alias gt="git status"
alias gaa="git add ."
alias copy='xclip -selection clipboard'
alias antlr4="java -jar /usr/local/lib/antlr-4.13.1-complete.jar"
alias grun="java org.antlr.v4.gui.TestRig"
alias antlr4='java -Xmx500M -cp "/usr/local/lib/antlr-4.13.1-complete.jar:$CLASSPATH" org.antlr.v4.Tool'
alias grun='java -Xmx500M -cp "/usr/local/lib/antlr-4.13.1-complete.jar:$CLASSPATH" org.antlr.v4.gui.TestRig'
# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias cl="clear"
alias cd..="cd .."
alias nlp="conda activate nlp"
ec() {
    emacsclient "$(find . -type f | fzf)"
}

if [[ "$OSTYPE" == "darwin"* ]]; then
    alias cat=bat
else
    alias cat=batcat
fi
