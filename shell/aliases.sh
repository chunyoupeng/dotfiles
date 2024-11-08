alias shut="sudo shutdown -h now"
alias ll='ls -alF'
alias la='ls -A'
alias gmm="git commit -m"
alias gam="gaa; gcm"
alias gd="git diff"
alias gt="git status"
alias gaa="git add --all"
alias ga="git add"
alias gg="git lg"
alias cl="clear"
alias cd..="cd .."
alias pi="python -m pip install --trusted-host files.pythonhosted.org --trusted-host pypi.org --trusted-host pypi.python.org "

if [[ "$OSTYPE" == "darwin"* ]]; then
    alias cat=bat
else
    alias copy='xclip -selection clipboard'
    alias antlr4="java -jar /usr/local/lib/antlr-4.13.1-complete.jar"
    alias grun="java org.antlr.v4.gui.TestRig"
    alias antlr4='java -Xmx500M -cp "/usr/local/lib/antlr-4.13.1-complete.jar:$CLASSPATH" org.antlr.v4.Tool'
    alias grun='java -Xmx500M -cp "/usr/local/lib/antlr-4.13.1-complete.jar:$CLASSPATH" org.antlr.v4.gui.TestRig'
    alias cat=batcat
fi
