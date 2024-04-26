# General exports
export CLASSPATH=".:/usr/local/lib/antlr-4.13.1-complete.jar:$CLASSPATH"
export CXX=/usr/bin/g++
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
export LANG=zh_CN.UTF-8
export LC_ALL=zh_CN.UTF-8
export http_proxy=http://127.0.0.1:7890
export https_proxy=http://127.0.0.1:7890
export LLAMA_CUBLAS=1
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/zsh_completion" ] && \. "$NVM_DIR/zsh_completion"  # This loads nvm zsh_completion
eval "$(zoxide init zsh)"
eval "$(starship init zsh)"
. "$HOME/.cargo/env"

# Check if on macOS or Linux (Ubuntu assumed here)
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS specific exports
    export PATH=/opt/homebrew/bin:$PATH
    export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
else
    # Ubuntu specific exports
    export PATH="$PATH:/usr/local/bin/code"  # Adjust if VSCode is installed differently
fi

# CUDA and related configurations for machines with CUDA installed
if [ -d "/usr/local/cuda-12.2" ]; then
    export PATH="/usr/local/cuda-12.2/bin:$PATH"
    export CUDA_HOME="/usr/local/cuda-12.2"
    export LD_LIBRARY_PATH="/usr/local/cuda-12.2/lib64:$LD_LIBRARY_PATH"
fi

