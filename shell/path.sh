# General exports
export CLASSPATH=".:/usr/local/lib/antlr-4.13.1-complete.jar:$CLASSPATH"
# export CXX=/usr/bin/g++ 
export LANG=zh_CN.UTF-8
export LC_ALL=zh_CN.UTF-8
export http_proxy=http://127.0.0.1:7890
export https_proxy=http://127.0.0.1:7890
export GLOBAL_AGENT_ENVIRONMENT_VARIABLE_NAMESPACE=http://127.0.0.1:7890
export USER_AGENT="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/112.0.0.0 Safari/537.36"
export HOMEBREW_NO_AUTO_UPDATE=1

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

eval "$(zoxide init zsh)"
eval $(thefuck --alias)
. "$HOME/.cargo/env"

# Check if on macOS or Linux (Ubuntu assumed here)
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS specific exports
    export PATH=/opt/homebrew/bin:$PATH
    export PATH=/Users/pengyo/.local/bin:$PATH
    export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
    export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
    export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"
    export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
else
    # Ubuntu specific exports
    export PATH="$PATH:/usr/local/bin/code"  # Adjust if VSCode is installed differently
fi

# CUDA and related configurations for machines with CUDA installed
if [ -d "/usr/local/cuda-12.2" ]; then
    export PATH=/usr/local/cuda-12.2/bin:$PATH
    export LD_LIBRARY_PATH="/usr/local/cuda-12.2/lib64:$LD_LIBRARY_PATH"
    export PATH=/home/dell/.local/bin:$PATH
    export CUDA_HOME=/usr/local/cuda-12.2
    export LD_LIBRARY_PATH=/usr/local/cuda-12.2/lib64:$LD_LIBRARY_PATH

fi
