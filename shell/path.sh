export CLASSPATH=".:/usr/local/lib/antlr-4.13.1-complete.jar:$CLASSPATH"
export CXX=/usr/bin/g++
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
export LANG=zh_CN.UTF-8
export LC_ALL=zh_CN.UTF-8
export http_proxy=http://127.0.0.1:7890
export https_proxy=http://127.0.0.1:7890
export REDIS_URL=redis://localhost:6379
export GLOBAL_AGENT_ENVIRONMENT_VARIABLE_NAMESPACE=http://127.0.0.1:7890
export LLAMA_CUBLAS=1
export PATH=/usr/local/cuda-12.2/bin:$PATH
export BNB_CUDA_VERSION=122
export PATH=/home/dell/.local/bin:$PATH
export CUDA_HOME=/usr/local/cuda-12.2
export LD_LIBRARY_PATH=/usr/local/cuda-12.2/lib64:$LD_LIBRARY_PATH
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/dell/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/dell/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/dell/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/dell/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
eval "$(zoxide init zsh)"
eval "$(starship init zsh)"
. "$HOME/.cargo/env"


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/zsh_completion" ] && \. "$NVM_DIR/zsh_completion"  # This loads nvm zsh_completion
