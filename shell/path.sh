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
export REDIS_URL=redis://localhost:6379
export GLOBAL_AGENT_ENVIRONMENT_VARIABLE_NAMESPACE=http://127.0.0.1:7890
export LLAMA_CUBLAS=1
export PATH=/usr/local/cuda-12.2/bin:$PATH
export HOMEBREW_NO_AUTO_UPDATE=1
eval "$(zoxide init zsh)"
eval "$(starship init zsh)"
eval $(thefuck --alias)
. "$HOME/.cargo/env"

# Check if on macOS or Linux (Ubuntu assumed here)
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS specific exports
    export PATH=/opt/homebrew/bin:$PATH
    export PATH=/Users/pengyo/.local/bin:$PATH
    export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
    # >>> conda initialize >>>
    # !! Contents within this block are managed by 'conda init' !!
    __conda_setup="$('/Users/pengyo/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
	eval "$__conda_setup"
    else
	if [ -f "/Users/pengyo/miniconda3/etc/profile.d/conda.sh" ]; then
	    . "/Users/pengyo/miniconda3/etc/profile.d/conda.sh"
	else
	    export PATH="/Users/pengyo/miniconda3/bin:$PATH"
	fi
    fi
    unset __conda_setup
    # <<< conda initialize <<<

else
    # Ubuntu specific exports
    export PATH="$PATH:/usr/local/bin/code"  # Adjust if VSCode is installed differently
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

fi

# CUDA and related configurations for machines with CUDA installed
if [ -d "/usr/local/cuda-12.2" ]; then
    export PATH=/usr/local/cuda-12.2/bin:$PATH
    export LD_LIBRARY_PATH="/usr/local/cuda-12.2/lib64:$LD_LIBRARY_PATH"
    export PATH=/home/dell/.local/bin:$PATH
    export CUDA_HOME=/usr/local/cuda-12.2
    export LD_LIBRARY_PATH=/usr/local/cuda-12.2/lib64:$LD_LIBRARY_PATH

fi
