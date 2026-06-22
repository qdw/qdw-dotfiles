MODEL_DIR="$HOME/ai/weights"

#### AI/LLMs/ML

for MODEL_DIR in "$HOME/ai/models"/*; do
    MODEL_NAME="$(basename "$MODEL_DIR")"
    if [[ $MODEL_NAME =~ [a-zA-Z]+ ]]; then
        SHORT_NAME="${BASH_REMATCH[0]}"
        LC="${SHORT_NAME,,}"
    
        OLLAMA_FUNCNAME="$LC-ollama"
        LLAMAFILE_FUNCNAME="$LC-llamafile"

        eval "
            function $OLLAMA_FUNCNAME {
                ollama run $MODEL_NAME \"\$@\"
            }

            function $LLAMAFILE_FUNCNAME {
                llamafile -m \"$HOME/ai/models/${MODEL_NAME}/${MODEL_NAME}.gguf\" \"\$@\"
            }
        "
    fi
done

function ai {
    tmux attach-session -t ai
}

function mistral {
    Mistral-7B-Instruct-v0.3.Q4_0-ollama "$@"
}
