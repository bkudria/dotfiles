export EDITOR='emacsclient -q -r'
export VISUAL='emacsclient -q -r'
export TERM=screen-256color
export GOPATH=~/.go
export TSC_WATCHFILE=UseFsEventsWithFallbackDynamicPolling
export EZA_ICON_SPACING=2

alias ll='eza -lF --colour-scale all --group-directories-first --icons auto'
alias la='ll -a'
alias lt='ll -T'
alias cat=bat
# alias git=hub

alias brewi='brew info'
alias brewI='brew install'
alias rbbi='bundle install'

path=(
  "/opt/homebrew/bin"
  "/opt/homebrew/sbin"
  "$HOME/bin"
  "$HOME/.emacs.doom/bin"
  "$HOME/.local/bin" # uv
  "$HOME/.cargo/bin"
  "/usr/local/opt/node@16/bin"
  $path
)

cdpath=($HOME/code)

[[ -f ~/.vterm.zsh ]] && source ~/.vterm.zsh
[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

[[ -f ~/.local.zsh ]] && source ~/.local.zsh

bindkey '^Z^Z' __llm_cmdcomp

__llm_cmdcomp() {
  local old_cmd=$BUFFER
  local cursor_pos=$CURSOR
  echo # Start the program on a blank line
  local result=$(llm cmdcomp "$old_cmd")
  if [ $? -eq 0 ] && [ ! -z "$result" ]; then
    BUFFER=$result
  else
    BUFFER=$old_cmd
  fi
  zle reset-prompt
}

zle -N __llm_cmdcomp

function llm_prompts() {
    if [[ $# -ne 1 ]]; then
        echo "Usage: llm-template-keys <template_name>"
        return 1
    fi

    local template_path=$(llm templates path)/$1.yaml

    if [[ ! -f "$template_path" ]]; then
        echo "Template '$1.yaml' not found in $(llm templates path)"
        return 1
    fi

    echo "System Keys:"
    yq '.system // {} | keys' "$template_path"

    echo -e "\nPrompt Keys:"
    yq '.prompts // {} | keys' "$template_path"
}

fix_yaml() {
  [ -z "$1" ] && echo "Usage: fix_yaml <file.yaml> [files...]" && return 1
  yq eval -i ' (.. | select(tag == "!!str") | select(test("\\n"))) style = "literal" ' "$@"
}

fix_prompt() {
  [ -z "$1" ] && echo "Usage: fix_prompt <template_name>" && return 1
  local file="$(llm templates path)/${1}.yaml"
  [ ! -f "$file" ] && echo "Error: $file not found" && return 1
  fix_yaml "$file"
}

as_system_prompt() {
  local output
  output="$(cat)" # read all stdin
  llm --system "$output" "$@"
}


# Counts tokens using Anthropic API for given text or files with optional system prompt
# Usage:
#   count_tokens_anthropic "message content"                 # Count tokens in a string
#   echo "system prompt" | count_tokens_anthropic "message"  # With system prompt from stdin
#   count_tokens_anthropic file1.txt file2.txt               # Count tokens in multiple files
#   count_tokens_anthropic directory/                        # Recursively count tokens in directory
#   count_tokens_anthropic -v file.txt                       # Verbose mode with detailed logging
count_tokens_anthropic() {
    # Use absolute paths for commands to avoid alias issues
    local CAT="/bin/cat"
    local JQ="/opt/homebrew/bin/jq"
    local CURL="/usr/bin/curl"
    local FIND="/usr/bin/find"
    local GREP="/usr/bin/grep"
    local FILE="/usr/bin/file"
    local CUT="/usr/bin/cut"
    local WC="/usr/bin/wc"
    local DIRNAME="/usr/bin/dirname"
    local BASENAME="/usr/bin/basename"
    local ECHO="/bin/echo"
    
    # Process options
    local verbose=false
    local args=()
    for arg in "$@"; do
        if [[ "$arg" == "-v" || "$arg" == "--verbose" ]]; then
            verbose=true
        else
            args+=("$arg")
        fi
    done
    
    # Process stdin for system prompt
    local system_prompt=""
    if [[ ! -t 0 ]]; then
        system_prompt=$($CAT)
    fi

    # Validate arguments
    if [[ ${#args[@]} -eq 0 ]]; then
        echo "Error: No input provided. Please provide text, files, or directories." >&2
        return 1
    fi
    
    # Debug function
    debug() {
        if [[ $verbose == true ]]; then
            echo "DEBUG: $*" >&2
        fi
    }
    
    debug "Starting count_tokens_anthropic with ${#args[@]} arguments"
    debug "ANTHROPIC_API_KEY exists: $(if [[ -n "$ANTHROPIC_API_KEY" ]]; then echo "yes"; else echo "no"; fi)"

    # Function to get token count
    get_token_count() {
        local content="$1"
        local model="claude-3-7-sonnet-20250219"
        local payload=""

        # Create appropriate JSON payload
        if [[ -n "$system_prompt" ]]; then
            payload=$($JQ -n \
                --arg model "$model" \
                --arg system "$system_prompt" \
                --arg content "$content" \
                '{
                  model: $model,
                  system: $system,
                  messages: [{
                    role: "user",
                    content: $content
                  }]
                }')
        else
            payload=$($JQ -n \
                --arg model "$model" \
                --arg content "$content" \
                '{
                  model: $model,
                  messages: [{
                    role: "user",
                    content: $content
                  }]
                }')
        fi

        # Make API request - capture stderr for debugging
        debug "Making API request to count tokens"
        local result
        local curl_error
        
        if $verbose; then
            debug "Payload: $payload"
        fi
        
        result=$($CURL -s -m 30 https://api.anthropic.com/v1/messages/count_tokens \
            -H "x-api-key: $ANTHROPIC_API_KEY" \
            -H "content-type: application/json" \
            -H "anthropic-version: 2023-06-01" \
            -d "$payload" 2>&1)

        local curl_status=$?
        if [[ $curl_status -ne 0 ]]; then
            echo "Error: API request failed with status $curl_status: $result" >&2
            return 1
        fi
        
        # Check if response is valid JSON
        if ! echo "$result" | $JQ . &>/dev/null; then
            echo "Error: Invalid JSON response: $result" >&2
            return 1
        fi

        # Extract token count and check if it exists
        local token_count
        token_count=$(echo "$result" | $JQ -r '.token_count // empty')
        
        if [[ -z "$token_count" ]]; then
            debug "Full API response: $result"
            debug "Checking input_tokens field instead"
            token_count=$(echo "$result" | $JQ -r '.input_tokens // empty')
            
            if [[ -z "$token_count" ]]; then
                echo "Error: No token count found in API response" >&2
                debug "Full response: $result"
                return 1
            fi
        fi
        
        echo "$token_count"
    }

    # Check for file or directory arguments
    local has_files=false
    for arg in "${args[@]}"; do
        debug "Checking argument: $arg"
        if [[ -f "$arg" || -d "$arg" ]]; then
            has_files=true
            debug "Found file/directory: $arg"
            break
        fi
    done

    # Process string argument if no files or directories
    if [[ $has_files == false ]]; then
        debug "No files/directories found, treating as text input"
        local text="${args[*]}"
        get_token_count "$text"
        return $?
    fi

    # Process files and directories
    local -A file_tokens=()
    local -A dir_tokens=()
    local total_tokens=0

    # Process individual files
    for path in "${args[@]}"; do
        debug "Processing argument: $path"
        if [[ -f "$path" ]]; then
            # Process file
            debug "Reading file: $path"
            local content=$($CAT "$path")
            debug "File size: $(echo -n "$content" | $WC -c) bytes"
            debug "Getting token count for file: $path"
            local count=$(get_token_count "$content")
            debug "Token count result: $count"

            if [[ -z "$count" || ! "$count" =~ ^[0-9]+$ ]]; then
                echo "Error processing file: $path (invalid token count: $count)" >&2
                continue
            fi

            file_tokens["$path"]=$count
            total_tokens=$((total_tokens + count))

            # Add to directory totals
            local dir=$($DIRNAME "$path")
            while [[ "$dir" != "." && "$dir" != "/" ]]; do
                dir_tokens["$dir"]=$((${dir_tokens["$dir"]:-0} + count))
                dir=$($DIRNAME "$dir")
            done

        elif [[ -d "$path" ]]; then
            # Find text files in directory
            debug "Finding text files in directory: $path"
            local text_files=$($FIND "$path" -type f -exec $FILE {} \; | $GREP -i "text" | $CUT -d: -f1)
            debug "Found $(echo "$text_files" | $WC -l) text files"

            for file in $text_files; do
                debug "Processing file from directory: $file"
                local content=$($CAT "$file")
                debug "File size: $(echo -n "$content" | $WC -c) bytes"
                debug "Getting token count for file: $file"
                local count=$(get_token_count "$content")
                debug "Token count result: $count"

                if [[ -z "$count" || ! "$count" =~ ^[0-9]+$ ]]; then
                    echo "Error processing file: $file (invalid token count: $count)" >&2
                    continue
                fi

                file_tokens["$file"]=$count
                total_tokens=$((total_tokens + count))

                # Add to directory totals
                local dir=$($DIRNAME "$file")
                while [[ "$dir" != "." && "$dir" != "/" ]]; do
                    dir_tokens["$dir"]=$((${dir_tokens["$dir"]:-0} + count))
                    dir=$($DIRNAME "$dir")
                done
            done
        fi
    done

    # Print results
    if [[ ${#file_tokens[@]} -gt 0 ]]; then
        # Print file results
        for file in "${!file_tokens[@]}"; do
            echo "$file: ${file_tokens[$file]}"
        done

        # Print directory summaries
        if [[ ${#dir_tokens[@]} -gt 0 ]]; then
            echo -e "\nDirectory Totals:"
            for dir in "${!dir_tokens[@]}"; do
                echo "$dir/: ${dir_tokens[$dir]}"
            done
        fi

        echo -e "\nTotal Tokens: $total_tokens"
    fi
}
