### Generic AWS settings

function aws-account {
    AWS_PAGER=cat aws sts get-caller-identity --query "Account" --output text
}

function clear-env {
    unset AWS_ACCOUNT \
          AWS_ACCESS_KEY_ID \
          AWS_SECRET_ACCESS_KEY \
          AWS_SESSION_TOKEN \
          PGDATABASE \
          PGHOST \
          PGPASSWORD \
          PGPORT \
          PGSERVICE \
          PGUSER
}

### AWS IAM settings

function iam-provision-pgpassword {
    if [[ $# -eq 0 ]]; then
        echo "USAGE:"
        echo
        # shellcheck disable=SC2016
        echo 'iam --hostname "$host" --port "$port" --region "$region" --username "$user" --profile "$profile"'
    else
        aws rds generate-db-auth-token "$@" | tee >(pbcopy)
    fi
}

# Print a list of IAM instances in the current account (each double-quoted)
function instances {
    aws rds describe-db-instances \
        | jq '.DBInstances[] | .DBInstanceIdentifier'
}

function cdk {
    case "$1" in
        diff | deploy) SUBCOMMAND="$1"
                       shift
                       command cdk "$SUBCOMMAND" --quiet "$@" \
                           2> >(grep -v "Hold on while" >&2)
                       stty sane;;
        *) command cdk "$@";;
    esac
}

# tags - show CDK tags for a project
function tags {
    local ORIG_DIR
    ORIG_DIR="$(pwd)"

    local GIT_WORKING_COPY_ROOT_DIR
    if GIT_WORKING_COPY_ROOT_DIR="$(git rev-parse --show-toplevel)"; then
        if cd "$GIT_WORKING_COPY_ROOT_DIR/cdk.out"; then
            CDK_OUTPUT_FILES="$(fdd -t f '.json$')"
            if [[ -n "$CDK_OUTPUT_FILES" ]]; then
                for FILENAME in $CDK_OUTPUT_FILES; do
                    gron "$FILENAME" | grep tags | awk "{print \"$FILENAME: \" \$0}"
                done
            fi
        fi
    fi
    
    cd "$ORIG_DIR"
}

function wait_for_proxy_to_come_up {
    check-args 1 1 'PROXY_NAME' "$@" || return 1

    local PROXY_NAME
    PROXY_NAME="$1"
    while true; do
 
            # Broken
        local STATUS="$(aws rds describe-db-proxies | jq ".DBProxies | select(.DBProxyName==\"$PROXY_NAME\") | .Status")"
 
 
        if [[ $STATUS = '"creating"' ]]; then
            sleep 60
        else
            break
        fi
    done
    echo "$STATUS at $(date)"
}

function rds {
    function usage {
        echo "Usage: rds [ls | list]"
    }

    if [[ $# -eq 0 ]]; then
        echo -n "Missing argument."; usage
    elif [[ $# -gt 1 ]]; then
        echo "Too many arguments."; usage
    elif [[ "$1" = "ls" ]] || [[ "$1" = "list" ]]; then
        shift
        rds-ls "$@"
    else
        echo "No such subcommand '$1'."; usage
    fi
}

# rds-ls ("RDS LiSt") - list RDS instances (highlighting the hostname portion of the FQDN)
function rds-ls {
    check-args 0 1 'rds-ls [HOSTNAME_REGEX]' "$@" || return 1

    if [[ $# -eq 1 ]]; then
        local REGEX="$1"
        aws rds describe-db-instances --no-paginate \
            | jq --raw-output '.DBInstances[].Endpoint.Address' \
            | grep '^[^.]*' \
            | grep "$1"
        
    else
        aws rds describe-db-instances --no-paginate \
            | jq --raw-output '.DBInstances[].Endpoint.Address' \
            | grep '^[^.]*'
    fi
}

function list-rds-instances { rds-ls "$@" ;}

#### AWS SSM (a.k.a. Secrets Manager) settings

function list-secrets {
    check-args 0 varargs '[GREP_REGEX]' "$@" || return 1
    local REGEX="$1" # may be ""
    shift # remaining arguments (if any) are flags to pass to grep

    function list-all-secrets {
        aws secretsmanager list-secrets \
            | jq -r '.SecretList[].Name' \
            | sort -u
    }

    if [[ "$REGEX" ]]; then
        list-all-secrets | command grep -E "$@" "$REGEX"
    else
        list-all-secrets
    fi
}

function get-secret {
    aws secretsmanager get-secret-value --secret-id "$1" \
        | jq '.SecretString | fromjson'
}

function secret { get-secret "$@" ;}

### AWS logs/CloudWatch/LogGroup/log group settings

function rds-ls-log-groups {
    set -o pipefail

    aws logs describe-log-groups --log-group-name-prefix "/aws/rds" \
        | command grep logGroupName \
        | command grep -v "proxy" \
        | sort \
        | sed -e 's|.*/aws/rds/instance/||; s|/postgresql",$||'
    
    set +o pipefail
}

### AWS S3 setttings

function in-dev {
    # (AWS_DEV_ACCOUNT_NUMBER must be defined in preamble.bash)
    if ! [[ "$AWS_ACCESS_KEY_ID" && "$AWS_SECRET_ACCESS_KEY" && "$AWS_SESSION_TOKEN" ]]; then
        err "AWS_ACCOUNT is defined, but not the auth-related AWS_* env vars"
        return 1
    else
        local ACCOUNT_NUMBER
        ACCOUNT_NUMBER="$(AWS_PAGER=cat aws sts get-caller-identity --query "Account" --output text)"
        if [[ "$ACCOUNT_NUMBER" != "$AWS_DEV_ACCOUNT_NUMBER" ]]; then
            err "You're logged into '$ACCOUNT_NUMBER', but the dev account is '$AWS_DEV_ACCOUNT_NUMBER'"
            return 1
        else
            return 0
        fi
    fi
}

function ls-buckets {
    check-args 0 1 'ls-buckets [MATCHING_PREFIX]' "$@" || return 1
    if [[ $# -eq 0 ]]; then
        aws s3 ls
    elif [[ $# -eq 1 ]]; then
        aws s3 ls --bucket-name-prefix "$1"
    fi
}

function rm-bucket {
    check-args 1 1 'rm-bucket BUCKET_NAME' "$@" || return
    if ! in-dev 2>/dev/null; then
        err "not in dev; refusing to delete data!"
        return 1
    else
        local BUCKET_NAME="$1"
        
        # Tell the bucket to stop writing a log message every time
        # someone accesses it. This is necessary in the case that
        # these access logs are being stored in the bucket itself;
        # in that case, we won't be able to empty the bucket,
        # because new logs will keep streaming in while we delete
        # the old ones. And S3 refuses to delete buckets with files
        # in them.
        #
        # Of course, other AWS stuff may be writing to the bucket
        # as well. It's the caller's responsibility to stop
        # that stuff from writing, since we can't know what it is.
        aws s3api put-bucket-logging --bucket "$BUCKET_NAME" \
                                     --bucket-logging-status '{}' \
            || return 1
        
        aws s3 rm --recursive "s3://${BUCKET_NAME}" >/dev/null \
            && aws s3api delete-bucket --bucket "$BUCKET_NAME"
    fi
}

## AWS environment variable helper functions

function enva {
    check-args 0 0 "[no arguments]" "$@" || return 1
    env | ggrep --color=always -P '^AWS_[^=]*' | command grep -vE '(AWS_DEV_ACCOUNT)|(AWS_PAGER)' | sort
}

function envp {
    check-args 0 0 "[no arguments]" "$@" || return 1
    env | ggrep --color=always -P '^PG_[^=]*' | sort
}

function envap {
    check-args 0 0 "[no arguments]" "$@" || return 1
    enva
    envp
}

