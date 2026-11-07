#### Kubernetes (and OpenShift) settings

function oc {
    if [[ "$1" = "login" ]] && [[ $# -eq 1 ]]; then
        local AUTH_URL
        if AUTH_URL=$(command oc login | sed s'/.*\(https.*\)/\1/'); then
            open "$AUTH_URL"
        fi
    else
        command oc "$@" | $PAGER
    fi
}

# Show all images in use in the current cluster; code ganked from https://kubernetes.io/docs/tasks/access-application-cluster/list-all-running-container-images/
function kube-ls-images { 
    kubectl get pods --all-namespaces \
                      -o jsonpath="{.items[*].spec.containers[*].image}" \
        | tr -s '[:space:]' '\n' | sort | uniq -c
}

# If I'm going to make the same typo all the time, I might as well autocorrect it
function lube-aws { kube-aws "$@" ;}
function lube-ls-images { kube-ls-images "$@" ;}
function lubectl { kubectl "$@" ;}
function lubecfg { kubecfg "$@" ;}
function minilube { minikube "$@" ;}
