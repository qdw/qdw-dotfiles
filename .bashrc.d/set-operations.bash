## All these operators take a list of two or more files,
## where each file consists of one or more lines.
## Each file is treated as a set,
## and each line is treated as an element of that set.

## Totally broken: gives wrong results and fails to match OP

# function set-op {
#     local OP="$1"
#     shift
    
#     if [[ $# -ne 2 ]]; then
#         echo "set-op: error: expected 2 arguments but got $#"
#     else
#         case OP in
#             union | +)
#                 sort -u "$1" "$2" ;;
#             intersection | intersect | isect | ^)
#                 comm -12 <(sort "$1") <(sort "$2") ;;
#             difference | diff | subtraction | subtract | minus | -)
#                 comm -23 <(sort "$1") <(sort "$2") ;;
#             membership | in | E)
#                 command grep -Fxq "$1" "$2" ;;
#             *)
#                 err "unsupported set operation '$OP'"
#                 return 255 ;;
#         esac
#     fi
# }            

# function set-intersection {
#     check-args 2 2 'SET_1_FILE SET_2_FILE' "$@" || return 1
#     comm -12 "$1" "$2"
# }

function set-difference {
    check-args 2 2 'SET_1_FILE SET_2_FILE' "$@" || return 1
    comm -23 <(sort -u "$1") <(sort -u "$2")
}

# function set-disjunctive-union {
#     set-difference "$@"
# }

# function set-union {
#     check-args 2 2 'SET_1_FILE SET_2_FILE' "$@" || return 1
#     cat "$1" "$2" | sort | uniq
# }
