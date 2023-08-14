iac2() {
    operation=$1
    resources=$2
    shift 2
    other_flags="$*"
    racket command.rkt -s ${STATE:-state.dat} $other_flags --$operation $resources
}
