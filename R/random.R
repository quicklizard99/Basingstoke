RandomLinks <- function(n, S, C=0.15) {
    # Links assigned at random
    stopifnot(0<n)
    stopifnot(0<S)
    stopifnot(0<C && C<1)

    possible <- cbind(resource=rep(1:S, each=S), consumer=1:S)

    # sample.int rounds size down, so round up before calling sample.int
    size <- round(C*S*S, 0)
    rows <- replicate(n, sample.int(S*S, size), simplify=FALSE)
    return (lapply(rows, function(r) possible[r,,drop=FALSE]))
}
