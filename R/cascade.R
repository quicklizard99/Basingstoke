CascadeModelLinks <- function(n, S, C=0.15) {
    # Cascade model of Cohen, J. E., and C. M. Newman. 1985. A stochastic 
    # theory of community food webs. I. Models and aggregated data. Proceedings 
    # of the Royal Society of London Series B 224:421-448.
    stopifnot(0<n)
    stopifnot(0<S)
    stopifnot(0<C && C<1)

    possible <- cbind(resource=1:S, consumer=rep(1:S, each=S))

    # A matrix of S x S  - TRUE if that link is possible
    p <- matrix(FALSE, ncol=S,  nrow=S)

    # Only links in the upper triangle are possible
    p[upper.tri(p)] <- TRUE

    link.possible <- which(p)

    # The number of possible links
    n.possible <- length(link.possible)

    res <- vector("list", n)    # Container for output
    for(index in 1:n) {
        pm <- p
        pm[link.possible] <- 2*C*S/(S-1)>runif(n.possible)
        if(any(pm[link.possible]>0)) {
            res[[index]] <- possible[which(pm>0),,drop=FALSE]
        }
        else {
            # No links - nothing to do
        }
    }
    return (res)
}
