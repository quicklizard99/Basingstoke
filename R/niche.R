NicheModelLinks <- function(n, S, C=0.15, niche.positions=NULL, 
                            probabilistic=FALSE) {
    # Niche model of Williams, R.J. and Martinez, N.D. (2000) Nature 404, 6447, 
    # 180--183.
    # Probabilistic niche model of Williams, R.J. and Anandanadesan, A. and 
    # Purves, D.W. (2010) PLoS One 5, 8, e12092.

    # niche.positions - either NULL or S real numbers between 0 and 1

    # Arguments for niche centres and/or niche ranges?

    stopifnot(0<n)
    stopifnot(0<S)
    stopifnot(0<C && C<1)

    possible <- cbind(resource=1:S, consumer=rep(1:S, each=S))

    stopifnot(is.null(niche.positions) || 
              (length(niche.positions)==S && 
               !any(is.na(niche.positions)) &&
               all(0<=niche.positions & niche.positions<=1)))

    if(is.null(niche.positions)) {
        # A new set of niche positions for each community, if not provided - a 
        # matrix of n rows and S cols.
        niche.positions <- t(replicate(n, sort(runif(S))))
        smallest.niche.index <- 1
    }
    else {
        smallest.niche.index <- which.min(niche.positions)
        niche.positions <- matrix(niche.positions, ncol=S, nrow=n, byrow=TRUE)
    }

    # Feeding ranges - a matrix of n rows and S cols.
    r <- niche.positions * matrix(rbeta(n*S, 1, 1/(2*C)-1), ncol=S)

    # Set range of species with the smallest niche position to 0 to ensure 
    # that there is at least one basal species. This is the first column.
    r[,smallest.niche.index] <- 0

    if(probabilistic) {
        probabilistic.runif <- matrix(runif(n*S*S), ncol=S)
    }

    # Diet positions - a matrix of n rows and S cols. Prevent range from 
    # being greater than niche position or greater than 1.
    c <- matrix(runif(n*S, min=r/2, max=pmin(niche.positions, 1-r/2)), ncol=S)

    if(probabilistic) {
        fn <- function(index) {
            pm <- exp(-sweep(outer(niche.positions[index,], c[index,], '-'), 2, r[index,]/2, "/")^2)>probabilistic.runif[index,]
            return (possible[which(pm),,drop=FALSE])
        }
    }
    else {
        fn <- function(index) {
            # Compare niche positions to upper and lower bounds feeding ranges
            pm <- outer(niche.positions[index,], c[index,]-r[index,]/2, '>=') & 
                  outer(niche.positions[index,], c[index,]+r[index,]/2, '<=')
            return (possible[which(pm),,drop=FALSE])
        }
    }

    return (lapply(1:n, fn))
}
NicheModelLinks(1, 10)
