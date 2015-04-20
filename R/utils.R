PMOfLinks <- function(links, S) {
    stopifnot(S > 0)

    # TODO Quicker to allocation and fill?
    return (lapply(links, function(l) {
        stopifnot('resource' %in% colnames(l))
        stopifnot('consumer' %in% colnames(l))

        stopifnot(all(0 < l[,c('resource', 'consumer')] &
                      l[,c('resource', 'consumer')] <= S))

        pm <- matrix(as.integer(0), ncol=S, nrow=S)
        pm[l[,'resource'] + S * (l[,'consumer'] - 1)] <- 1
        return(pm)
    }))
}

LinksOfPM <- function(matrices) {
    # TODO Quicker to allocation and fill?
    return (lapply(matrices, function(pm) {
        if (2 != length(dim(pm))) {
            stop("pm is not a matrix")
        }

        resource <- which(pm != 0 & !is.na(pm))%%nrow(pm)
        consumer <- 1 + (which(pm != 0 & !is.na(pm))%/%nrow(pm))
        last.row <- which(resource == 0)
        consumer[last.row] <- consumer[last.row] - 1
        resource[last.row] <- nrow(pm)

        return (cbind(resource=resource, consumer=consumer))
    }))
}
