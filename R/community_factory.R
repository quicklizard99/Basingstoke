CommunityFactory <- function(S, nodes, generator=NicheModelLinks, n=1, 
                             accept=NULL, energetically.feasible=TRUE, 
                             trace.progress=FALSE, validate=TRUE,
                             properties=NULL, ...) {
    # Returns a collection of artificially generated communities. 
    # Either S or nodes should be provided - no need to provide both. 
    # S - number of nodes in the generated communities
    # nodes - either node names or a data.frame of node properties that 
    # satisfies the conditions nodes of Community.

    # generator - function used to generate sets of trophic links
    # n - number of communities to generate
    # accept - either NULL or a function that takes a community as its only 
    # argument and returns a logical
    # energetically.feasible - if TRUE, communities that are not energetically 
    # feasible according to PreyAveragedTrophicLevel are discarded
    # trace.progress - if TRUE, the feedback is printed
    # ... - other arguments to generator
    if(missing(nodes)) {
        nodes <- paste('Node', 1:S)
    }

    if(is.character(nodes)) {
        nodes <- data.frame(node=nodes, row.names=nodes, stringsAsFactors=FALSE)
    }

    if(missing(S))    S <- nrow(nodes)

    stopifnot(0<S)
    stopifnot(0<n)
    stopifnot(nrow(nodes)==S)

    if(trace.progress) {
        tracefn <- cat
    }
    else {
        tracefn <- function(...) {}
    }

    if('category' %in% colnames(nodes)) {
        tracefn('Removing node category')
        nodes$category <- NULL
    }

    # The properties used by each generated community
    if(!is.null(properties) && 'title' %in% names(properties)) {
        properties$title <- NULL
    }
    properties <- c(properties, list(title='Artificial community'))

    # TODO Faster to use a fixed-length list?
    #      All elements initially NULL
    #      Set infeasable and unacceptable communities to NULL.
    #      Loop while any(sapply(communities, is.null))
    communities <- NULL
    while(length(communities)<n) {
        # Produce communities
        tracefn(paste('Generating', n-length(communities), 'communities\n'))
        args <- c(list(n=n-length(communities)), S=S, list(...))
        new <- lapply(do.call(generator, args), function(links) {
            links <- cbind(resource=nodes$node[links[,1]],
                           consumer=nodes$node[links[,2]])
            if(validate) {
                # Validate generated values
                return (cheddar::Community(nodes=nodes, trophic.links=links,
                                           properties=properties))
            }
            else {
                # Avoid the time-consuming validation carried out by Community
                community <- list(nodes=nodes, trophic.links=links, 
                                  properties=properties)
                class(community) <- c('Community', 'list')
                return (community)
            }
        })

        if(energetically.feasible) {
            acceptable <- sapply(new, function(community) {
                !(all(is.na(cheddar::PreyAveragedTrophicLevel(community))))
            })

            if(any(!acceptable)) {
                new <- new[acceptable]
                tracefn('Removing', sum(!acceptable), 'communities that are',
                        'not energetically feasible.', length(new),
                        'communities remain\n')
            }
        }

        if(!is.null(accept) && length(new)>0) {
            acceptable <- sapply(new, accept)
            if(any(!acceptable)) {
                new <- new[acceptable]
                tracefn('Removing', sum(!acceptable), 'communities that are',
                        'not acceptable.', length(new), 'communities remain\n')
            }
        }

        if(length(new)>0) {
            communities <- c(communities, new)
        }
    }

    # Assign sensible titles
    communities <- mapply(title=paste('Artificial community', 1:n), 
                          community=communities, 
                          SIMPLIFY=FALSE, 
                          FUN=function(title, community) {
        # Community objects cannot be modified. Get rid of the Community class 
        # so that the title can be assigned.
        class(community) <- 'list'
        community$properties$title <- title
        class(community) <- c('Community', 'list')
        return (community)
    })

    return (cheddar::CommunityCollection(communities))
}
