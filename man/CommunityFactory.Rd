\name{CommunityFactory}
\alias{CommunityFactory}
\title{Community factory}
\description{Community factory.}
\usage{
CommunityFactory(S, nodes, generator, n=1, 
                 accept=NULL, energetically.feasible=TRUE, 
                 trace.progress=FALSE, validate=TRUE,
                 properties=NULL, ...)
}

\arguments{
  \item{S}{number of nodes in the generated communities.}
  \item{nodes}{either a vector of node names or a \code{data.frame} of node
    properties that satisfies the conditions nodes of Community.}
  \item{generator}{function used to generate sets of trophic links.}
  \item{n}{number of communities to generate.}
  \item{accept}{either \code{NULL} or a function that takes a community as its
    only argument and returns a \code{logical}.}
  \item{energetically.feasible}{if \code{TRUE}, communities that are not
    energetically feasible according to Cheddar's
    \code{PreyAveragedTrophicLevel} function are discarded.}
  \item{trace.progress}{if \code{TRUE} then feedback is printed as the
    function runs.}
  \item{validate}{if `TRUE`, created communities are validated using the checks
    provided by Cheddar's \code{Community} function.}
  \item{properties}{either \code{NULL} or a \code{list} of community-level
    properties.}
  \item{\dots}{.}
}

\details{
Either `S' or `nodes' should be provided - no need to provide both.
}

\value{A \code{list} of `n' \code{Community} objects.}

\author{Lawrence Hudson}

\seealso{\code{\link{Community}}, \code{\link{PreyAveragedTrophicLevel}}
         \code{\link{IsIsolatedNode}}, \code{\link{IsolatedNodes}}
         \code{\link{IsBasalNode}}, \code{\link{BasalNodes}}
        }

\examples{
# Example 1
# Use the niche model to create 12 communities containing 25 species with a
# directed connectance close to 0.15. 
communities <- CommunityFactory(S=25, n=12, generator=NicheModelLinks, C=0.15)

# Communities are named 'Artificial community 1'..'Artificial community n'
names(communities)

# Each has 25 nodes
sapply(communities, NumberOfNodes)

# Each has a C close to 0.15
sapply(communities, DirectedConnectance)
hist(sapply(communities, DirectedConnectance))

# Plot nodes stacked vertically by trophic level
plot(communities, ncol=4)

# At least one community will have a non-zero number of isolated nodes
sapply(communities, function(c) return(length(IsolatedNodes(c))))


# Example 2
# Use the accept argument to ensure that the communities have no isolated
# nodes, using Cheddar' IsIsolatedNode function
communities <- CommunityFactory(S=25,
                                n=12,
                                generator=NicheModelLinks,
                                C=0.15,
                                accept=function(c) 0==sum(IsIsolatedNode(c)))

# All zero
sapply(communities, function(c) return(length(IsolatedNodes(c))))


# Example 3
# Guarantee that communities have no isolated nodes and exactly 5 basal nodes -
# use Cheddar's IsBasalNode function for this; also get CommunityFactory to
# trace its progress.
communities <- CommunityFactory(S=25,
                                n=12,
                                generator=NicheModelLinks,
                                C=0.15,
                                accept=function(c) 0==sum(IsIsolatedNode(c)) &&
                                                   5==sum(IsBasalNode(c)),
                                trace.progress=TRUE)

# No community has isolated nodes
all(0==sapply(communities, function(c) return(length(IsolatedNodes(c)))))

# All communities have five basal nodes
all(5==sapply(communities, function(c) return(length(BasalNodes(c)))))
}
\keyword{utilities}
