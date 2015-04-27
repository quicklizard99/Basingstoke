\name{PMOfLinks}
\alias{PMOfLinks}
\alias{LinksOfPM}
\title{Links to predation matrices and predation matrices to links}
\description{RandomLinks.}
\usage{
PMOfLinks(links, S)
LinksOfPM(matrices)
}

\arguments{
  \item{links}{a list of \code{matrices} with columns named `resource' and
    `consumer' with the values either $0$ and $1$.}
  \item{S}{the number of species in the pool from which `links' were drawn}
  \item{matrices}{a \code{list} of square \code{matrix} objects with the values
    either $0$ and $1$.}
}

\details{\code{PMOfLinks} converts the `list of links' form to `predation
    matrix' form, a square matrix with `S' rows and `S' columns.
}

\value{Either a \code{list} of square \code{matrices} or a \code{list} of
    \code{matrices} with columns named `resource' and `consumer'.}

\author{Lawrence Hudson}

\examples{
# Generate some links
links <- RandomLinks(n=10, S=10, C=0.15)
links[[1]]

# Get predation matrices
pm <- PMOfLinks(links, 10)
pm[[1]]

# Get lists of links
reconstructed.links <- LinksOfPM(pm)

# Randomly generated links will have an arbitraty order - order by consumer,
# resource for the purposes of comparison
links <- lapply(links, function(l) l[order(l[,'consumer'], l[,'resource']),])

# Should be TRUE
all.equal(links, reconstructed.links)
}
\keyword{utilities}
