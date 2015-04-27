\name{RandomLinks}
\alias{RandomLinks}
\title{Random model}
\description{RandomLinks.}
\usage{
RandomLinks(n, S, C=0.15)
}

\arguments{
  \item{n}{the number of sets of links to generate.}
  \item{S}{the number of species in the pool.}
  \item{C}{the directed connectance (number of links / number of possible
  links) of each set of links.}
}

\details{Links chosen at random.
}

\value{A \code{list} of `n' \code{data.frames}, each with exactly $S * S * C$
    links.}

\author{Lawrence Hudson}

\examples{
# 1 set of links drawn from a pool of 10 species
links <- RandomLinks(n=1, S=10, C=0.15)
links
}
\keyword{utilities}
