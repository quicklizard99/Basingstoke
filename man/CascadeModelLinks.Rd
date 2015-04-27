\name{CascadeModelLinks}
\alias{CascadeModelLinks}
\title{Cascade model}
\description{CascadeModelLinks.}
\usage{
CascadeModelLinks(n, S, C=0.15)
}

\arguments{
  \item{n}{the number of sets of links to generate.}
  \item{S}{the number of species in the pool.}
  \item{C}{the directed connectance (number of links / number of possible
links) of each set of links.}
}

\details{Implements the Cascade model of Cohen and Newman (1985).
}

\value{A \code{list} of `n' \code{matrices}, each with approximately
    $S * S * C$ links.}

\references{Cohen J. and Newman C. M. (1985) A Stochastic Theory of Community
  Food Webs: I. Models and Aggregated Data. Proceedings of the Royal Society B
  - Biological Sciences \bold{1237}, 421--448.
}

\author{Lawrence Hudson}

\examples{
# 1 set of links drawn from a pool of 10 species
links <- CascadeModelLinks(n=1, S=10, C=0.15)
links
}
\keyword{utilities}
