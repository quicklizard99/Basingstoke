\name{NicheModelLinks}
\alias{NicheModelLinks}
\title{Niche and probabilistic niche models}
\description{NicheModelLinks.}
\usage{
NicheModelLinks(n, S, C=0.15, niche.positions=NULL, probabilistic=FALSE)
}

\arguments{
  \item{n}{the number of sets of links to generate.}
  \item{S}{the number of species in the pool.}
  \item{C}{the directed connectance (number of links / number of possible
    links) of each set of links.}
  \item{niche.positions}{a vector of `S' numbers, givien species' positions on
    the niche axis, or \code{NULL}}
  \item{probabilistic}{if\code{TRUE}, the function uses the
    probabilistic niche model}
}

\details{Implements the Niche model of Williams and Martinez (2000) and
the Probabilistic Niche model of Williams et al 2010
}

\value{A \code{list} of `n' \code{matrices}, each with approximately
    $S * S * C$ links.}

\references{Williams R. J. and Martinez N.D. (2000) Simple rules yield complex
    food webs. Nature \bold{404}, 180--183.

Williams R. J. and Anandanadesan A. and Purves D. (2010) The Probabilistic Niche
    Model Reveals the Niche Structure and Role of Body Size in a Complex Food
    Web. PLOS ONE \bold{5}, e12092.
}

\author{Lawrence Hudson}

\examples{
# 1 set of links drawn from a pool of 10 species
niche <- NicheModelLinks(n=1, S=10, C=0.15)
niche

probabilistic <- NicheModelLinks(n=1, S=10, C=0.15, probabilistic=TRUE)
probabilistic

}
\keyword{utilities}
