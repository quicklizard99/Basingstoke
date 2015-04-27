\name{Basingstoke-package}
\alias{Basingstoke-package}
\alias{Basingstoke}
\docType{package}
\title{
    \packageTitle{Basingstoke}
}

\description{
    \packageDescription{Basingstoke}
}

\details{
    \packageDESCRIPTION{Basingstoke}
    \packageIndices{Basingstoke}
}

\author{
    \packageAuthor{Basingstoke}
    Maintainer: \packageMaintainer{Basingstoke}
}

\keyword{package}

\examples{
# A list containing 10 matrices each of 15 links
links <- RandomLinks(n=10, S=10, C=0.15)

# A list containing 10 matrices each of around 15 links
cascade <- CascadeModelLinks(n=10, S=10, C=0.15)

# A list containing 10 matrices each of around 15 links
niche <- NicheModelLinks(n=10, S=10, C=0.15)
}
