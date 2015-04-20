TestPMOfLinks <- function() {
    links <- list(cbind(resource=c(1, 10), consumer=c(1, 10)))

    # Invalid S
    AssertRaises(PMOfLinks(links, S=0))

    # Value in links > S
    AssertRaises(PMOfLinks(links, S=9))

    expected <- matrix(0, nrow=10, ncol=10)
    expected[ 1, 1] <- 1
    expected[10,10] <- 1
    expected <- list(expected)
    AssertEqual(expected, PMOfLinks(links, S=10))

    AssertEqual(links, LinksOfPM(PMOfLinks(links, S=10)))
}

TestLinksOfPM <- function() {
    pm <- matrix(0, nrow=10, ncol=10)
    pm[ 1, 1] <- 1
    pm[10,10] <- 1
    pm <- list(pm)

    expected <- list(cbind(resource=c(1, 10), consumer=c(1, 10)))

    AssertEqual(expected, LinksOfPM(pm))

    AssertEqual(pm, PMOfLinks(LinksOfPM(pm), S=10))    
}
