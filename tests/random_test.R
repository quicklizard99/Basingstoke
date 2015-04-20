TestRandomLinks <- function() {
    AssertRaises(RandomLinks(S=0))
    AssertRaises(RandomLinks(S=1, n=0))
    AssertRaises(RandomLinks(S=1, n=1, C=0))
    AssertRaises(RandomLinks(S=1, n=1, C=1))

    # 10 sets of links with directed connectance of 0.15 taken from from a pool 
    # of 10 species.
    res <- RandomLinks(S=10, n=10, C=0.15)
    AssertEqual(10, length(res))
    AssertEqual(15, mean(sapply(res, nrow)))
    AssertEqual(FALSE, any(sapply(res, function(r) any(duplicated(r)))))

    # 100 sets of links with directed connectance of 0.5 taken from from a pool 
    # of 10 species.
    res <- RandomLinks(S=10, n=100, C=0.5)
    AssertEqual(100, length(res))
    AssertEqual(50, mean(sapply(res, nrow)))
    AssertEqual(FALSE, any(sapply(res, function(r) any(duplicated(r)))))
}
