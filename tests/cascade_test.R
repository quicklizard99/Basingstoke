TestCascadeModelLinks <- function() {
    AssertRaises(CascadeModelLinks(S=0))
    AssertRaises(CascadeModelLinks(S=1, n=0))
    AssertRaises(CascadeModelLinks(S=1, n=1, C=0))
    AssertRaises(CascadeModelLinks(S=1, n=1, C=1))

    # 100 sets of links with directed connectance of 0.15 taken from from a 
    # pool of 10 species.
    res <- CascadeModelLinks(S=10, n=500, C=0.15)
    AssertEqual(500, length(res))
    AssertEqual(15, mean(sapply(res, nrow)), tolerance=1, scale=1)
    AssertEqual(FALSE, any(sapply(res, function(r) any(duplicated(r)))))

    # 100 sets of links with directed connectance of 0.5 taken from from a 
    # pool of 10 species.
    res <- CascadeModelLinks(S=10, n=500, C=0.5)
    AssertEqual(500, length(res))
    AssertEqual(50, mean(sapply(res, nrow)), tolerance=6, scale=1)
    AssertEqual(FALSE, any(sapply(res, function(r) any(duplicated(r)))))
}

