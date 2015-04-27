# Basingstoke - food-web structural models in R
Basingstoke provides implementations of some published food-web structural
models, allowing artificial ecological communities to be generated *in silico*.

Visual comparison of the different types of structural models
```{r}
library(Basingstoke)

# A helper function to make the comparisons easier
Artificial <- function(original, main, ...) {
    artificial <- CommunityFactory(nodes=NPS(original),
                                   C=DirectedConnectance(original),
                                   properties=CPS(original),
                                   n=1,
                                   ...)[[1]]
    PlotPredationMatrix(artificial, main=paste0(main, ', C=',
        sprintf('%.4f', DirectedConnectance(artificial))))
    return (invisible(artificial))
}

data(TL84)
TL84 <- RemoveIsolatedNodes(OrderCommunity(TL84, 'M'), title='Tuesday Lake 1984')
par(mfrow=c(2,2))

PlotPredationMatrix(TL84,
    main=paste0('Original, C=', sprintf('%.4f', DirectedConnectance(TL84))))
Artificial(TL84, generator=RandomLinks, main='Random')
Artificial(TL84, generator=CascadeModelLinks, main='Cacade')
Artificial(TL84, generator=NicheModelLinks, main='Niche')
```

![Example artificial communities](../screenshots/example1.png?raw=true)

The `NicheModelLinks` function has some more flexibility. First, we can specify
species' positions on the niche axis, which takes values between 0 and 1.
Second, it can generate links using the probabilistic niche model, which allows
gaps in consumers' diets - seen in the bottom two plots.
```{r}
par(mfrow=c(2,2))
PlotPredationMatrix(TL84, main=paste0('Original, C=',
    sprintf('%.4f', DirectedConnectance(TL84))))

niche.positions <- order(NP(TL84, 'M'))/NumberOfNodes(TL84)
Artificial(TL84, generator=NicheModelLinks,
    main='Niche,\ncentres are order of M',
    niche.positions=niche.positions)

Artificial(TL84, generator=NicheModelLinks,
    main='Probabilistic niche,\ncentres are order of M',
    niche.positions=niche.positions, probabilistic=TRUE)

# The allometric niche model is the probabilistic niche model with niche
# positions computed from log-transformed body masses scaled to be between 0
# and 1 (Williams et al 2010 PLoS ONE)
logM <- Log10M(TL84)
niche.positions <- (logM-min(logM)) / diff(range(logM))
Artificial(TL84, generator=NicheModelLinks,
    main='Probabilistic niche,\ncentres are normalised log M',
    niche.positions=niche.positions, probabilistic=TRUE)
```

![Example artificial communities](../screenshots/example2.png?raw=true)
