source("brakingsystem-dagstat16.R")

set.seed(424242)
b0 <- graph.formula(s -- 2:3 -- 4 -- 5:6 -- 1 -- t, 2 -- 5, 3 -- 6)
b0 <- setCompTypes(b0, list("T1"=c(2,3,5,6), "T2"=c(4), "T3"=c(1)))
b0nulldata <- list("T1"=NULL, "T2"=NULL, "T3"=NULL)
b0testdata <- list("T1"=rexp(20, rate=0.3),       #c(2:6),
                   "T2"=runif(10, min=0, max=10), #c(4.0, 4.2, 4.4, 4.6, 4.8, 5.0),
                   "T3"=runif(10, min=8, max=10)) #(14:19)/2) # T3 late failures
b0dat <- melt(b0testdata); names(b0dat) <- c("x", "Part")
b0dat$Part <- ordered(b0dat$Part, levels=c("T1", "T2", "T3", "System"))
#b0sig <- computeSystemSurvivalSignature(b0)
b0t <- seq(0, 10, length.out=101)

rel0L <- c(rep(0.9, 5), rep(0.8, 5), rep(0.7,10), rep(0.65, 15), rep(0.5, 10), rep(0.25, 5),
           rep(0.2, 5), rep(0.15, 10), rep(0.05, 15), rep(0.001, 10), rep(0.001,11))
rel0U <- c(rep(0.999, 5), rep(0.95, 5), rep(0.85,10), rep(0.75, 15), rep(0.7, 10), rep(0.6, 5),
           rep(0.55, 5), rep(0.5, 10), rep(0.2, 15), rep(0.1, 10), rep(0.05,11))

b0nL <- data.frame(T1=rep(1,101), T2=rep(1,101), T3=rep(1,101))
b0nU <- data.frame(T1=rep(2,101), T2=rep(2,101), T3=rep(8,101))
b0yL <- data.frame(T1=rep(0.0001,101), T2=relbt2, T3=rel0L)
b0yU <- data.frame(T1=rep(0.9999,101), T2=relbt1, T3=rel0U)
b0yL[b0yL == 0] <-   1e-6
b0yL[b0yL == 1] <- 1-1e-6
b0yU[b0yU == 0] <-   1e-6
b0yU[b0yU == 1] <- 1-1e-6

b0T1 <- oneCompPriorPostSet("T1", b0t, b0testdata, b0nL, b0nU, b0yL, b0yU)
b0T2 <- oneCompPriorPostSet("T2", b0t, b0testdata, b0nL, b0nU, b0yL, b0yU)
b0T3 <- oneCompPriorPostSet("T3", b0t, b0testdata, b0nL, b0nU, b0yL, b0yU)

b0df <- rbind(data.frame(b0T1, Part="T1"), data.frame(b0T2, Part="T2"), data.frame(b0T3, Part="T3"))
b0df$Item <- ordered(b0df$Item, levels=c("Prior", "Posterior"))

T1p <- ggplot(subset(b0df, Part=="T1"), aes(x=Time)) + priopostcolours1 + priopostcolours2 #+ theme_bw()
T1p <- T1p + geom_line(aes(y=Upper, group=Item, colour=Item)) + geom_line(aes(y=Lower, group=Item, colour=Item))
T1p <- T1p + geom_ribbon(aes(ymin=Lower, ymax=Upper, group=Item, colour=Item, fill=Item), alpha=0.5)
T1p <- T1p + geom_line(aes(y=Erf, group=Item), colour=tueorange, lty=2)
T1p <- T1p + geom_rug(aes(x=x), data=subset(b0dat, Part=="T1")) + xlab("Time") + ylab("Survival Probability")
T1p <- T1p + bottomlegend
pdf("figs/T1p.pdf", width=6, height=3.75)
T1p
dev.off()

T2p <- ggplot(subset(b0df, Part=="T2"), aes(x=Time)) + priopostcolours1 + priopostcolours2 #+ theme_bw()
T2p <- T2p + geom_line(aes(y=Upper, group=Item, colour=Item)) + geom_line(aes(y=Lower, group=Item, colour=Item))
T2p <- T2p + geom_ribbon(aes(ymin=Lower, ymax=Upper, group=Item, colour=Item, fill=Item), alpha=0.5)
T2p <- T2p + geom_line(aes(y=Erf, group=Item), colour=tueorange, lty=2)
T2p <- T2p + geom_rug(aes(x=x), data=subset(b0dat, Part=="T2")) + xlab("Time") + ylab("Survival Probability")
T2p <- T2p + bottomlegend
pdf("figs/T2p.pdf", width=6, height=3.75)
T2p
dev.off()

T3p <- ggplot(subset(b0df, Part=="T3"), aes(x=Time)) + priopostcolours1 + priopostcolours2 #+ theme_bw()
T3p <- T3p + geom_line(aes(y=Upper, group=Item, colour=Item)) + geom_line(aes(y=Lower, group=Item, colour=Item))
T3p <- T3p + geom_ribbon(aes(ymin=Lower, ymax=Upper, group=Item, colour=Item, fill=Item), alpha=0.5)
T3p <- T3p + geom_line(aes(y=Erf, group=Item), colour=tueorange, lty=2)
T3p <- T3p + geom_rug(aes(x=x), data=subset(b0dat, Part=="T3")) + xlab("Time") + ylab("Survival Probability")
T3p <- T3p + bottomlegend
pdf("figs/T3p.pdf", width=6, height=3.75)
T3p
dev.off()

#