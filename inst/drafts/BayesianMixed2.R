###################################################
### chunk number 1: SourceR
###################################################
library(favir)
library(runjags)
InitPaper()


###################################################
### chunk number 2: LatexPrelude
###################################################
IncludePrelude("Bayesian Claim Severity Part 2",
            author="Benedict Escoto",
            subtitle="Mixed Exponentials with Trend, Censoring, and Truncation",
            header.lines="\\usepackage{amsmath}")


###################################################
### chunk number 3: InputData
###################################################
actual.means <- c(50, 100, 500, 1500, 5000, 20000) * 1000
prior.weights <- c(.3, .25, .25, .1, .07, .03)

alpha0 <- 20
alpha <- prior.weights * alpha0
trend.prior.mu <- .05
trend.prior.sigma <- .01

claim.df <- data.frame(x = c(33750, 1e6, 22707, 54135, 174524, 19661,
                             140735, 1e6, 1127, 316483),
                       age = c(3, 1, 1, 1, 3, 2, 2, 3, 1, 2),
                       truncation = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                       capped = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, 
                         FALSE, TRUE, FALSE, FALSE))


###################################################
### chunk number 4: claimTable
###################################################
claim.fdf <- FavirDF(claim.df, caption="Observed Claim Data",
                     label="claim data")
claim.fdf$capped <- ifelse(claim.df$capped, "Yes", "No")
FieldHeadings(claim.fdf) <- list(x="Amount", age="Age",
                                 truncation="Deductible", capped="Capped?")
print(claim.fdf)


###################################################
### chunk number 5: priorMixed
###################################################
# Check means and weights
Assert(sum(prior.weights) == 1, "Prior weights need to sum to 1")
Assert(length(actual.means) == length(prior.weights),
       "Mixed exponential weights and means need the same length")
m <- length(actual.means)

# print table
prior.mixed.fdf <- FavirDF(data.frame(weight=prior.weights, mean=actual.means),
                           caption="Prior Means and Weights",
                           label="priorMixedTable")
FieldFormatters(prior.mixed.fdf) <- list(weight=formatters$percent1)
FieldHeadings(prior.mixed.fdf) <- list(weight="Weights (\\%)", mean="Means")
SummaryRow(prior.mixed.fdf) <- list(weight="Avg",
                                    mean=sum(prior.mixed.fdf$mean
                                      * prior.mixed.fdf$weight))
print(prior.mixed.fdf)


###################################################
### chunk number 6: defineFunctions
###################################################
rdirichlet <- function(n, alpha) {
  # Simulate n results from a dirichlet distribution with parameters alpha
  # Each row is an independent draw
  dim <- length(alpha)
  y <- matrix(data=NA, nrow=n, ncol=dim)
  for (i in seq(length=dim))
    y[, i] <- rgamma(n, alpha[i])
  for (j in seq(length=n))
    y[j, ] <- y[j, ] / sum(y[j, ])
  return(y)
}

rMixedExpon <- function(n, weights, means) {
  # Simulate data points from a mixed exponential distribution
  Assert(sum(weights) == 1, paste("Sum of weights equals", weights))
  m <- length(weights)
  Assert(m == length(means))

  sampled.means <- means[sample(m, n, weights, replace=TRUE)]
  return(rexp(n, 1/sampled.means))
}

ExponExpLiL <- function(mean, start, end) {
  # Return the expected loss in layer for an exponential distribution
  # with layer attaching at start and exhausting at end
  return(mean * (exp(-start / mean) - exp(-end / mean)))
}

MixedExponExpLiL <- function(weights, means, start, end) {
  # Return expected loss in layer for mixed exponential distribution
  # with layer attaching at start and exhausting at end
  return(sum(weights * means * (exp(-start / means) - exp(-end / means))))
}

DirichletStddev <- function(alpha, cond.exp) {
  # Return the standard deviation of the expected value of g(x) given
  # a set of weights.  cond.exp[i] should be E[g(x) | x is from bucket
  # i].
  m <- length(alpha)
  Assert(m == length(cond.exp))
  
  alpha0 <- sum(alpha)
  a <- alpha / alpha0
  variance <- 0
  for (j in 1:m) {
    variance <- variance + cond.exp[j]^2 * a[j] * (1 - a[j])
    for (k in 1:m)
      if (j != k)
        variance <- variance - cond.exp[j] * cond.exp[k] * a[j] * a[k]
  }
  return(sqrt(variance / (alpha0 + 1)))
}

DirichletLayerStddev <- function(alpha, means, start, end) {
  # Return the standard deviation of expected layer losses
  cond.exp <- sapply(means, function(mean) ExponExpLiL(mean, start, end))
  return(DirichletStddev(alpha, cond.exp))
}


###################################################
### chunk number 7: runModel
###################################################
# Calculate trend gamma parameters
trend.scale <- trend.prior.sigma^2 / (1 + trend.prior.mu)
trend.shape <- (1 + trend.prior.mu) / trend.scale

# Define JAGS inputs
jags.data <- list(claims=(claim.df$x[!claim.df$capped]
                          - claim.df$truncation[!claim.df$capped]),
                  capped.claims=(claim.df$x[claim.df$capped]
                                 - claim.df$truncation[claim.df$capped]),
                  alpha=alpha,
                  means=actual.means,
                  ones=rep(1, length(claim.df$x[claim.df$capped])),
                  ages=claim.df$age[!claim.df$capped],
                  capped.ages=claim.df$age[claim.df$capped],
                  trend.shape=trend.shape,
                  trend.rate=1 / trend.scale)
jags.init <- list(means=list(weights=prior.weights),
                  equal=list(weights=rep(1/m, m)))
model <- "model { 
  weights ~ ddirch(alpha)
  trend.factor ~ dgamma(trend.shape, trend.rate)
  for (i in 1:length(claims)) {
    buckets[i] ~ dcat(weights)
    mu[i] <- means[buckets[i]] / trend.factor^ages[i]
    claims[i] ~ dexp(1 / mu[i])
  }
  for (i in 1:length(capped.claims)) {
    capped.buckets[i] ~ dcat(weights)
    capped.mu[i] <- means[capped.buckets[i]] / trend.factor^capped.ages[i]
    prob.capped[i] <- exp(-capped.claims[i] / capped.mu[i])
    ones[i] ~ dbern(prob.capped[i])
  }
}"

# Run the actual model
thin.factor <- 3
n.chains <- 2
model.out <- autorun.jags(model, data=jags.data, inits=jags.init,
                          monitor=c("weights", "trend.factor"), 
                          method="parallel", # comment this out for windows
                          startburnin=1000, startsample=5000,
                          n.chains=n.chains, interactive=FALSE, thin=thin.factor)
# Above, can use method="parallel" at least on linux to use multiple processors
model.summary <- summary(model.out$mcmc)

# These are useful diagnostics using the coda package
#plot(model.out$mcmc)
#traceplot(model.out$mcmc)
#autocorr.diag(model.out$mcmc)
#autocorr.plot(model.out$mcmc)


###################################################
### chunk number 8: weightResults
###################################################
new.weights.fdf <- FavirDF(data.frame(prior.weight=prior.weights,
                      mean=actual.means,
                      post.weight=model.summary$statistics[1:m, "Mean"],
                      post.stddev=model.summary$statistics[1:m,
                        "Time-series SE"],
                      mean2=actual.means),
                           caption="Prior vs Posterior Exponential Weights",
                           label="weightResults")
FieldFormatters(new.weights.fdf) <- list(prior.weight=formatters$percent1,
                                         post.weight=formatters$percent1,
                                         post.stddev=formatters$percent2)
FieldHeadings(new.weights.fdf) <- list(prior.weight="Weight (\\%)",
         mean="Mean", post.weight="Weight (\\%)",
         post.stddev="Error (\\%)", mean2="Mean")
FieldGroup(new.weights.fdf, "priors") <- c("prior.weight", "mean")
GroupHeading(new.weights.fdf, "priors") <- "Prior to Data"
FieldGroup(new.weights.fdf, "posterior") <- c("post.weight", "post.stddev",
                                              "mean2")
GroupHeading(new.weights.fdf, "posterior") <- "Posterior to Data"
SummaryRow(new.weights.fdf) <- list(prior.weight="Avg",
      mean=sum(new.weights.fdf$prior.weight * actual.means), post.stddev="Avg",
      mean2=sum(new.weights.fdf$post.weight * actual.means))
print(new.weights.fdf)


###################################################
### chunk number 9: lossInLayerPlots
###################################################
layers <- c(0, 5e5, 7.5e5, 1e6, 1.5e6, 2e6, 3e6, 5e6)
layer.names <- c("500x0", "250x500", "250x750", "500x1M", "500x1.5M",
                 "1Mx2M", "2Mx3M")

MakeLiLDF <- function(means, layers, weight.matrix) {
  # Make a data frame with loss in layer amounts
  #
  # Result has a row for each layer, and simulation trial
  # weight.matrix has a set of weights for each row
  Helper <- function(layer.num) {
    start <- layers[layer.num]
    end <- layers[layer.num + 1]
    lil <- apply(weight.matrix, 1,
                 function(weights) MixedExponExpLiL(weights, means, start, end))
    return(data.frame(layer.num=layer.num, loss.in.layer=lil))
  }
  return(mdply(data.frame(layer.num=1:(length(layers) - 1)), Helper))
}

prior.weight.sim <- rdirichlet(1000, alpha)
Assert(n.chains==2, "Change this and the next line if n.chains != 2")
posterior.weight.sim <- rbind(model.out$mcmc[[1]][, 1:m],
                              model.out$mcmc[[2]][, 1:m])
prior.lil.df <- MakeLiLDF(actual.means, layers, prior.weight.sim)
loss.in.layer.df <- MakeLiLDF(actual.means, layers, posterior.weight.sim)
total.lil.df <- rbind(prior.lil.df, loss.in.layer.df)
total.lil.df$dist <- c(rep("Prior", nrow(prior.lil.df)),
                       rep("Posterior", nrow(loss.in.layer.df)))
total.lil.df$lil.short <- total.lil.df$loss.in.layer / 1000
BoxPlotHelper <- function(sub.df)
  return(data.frame(ymax=quantile(sub.df$lil.short, .9),
                    upper=quantile(sub.df$lil.short, .75),
                    middle=quantile(sub.df$lil.short, .5),
                    lower=quantile(sub.df$lil.short, .25),
                    ymin=quantile(sub.df$lil.short, .1),
                    layer.name=layer.names[sub.df$layer.num[1]]))
boxplot.df <- ddply(total.lil.df, .(layer.num, dist), BoxPlotHelper)
boxplot.df$dist <- factor(boxplot.df$dist, levels=c("Prior", "Posterior"))
plot.colors <- c(favir.colors["M3"], favir.colors["M4"])
names(plot.colors) <- NULL
lil.boxplot <- (ggplot(data=boxplot.df)
      + geom_boxplot(aes(ymax=ymax, upper=upper, middle=middle, lower=lower,
                         ymin=ymin, fill=dist, x=layer.name),
                     stat="identity", 
                     colour=favir.colors["M5"])
      + ylim(0, 350)
      + scale_fill_manual(name="Distribution", values=plot.colors)
      + labs(x="Layer", y="Expected Loss in Layer ($000)"))
IncludeGraph(lil.boxplot, height=9, width=18,
             caption="Prior vs Posterior Loss in Layer", label="lil boxplot")


###################################################
### chunk number 10: trendResults
###################################################
trend.samples <- c(model.out$mcmc[[1]][, m+1], model.out$mcmc[[2]][, m+1])
probs <- c(.1, .25, .5, .75, .9)
post.quants <- (quantile(trend.samples, probs=probs) - 1) * 100
prior.quants <- (qgamma(probs, shape=trend.shape,
                        scale=trend.scale) - 1) * 100
trend.df <- rbind(data.frame(dist="Prior",
                             ymin=prior.quants[1], lower=prior.quants[2],
                             middle=prior.quants[3],
                             upper=prior.quants[4], ymax=prior.quants[5]),
                  data.frame(dist="Posterior",
                             ymin=post.quants[1], lower=post.quants[2],
                             middle=post.quants[3],
                             upper=post.quants[4], ymax=post.quants[5]))
trend.df$dist <- factor(trend.df$dist, levels=c("Prior", "Posterior"))
trend.plot <- (ggplot(data=trend.df)
         + geom_boxplot(aes(ymax=ymax, upper=upper, middle=middle, lower=lower,
                            ymin=ymin, fill=dist, x=dist),
                        stat="identity", colour=favir.colors["M5"])
         + scale_fill_manual(name="Distribution", values=plot.colors)
         + labs(x="Distribution", y="Trend (%)")
         + opts(legend.position="none"))
IncludeGraph(trend.plot, height=8, width=6, caption="Trend Results",
             label="trend boxplot")


###################################################
### chunk number 11: ILFResults
###################################################
ilf.base <- 1e6

ILFQuantiles <- function(weight.sim, limit) {
  # Return a one-row data frame with quantiles for a single ILF
  SingleILF <- function(weights) {
    # Return ILFs for a single mixed exponential distribution
    return(MixedExponExpLiL(weights, actual.means, 0, limit)
           / MixedExponExpLiL(weights, actual.means, 0, ilf.base))
  }
  ilf.samples <- apply(weight.sim, 1, SingleILF)
  quants <- quantile(ilf.samples, probs=c(.1, .25, .5, .75, .9))
  return(data.frame(ymin=quants[1], lower=quants[2], middle=quants[3],
                    upper=quants[4], ymax=quants[5]))
  
}
input.df <- data.frame(dist=c(rep("Prior", length(layers) - 1),
                         rep("Posterior", length(layers) - 1)),
                       limit=c(layers[-1], layers[-1]))

MDHelper <- function(sub.df) {
  # Helper function for mdply; return quantile data frame
  if (sub.df$dist=="Prior")
    return(ILFQuantiles(prior.weight.sim, sub.df$limit))
  else return(ILFQuantiles(posterior.weight.sim, sub.df$limit))
}
ilf.plot.df <- ddply(input.df, .(dist, limit), MDHelper)
ilf.plot.df$dist <- factor(ilf.plot.df$dist, levels=c("Prior", "Posterior"))
ilf.plot.df$display.lim <- factor(ilf.plot.df$limit / 1000)

ilf.plot <- (ggplot(data=ilf.plot.df)
             + geom_boxplot(aes(ymax=ymax, upper=upper, middle=middle,
                                lower=lower, ymin=ymin, fill=dist,
                                x=display.lim),
                            stat="identity", colour=favir.colors["M5"])
             + scale_fill_manual(name="Distribution", values=plot.colors)
             + labs(x="Limit ($000)", y="Increased Limit Factor"))
IncludeGraph(ilf.plot, height=8, width=18,
             caption="Prior vs Posterior ILF Distribution", label="ilf boxplot")


###################################################
### chunk number 12: Legal
###################################################
IncludeLegal("Benedict Escoto", 2010)


