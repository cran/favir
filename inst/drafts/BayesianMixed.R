###################################################
### chunk number 1: SourceR
###################################################
library(favir)
InitPaper()


###################################################
### chunk number 2: LatexPrelude
###################################################
IncludePrelude("Bayesian Claim Severity with Mixed Distributions",
               "Benedict Escoto",
               header.lines="\\usepackage{amsmath}")


###################################################
### chunk number 3: defaultDist
###################################################
prior.weights <- c(0.1, 0.2, 0.3, 0.2, 0.1, 0.05, 0.035, 0.01, 0.005)
means <- c(0.3, 1, 3, 10, 30, 100, 300, 1000, 3000) * 1000
default.df <- data.frame(weight=prior.weights, mean=means)
Assert(sum(default.df$weight) == 1, err.msg="Weights don't add to 1!")
default.fdf <- FavirDF(default.df, caption="Default Mixed Exponential",
                       label="defaultDist")
FieldFormatters(default.fdf) <- list(weight=formatters$percent1)
FieldHeadings(default.fdf) <- list(weight="Weights (\\%)", mean="Means")
SummaryRow(default.fdf) <- list(weight="Avg",
                                mean=sum(default.df$mean * default.df$weight))
print(default.fdf)


###################################################
### chunk number 4: setSigma
###################################################
sigma <- 50000

GetAlpha0 <- function(sigma, prior.weights, means) {
  # Return the dirichlet parameter alpha 0
  Assert(length(prior.weights) == length(means), "mismatched lengths")
  AssertSingle(sigma)
  
  s <- sum(means^2 * prior.weights * (1 - prior.weights))
  for (j in seq(along=means))
    for (k in seq(along=means))
      if (j != k)
        s <- s + means[j] * means[k] * prior.weights[j] * prior.weights[k]
  return(s / sigma^2 - 1)
}
alpha0 <- GetAlpha0(sigma, prior.weights, means)
alpha <- prior.weights / sum(prior.weights) * alpha0


###################################################
### chunk number 5: claims
###################################################
claims <- c(500, 32.5, 8.2, 10, 750) * 1000
claim.fdf <- FavirDF(data.frame(claim=claims), label="claims",
                     caption="Claim Severities")
FieldHeadings(claim.fdf) <- list(claim="Amount")
print(claim.fdf)


###################################################
### chunk number 6: computeAnswer
###################################################
rdirichlet <- function(alpha) {
  # Simulate one result from a dirichlet distribution with parameters alpha
  dim <- length(alpha)
  y <- rep(NA, dim)
  for (i in seq(length=dim))
    y[i] <- rgamma(1, alpha[i])
  return(y/sum(y))
}

GibbsSample <- function(k, means, alpha, claims) {
  # Apply Gibbs sampling algorithm to observed claims
  # Arguments:
  #   k - number of iterations
  #   alpha - dirichlet parameters
  #   means - means of the exponential distributions
  #   claims - vector of observed claim severities
  # Return value: a data frame with two columns
  #   weight - the expected posterior weights
  #   error - approximate standard deviation of each estimate
  discard <- 10 # number of initial draws to discard
  Assert(length(alpha) == length(means),
         "Inconsistent number of terms in mixture")
  Assert(k > 0, "Number of iterations k must be a positive integer")
  # each row in the weight matrix holds one sample of the weights
  weight.matrix <- matrix(NA, nrow=k + discard, ncol=length(means))

  weight.matrix[1, ] <- alpha / sum(alpha)
  for (i in 2:(k + discard)) {
    buckets <- SampleBuckets(means, weight.matrix[i - 1, ], claims)
    weight.matrix[i, ] <- rdirichlet(alpha + buckets)
  }
  weight.matrix <- weight.matrix[(discard + 1):k, ]
  return(data.frame(weight=apply(weight.matrix, 2, mean),
                    error=apply(weight.matrix, 2, sd) / sqrt(k)))
}

SampleBuckets <- function(means, weights, claims) {
  # Sample from categorial distribution conditional on claims
  # Arguments:
  #   means - vector of m means of the exponential distributions
  #   claims - vector of observed claim severities
  #   weights - vector of m probabilities; weights prior to claims
  # Return value: vector of length n sampled from p(buckets|weights, claims)
  m <- length(means)
  result <- rep(0, m)
  buckets <- seq(along=means)
  for (claim in claims) {
    likelihoods <- exp(-claim / means) / means
    weighted.probs <- likelihoods * weights
    s <- sample(buckets, size=1, prob=weighted.probs)
    result[s] <- result[s] + 1
  }
  return(result)
}

answer <- GibbsSample(2000, means, alpha, claims)

answer.fdf <- FavirDF(data.frame(prior.weight=prior.weights,
                                 mean=means,
                                 post.weight=answer$weight,
                                 post.stddev=2 * answer$error, # be conservative
                                 mean2=means),
                      caption="Results of Bayesian Updating",
                      label="answer")
FieldFormatters(answer.fdf) <- list(prior.weight=formatters$percent1,
                                    post.weight=formatters$percent1,
                                    post.stddev=formatters$percent2)
FieldHeadings(answer.fdf) <- list(prior.weight="Weight (\\%)",
                                  mean="Mean",
                                  post.weight="Weight (\\%)",
                                  post.stddev="Error (\\%)",
                                  mean2="Mean")
FieldGroup(answer.fdf, "priors") <- c("prior.weight", "mean")
GroupHeading(answer.fdf, "priors") <- "Prior to Data"
FieldGroup(answer.fdf, "posterior") <- c("post.weight", "post.stddev", "mean2")
GroupHeading(answer.fdf, "posterior") <- "Posterior to Data"
SummaryRow(answer.fdf) <- list(prior.weight="Avg",
                               mean=sum(answer.fdf$prior.weight * means),
                               post.stddev="Avg",
                               mean2=sum(answer.fdf$post.weight * means))
print(answer.fdf)


###################################################
### chunk number 7: graphAnswer
###################################################
Densities <- function(means, weights, xs) {
  # Return probability density function for mixed exponential
  return(sapply(xs, function(x) sum(weights * dexp(x, 1/means))))
}

loss.points <- exp(seq(from=log(min(means / 3)), to=log(max(means * 3)),
                       length=100))
prior.densities <- log(Densities(means, prior.weights, loss.points), base=10)
posterior.densities <- log(Densities(means, answer$weight, loss.points), base=10)

weights.graph.df <- rbind(data.frame(x=means, y=prior.weights, dist="Prior"),
                          data.frame(x=means, y=answer$weight, dist="Posterior"))
weights.graph.df$section <- "Weights"
weights.graph.df$bar.alpha <- 1 # this is a hack to avoid plotting some geoms
weights.graph.df$line.alpha <- 0 # alpha = 0 means not plotted
                          
density.graph.df <- rbind(data.frame(x=loss.points, y=prior.densities,
                                     dist="Prior"),
                          data.frame(x=loss.points, y=posterior.densities,
                                     dist="Posterior"))
density.graph.df$section <- "Density"
density.graph.df$bar.alpha <- 0
density.graph.df$line.alpha <- 1

# This graph is complicated and took some trial and error, but hopefully it
# shows a lot of information
color.scale <- favir.colors[c("M3", "M4")]
names(color.scale) <- c("Prior", "Posterior")
answer.graph <- (ggplot()
                 + geom_vline(aes(xintercept=c(claims, claims),
                                  section=c(rep("Density", length(claims)),
                                            rep("Weights", length(claims)))),
                              color=favir.colors["A3"], size=0.25,
                              position="dodge")
                 + geom_bar(aes(x=weights.graph.df$x,
                                y=weights.graph.df$y,
                                color=weights.graph.df$dist,
                                fill=weights.graph.df$dist,
                                section="Weights"),
                            stat="identity", position="dodge")
                 + scale_colour_manual(name="Distribution", values=color.scale)
                 + scale_fill_manual(name="Distribution", values=color.scale)
                 + scale_x_log10()
                 + labs(x="Loss Severity", y="Weights and Log(Density)")
                 + geom_line(aes(x=density.graph.df$x,
                                 y=density.graph.df$y,
                                 group=density.graph.df$dist,
                                 color=density.graph.df$dist,
                                 section="Density"),
                             size=0.7)
                 + facet_grid(section ~ ., scale="free"))
answer.graph$legend.position <- "bottom"
IncludeGraph(answer.graph, height=8, width=18,
             caption="Graph of Results", label="answer graph")


###################################################
### chunk number 8: Legal
###################################################
IncludeLegal("FAViR Project", 2010)


