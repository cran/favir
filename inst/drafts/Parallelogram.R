###################################################
### chunk number 1: Init
###################################################
library(favir)
InitPaper()
para.mod <- FavirLoadModule("Parallelogram.fmod.R")


###################################################
### chunk number 2: LatexPrelude
###################################################
IncludePrelude("The Parallelogram Method", "Benedict Escoto")


###################################################
### chunk number 3: InputData
###################################################
### Begin Input Data
rate.change.df <- data.frame(year=c(2002, 2003.5, 2004.1, 2004.4, 2005.5),
                             rate.change=c(.07, -.03, .12, .02, .1))
term.len <- 1
written.df <- data.frame(year=2001:2005, written=c(30, 45, 75, 30, 55))
periods.out <- 2001:2006
### End Input Data


###################################################
### chunk number 4: RateChanges
###################################################
rate.change.fdf <- FavirDF(rate.change.df, label="rate.change.df",
                           caption="Historical Rate Changes")
FieldFormatters(rate.change.fdf) <- list(year=MakeFormatter(digits=1,
                                           big.mark=""),
                                         rate.change=formatters$percent0)
FieldHeadings(rate.change.fdf) <- list(year="Date (in years)",
                                       rate.change="Rate Change (\\%)")
print(rate.change.fdf)


###################################################
### chunk number 5: WrittenDF
###################################################
written.fdf <- FavirDF(written.df, label="written.df",
                       caption="Rate of Premium Written by Period")
FieldHeadings(written.fdf) <- list(year="Period Start", written="Premium Rate")
FieldFormatters(written.fdf) <- list(year=formatters$flat1)
print(written.fdf)


###################################################
### chunk number 6: BasicGraph
###################################################
p <- para.mod$Parallelogram(NULL, rate.change.df, term.len)

MakeInforceDF <- function(p, spacing=0.05) {
  # Return a data frame of inforce premium by rating period, suitable for ggplot
  xvals <- seq(from=periods.out[1] - term.len,
               to=Last(periods.out) + term.len,
               by=spacing)
  inforce.graph.df <- data.frame(rating.period=NULL, year=NULL, inforce=NULL)
  for (i in seq(along=p$inforce.funcs)) { # add points for rating period i - 1
    new.df <- data.frame(rating.period=i - 1, year=xvals,
                         inforce=p$inforce.funcs[[i]](xvals))
    inforce.graph.df <- rbind(inforce.graph.df, new.df)
  }
  # Reverse the rating period so they are layered in the right order
  # otherwise we get parallelograms that go in the wrong direction
  inforce.graph.df$neg.period <- as.factor(-inforce.graph.df$rating.period)
  return(inforce.graph.df)
}

FindPeriodMidpoints <- function(inforce.graph.df) {
  # Return the midpoints of the rating period, used for text placement
  Helper <- function(df) {
    # Return midpoint of one rating period
    df <- df[df$inforce > 0, ]
    if(nrow(df) == 0) return(data.frame(x=NA, y=NA))
    middle.row <- as.integer((nrow(df) + 1)/2)
    x <- df$year[middle.row]
    y <- sum(inforce.graph.df$inforce[inforce.graph.df$year == x]) / 2
    return(data.frame(x=x, y=y))
  }
  result.df <- ddply(inforce.graph.df, .(rating.period), Helper)
  
  # Now jitter the positions to prevent text overlap
  jitter <- ifelse(RowIndicies(result.df) %% 2 == 1, 1, -1)
  result.df$x <- result.df$x + 0.15 * term.len * jitter
  result.df$y <- result.df$y * (1 + 0.3 * jitter)
  return(result.df)
}

inforce.graph.df <- MakeInforceDF(p)
period.colors <- rep(favir.colors[c("M5", "M4")],
                     length.out=length(unique(inforce.graph.df$rating.period)))
names(period.colors) <- NULL # otherwise the names confuse ggplot
basic.plot <- (ggplot(data=inforce.graph.df)
               + geom_area(aes(x=year, y=inforce, group=neg.period,
                               fill=neg.period),
                           alpha=0.5, position="stack")
               + scale_fill_manual(name="Rating Period", values=period.colors)
               + labs(x="Year", y="Inforce Premium"))

# Now add the text to the graph
midpoint.df <- FindPeriodMidpoints(inforce.graph.df)
midpoint.df$rate.level <- MakeFormatter(digits=3, math.mode=FALSE)(
                                                   p$rate.level.df$rate.level)
basic.plot <- (basic.plot
               + geom_text(data=midpoint.df, color=favir.colors["A5"], size=4,
                           aes(x=x, y=y, label=rate.level))
               + opts(legend.position = "none"))
IncludeGraph(basic.plot, caption="Basic Parallelogram",
             label="basic.graph", width=7 * 2.54, height=2.5 * 2.54)


###################################################
### chunk number 7: BasicOLEF
###################################################
n <- length(periods.out)
basic.olef.df <- data.frame(start=periods.out[1:(n - 1)],
                            end=periods.out[2:n],
                            olef=para.mod$OLEF(p, periods.out))
basic.olef.fdf <- FavirDF(basic.olef.df,
                          caption="On-Level Factors",
                          label="basic.olef.df",
                          field.formatters=list(start=formatters$flat,
                            end=formatters$flat, olef=formatters$flat3),
                          field.headings=list(start="Period Start",
                            end="Period End", olef="On-Level Factor"))
print(basic.olef.fdf)


###################################################
### chunk number 8: VariableGraph
###################################################
p2 <- para.mod$Parallelogram(written.df, rate.change.df, term.len)

var.graph.df <- MakeInforceDF(p2)
var.plot <- (ggplot(data=var.graph.df)
             + geom_area(aes(x=year, y=inforce, group=neg.period,
                             fill=neg.period),
                         alpha=0.5, position="stack")
             + scale_fill_manual(name="Rating Period", values=period.colors)
             + labs(x="Year", y="Inforce Premium"))
# Now add text
var.midpoint.df <- FindPeriodMidpoints(var.graph.df)
var.midpoint.df$rate.level <- MakeFormatter(digits=3, math.mode=FALSE)(
                                                    p2$rate.level.df$rate.level)
var.plot <- (var.plot
             + geom_text(data=var.midpoint.df, color=favir.colors["A5"], size=4,
                         aes(x=x, y=y, label=rate.level)))

# Finally add the written premium rate
total.written <- para.mod$TotalWrittenFunc(p2)(var.graph.df$year)
var.plot <- (var.plot
             + geom_line(aes(x=var.graph.df$year, y=total.written),
                         color=favir.colors["M5"], linetype=2)
             + opts(legend.position = "none"))

IncludeGraph(var.plot, caption="Parallelogram with Variable Premium Rate",
             label="variable.graph", width=7 * 2.54, height=2.5 * 2.54)


###################################################
### chunk number 9: VariableOLEF
###################################################
var.olef.df <- data.frame(start=periods.out[1:(n - 1)],
                          end=periods.out[2:n],
                          earned.premium=para.mod$EarnedPremium(p2, periods.out),
                          olef=para.mod$OLEF(p2, periods.out),
                          olep=para.mod$OLEP(p2, periods.out))
var.olef.fdf <- FavirDF(var.olef.df,
                        caption="Variable Premium Results",
                        label="var.olef.df",
                        field.formatters=list(start=formatters$flat,
                          end=formatters$flat,
                          earned.premium=formatters$comma0,
                          olef=formatters$flat3,
                          olep=formatters$comma1),
                        field.headings=list(start="Period Start",
                          end="Period End",
                          earned.premium="Earned Premium",
                          olef="On-Level Factor",
                          olep="On-Level Earned Premium"))
print(var.olef.fdf)


###################################################
### chunk number 10: Legal
###################################################
IncludeLegal(author="Benedict Escoto", year=2010)


