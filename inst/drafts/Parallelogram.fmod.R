# Copyright 2010 Benedict Escoto
#
# This file is part of FAViR.

# FAViR is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.

# FAViR is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
# License for more details.

# You should have received a copy of the GNU General Public License
# along with FAViR.  If not, see <http://www.gnu.org/licenses/>.

library("favir")

PolicyPeriod <- function(written.df, period.dates) {
  # Tag each day in written.df with numeric policy period in period.df
  #
  # Args:
  # written.df - a data frame with the fields date and written
  # period.dates - a vector of dates, indicating the start of each period
  #
  # Output: a vector of period numbers.  Days before the first period
  # get number 0.  The day of the first period until the day before
  # the second period get 1, etc.
  ...
}

InforceDFbyDay <- function(written.df, term.len, term.unit) {
  # Get in force premium by day
  #
  # Args:
  # written.df - a data frame with premium written each day
  #    it should have the fields date, written, and period
  # term.length - the length of the policy term in term.units
  # term.unit - either "day", "month", or "year"
  #
  # Output: a data frame that says how much premium was in force and
  # how much was earned at each day from each policy period.  It will
  # have the fields date, inforce, earned, and period.  There may be
  # multiple rows for each period.
  inforce <- earned <- .InforceDFInitMatrix(written.df, term.len, term.unit)
  for(i in RowIndicies(written.df)) {
    premium <- written.df$written[i]
    term.length <- TermLength(written.df$date[i], term.len, term.unit)
    date.indicies <- i:(i + term.length - 1)
    period <- written.df$period[i]
    inforce[date.indicies, period] <- inforce[date.indicies, period] + premium
    earned[date.indicies, period] <- (earned[date.indicies, period] +
                                      premium / term.length)
  }
  return(.CollapseMatrix(written.df$date[1], inforce, earned))
}

.InforceDFInitMatrix <- function(written.df, term.len, term.unit) {
  # Return a matrix with a row for each date and a column for each period
  last.date <- Last(written.df$date)
  row.num <- nrow(written.df) + TermLength(last.date, term.len, term.unit) - 1
  return(matrix(0, nrow=row.num, ncol=max(written.df$period) ))
}

.CollapseMatrix <- function(start.date, inforce.matrix, earned.matrix) {
  # Return a data frame by removing 0's from matricies
  #
  # The data frame will have an entry for each non-0 row of the
  # matricies.
  all.dates <- seq(start.date, by=1, length.out=nrow(inforce.matrix))
  date <- period <- inforce <- earned <- NULL
  for(i in RowIndicies(inforce.matrix))
    for(j in seq(length.out=ncol(inforce.matrix)))
      if(inforce.matrix[i, j] != 0) {
        date <- c(date, all.dates[i])
        period <- c(period, j)
        inforce <- c(inforce, inforce.matrix[i, j])
        earned <- c(earned, earned.matrix[i, j])
      }
  return(data.frame(date=date, period=period, inforce=inforce, earned=earned))
}

TermLength <- function(date, term.len, term.unit) {
  # Return the length in days of a term starting on the given date.
  #
  # Args: see GetInforce for term.len and term.unit
  #
  # Output: The number of days of a term starting on the given date.
  if(term.unit == "day")
    return(term.len)
  posixlt <- as.POSIXlt(date)
  if(term.unit == "month") {
    tot.months <- posixlt$mon + term.len + 1
    posixlt$mon <- (tot.months %% 12) - 1
    posixlt$year <- posixlt$year + (tot.months %/% 12)
  } else {
    Assert(term.unit == "year", 'Valid units are "day", "month", or "year"')
    posixlt$year <- posixlt$year + term.len
  }
  return(c(as.Date(posixlt) - date)) # unclass before returning
}
  
EarnedDF <- function(inforce.df, interval.len, interval.unit,
                     term.len, term.unit) {
  # Return a data frame aggregating the inforce into earned premium
  #
  # Args:
  # inforce.df - data frame like the output of InforceDF
  # interval.len - the length of the interval to aggregate to in units
  # interval.unit - either "day", "month", or "year"
  # term.len and term.unit - see InforceDF
  #
  # Output: a data frame giving earned premium by policy period and
  # interval.  Fields are: date, earned, period.
  ...
}

###################################################################

Parallelogram <- function(written.df, rate.change.df, term.len) {
  # Return on-level written and earned premium by interval
  #
  # Args:
  # written.df - a data frame with written premium by period.  Fields are
  #   year and written.
  #
  # rate.change.df - a data frame with dates and rate changes.  Fields
  #   are year and rate.change
  # term.len - length of policy term in years
  written.df <- .CheckWrittenDF(written.df, term.len)
  rate.level.df <- .RateLevelDF(rate.change.df)
  written.steps <- .WrittenSteps(written.df, rate.level.df)
  inforce.pieces <- .InforcePieces(written.steps, term.len)
  result <- list(term.len=term.len, rate.level.df=rate.level.df,
                 written.steps=written.steps,
                 inforce.funcs=lapply(inforce.pieces, function(pair) pair$f),
                 inforce.knots=lapply(inforce.pieces,
                   function(pair) pair$knots))
  class(result) <- "parallelogram"
  return(result)
}

.CheckWrittenDF <- function(written.df, term.len) {
  # Make sure written.df is in the right format
  if (is.null(written.df)) # Default to constant writing
    return(data.frame(written=1, year=0))
  Assert("data.frame" %in% class(written.df),
         "written.df should be a data frame with premium in it")
  Assert(nrow(written.df) >= 1, "written.df has no rows!")
  Assert("year" %in% names(written.df),
         "No year column found in written.df data frame")
  Assert("written" %in% names(written.df) || "earned" %in% names(written.df),
         'written.df data frame requires either "written" or "earned" column')
  if (!("written" %in% names(written.df)))
    written.df$written <- written.df$earned - term.len / 2
  Assert(!any(is.na(written.df$written)) && !any(is.na(written.df$year)),
         "NAs not allowed inside written.df")
  n <- nrow(written.df)
  if (n > 1)
    Assert(all(written.df$year[2:n] > written.df$year[1:(n - 1)]),
           "years in written.df must be in order")
  return(written.df)
}

.RateLevelDF <- function(rate.change.df) {
  # Check rate.change.df and return rate.level.df
  #
  # rate.level.df will have one row for every different rate level.
  # The columns are:
  #    start - starting time of the period, possibly -Inf
  #    end - end time of the period, possibly Inf
  #    rate.level - the rate level, where 1.0 is the current rate level
  if (is.null(rate.change.df) || nrow(rate.change.df) == 0)
    return(data.frame(start=-Inf, end=Inf, rate.level=1))
  Assert(all(c("year", "rate.change") %in% names(rate.change.df)),
         'rate.change.df must contain "year" and "rate.change" columns')
  rate.change.df <- rate.change.df[order(rate.change.df$year), ]
  start <- c(-Inf, rate.change.df$year)
  end <- c(rate.change.df$year, Inf)
  rate.level <- c(rev(cumprod(rev(1 + rate.change.df$rate.change))), 1.0)
  return(data.frame(start=start, end=end, rate.level=rate.level))
}

.WrittenSteps <- function(written.df, rate.level.df) {
  # Return list of step functions, one for each rate change period
  Helper <- function(start, end) {
    # Return a single step function covering written from start to end
    prev.written <- if (any(written.df$year < start))
      written.df[max(which(written.df$year < start)), "written"]
      else written.df$written[1]
    sub.df <- written.df[start <= written.df$year & written.df$year < end, ]
    return(stepfun(x=c(start, sub.df$year, end),
                   y=c(0, prev.written, sub.df$written, 0)))
    
  }
  return(mlply(rate.level.df[, c("start", "end")], Helper))
}

.InforcePieces <- function(written.steps, term.len) {
  # Return list of inforce information, one element for each rating period
  #
  # Each element in the result be a list with two subelements:
  #   f - a piecewise linear functions giving inforce premium at that time
  #   knots - a numeric vector of f's turning points
  Helper <- function(stepfun) {
    # Return f and knots for a single step function
    f.knots <- GetKnots(stepfun)
    if (length(f.knots) == 0) # written and inforce are constant
      return(list(f=function(x) stepfun(0) * term.len, knots=NULL))
    y <- YVals(f.knots, stepfun)
    f <- approxfun(f.knots, y, rule=2, method="linear")
    return(list(knots=f.knots, f=f))
  }

  GetKnots <- function(stepfun) {
    # Return the knots of the inforce premium function given written step fun
    f.knots <- sort(unique(c(knots(stepfun), knots(stepfun) + term.len)))
    f.knots <- f.knots[-Inf < f.knots & f.knots < Inf]
    Assert(length(f.knots) >= 2, "Sanity check--this shouldn't be false")
    return(f.knots)
  }

  YVals <- function(f.knots, stepfun) {
    # Given knots of inforce function and stepfun, return inforce prem function
    #
    # This integrates the stepfun by rolling over the interval (x -
    # term.len, x) one unit at a time (once per iteration of the for
    # loop).
    x <- f.knots[1]
    y <- stepfun(x - 1) * term.len # stepfun is constant before first knot
    for (new.x in f.knots[2:length(f.knots)]) {
      new.inforce <- stepfun(Last(x)) * (new.x - Last(x))
      expired <- stepfun(Last(x) - term.len) * (new.x - Last(x))
      x <- c(x, new.x)
      y <- c(y, Last(y) + new.inforce - expired)
    }
    return(y)
  }

  return(lapply(written.steps, Helper))
}

TotalWrittenFunc <- function(parallelogram) {
  # Return a function yielding total written premium given year
  Assert("parallelogram" %in% class(parallelogram),
         "First argument of TotalWrittenFunc should be parallelogram object")
  written.funcs <- parallelogram$written.steps
  ReturnFunc <- function(years) {
    # Return total written for each year specified
    result <- rep(0, length(years))
    for (i in seq(along=written.funcs))
      result <- result + written.funcs[[i]](years)
    return(result)
  }
}

TotalInforceFunc <- function(parallelogram) {
  # Return function that yields total inforce premium as function of time
  Assert("parallelogram" %in% class(parallelogram),
         "First argument of TotalInforceFunc should be parallelogram")
  inforce.funcs <- parallelogram$inforce.funcs
  ReturnFunc <- function(years) {
    # Return total inforce for each year specified
    result <- rep(0, length(years))
    for (i in seq(along=inforce.funcs))
      result <- result + inforce.funcs[[i]](years)
    return(result)
  }
  return(ReturnFunc)
}

EarnedByPeriod <- function(parallelogram, periods.out) {
  # Return earned premium by period from parallelogram results
  #
  # Inputs:
  #   parallelogram - parallelogram results object
  #   periods.out - vector of period begin/end numbers
  # Output will be a data frame with these columns:
  #   start, end - the year of the start and end of the period.out
  #   rating.period - the number of the rating period
  #   earned - the premium earned in that out period that was written under
  #     that rating period.
  Helper <- function(period.df) {
    # Given start and end times for the period, return section of final result
    Assert(nrow(period.df) == 1, "Sanity Check")

    RatingHelper <- function(rating.period) {
      # Returned earned premium during the period for a single rating period
      #
      # To do this we integrate over the piecewise linear inforce
      # function.  Since we know where the knots are, to find the area
      # we can just average the beginning and end of each section and
      # multiply by the width (each section is trapezoidal).
      knots <- parallelogram$inforce.knots[[rating.period + 1]]
      knots <- knots[period.df$start < knots & knots < period.df$end]
      Inforce <- parallelogram$inforce.funcs[[rating.period + 1]]
      oldx <- period.df$start
      area <- 0
      for (newx in c(knots, period.df$end)) {
        area <- area + (Inforce(oldx) + Inforce(newx)) / 2 * (newx - oldx)
        oldx <- newx
      }
      return(area / parallelogram$term.len)
    }

    rating.periods <- RowIndicies(parallelogram$rate.level.df) - 1 # start at 0
    earned <- sapply(rating.periods, RatingHelper)
    return(data.frame(rating.period=rating.periods, earned=earned))
  }

  Assert("parallelogram" %in% class(parallelogram),
         "First parameter should be a parallelogram as made by Parallelogram")
  periods.df <- .CheckPeriodsOut(periods.out)
  return(ddply(periods.df, .(start, end), Helper))
}

.CheckPeriodsOut <- function(periods.out) {
  # Make sure periods.out is in right format, return periods.df
  n <- length(periods.out)
  Assert(n >= 2, "periods.out requires at least beginning and end points")
  Assert(all(periods.out[2:n] > periods.out[1:(n - 1)]),
         "times in periods.out need to be in order")
  Assert(!any(is.na(periods.out)), "NA's not allowed in periods.out")
  return(data.frame(period.out=1:(n - 1),
                    start=periods.out[1:(n - 1)], end=periods.out[2:n]))
}

OLEP <- function(parallelogram, periods.out, earned.df=NULL) {
  # Return On-Level Earned Premium by period
  if (is.null(earned.df))
    earned.df <- EarnedByPeriod(parallelogram, periods.out)
  earned.df$rate.level <- parallelogram$rate.level.df$rate.level[
                                   earned.df$rating.period + 1]
  return(daply(earned.df, .(start),
               function(df) sum(df$earned * df$rate.level)))
}

OLEF <- function(parallelogram, periods.out, earned.df=NULL) {
  # Return on-level premium factors by specified period
  if (is.null(earned.df))
    earned.df <- EarnedByPeriod(parallelogram, periods.out)
  earned.df$rate.level <- parallelogram$rate.level.df$rate.level[
                                   earned.df$rating.period + 1]
  Helper <- function(df) sum(df$earned * df$rate.level) / sum(df$earned)
  return(daply(earned.df, .(start), Helper))
}

EarnedPremium <- function(parallelogram, periods.out, earned.df=NULL) {
  # Return raw earned premium by period
  if (is.null(earned.df))
    earned.df <- EarnedByPeriod(parallelogram, periods.out)
  return(daply(earned.df, .(start), function(df) sum(df$earned)))
}

SimpleOLEF <- function(rate.change.df, periods.out, term.len=1) {
  # A convenience function to compute the on-level factors for the given periods
  p <- Parallelogram(written.df=NULL, rate.change.df, term.len)
  return(OLEF(p, periods.out))
}
