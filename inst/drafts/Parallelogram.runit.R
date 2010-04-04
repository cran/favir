#library("favir")
library("RUnit")
source("Parallelogram.fmod.R")

test.InforceDF <- function() {
  # Test the InforceDF function
  len <- 30
  date.seq <- seq(as.Date(ISOdate(2010, 1, 1)), length.out=len, by=1)
  written.df <- data.frame(date=date.seq, written=1, period=0)
  result <- InforceDF(written.df, 1, "month")
  Assert(nrow(result) == 59, "Wrong number of rows")
  Assert(result$inforce[1] == 1 && result$earned[1] == 1/31)
  Assert(result$inforce[2] == 2 && result$earned[1] == 2/31)
}

test.CollapseMatrix <- function() {
  # Test the collapse matrix function
  m1 <- matrix(c(0, 1, 0, 4, 0, 8, 3, 0, 0), nrow=3, ncol=3)
  m2 <- matrix(c(0, 2, 0, 1, 0, 8, 3, 0, 0), nrow=3, ncol=3)
  result.df <- .CollapseMatrix(1:3, m1, m2)
  correct.df <- data.frame(date=c(1, 1, 2, 3), period=c(1, 2, 0, 1),
                           inforce=c(1, 4, 8, 3), earned=c(2, 1, 8, 3))
  Assert(result.df == correct.df)
}

test.TermLength <- function() {
  # Test the TermLength function
  date <- as.Date(ISOdate(2010, 1, 1))
  date2 <- as.Date(ISOdate(2008, 1, 1))

  Assert(TermLength(date, 7, "day") == 7)
  Assert(TermLength(date, 1, "month") == 31)
  Assert(TermLength(date, 2, "month") == 59)
  Assert(TermLength(date2, 2, "month") == 60)
  Assert(TermLength(date, 1, "year") == 365)
  Assert(TermLength(date, 1, "year") == 366)
}

MakeZeroInforce <- function() {
  # Return a data frame representing the inforce premium by rating period
  # Fixed total written case.  Start by defaulting to 0
  df <- data.frame(time=NULL, period=NULL, inforce=NULL)
  for (time in c(-1, 0, 0.5, 0.75, 1, 2))
    for (period in 0:3)
      df <- rbind(df, data.frame(time=time, period=period, inforce=0))
  return(df)
}

UpdateInforceDF <- function(df, time, period, inforce) {
  # Update the inforce premium at time and period
  Assert(LookupDF(df, time=time, period=period)$inforce == 0,
         "Current value already set (not 0)")
  df[df$time == time & df$period == period, ]$inforce <- inforce
  return(df)
}

InforceDFFixed <- function() {
  # Return Inforce testing data frame for fixed earned case
  df <- MakeZeroInforce()
  df <- UpdateInforceDF(df, 2, 3, 1)
  df <- UpdateInforceDF(df, 1, 2, 1)
  df <- UpdateInforceDF(df, 0.75, 1, 0.25)
  df <- UpdateInforceDF(df, 0.75, 2, 0.75)
  df <- UpdateInforceDF(df, 0.5, 1, 0.5)
  df <- UpdateInforceDF(df, 0.5, 2, 0.5)
  df <- UpdateInforceDF(df, 0, 0, 0.5)
  df <- UpdateInforceDF(df, 0, 1, 0.5)
  df <- UpdateInforceDF(df, -1, 0, 1)
  return(df)
}

InforceDFVariable <- function() {
  # Return Inforce testing data frame for variable earned premium case
  df <- MakeZeroInforce()
  df <- UpdateInforceDF(df, -1, 0, 1)
  df <- UpdateInforceDF(df, 0, 0, 0.5)
  df <- UpdateInforceDF(df, 0, 1, 0.5)
  df <- UpdateInforceDF(df, 0.5, 1, 0.5)
  df <- UpdateInforceDF(df, 0.5, 2, 1)
  df <- UpdateInforceDF(df, 0.75, 1, 0.25)
  df <- UpdateInforceDF(df, 0.75, 2, 1.5)
  df <- UpdateInforceDF(df, 1, 2, 2)
  df <- UpdateInforceDF(df, 2, 3, 2)
  return(df)
}

ZeroEarnedDF <- function(test.cases, starts) {
  # Return zero'd data frame with earned per period
  df <- data.frame(test.case=NULL, start=NULL, period=NULL, earned=NULL)
  for (test.case in test.cases)
    for (start in starts)
      for (period in 0:3)
        df <- rbind(df, data.frame(test.case=test.case,
                                   start=start, period=period, earned=0))
  return(df)
}

UpdateEarnedDF <- function(df, test.cases, starts, periods, earned) {
  # Update the inforce premium at time and period
  for (test.case in test.cases)
    for (start in starts)
      for (period in periods) {
        Assert(LookupDF(df, test.case=test.case, start=start,
                        period=period)$earned == 0,
               "Current value already set (not 0)")
        df[df$test.case == test.case & df$start == start
           & df$period == period, ]$earned <- earned
      }
  return(df)
}

EarnedTestDFFixed <- function() {
  # Return testing data frame to test fixed earned case
  # term length is 1
  # rate changes at 1, 0, and -0.5 (for case "d")
  # rate changes at 1 and 0 (for case "c"), only 1 at "b", none for "a"
  all.cases <- c("a", "b", "c", "d")
  all.starts <- c(-1, 0, 0.5, 1, 2)
  all.periods <- 0:3
  df <- ZeroEarnedDF(all.cases, all.starts)
  df <- UpdateEarnedDF(df, "a", all.starts, 0, 1.0)

  df <- UpdateEarnedDF(df, "b", c(-1, 0), 0, 1.0)
  df <- UpdateEarnedDF(df, "b", 0.5, 0, 7/8)
  df <- UpdateEarnedDF(df, "b", 0.5, 1, 1/8)
  df <- UpdateEarnedDF(df, "b", 1, c(0, 1), 0.5)
  df <- UpdateEarnedDF(df, "b", 2, 1, 1.0)

  df <- UpdateEarnedDF(df, "c", -1, 0, 1.0)
  df <- UpdateEarnedDF(df, "c", 0, c(0, 1), 0.5)
  df <- UpdateEarnedDF(df, "c", 0.5, 0, 1/8)
  df <- UpdateEarnedDF(df, "c", 0.5, 1, 6/8)
  df <- UpdateEarnedDF(df, "c", 0.5, 2, 1/8)
  df <- UpdateEarnedDF(df, "c", 1, c(1, 2), 0.5)
  df <- UpdateEarnedDF(df, "c", 2, 2, 1.0)

  df <- UpdateEarnedDF(df, "d", -1, 0, 7/8)
  df <- UpdateEarnedDF(df, "d", -1, 1, 1/8)
  df <- UpdateEarnedDF(df, "d", 0, 0, 1/8)
  df <- UpdateEarnedDF(df, "d", 0, 1, 3/8)
  df <- UpdateEarnedDF(df, "d", 0, 2, 4/8)
  df <- UpdateEarnedDF(df, "d", 0.5, 1, 1/8)
  df <- UpdateEarnedDF(df, "d", 0.5, 2, 6/8)
  df <- UpdateEarnedDF(df, "d", 0.5, 3, 1/8)
  df <- UpdateEarnedDF(df, "d", 1, c(2, 3), 0.5)
  df <- UpdateEarnedDF(df, "d", 2, 3, 1.0)
  
  return(df)
}

EarnedTestDFVariable <- function() {
  # Return testing data frame to test variable earned case
  # term length is 1 again and rate changes at 1, 0, -0.5
  # Written rate is 1 before 0 and 1 after 0
  all.cases <- c("c", "d")
  all.starts <- c(-1, -0.5, 0, 0.5, 1, 2)
  all.periods <- 0:3
  df <- ZeroEarnedDF(all.cases, all.starts)

  df <- UpdateEarnedDF(df, "c", -1, 0, 1.0)
  df <- UpdateEarnedDF(df, "c", -0.5, 0, 7/8)
  df <- UpdateEarnedDF(df, "c", -0.5, 1, 2/8)
  df <- UpdateEarnedDF(df, "c", 0, 0, 1/2)
  df <- UpdateEarnedDF(df, "c", 0, 1, 1)
  df <- UpdateEarnedDF(df, "c", 0.5, 0, 1/8)
  df <- UpdateEarnedDF(df, "c", 0.5, 1, 12/8)
  df <- UpdateEarnedDF(df, "c", 0.5, 2, 2/8)
  df <- UpdateEarnedDF(df, "c", 1, c(1, 2), 1)
  df <- UpdateEarnedDF(df, "c", 2, 2, 2)

  df <- UpdateEarnedDF(df, "d", -1, 0, 7/8)
  df <- UpdateEarnedDF(df, "d", -1, 1, 1/8)
  df <- UpdateEarnedDF(df, "d", -0.5, 0, 4/8)
  df <- UpdateEarnedDF(df, "d", -0.5, 1, 3/8)
  df <- UpdateEarnedDF(df, "d", -0.5, 2, 2/8)
  df <- UpdateEarnedDF(df, "d", 0, 0, 1/8)
  df <- UpdateEarnedDF(df, "d", 0, 1, 3/8)
  df <- UpdateEarnedDF(df, "d", 0, 2, 8/8)
  df <- UpdateEarnedDF(df, "d", 0.5, 1, 1/8)
  df <- UpdateEarnedDF(df, "d", 0.5, 2, 12/8)
  df <- UpdateEarnedDF(df, "d", 0.5, 3, 2/8)
  df <- UpdateEarnedDF(df, "d", 1, c(2, 3), 1)
  df <- UpdateEarnedDF(df, "d", 2, 3, 2)

  return(df)
}

GetTestCasePara <- function(test.case, fixed=TRUE) {
  # Return the Parallelogram object for the fixed earning rate case
  AssertSingle(test.case)
  rate.change.df <- switch(test.case,
           a=NULL,
           b=data.frame(year=1, rate.change=0.1),
           c=data.frame(year=0:1, rate.change=c(0.05, 0.1)),
           d=data.frame(year=c(-0.5, 0, 1), rate.change=c(-0.05, 0.05, 0.1)))
  written.df <- if (fixed) NULL else data.frame(year=-1:0, written=1:2)
  return(Parallelogram(written.df, rate.change.df, term.len=1))
}

CheckInforce <- function(para, inforce.check.df) {
  # Compare the inforce premium from parallelogram and data frame
  total.inforce.df <- ddply(inforce.check.df, .(time),
                            function(df) data.frame(inforce=sum(df$inforce)))
  for (i in RowIndicies(total.inforce.df)) {
    time <- total.inforce.df$time[i]
    inforce <- total.inforce.df$inforce[i]
    Assert(TotalInforceFunc(para)(time) == inforce,
           "Wrong total inforce premium from parallelogram function")
  }
  # Now check by time and by period
  for (i in RowIndicies(inforce.check.df)) {
    period <- inforce.check.df$period[i]
    time <- inforce.check.df$time[i]
    inforce <- inforce.check.df$inforce[i]
    Assert(para$inforce.funcs[[period + 1]](time) == inforce,
           "Wrong inforce premium from parallelogram function")
  }
}

CheckEarned <- function(para, earned.check.df) {
  # Compare the earned premium in a period with the results in data frame
  for (i in RowIndicies(earned.check.df)) {
    period <- earned.check.df$period[i]
    start <- earned.check.df$start[i]
    earned.check <- earned.check.df$earned[i]
    earned.df <- EarnedByPeriod(para, c(start, start + 1))
    if (period > max(earned.df$rating.period))
      Assert(earned.check == 0, "Not enough periods returned")
    else
      Assert(LookupDF(earned.df, rating.period=period)$earned == earned.check,
             "Wrong earned premium from parallelogram function")
  }
}

test.FixedPara <- function() {
  # Test the parallelogram function for the fixed written case
  test.cases <- c("a", "b", "c", "d")
  para.list <- lapply(as.list(test.cases),
                      function(test.case) GetTestCasePara(test.case, TRUE))
  names(para.list) <- test.cases
  inforce.check.df <- InforceDFFixed()
  earned.check.df <- EarnedTestDFFixed()
  
  for (test.case in test.cases) {
    if (test.case == "d") # only check the final case
      CheckInforce(para.list[[test.case]], inforce.check.df)
    CheckEarned(para.list[[test.case]],
                earned.check.df[earned.check.df$test.case == test.case, ])
  }
}

test.OtherTermLength <- function() {
  # Do a quick check for term length 0.5
  p <- Parallelogram(NULL, data.frame(year=0, rate.change=0.1), term.len=0.5)
  Assert(p$inforce.funcs[[1]](0) == 0.5, "Wrong inforce premium")
  Assert(p$inforce.funcs[[1]](0.1) == 0.4, "Wrong inforce premium")
  Assert(p$inforce.funcs[[2]](0.5) == 0.5, "Wrong inforce premium")

  earned.df1 <- EarnedByPeriod(p, c(0, 1))
  Assert(all(earned.df1$earned == c(0.25, 0.75)),
         "Wrong earned premium by period")
  earned.df2 <- EarnedByPeriod(p, c(0, 1.5))
  Assert(all(earned.df2$earned == c(0.25, 1.25)),
         "Wrong earned premium by period")
}

test.VariablePara <- function() {
  # Test the parallelogram function for variable written case
  test.cases <- c("c", "d")
  para.list <- lapply(as.list(test.cases),
                      function(test.case) GetTestCasePara(test.case, FALSE))
  names(para.list) <- test.cases
  inforce.check.df <- InforceDFVariable()
  earned.check.df <- EarnedTestDFVariable()
  
  for (test.case in test.cases) {
    if (test.case == "d") # only check the final case
      CheckInforce(para.list[[test.case]], inforce.check.df)
    CheckEarned(para.list[[test.case]],
                earned.check.df[earned.check.df$test.case == test.case, ])
  }
}

test.OLEP <- function() {
  # Test the OLEP function, variable written case
  p <- GetTestCasePara("d", FALSE) # do single test on final variable case
  olep1 <- OLEP(p, c(-1, 0, 1, 2, 3))
  olep1.check <- c(1.1 * 1.05 * (7/8 * 0.95 + 1/8),
                   1.1 * (1/8 * 1.05 * 0.95 + 3/8 * 1.05 + 1),
                   1.1 + 1,
                   2)
  Assert(all(olep1 == olep1.check), "Wrong on-level premium case 1")

  olep2 <- OLEP(p, c(-0.5, 0.5, 1.5))
  olep2.check <- c(1.1 * (1/2 * 1.05 * 0.95 + 3/8 * 1.05 + 1/4),
                   1/8 * 1.1 * 1.05 + 3/2 * 1.1 + 1/4)
  Assert(all(olep2 == olep2.check), "wrong on-level premium case 2")
}
