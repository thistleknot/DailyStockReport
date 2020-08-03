#' Get Trading Dates for one or more years
#'
#' Get a vector of dates of non-holiday weekdays.
#' 
#' This uses holiday calendar functions (\code{holidayNYSE} by default)
#' from the \emph{timeDate} package.  If \emph{timeDate} is not loaded, it 
#' will be temporarily loaded, then unloaded \code{on.exit}.
#' 
#' @param year vector of 4 digit years or something that is coercible to 
#'   a vector 4 digit years via \code{as.numeric}
#' @param FUN a function that takes a \code{year} argument and returns a vector
#'   that can be coerced to a \code{Date}.  \code{holidayNYSE} by default. Most
#'   likely, this will be one of:  \sQuote{holidayLONDON}, \sQuote{holidayNERC}, 
#'   \sQuote{holidayNYSE}, \sQuote{holidayTSX}, \sQuote{holidayZURICH}
#' @return a vector of all dates in \code{years} that are weekdays and not 
#'   holidays.
#' @author GSee
#' @examples
#' \dontrun{
#' TradingDates(2012)
#' TradingDates(2010:2011)
#' }
#' @export
TradingDates <- function(year=format(Sys.Date(), "%Y"), FUN=holidayNYSE) {
  # the next few lines should be removed when this code is added to a package
  # that Imports timeDate
  if (!"package:timeDate" %in% search()) {
    suppressPackageStartupMessages({ 
      if (!require(timeDate)) {
        stop("timeDate must be installed to use this function.")
      }
    })
    on.exit(detach(package:timeDate, unload=TRUE))
  }
  ## End of code that should be removed when this is added to a package
  year <- as.numeric(year)
  fun <- match.fun(FUN)
  do.call('c', lapply(year, function(y) {
    holidays <- as.Date(fun(year=y))
    all.days <- seq.Date(as.Date(paste(y, '01-01', sep='-')), 
                         as.Date(paste(y, '12-31', sep='-')), by='days')
    nohol <- all.days[!all.days %in% holidays]
    nohol[!format(nohol, '%w') %in% c("6", "0")] #neither holiday nor weekend
  }))
}


#' Get Date of previous (next) trading day
#' 
#' Get the Date of the previous (next) trading day.
#' 
#' For \code{PrevTradingDate}, \code{n} is the number of days to go back. So,
#' if \code{n} is 2 and today is a a Monday, it would return the date of the
#' prior Thursday because that would be 2 trading days ago.
#' \code{n} works analogously in \code{NextTradingDate}.
#'
#' The maximum value that \code{n} can be is the total number of days in the
#' year prior to \code{Date} plus the total number of years in the current 
#' year of \code{Date}.  So, on the last day of the year, the max value of
#' \code{n} will usually be \code{504} (because most years have 252 trading 
#' days).  One the first day of the year, the max value of \code{n} will usually
#' be \code{252}.
#'
#' @param n number of days to go back. 1 is the previous trading day; 2 is the
#'   trading day before that, etc.  \code{n} should be less than 365, but see
#'   details
#' @param Date a \code{Date} or something that can be coerced to a \code{Date}
#' @return \code{PrevTradingDate} returns the date of the previous trading day 
#' up to, but not including, \code{Date}.  \code{NextTradingDate} returns the 
#' date of the next trading day.
#' @author GSee
#' @seealso \code{\link{TradingDates}}
#' @examples
#' \dontrun{
#' PrevTradingDate()
#' PrevTradingDate('2012-01-03')
#' NextTradingDate()
#' NextTradingDate('2012-12-24')
#' }
#' @export
#' @rdname PrevTradingDate
PrevTradingDate <- function(Date=Sys.Date(), n=1) {
  stopifnot(require(xts)) #remove this line when this is added to a package that Imports xts (needed for first/last)
  D <- as.Date(Date)
  y <- as.numeric(format(D, "%Y"))
  trading.days <- TradingDates(y)
  out <- trading.days[trading.days < Date]
  if (length(out) >= n) {
    first(last(out, n))
  } else { 
    prev.year.td <- TradingDates(y - 1)
    max.n <- length(out) + length(prev.year.td)
    if (n > max.n) stop("'n' is too large. Try something less than 252.")
    new.n <- n - length(out) # we need this many trading days from previous year
    # if it's the 1st trading day of the year, return the last trading date of
    # previous year
    first(last(TradingDates(y - 1), new.n))
  } 
}

#' @export
#' @rdname PrevTradingDate
NextTradingDate <- function(Date=Sys.Date(), n=1) {
  stopifnot(require(xts)) #remove this line when this is added to a package that Imports xts (needed for first/last)
  D <- as.Date(Date)
  y <- as.numeric(format(D, "%Y"))
  trading.days <- TradingDates(y)
  out <- trading.days[trading.days > Date]
  if (length(out) >= n) {
    last(first(out, n))
  } else { 
    next.year.td <- TradingDates(y + 1)
    max.n <- length(out) + length(next.year.td)
    new.n <- n - length(out) # how many trading days we need from next year
    if (n > max.n) stop("'n' is too large. Try something less than 252.")
    # if it's the last trading day of the year, return the first trading date of
    # next year
    last(first(TradingDates(y + 1), new.n))
  }
}	
