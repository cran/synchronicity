% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutexes.R
\name{is.timed}
\alias{is.timed}
\alias{timeout}
\alias{is.timed,boost.mutex-method}
\alias{timeout,boost.mutex-method}
\title{Timeout operations for boost.mutex objects}
\usage{
is.timed(m)

\S4method{is.timed}{boost.mutex}(m)
}
\arguments{
\item{m}{a \code{boost.mutex} object to get timeout information for.}
}
\value{
\code{is.timed} returns \code{TRUE} if the object has a timeout and 
\code{FALSE} otherwise.  If a timeout has been set \code{timeout} returns 
the number of seconds a \code{boost.mutex} object will attempt to acquire 
a lock and \code{NULL} otherwise.
}
\description{
The \code{is.timed} function tells if a \code{boost.mutex} 
object has a timeout.  The \code{timeout} function tells how long a mutex 
will wait for a timeout.
}
\examples{
x = boost.mutex(timeout=5)
y = boost.mutex()
print(is.timed(x))
print(is.timed(y))
print(timeout(x))
print(timeout(y))
}
