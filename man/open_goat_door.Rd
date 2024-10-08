% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg.R
\name{open_goat_door}
\alias{open_goat_door}
\title{Open a GOAT door}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{`game`}{\itemize{
\item A vector of three containing two insistence of "goat" and one instance of "car".
\code{a.pick} - A number representing the contestants pick.
}}
}
\value{
A number representing the door the host picked.
}
\description{
\code{open_goat_door} randomly chooses a door that contains a goat, but is also not your selected door.
}
\details{
This is similar to Lets Make a Deal when after the contestant chooses their selected door the host then opens a non winning door.
}
\examples{
open_goat_door(new.game, my.initial.pick)
}
