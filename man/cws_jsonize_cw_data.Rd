% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cws_jsonize_cw_data.R
\name{cws_jsonize_cw_data}
\alias{cws_jsonize_cw_data}
\title{Helper: transform cw data to JSON for Crossword.js}
\usage{
cws_jsonize_cw_data(cw)
}
\arguments{
\item{cw}{A crossword object (see \code{crossword::Crossword$new()})}
}
\value{
JSON string of clue, answer, positions ready to use with Crossword.js
}
\description{
Helper: transform cw data to JSON for Crossword.js
}
