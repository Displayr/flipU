% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilites.R
\name{RequireQuestionType}
\alias{RequireQuestionType}
\title{Check that variables supplied belong to a certain question type.}
\usage{
RequireQuestionType(variables, required.type, message.prefix, message.suffix)
}
\arguments{
\item{variables}{A list containing the variables (or questions) to check.}

\item{required.type}{A string indicating the required questiontype value.
Should be one of: "PickAny", "PickOne", "PickOneMulti", "Number",
"NumberMulti", "NumberGrid", "PickAnyGrid", "Text", "TextMulti",
"Ranking", or "Experiment".}

\item{message.prefix}{Text to display in the error message before the
required question type / structure.}

\item{message.suffix}{Text to display at the end of the message.}
}
\description{
Helper function to use in Standard R items which require the user
to select variables of a certain question type / variable set
structure.
}
