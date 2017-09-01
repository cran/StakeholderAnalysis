\name{Histograms}
\alias{Histograms}
\title{Histograms}
\description{Draws histograms of responses}
\usage{Histograms(path, tofile, CountResponses)}
\arguments{
  \item{path}{a path of a particular catalogue in which pictures are saved, set path="" when tofile=0}
  \item{tofile}{logical. 1=save-to-file. 0=show-on-screen}
  \item{CountResponses}{the result of the PrelCalc function}
}
\details{The function draws histograms of responses}
\value{
    \item{Histograms}{Histograms of responses}
}
\author{Sebastian Susmarski, Lech Kujawski, Anna Zamojska, Piort Zientar}
\examples{
# first import DataExp
data(DataExp)
# then execute PrelCalc()
PrelCalcExp=PrelCalc(data=DataExp, NoAtt=c(2,11,13,15),NoPow=c(3,8,14,16),
NoUrg=c(4,6,10,12),NoLeg=c(5,7,9,17),NoBen=18:22,NoCos=23:27)
# Histograms()
Histograms(path="",tofile=0,CountResponses=PrelCalcExp$CountResponses)
}
