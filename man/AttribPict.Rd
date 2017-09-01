\name{AttribPict}
\alias{AttribPict}
\title{AttribPict}
\description{Draws a picture of stakeholder attributes in the form of three overlapping circles}
\usage{AttribPict(path, tofile, AttribIdent, CollabPotential)}
\arguments{
  \item{path}{a path of a particular catalogue in which pictures are saved, set path="" when tofile=0}
  \item{tofile}{logical. 1=save-to-file. 0=show-on-screen}
  \item{AttribIdent}{stakeholder attributes. The $Mean or the $Fra from the AttribIdent function should be used}
  \item{CollabPotential}{potential for collaboration. The $Mean or the $Fra from the CollabPotential function should be used}
}
\details{The function draws a picture of stakeholder attributes in the form of three overlapping circles in different colours}
\value{
   \item{drow}{A drow of stakeholder attributes}
}
\author{Sebastian Susmarski, Lech Kujawski, Anna Zamojska, Piotr Zientara}
\examples{
# first import DataExp
data(DataExp)
# then execute PrelCalc(), RespVerif(), AttibIdent(), CollabPotential()
PrelCalcExp=PrelCalc(data=DataExp, NoAtt=c(2,11,13,15),NoPow=c(3,8,14,16),
NoUrg=c(4,6,10,12),NoLeg=c(5,7,9,17),NoBen=18:22,NoCos=23:27)
RespVerifExp=RespVerif(CountResponses=PrelCalcExp$CountResponses,
NoStakeholders=PrelCalcExp$NoStakeholders)
AttribIdentExp=AttribIdent(TestedResponses=RespVerifExp, NoAttrib=PrelCalcExp$NoAttrib,
NoStakeholders=PrelCalcExp$NoStakeholders, NameStakeholders=PrelCalcExp$NameStakeholders)
CollabPotentialExp=CollabPotential(AttribIdent=AttribIdentExp)
# AttribPict()
AttribPict(path="",tofile=0,AttribIdent=AttribIdentExp$Mean,CollabPotential=CollabPotentialExp$Mean)
}
