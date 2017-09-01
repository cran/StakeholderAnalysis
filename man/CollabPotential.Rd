\name{CollabPotential}
\alias{CollabPotential}
\title{CollabPotential}
\description{Determines the potential of particular stakeholder groups for collaboration}
\usage{CollabPotential(AttribIdent)}
\arguments{
  \item{AttribIdent}{Identifies stakeholder attributes. The $Mean or $Fra from the AttribIdent function should be used}
}
\details{Based on responses to items measuring Power, Legitimacy and Urgency, the function determines the potential of particular stakeholder groups for collaboration ("high" and "low")}
\value{
    \item{Mean}{the potential for collaboration determined on the basis of the mean value}
    \item{Fra}{the potential for collaboration determined on the basis of the fractions of responses}
}
\author{Sebastian Susmarski, Lech Kujawski, Anna Zamojska, Piotr Zientara}
\examples{
# first import DataExp
data(DataExp)
# then execute PrelCalc(), RespVerif(), AttibIdent()
PrelCalcExp=PrelCalc(data=DataExp, NoAtt=c(2,11,13,15),NoPow=c(3,8,14,16),
NoUrg=c(4,6,10,12),NoLeg=c(5,7,9,17),NoBen=18:22,NoCos=23:27)
RespVerifExp=RespVerif(CountResponses=PrelCalcExp$CountResponses,
NoStakeholders=PrelCalcExp$NoStakeholders)
AttribIdentExp=AttribIdent(TestedResponses=RespVerifExp, NoAttrib=PrelCalcExp$NoAttrib,
NoStakeholders=PrelCalcExp$NoStakeholders, NameStakeholders=PrelCalcExp$NameStakeholders)
# CollabPotential
CollabPotentialExp=CollabPotential(AttribIdent=AttribIdentExp)
CollabPotentialExp
}
