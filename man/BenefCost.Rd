\name{BenefCost}
\alias{BenefCost}
\title{BenefCost}
\description{Calculates the benefit indicator (normalized on the 0-1 scale) and the cost indicator (normalized on the 0-1 scale), and performs a Student's t-test (with H0 stating that the mean of benefits and the mean of costs are equal)}
\usage{BenefCost(CountResponses)}
\arguments{
  \item{CountResponses}{the number of stakeholder groups x 30 matrix comprising counted responses to particular items. The $CountResponses from the PreCalc function should be used}
}
\details{Based on responses to relevant items, the function calculates the benefit indicator (normalized on the 0-1 scale) and the cost indicator (normalized on the 0-1 scale). Subsequently, it performs a Student's t-test (with H0 stating that the mean of benefits and the mean of costs are equal) with a view to ascertaining whether benefits are greater than costs (which is indicated by the "+" sign) or vice versa (which is indicated by the "-" sign)}
\value{
    \item{BenefCostInd}{the benefit indicator and the cost indicator}
    \item{BenefCostTest}{the results of Student's t-tests}
}
\author{Sebastian Susmarski, Lech Kujawski, Anna Zamojska, Piotr Zientara}
\examples{
# first import DataExp
data(DataExp)
# then execute PrelCalc()
PrelCalcExp=PrelCalc(data=DataExp, NoAtt=c(2,11,13,15),NoPow=c(3,8,14,16),
NoUrg=c(4,6,10,12),NoLeg=c(5,7,9,17),NoBen=18:22,NoCos=23:27)
# BenefCost()
BenefCostExp=BenefCost(CountResponses=PrelCalcExp$CountResponses)
BenefCostExp
}
