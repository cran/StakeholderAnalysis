\name{ImpactAnalysis}
\alias{ImpactAnalysis}
\title{ImpactAnalysis}
\description{Calculates, based on the Leontief model, qS or the reduction in stakeholder ineffectiveness}
\usage{ImpactAnalysis(data, BenefCost, NoStakeholders, NameStakeholders)}
\arguments{
  \item{data}{data gathered from a questionnaire employing a five-point Likert scale. The csv file is preferable due to the volume of data}
  \item{BenefCost}{the benefit indicator and the cost indicator. The $BenefCostInd from the BenefCost function should be used}
  \item{NoStakeholders}{the number of stakeholder groups (from the PrelCalc function)}
  \item{NameStakeholders}{the names of stakeholder groups (from the PrelCalc function)}
}
\details{The function calculates, based on the Leontief model, qS or the reduction in stakeholder ineffectiveness and then determines the stakeholder influence (SI) indicator, as described by Hester and Adams (2013). In addition, it calculates the indicator of benefits and the indicator of costs, factoring in the Leontief coefficient matrix}
\value{
    \item{Leontief}{the first two columns show the indicator of benefits and the indicator of costs. The middle column indicates qS. The two final columns show the indicator of benefits and the indicator of costs, factoring in the Leontief coefficient matrix}
    \item{MyImpact,OnMeImpact,MeanImpact}{matrices of impact-based stakeholder relationships}
}
\references{Hester and Adams (2013)}
\author{Sebastian Susmarski, Lech Kujawski, Anna Zamojska, Piotr Zientara}
\examples{
# first import DataExp
data(DataExp)
# then execute PrelCalc(), BenefCost()
PrelCalcExp=PrelCalc(data=DataExp, NoAtt=c(2,11,13,15),NoPow=c(3,8,14,16),
NoUrg=c(4,6,10,12),NoLeg=c(5,7,9,17),NoBen=18:22,NoCos=23:27)
BenefCostExp=BenefCost(CountResponses=PrelCalcExp$CountResponses)
# ImpactAnalysis()
ImpactAnalysisExp=ImpactAnalysis(data=DataExp, BenefCost=BenefCostExp$BenefCostInd,
NoStakeholders=PrelCalcExp$NoStakeholders, NameStakeholders=PrelCalcExp$NameStakeholders)
ImpactAnalysisExp
}
