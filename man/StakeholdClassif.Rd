\name{StakeholdClassif}
\alias{StakeholdClassif}
\title{StakeholdClassif}
\description{Classifies stakeholder groups and suggests communication strategies}
\usage{StakeholdClassif(BenefCostTest, CollabPotential, AttribIdent)}
\arguments{
  \item{BenefCostTest}{the result of a Student's t-test (with H0 stating that the mean of benefits and the mean of costs are equal). The $BenefCostTest from the BenefCost function should be used}
  \item{CollabPotential}{the potential for collaboration. The $Mean or the $Fra from the CollabPotential function should be used}
  \item{AttribIdent}{identified stakeholder attributes. The $Mean or the $Fra from the AttribIdent function should be used}
}
\details{The function first classifies stakeholder groups into categories, as described by Mitchell, Agle and Wood (1997). It then determines their attitudes ("supportive", "non-supportive", "mixed", "neutral", "insignificant") and, with the potential for collaboration taken into account, suggests a communication strategy vis-a-vis a particular stakeholder group}
\value{The number of stakeholder groups x 3 data frame. The first column indicates stakeholder classification. The second column shows stakeholder attitudes. The third column suggests a communication strategy}
\references{Mitchell, Agle and Wood (1997)}
\author{Sebastian Susmarski, Lech Kujawski, Anna Zamojska, Piotr Zientara}
\examples{
# first import DataExp
data(DataExp)
# then execute PrelCalc(), RespVerif(), AttibIdent(), BenefCost()
PrelCalcExp=PrelCalc(data=DataExp, NoAtt=c(2,11,13,15),NoPow=c(3,8,14,16),
NoUrg=c(4,6,10,12),NoLeg=c(5,7,9,17),NoBen=18:22,NoCos=23:27)
RespVerifExp=RespVerif(CountResponses=PrelCalcExp$CountResponses,
NoStakeholders=PrelCalcExp$NoStakeholders)
AttribIdentExp=AttribIdent(TestedResponses=RespVerifExp,
NoAttrib=PrelCalcExp$NoAttrib, NoStakeholders=PrelCalcExp$NoStakeholders,
NameStakeholders=PrelCalcExp$NameStakeholders)
CollabPotentialExp=CollabPotential(AttribIdent=AttribIdentExp)
BenefCostExp=BenefCost(CountResponses=PrelCalcExp$CountResponses)
# StakeholdClassif()
StakeholdClassifByMean=StakeholdClassif(BenefCostTest=BenefCostExp$BenefCostTest,
CollabPotential=CollabPotentialExp$Mean,AttribIdent=AttribIdentExp$Mean)
StakeholdClassifByFraction=StakeholdClassif(BenefCostTest=BenefCostExp$BenefCostTest,
CollabPotential=CollabPotentialExp$Fra,AttribIdent=AttribIdentExp$Fra)
StakeholdClassifByMean
StakeholdClassifByFraction
}
