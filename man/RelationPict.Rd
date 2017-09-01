\name{RelationPict}
\alias{RelationPict}
\title{RelationPict}
\description{Draws a picture of stakeholder relationships}
\usage{RelationPict(path, tofile, MeanImpact, StakeholdClassif)}
\arguments{
  \item{path}{a path of a particular catalogue in which pictures are saved, set path="" when tofile=0}
  \item{tofile}{logical. 1=save-to-file. 0=show-on-screen}
  \item{MeanImpact}{the Leontief coefficient matrix. The $MeanImpact from the ImpactAnalysis function should be used}
  \item{StakeholdClassif}{the result of the StakeholdClassif function}
}
\details{The function draws a picture of stakeholder relationships with arrows and circles in different colours}
\value{A picture of stakeholder relationships}
\author{Sebastian Susmarski, Lech Kujawski, Anna Zamojska, Piotr Zientar}
\examples{
# first import DataExp
data(DataExp)
# then execute PrelCalc(), RespVerif(), AttribIdent(), CollabPotential()
# BenefCost(), StakeholdClassif(), ImpactAnalysis()
PrelCalcExp=PrelCalc(data=DataExp, NoAtt=c(2,11,13,15),NoPow=c(3,8,14,16),
NoUrg=c(4,6,10,12),NoLeg=c(5,7,9,17),NoBen=18:22,NoCos=23:27)
RespVerifExp=RespVerif(CountResponses=PrelCalcExp$CountResponses,
NoStakeholders=PrelCalcExp$NoStakeholders)
AttribIdentExp=AttribIdent(TestedResponses=RespVerifExp,
NoAttrib=PrelCalcExp$NoAttrib, NoStakeholders=PrelCalcExp$NoStakeholders,
NameStakeholders=PrelCalcExp$NameStakeholders)
CollabPotentialExp=CollabPotential(AttribIdent=AttribIdentExp)
BenefCostExp=BenefCost(CountResponses=PrelCalcExp$CountResponses)
StakeholdClassifByMean=StakeholdClassif(BenefCostTest=BenefCostExp$BenefCostTest,
CollabPotential=CollabPotentialExp$Mean,AttribIdent=AttribIdentExp$Mean)
ImpactAnalysisExp=ImpactAnalysis(data=DataExp, BenefCost=BenefCostExp$BenefCostInd,
NoStakeholders=PrelCalcExp$NoStakeholders,
NameStakeholders=PrelCalcExp$NameStakeholders)
# RelationPict()
RelationPict(path="",tofile=0,MeanImpact=ImpactAnalysisExp$MeanImpact,
StakeholdClassif=StakeholdClassifByMean)
}
