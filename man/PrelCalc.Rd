\name{PrelCalc}
\alias{PrelCalc}
\title{PrelCalc}
\description{Performs preliminary calculations on raw data. Counts responses to items measuring stakeholder attributes as well as benefits and costs from a questionnaire employing a five-point Likert scale}
\usage{PrelCalc(data, NoAtt, NoPow, NoUrg, NoLeg, NoBen, NoCos)}
\arguments{
  \item{data}{data gathered from a questionnaire employing a five-point Likert scale. The csv file is preferable due to the volume of data}
  \item{NoAtt}{indicates col numbers related to the Attitude construct}
  \item{NoPow}{indicates col numbers related to the Power construct}
  \item{NoUrg}{indicates col numbers related to the Urgency construct}
  \item{NoLeg}{indicates col numbers related to the Legitimacy construct}
  \item{NoBen}{indicates col numbers related to the Benefits construct}
  \item{NoCos}{indicates col numbers related to the Costs construct}
}
\details{Data are collected by means of a questionnaire survey with a five-point Likert scale. PrelCalc performs preliminary calculations on raw data, counting responses to items measuring all the constructs (Attitude, Power, Urgency, Legitimacy, Benefits, Costs). These denote stakeholder attributes and benefits/costs. In addition, it identifies particular stakeholder groups (based on their names)}
\value{
   \item{CountResponses}{30 x number of stakeholder groups matrix of counted responses related to all the constructs}
   \item{NoStakeholders}{the number of stakeholder groups}
   \item{NameStakeholders}{the names of stakeholder groups}
   \item{NoAttrib}{$Att, $Pow, $Urg, $Leg, $Ben, $Cos}
   \item{NoAttrib$Att}{numbers in the raw data set related to the Attitude construct}
   \item{NoAttrib$Pow}{col numbers in the raw data set related to the Power construct}
   \item{NoAttrib$Urg}{col numbers in the raw data set related to the Urgency construct}
   \item{NoAttrib$Leg}{col numbers in the raw data set related to the Legitimacy construct}
   \item{NoAttrib$Ben}{col numbers in the raw data set related to the Benefits construct}
   \item{NoAttrib$Cos}{col numbers in the raw data set related to the Costs construct}
}
\author{Lech Kujawski, Sebastian Susmarski, Anna Zamojska, Piotr Zientara}
\examples{
# first import DataExp
data(DataExp)
# PrelCalc()
PrelCalcExp=PrelCalc(data=DataExp, NoAtt=c(2,11,13,15),NoPow=c(3,8,14,16),
NoUrg=c(4,6,10,12),NoLeg=c(5,7,9,17),NoBen=18:22,NoCos=23:27)
PrelCalcExp
}
