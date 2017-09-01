\name{RespVerif}
\alias{RespVerif}
\title{RespVefif}
\description{Performs tests of statistical significance of the means and fractions of responses}
\usage{RespVerif(CountResponses, NoStakeholders)}
\arguments{
  \item{CountResponses}{the $CountResponses result of the PreCalc function (i.e., 30 x number of stakeholder groups matrix of counted responses related to all the constructs)}
  \item{NoStakeholders}{the number of stakeholder groups (i.e., the $NoStakeholders result of the PreCalc function)}
}
\details{The function performs two tests of statistical significance: (1) the means and (2) the fractions of responses. As regards (1), H0 states that the mean of responses to a particular item is equal to "3" (i.e., a neutral response on a five-point Likert scale); H1 states that the mean is not equal to "3" (i.e., a two-sides alternative hypothesis). As regards (2), H0 states that the fraction of the "1" and "2" responses is equal to the fraction of the "4" and "5" responses; H1 states that the fraction of the "1" and "2" responses is not equal to the fraction of the "4" and "5" responses (i.e., a two-sides alternative hypothesis)}
\value{
   \item{Mean}{(3 x the number of stakeholder groups) x 6 matrix. The first row indicates the mean response. The second row reports stat value. The third row indicates prob. The same pattern applies to each stakeholder group}
   \item{Fra}{(3 x the number of stakeholder groups) x (3 x 6) matrix. The first row indicates the number of negative ("1" and "2"), neutral ("3") and positive ("4" and "5") responses. The second row reports fractions of responses. The third row reports stat/prob}
}
\author{Sebastian Susmarski, Lech Kujawski, Anna Zamojska, Piotr Zientara}
\examples{
# first import DataExp
data(DataExp)
# then execute PrelCalc()
PrelCalcExp=PrelCalc(data=DataExp, NoAtt=c(2,11,13,15),NoPow=c(3,8,14,16),
NoUrg=c(4,6,10,12),NoLeg=c(5,7,9,17),NoBen=18:22,NoCos=23:27)
# RespVerif
RespVerifExp=RespVerif(CountResponses=PrelCalcExp$CountResponses,
NoStakeholders=PrelCalcExp$NoStakeholders)
RespVerifExp
}
