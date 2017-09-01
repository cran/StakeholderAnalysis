\name{AttribIdent}
\alias{AttribIdent}
\title{AttribIdent}
\description{Identifies stakeholder attributes as well as benefits and costs}
\usage{AttribIdent(TestedResponses, NoAttrib, NoStakeholders, NameStakeholders)}
\arguments{
  \item{TestedResponses}{the result of the RespVerif function}
  \item{NoAttrib}{col numbers in the raw data set related to particular constructs. The $NoAttrib from the PreCalc function should be used}
  \item{NoStakeholders}{the number of stakeholder groups. The $NoStakeholders from the PreCalc function should be used}
  \item{NameStakeholders}{the names of stakeholder groups. The $NameStakeholders from the PreCalc function should be used}
}
\details{Based on previously performed tests of means and fractions (see the RespVerif function), the function determines whether a particular attribute is statistically significant or not (.<05)}
\value{
  \item{Mean}{(the number of stakeholder groups) x 6 matrix. In each row, the "+" sign indicates that an attribute is statistically significant, while the "-" sign indicates that an attribute is not statistically significant (based on the mean test). The "0" sign shows that the response is neutral, meaning that respondents do not know whether they possess a particular attribute or not}
  \item{Fra}{(the number of stakeholder groups) x 6 matrix. In each row, the "+" sign indicates that an attribute is statistically significant, while the "-" sign indicates that an attribute is not statistically significant (based on the fraction test). The "0" sign shows that the response is neutral, meaning that respondents do not know whether they possess a particular attribute or not}
}
\author{Sebastian Susmarski, Lech Kujawski, Anna Zamojska, Piotr Zientara}
\examples{
# first import DataExp
data(DataExp)
# then execute PrelCalc(), RespVerif()
PrelCalcExp=PrelCalc(data=DataExp, NoAtt=c(2,11,13,15),NoPow=c(3,8,14,16),
NoUrg=c(4,6,10,12),NoLeg=c(5,7,9,17),NoBen=18:22,NoCos=23:27)
RespVerifExp=RespVerif(CountResponses=PrelCalcExp$CountResponses,
NoStakeholders=PrelCalcExp$NoStakeholders)
# AttribIdent()
AttribIdentExp=AttribIdent(TestedResponses=RespVerifExp, NoAttrib=PrelCalcExp$NoAttrib,
NoStakeholders=PrelCalcExp$NoStakeholders, NameStakeholders=PrelCalcExp$NameStakeholders)
AttribIdentExp
}
