PrelCalc=function(data,NoAtt,NoPow,NoUrg,NoLeg,NoBen,NoCos)
{ # pocz RawData
dane=data

Inter=factor(dane[,1])
# liczba typow interesariuszy
LInter=length(levels(Inter))
ListaInter=matrix(levels(Inter),LInter,1)

# identyfikacja typow atrybutow (kolumn)
# tu nalezy podac, ktore numery kolumn z obiekcie dane odpowiadaja ktorym atrybutom
# attitude, power, urgency, legitimacy, profits, costs
#NrAtryb=list(At=c(2,11,13,15), Po=c(3,8,14,16), Ur=c(4,6,10,12), Le=c(5,7,9,17), Pr=18:22, Co=23:27)
NrAtryb=list(Att=NoAtt, Pow=NoPow, Urg=NoUrg, Leg=NoLeg, Ben=NoBen, Cos=NoCos)

NazwyAtrybutow=NULL
for (i in 1:length(names(NrAtryb)))
{ # pocz i
NazwyAtrybutow=c(NazwyAtrybutow, paste(names(NrAtryb)[i], 1:5, sep=""))
} # kon i

# idntyfikacja odpowiedzi 1,2,3,4,5
Odpowiedzi=matrix(0, LInter, length(NrAtryb)*5)
Odpowiedzi=data.frame(Odpowiedzi, row.names=levels(Inter))
names(Odpowiedzi)=NazwyAtrybutow
#Odpowiedzi
for (i in 1:LInter)
{ # pocz i
# i=1
ind01=(Inter==levels(Inter)[i])
dane_pom=dane[ind01,]                      # dane dla wybranego (itego) interesariusza
dane_pom01=dane_pom[,unlist(NrAtryb[1])]   # dane atrybut1
dane_pom02=dane_pom[,unlist(NrAtryb[2])]   # dane atrybut2
dane_pom03=dane_pom[,unlist(NrAtryb[3])]   # dane atrybut3
dane_pom04=dane_pom[,unlist(NrAtryb[4])]   # dane atrybut4
dane_pom05=dane_pom[,unlist(NrAtryb[5])]   # dane profits
dane_pom06=dane_pom[,unlist(NrAtryb[6])]   # dane costs
# zmiana ksztaltu macierzy
dane_pom01=as.matrix(dane_pom01)
dane_pom01=matrix(dane_pom01, ncol(dane_pom01)*nrow(dane_pom01),1)
dane_pom02=as.matrix(dane_pom02)
dane_pom02=matrix(dane_pom02, ncol(dane_pom02)*nrow(dane_pom02),1)
dane_pom03=as.matrix(dane_pom03)
dane_pom03=matrix(dane_pom03, ncol(dane_pom03)*nrow(dane_pom03),1)
dane_pom04=as.matrix(dane_pom04)
dane_pom04=matrix(dane_pom04, ncol(dane_pom04)*nrow(dane_pom04),1)
dane_pom05=as.matrix(dane_pom05)
dane_pom05=matrix(dane_pom05, ncol(dane_pom05)*nrow(dane_pom05),1)
dane_pom06=as.matrix(dane_pom06)
dane_pom06=matrix(dane_pom06, ncol(dane_pom06)*nrow(dane_pom06),1)
przedz01=cut(x=dane_pom01, include.lowest=TRUE, breaks=c(0.5,1.5,2.5,3.5,4.5,5.5))
przedz02=cut(x=dane_pom02, include.lowest=TRUE, breaks=c(0.5,1.5,2.5,3.5,4.5,5.5))
przedz03=cut(x=dane_pom03, include.lowest=TRUE, breaks=c(0.5,1.5,2.5,3.5,4.5,5.5))
przedz04=cut(x=dane_pom04, include.lowest=TRUE, breaks=c(0.5,1.5,2.5,3.5,4.5,5.5))
przedz05=cut(x=dane_pom05, include.lowest=TRUE, breaks=c(0.5,1.5,2.5,3.5,4.5,5.5))
przedz06=cut(x=dane_pom06, include.lowest=TRUE, breaks=c(0.5,1.5,2.5,3.5,4.5,5.5))
Odpowiedzi[i,]=c(table(przedz01), table(przedz02), table(przedz03), table(przedz04),table(przedz05),table(przedz06))
} # kon i
RawData=list(CountResponses=Odpowiedzi, NoStakeholders=LInter, NameStakeholders=ListaInter, NoAttrib=NrAtryb)
} # kon PrelCalc


RespVerif=function(CountResponses, NoStakeholders)
{ # pocz TestedAnswers
# idntyfikacja odpowiedzi negatywnych, neutralnych, pozytywnych
# metoda zliczeniowa, identyfikacja po frakcji odpowiedzi
Odpowiedzi=CountResponses
LInter=NoStakeholders

Odpowiedzi2=matrix(0, nrow(Odpowiedzi)*3, ncol(Odpowiedzi)/5*3)
NazwyWierszy2=NULL
for (i in 1:LInter)
{ # pocz i
NazwyWierszy2=c(NazwyWierszy2, paste(row.names(Odpowiedzi)[i], c("No", "%", "St/pr")))
} # kon i
NazwyAtrybPom=names(Odpowiedzi)
NazwyAtrybPom=NazwyAtrybPom[seq(from=1, by=5, len=length(NazwyAtrybPom)/5)]
NazwyAtrybPom=substr(x=NazwyAtrybPom, start=1, stop=3)
NazwyAtrybutow2=NULL
for (i in 1:length(NazwyAtrybPom))
{ # pocz i
NazwyAtrybutow2=c(NazwyAtrybutow2, paste(NazwyAtrybPom[i], c("Neg","Neu","Poz"), sep=" "))
} # kon i
Odpowiedzi2=data.frame(Odpowiedzi2, row.names=NazwyWierszy2)
names(Odpowiedzi2)=NazwyAtrybutow2
#Odpowiedzi2

for (i in 1:nrow(Odpowiedzi))
{ # pocz i
#i=1
NoRowsO2=seq(from=1, by=3, len=nrow(Odpowiedzi))
odO1=1
doO1=odO1+4
odO2=1
RowO2=NoRowsO2[i]
OdpPom01=Odpowiedzi[i,]
for (j in 1:length(NazwyAtrybPom))
{ # pocz j
#j=1
OdpPom02=OdpPom01[odO1:doO1]
Neg=OdpPom02[1]+OdpPom02[2]
Neut=OdpPom02[3]
Poz=OdpPom02[4]+OdpPom02[5]
ProcNeg=Neg/sum(OdpPom02)
ProcNeut=Neut/sum(OdpPom02)
ProcPoz=Poz/sum(OdpPom02)
# liczenie statystyki Z (str 167 Balicki Makac Metody Wnioskowania)
n1=Neg+Neut+Poz
n2=Neg+Neut+Poz
pp=(Neg+Poz)/(n1+n2)   # wzor 7.35, w mianowniku razy 2, bo kazda z dwoch licznosc populacji (n1 i n2) to Neg+Neut+Poz
ZZ=abs(ProcNeg-ProcPoz)/sqrt(pp*(1-pp)*((n1+n2)/(n1*n2)))
prob=(1-pnorm(q=as.numeric(ZZ), mean=0, sd=1))*2
# zapamietywanie wynikow
Odpowiedzi2[RowO2,odO2]=as.character(round(Neg,0))
Odpowiedzi2[RowO2,(odO2+1)]=as.character(round(Neut,0))
Odpowiedzi2[RowO2,(odO2+2)]=as.character(round(Poz,0))
Odpowiedzi2[(RowO2+1),odO2]=as.character(round(ProcNeg,2))
Odpowiedzi2[(RowO2+1),(odO2+1)]=as.character(round(ProcNeut,2))
Odpowiedzi2[(RowO2+1),(odO2+2)]=as.character(round(ProcPoz,2))

ZZchar=as.character(round(ZZ,2))
prob2=format(x=prob,nsmall=3,scientific=FALSE)
probChar=substr(prob2,1,5)
Odpowiedzi2[(RowO2+2),odO2]=""
Odpowiedzi2[(RowO2+2),(odO2+1)]=paste(ZZchar, probChar, sep="/")
Odpowiedzi2[(RowO2+2),(odO2+2)]=""

odO1=doO1+1
doO1=odO1+4
odO2=odO2+3

} # kon j
} # kon i
#Odpowiedzi2

# idntyfikacja odpowiedzi negatywnych, neutralnych, pozytywnych
# identyfikacja po sredniej arytmetycznej
Odpowiedzi2SR=matrix(0, nrow(Odpowiedzi)*3, ncol(Odpowiedzi)/5)
NazwyWierszy2=NULL
for (i in 1:LInter)
{ # pocz i
NazwyWierszy2=c(NazwyWierszy2, paste(row.names(Odpowiedzi)[i], c("Mean", "stat", "prob")))
} # kon i
NazwyAtrybPom=names(Odpowiedzi)
NazwyAtrybPom=NazwyAtrybPom[seq(from=1, by=5, len=length(NazwyAtrybPom)/5)]
NazwyAtrybPom=substr(x=NazwyAtrybPom, start=1, stop=3)
NazwyAtrybutow2SR=NazwyAtrybPom
Odpowiedzi2SR=data.frame(Odpowiedzi2SR, row.names=NazwyWierszy2)
names(Odpowiedzi2SR)=NazwyAtrybutow2SR
#Odpowiedzi2SR

for (i in 1:nrow(Odpowiedzi))
{ # pocz i
#i=2
NoRowsO2=seq(from=1, by=3, len=nrow(Odpowiedzi))
RowO2=NoRowsO2[i]
od=1
do=od+4
OdpPom01=Odpowiedzi[i,]
for (j in 1:length(NazwyAtrybPom))
{ # pocz j
#j=2
OdpPom02=OdpPom01[od:do]
SRPom01=as.matrix(OdpPom02)%*%matrix(c(1,2,3,4,5),5,1)/sum(OdpPom02)  # srednia arytmetyczna odpowiedzi
# liczenie statystyki Z (str 145 Balicki Makac Metody Wnioskowania)
# obliczenie wariancji DX=E(X^2)-(EX)^2
EX2=as.matrix(OdpPom02)%*%matrix(c(1,2,3,4,5),5,1)^2/sum(OdpPom02)
S2=EX2-SRPom01^2
ZZ=abs(SRPom01-3)/(S2^0.5)*(sum(OdpPom02))^0.5
ZZ2=format(x=ZZ,nsmall=3,scientific=FALSE)
prob=(1-pnorm(q=as.numeric(ZZ), mean=0, sd=1))*2
prob2=format(x=prob,nsmall=3,scientific=FALSE)
# zapamietywanie wynikow
SRPom02=format(SRPom01,nsmall=3,scientific=FALSE)
Odpowiedzi2SR[RowO2,j]=substr(SRPom02,1,5)
Odpowiedzi2SR[(RowO2+1),j]=substr(ZZ2,1,5)
Odpowiedzi2SR[(RowO2+2),j]=substr(prob2,1,5)
od=do+1
do=od+4
} # kon j
} # kon i
#Odpowiedzi2SR
TestedResponses=list(Mean=Odpowiedzi2SR, Fra=Odpowiedzi2)
} # RespVerif

AttribIdent=function(TestedResponses, NoAttrib, NoStakeholders, NameStakeholders)
{ # pocz AtribIdent
Odpowiedzi2=TestedResponses$Fra
Odpowiedzi2SR=TestedResponses$Mean
NrAtryb=NoAttrib
LInter=NoStakeholders
ListaInter=NameStakeholders

Odpowiedzi3=matrix(NA, LInter, length(NrAtryb))
Odpowiedzi3=data.frame(Odpowiedzi3, row.names=ListaInter)
NazwyAtrybutow3=NULL
for (i in 1:length(names(NrAtryb)))
{ # pocz i
NazwyAtrybutow3=c(NazwyAtrybutow3, names(NrAtryb)[i])
} # kon i
names(Odpowiedzi3)=NazwyAtrybutow3
#Odpowiedzi3

#i=1
#j=1
odw1=1    # wiersz od 1 w obiekcie Odpowiedzi2
odw2=2    # wiersz od 2 w obiekcie Odpowiedzi2
odw3=3    # wiersz od 3 w obiekcie Odpowiedzi2
for (i in 1:nrow(Odpowiedzi3))
{ # pocz i
odk1=1    # kolumna od 1 w obiekcie Odpowiedzi2
odk2=2    # kolumna od 2 w obiekcie Odpowiedzi2
odk3=3    # kolumna od 3 w obiekcie Odpowiedzi2
for (j in 1:ncol(Odpowiedzi3))
{ # pocz j
PlusMinus="N"
if (as.numeric(Odpowiedzi2[odw2,odk1])<as.numeric(Odpowiedzi2[odw2,odk3])) PlusMinus="P"    # pozytywne
if (as.numeric(Odpowiedzi2[odw2,odk1])>as.numeric(Odpowiedzi2[odw2,odk3])) PlusMinus="M"    # negatywne
if (as.numeric(Odpowiedzi2[odw2,odk1])==as.numeric(Odpowiedzi2[odw2,odk3])) PlusMinus="N"   # neutralne

StatIst="N"
StatIstPom=regexpr(pattern="/", text=Odpowiedzi2[odw3,odk2])
StatIstPom[1]
if (as.numeric(substr(x=Odpowiedzi2[odw3,odk2], start=(StatIstPom[1]+1), stop=10))<=0.1) StatIst="T"

if (PlusMinus=="P" & StatIst=="T") Odpowiedzi3[i,j]=paste(NazwyAtrybutow3[j],"(","+",")", sep="")
if (StatIst=="N") Odpowiedzi3[i,j]=paste(NazwyAtrybutow3[j],"(","0",")", sep="")
if (PlusMinus=="M" & StatIst=="T") Odpowiedzi3[i,j]=paste(NazwyAtrybutow3[j],"(","-",")", sep="")

odk1=odk1+3
odk2=odk2+3
odk3=odk3+3
#j=j+1
} # kon j

odw1=odw1+3
odw2=odw2+3
odw3=odw3+3
} # kon i
#Odpowiedzi3


# trzeci etap, po tescie atrybutow ich identyfikacja
# na podstawie srednich arytmetycznych
Odpowiedzi3SR=matrix(NA, LInter, length(NrAtryb))
Odpowiedzi3SR=data.frame(Odpowiedzi3SR, row.names=ListaInter)
NazwyAtrybutow3SR=NULL
for (i in 1:length(names(NrAtryb)))
{ # pocz i
NazwyAtrybutow3SR=c(NazwyAtrybutow3SR, names(NrAtryb)[i])
} # kon i
names(Odpowiedzi3SR)=NazwyAtrybutow3SR
#Odpowiedzi3SR
#i=1
#j=1
odw1=1    # wiersz od 1 w obiekcie Odpowiedzi2SR
odw2=2    # wiersz od 2 w obiekcie Odpowiedzi2SR
odw3=3    # wiersz od 3 w obiekcie Odpowiedzi2SR
for (i in 1:nrow(Odpowiedzi3SR))
{ # pocz i
for (j in 1:ncol(Odpowiedzi3SR))
{ # pocz j
PlusMinus="N"
if (as.numeric(Odpowiedzi2SR[odw1,j])<3) PlusMinus="M"  # negatywne
if (as.numeric(Odpowiedzi2SR[odw1,j])>3) PlusMinus="P"    # pozytywne
if (as.numeric(Odpowiedzi2SR[odw1,j])==3) PlusMinus="N"   # neutralne

StatIst="N"
if (as.numeric(Odpowiedzi2SR[odw3,j])<=0.1) StatIst="T"

if (PlusMinus=="P" & StatIst=="T") Odpowiedzi3SR[i,j]=paste(NazwyAtrybutow3SR[j],"(","+",")", sep="")
if (StatIst=="N") Odpowiedzi3SR[i,j]=paste(NazwyAtrybutow3SR[j],"(","0",")", sep="")
if (PlusMinus=="M" & StatIst=="T") Odpowiedzi3SR[i,j]=paste(NazwyAtrybutow3SR[j],"(","-",")", sep="")
} # kon j

odw1=odw1+3
odw2=odw2+3
odw3=odw3+3
} # kon i
#Odpowiedzi3SR
AtribIdent=list(Mean=Odpowiedzi3SR, Fra=Odpowiedzi3)
} # kon AttribIdent

BenefCost=function(CountResponses)
{ # pocz BenefCost
Odpowiedzi=CountResponses
# ustalenie gdzie w CountResponses sa odpowiedzi dla benef i cost
NamesOdpowiedzi=substr(names(Odpowiedzi),1,3)
pom01=(NamesOdpowiedzi=="Ben")
pom02=(NamesOdpowiedzi=="Cos")
LicznikDanychProf=NULL
LicznikDanychCost=NULL
for (i in 1:length(pom01))
{ # pocz i
if (pom01[i]) LicznikDanychProf=c(LicznikDanychProf,i)
if (pom02[i]) LicznikDanychCost=c(LicznikDanychCost,i)
} # kon i
LicznikDanychProfCost=c(LicznikDanychProf,LicznikDanychCost)

ProfCost=data.frame(matrix(NA,nrow(Odpowiedzi),2), row.names=row.names(Odpowiedzi))
names(ProfCost)=c("BenefInd","CostInd")
ProfCostDane=Odpowiedzi[,LicznikDanychProfCost]
for (i in 1:nrow(Odpowiedzi))
{ # pocz i
#i=1
Prof1=Odpowiedzi[i,LicznikDanychProf]
Cost1=Odpowiedzi[i,LicznikDanychCost]
# do korzysci
max1=sum(Prof1)
min1=0
# unitaryzacja zerowana
uz1=(max1-Prof1[1])/(max1-min1)  # negatywna, im blizsza 1 tym wieksze poparcie dla projektu
uz2=(max1-Prof1[2])/(max1-min1)  # negatywna
uz4=(Prof1[4]-min1)/(max1-min1)  # pozytywna, im blizsza 1 tym bardziej pozytywna, widac korzysci z inwestycji
uz5=(Prof1[5]-min1)/(max1-min1)  # pozytywna
wsk=(uz1+uz2+uz4+uz5)/4          # wskaznik, czyli srednia z wynikow unitaryzacji
wsk1=format(x=wsk,digits=3,nsmall=3,scientific=FALSE)
ProfCost[i,1]=wsk1
# do kosztow
max1=sum(Cost1)
min1=0
# unitaryzacja zerowana
uz1=(max1-Cost1[1])/(max1-min1)  # negatywna, im blizsza 1 tym bardziej nie widac kosztow projektu
uz2=(max1-Cost1[2])/(max1-min1)  # negatywna
uz4=(Cost1[4]-min1)/(max1-min1)  # pozytywna, im blizsza 1 tym bardziej widac koszty projektu
uz5=(Cost1[5]-min1)/(max1-min1)  # pozytywna
wsk=(uz1+uz2+uz4+uz5)/4           # wskaznik, czyli srednia z wynikow unitaryzacji
wsk1=format(x=wsk,digits=3,nsmall=3,scientific=FALSE)
ProfCost[i,2]=wsk1
} # kon i

# usrednianie kosztow i korzysci do testu
# liczenie wariancji obu, do testu rownosci srednich
ProfCostTest=data.frame(matrix(NA, nrow(Odpowiedzi),5), row.names=row.names(Odpowiedzi))
names(ProfCostTest)=c("BenefAvg","CostAvg","|t-Stat|","prob","prof>cost")
#i=1
skala=1:5
for (i in 1:nrow(Odpowiedzi))
{ # pocz i
Prof1=Odpowiedzi[i,LicznikDanychProf]
Cost1=Odpowiedzi[i,LicznikDanychCost]
# srednie
AvgProfi=sum(Prof1*skala)/sum(Prof1)
AvgCost=sum(Cost1*skala)/sum(Cost1)
# wariancje
VarProfi=sum(((skala-rep(AvgProfi,length(skala)))^2)*Prof1)/sum(Prof1)
VarCost=sum(((skala-rep(AvgCost,length(skala)))^2)*Cost1)/sum(Cost1)
# liczebnosc proby
n1=sum(Prof1)
# statystyka t-Studenta testu rownosci srednich str 164 [7.29] Balicki, Makac
t_KK=abs(AvgProfi-AvgCost)/sqrt((VarProfi/(n1-1))+(VarCost/(n1-1)))
# liczba stopni swobody
lss_licznik=((VarProfi/(n1-1))+(VarCost/(n1-1)))^2
lss_mianownik=(VarProfi/(n1-1))^2/(n1-1)+(VarCost/(n1-1))^2/(n1-1)
lss=lss_licznik/lss_mianownik
lss=ceiling(lss)
pval=2*pt(q=as.numeric(t_KK), df=as.numeric(lss), lower.tail=FALSE)
t_char=format(x=as.numeric(as.matrix(t_KK)), nsmall=3, scientific=FALSE)
t_char=substr(t_char,1,5)
pval2=format(x=pval,nsmall=3,scientific=FALSE)
pval_char=substr(pval2,1,5)
ProfCostTest[i,1]=substr(format(x=AvgProfi, nsmall=3, scientific=FALSE),1,5)
ProfCostTest[i,2]=substr(format(x=AvgCost, nsmall=3, scientific=FALSE),1,5)
ProfCostTest[i,3]=t_char
ProfCostTest[i,4]=pval_char
ProfCostTest[i,5]=0
if (AvgProfi>AvgCost & pval<0.1) ProfCostTest[i,5]="+"
if (AvgProfi<AvgCost & pval<0.1) ProfCostTest[i,5]="-"
} # kon i
#ProfCostTest
BenefCost=list(BenefCostInd=ProfCost, BenefCostTest=ProfCostTest)
} # kon BenefCost


CollabPotential=function(AttribIdent)
{ # pocz Colab
# AA Attitude   stanowisko, podejscie
# II Interest   zainteresowanie
# PO Power      sila
# PR Proximity  bliskosc, sasiedztwo
# LL Legitimacy zasadnosc, legalnosc
# etap 4 potencjal wspolpracy Power, Legitimacy, Urgency
Odpowiedzi3=AttribIdent$Mean
Odpowiedzi31=AttribIdent$Fra

# szukanie kolumn dla power legitimacy i urgency
NameOdp3i31=names(Odpowiedzi3)
pom01=(NameOdp3i31=="Pow")
pom02=(NameOdp3i31=="Leg")
pom03=(NameOdp3i31=="Urg")
NoPowLegUrg=c(NA,NA,NA)
for (i in 1:length(pom01))
{ # pocz i
if (pom01[i]) NoPowLegUrg[1]=i
if (pom02[i]) NoPowLegUrg[2]=i
if (pom03[i]) NoPowLegUrg[3]=i
} # kon i

Odpowiedzi4=Odpowiedzi3[,NoPowLegUrg]
Nazwy4=names(Odpowiedzi4)
Odpowiedzi4=cbind(Odpowiedzi4,NA)
names(Odpowiedzi4)=c(Nazwy4,"colaboration")

Odpowiedzi41=Odpowiedzi31[,NoPowLegUrg]
Nazwy41=names(Odpowiedzi41)
Odpowiedzi41=cbind(Odpowiedzi41,NA)
names(Odpowiedzi41)=c(Nazwy41,"colaboration")

for (i in 1:nrow(Odpowiedzi4))
{ # pocz i
#i=1
pom01=substr(Odpowiedzi4[i,1:3], start=5, stop=5)
pom011=substr(Odpowiedzi41[i,1:3], start=5, stop=5)
# ustalanie liczby minusow, zer i plusow (do potencjalu wspolpracy)
ind_minus=(pom01=="-")  
ind_minus1=(pom011=="-")  
liczba_minus=sum(ind_minus)
liczba_minus1=sum(ind_minus1)
ind_zero=(pom01==0)
ind_zero1=(pom011==0)
liczba_zer=sum(ind_zero)
liczba_zer1=sum(ind_zero1)
ind_plus=(pom01=="+")
ind_plus1=(pom011=="+")
liczba_plus=sum(ind_plus)
liczba_plus1=sum(ind_plus1)
if (liczba_plus>=2) Odpowiedzi4[i,4]="high" else Odpowiedzi4[i,4]="low"
if (liczba_plus1>=2) Odpowiedzi41[i,4]="high" else Odpowiedzi41[i,4]="low"
} # kon i
#Odpowiedzi4
CollabPotential=list(Mean=Odpowiedzi4, Fra=Odpowiedzi41)
} # kon Colab

StakeholdClassif=function(BenefCostTest,CollabPotential,AttribIdent)
{ # pocz classif
ProfCostTest=BenefCostTest
Odpowiedzi4=CollabPotential
Odpowiedzi3=AttribIdent

Odpowiedzi5=ProfCostTest[,1:3]
Odpowiedzi5[,]=NA
names(Odpowiedzi5)=c("class","attit","comm")
#i=1
for (i in 1:nrow(Odpowiedzi5))
{ # pocz i
# wybor interesariusza (bo w Odpowiedzi4 i Odpowiedzi5 nie musza byc ci sami)
int01=row.names(Odpowiedzi5)[i]
indeks01=(row.names(Odpowiedzi4)==int01)
PomOdp4=Odpowiedzi4[indeks01,]
# pobranie danych od identycznych interesariuszy
pom01=substr(PomOdp4[1,1:3], start=5, stop=5)    # pierwsze3
pom01[(pom01==0)]="-"                            # zamiana (0) na (-1)
indeks01=(row.names(Odpowiedzi3)==int01)         # interesariusz znaleziony w Odpowiedzi3
PomOdpAtt=Odpowiedzi3[indeks01,][1]              # tylko Attitude
pot.zagr=substr(PomOdpAtt,5,5)                   # korzysci/koszty z attitude, to daje potencjal zagrozenia
pot.wsp=PomOdp4[1,4][1]                          # potencjal wspolpracy

# definitywni
if (sum(pom01==c("+","+","+"))==length(pom01) & pot.zagr=="+") Odpowiedzi5[i,1:3]=c("Definitive","Supportive","Involve")
if (sum(pom01==c("+","+","+"))==length(pom01) & pot.zagr=="-") Odpowiedzi5[i,1:3]=c("Definitive","Mixed","Involve")
if (sum(pom01==c("+","+","+"))==length(pom01) & pot.zagr=="0") Odpowiedzi5[i,1:3]=c("Definitive","Neutral","No action")

# utajeni uspieni
if (sum(pom01==c("+","-","-"))==length(pom01) & pot.zagr=="+") Odpowiedzi5[i,1:3]=c("Dormant","Insignificant","Monitor")
if (sum(pom01==c("+","-","-"))==length(pom01) & pot.zagr=="-") Odpowiedzi5[i,1:3]=c("Dormant","Non-supportive","Defend")
if (sum(pom01==c("+","-","-"))==length(pom01) & pot.zagr=="0") Odpowiedzi5[i,1:3]=c("Dormant","Neutral","No action")

# utajeni dyskretni
if (sum(pom01==c("-","+","-"))==length(pom01) & pot.zagr=="+") Odpowiedzi5[i,1:3]=c("Discretionary","Insignificant","Monitor")
if (sum(pom01==c("-","+","-"))==length(pom01) & pot.zagr=="-") Odpowiedzi5[i,1:3]=c("Discretionary","Non-supportive","Defend")
if (sum(pom01==c("-","+","-"))==length(pom01) & pot.zagr=="0") Odpowiedzi5[i,1:3]=c("Discretionary","Neutral","No action")

# utajeni wymagajacy
if (sum(pom01==c("-","-","+"))==length(pom01) & pot.zagr=="+") Odpowiedzi5[i,1:3]=c("Demanding","Insignificant","Monitor")
if (sum(pom01==c("-","-","+"))==length(pom01) & pot.zagr=="-") Odpowiedzi5[i,1:3]=c("Demanding","Non-supportive","Defend")
if (sum(pom01==c("-","-","+"))==length(pom01) & pot.zagr=="0") Odpowiedzi5[i,1:3]=c("Demanding","Neutral","No action")

# oczekujacy dominujacy
if (sum(pom01==c("+","+","-"))==length(pom01) & pot.zagr=="+") Odpowiedzi5[i,1:3]=c("Dominant","Supportive","Involve")
if (sum(pom01==c("+","+","-"))==length(pom01) & pot.zagr=="-") Odpowiedzi5[i,1:3]=c("Dominant","Mixed","Colaborate")
if (sum(pom01==c("+","+","-"))==length(pom01) & pot.zagr=="0") Odpowiedzi5[i,1:3]=c("Dominant","Neutral","No action")

# oczekujacy zalezni
if (sum(pom01==c("-","+","+"))==length(pom01) & pot.zagr=="+") Odpowiedzi5[i,1:3]=c("Dependant","Supportive","Involve")
if (sum(pom01==c("-","+","+"))==length(pom01) & pot.zagr=="-") Odpowiedzi5[i,1:3]=c("Dependant","Mixed","Colaborate")
if (sum(pom01==c("-","+","+"))==length(pom01) & pot.zagr=="0") Odpowiedzi5[i,1:3]=c("Dependant","Neutral","No action")

# oczekujacy niebezpieczni
if (sum(pom01==c("+","-","+"))==length(pom01) & pot.zagr=="+") Odpowiedzi5[i,1:3]=c("Dangerous","Supportive","Involve")
if (sum(pom01==c("+","-","+"))==length(pom01) & pot.zagr=="-") Odpowiedzi5[i,1:3]=c("Dangerous","Mixed","Colaborate")
if (sum(pom01==c("+","-","+"))==length(pom01) & pot.zagr=="0") Odpowiedzi5[i,1:3]=c("Dangerous","Neutral","No action")

# niezidentyfikowani
if (sum(pom01==c("-","-","-"))==length(pom01) & pot.zagr=="+") Odpowiedzi5[i,1:3]=c("Non-Stakeholder","Insignificant","No action")
if (sum(pom01==c("-","-","-"))==length(pom01) & pot.zagr=="-") Odpowiedzi5[i,1:3]=c("Non-Stakeholder","Non-supportive","No action")
if (sum(pom01==c("-","-","-"))==length(pom01) & pot.zagr=="0") Odpowiedzi5[i,1:3]=c("Non-Stakeholder","Neutral","No action")

} # kon i
#Odpowiedzi5
StakeholdClassif=Odpowiedzi5
} # kon classif

ImpactAnalysis=function(data,BenefCost,NoStakeholders,NameStakeholders)
{ # pocz impact
dane=data
ProfCost=BenefCost
LInterPop=NoStakeholders
ListaInterPop=NameStakeholders
Inter=factor(dane[,1])
skala=1:5

NazwyKol=names(dane)
NazwyKol2=substr(NazwyKol,1,2)
indMe=(NazwyKol2=="me")
indMy=(NazwyKol2=="my")
DaneMy=data.frame(cbind(dane[,1],dane[,indMy]))
names(DaneMy)=c("ID",names(dane[,indMy]))
DaneMe=data.frame(cbind(dane[,1],dane[,indMe]))
names(DaneMe)=c("ID",names(dane[,indMe]))
MacierzMy=diag(0,ncol(DaneMy)-1, ncol(DaneMy)-1)   # macierz zaleznosci na kogo wplywam
MacierzMe=diag(0,ncol(DaneMe)-1, ncol(DaneMe)-1)   # macierz zaleznosci kto ma mnie wplywa
MacierzMyMe=diag(0,ncol(DaneMy)-1, ncol(DaneMy)-1)   # macierz zaleznosci na kogo wplywam
# por Hester 2013 (leontief)
WagaLow=0.25
WagaMed=0.5
WagaHigh=0.75
for (i in 1:LInterPop)
{ # pocz i
#i=1
ind01=(Inter==ListaInterPop[i])
daneMy_pom01=DaneMy[ind01,2:ncol(DaneMy)]                      # dane dla wybranego (itego) interesariusza
daneMe_pom01=DaneMe[ind01,2:ncol(DaneMe)]                      # dane dla wybranego (itego) interesariusza
for (j in 1:ncol(daneMy_pom01))
{ # pocz j
#j=2
if (i!=j)
{ # pocz if
OdpMy=table(daneMy_pom01[,j])       # odpowiedzi respondentow
OdpMyAvg=sum(as.numeric(names(OdpMy))*OdpMy)/sum(OdpMy)
OdpMyProc=OdpMyAvg/max(skala)
OdpMe=table(daneMe_pom01[,j])       # odpowiedzi respondentow
OdpMeAvg=sum(as.numeric(names(OdpMe))*OdpMe)/sum(OdpMe)
OdpMeProc=OdpMeAvg/max(skala)
# all(1)=skala 0.2, all(2)=skala 0.4, all(3)=0.6, all(4)= skala 0.8, all(5)=skala 1
# skala dla wag przyjeta za Hesterem 2013
if (OdpMyProc>0.65 & OdpMyProc<=0.75) MacierzMy[i,j]=WagaLow
if (OdpMyProc>0.75 & OdpMyProc<=0.9) MacierzMy[i,j]=WagaMed
if (OdpMyProc>0.9) MacierzMy[i,j]=WagaHigh
if (OdpMeProc>0.65 & OdpMeProc<=0.75) MacierzMe[i,j]=WagaLow
if (OdpMeProc>0.75 & OdpMeProc<=0.9) MacierzMe[i,j]=WagaMed
if (OdpMeProc>0.9) MacierzMe[i,j]=WagaHigh
} # kon if

} # kon j
} # kon i
#MacierzMy
#MacierzMe
# usrednianie wynikow mojego wplywu na i wplywu na mnie
for (i in 1:nrow(MacierzMy))
{ # pocz i
for (j in 1:nrow(MacierzMe))
{ # pocz j
pomMy=MacierzMy[i,j]
pomMe=MacierzMe[i,j]
if (pomMy==pomMe) MacierzMyMe[i,j]=pomMe
if ((pomMy==WagaHigh & pomMe==0) | (pomMe==WagaHigh & pomMy==0)) MacierzMyMe[i,j]=WagaMed
if ((pomMy==WagaMed & pomMe==0) | (pomMe==WagaMed & pomMy==0)) MacierzMyMe[i,j]=WagaLow
if ((pomMy==WagaLow & pomMe==0) | (pomMe==WagaLow & pomMy==0)) MacierzMyMe[i,j]=WagaLow
if ((pomMy==WagaMed & pomMe==WagaLow) | (pomMy==WagaLow & pomMe==WagaMed)) MacierzMyMe[i,j]=WagaLow
if ((pomMy==WagaMed & pomMe==WagaHigh) | (pomMy==WagaHigh & pomMe==WagaMed)) MacierzMyMe[i,j]=WagaMed
if ((pomMy==WagaLow & pomMe==WagaHigh) | (pomMy==WagaHigh & pomMe==WagaLow)) MacierzMyMe[i,j]=WagaMed
} # kon j
} # kon i

MacierzMy=data.frame(MacierzMy, row.names=ListaInterPop)
names(MacierzMy)=ListaInterPop
MacierzMe=data.frame(MacierzMe, row.names=ListaInterPop)
names(MacierzMe)=ListaInterPop
MacierzMyMe=data.frame(MacierzMyMe, row.names=ListaInterPop)
names(MacierzMyMe)=ListaInterPop
#MacierzMy
#MacierzMe
#MacierzMyMe


A=MacierzMyMe   # zwyczajowa nazwa dla macierzy przeplywow to A, a nie MacierzMyMe!
AA=matrix(0,nrow(A), ncol(A))
for (i in 1:nrow(AA))
{ #
for (j in 1:ncol(AA))
{ #
AA[i,j]=as.numeric(A[i,j])
} #
} #
#A=AA  # zamiana na typ numeryczny

qs=data.frame(matrix(0,nrow(A)), row.names=row.names(A))
names(qs)=("qs")
for (i in 1:nrow(qs))  
{ # pocz i
#i=1
cs=matrix(0,nrow(qs))
cs[i,1]=1
qsi=solve(diag(1,nrow(A))-as.matrix(A))%*%cs
qs[i,1]=qsi[i,1]
} # kon i 
# unitaryzacja zerowana dla qs
# stare
#qs.pom=as.numeric(unlist(qs))
#qs2=as.matrix((qs.pom-min(qs.pom))/(max(qs.pom)-min(qs.pom)))
#qs2=round(qs2,3)
#qs3=format(x=qs2,digits=3,nsmall=3,scientific=FALSE)

CS=diag(1,nrow(A))
qs=solve(diag(1,nrow(A))-A)%*%CS
qs=apply(abs(qs),1,mean)    # usrednianie qs po wierszach
qs=format(x=qs,digits=3,nsmall=3,scientific=FALSE)

# profits po modelu lenontiefa
PLeo=as.numeric(ProfCost[,1])*as.numeric(qs)
PLeo2=round(PLeo,3)
PLeo2=format(x=PLeo2,digits=3,nsmall=3,scientific=FALSE)
# costs po modelu lenotiefa
CLeo=as.numeric(ProfCost[,2])*as.numeric(qs)
CLeo2=round(CLeo,3)
CLeo2=format(x=CLeo2,digits=3,nsmall=3,scientific=FALSE)
ProfCost2=cbind(ProfCost,qs,PLeo2, CLeo2)
names(ProfCost2)=c("PforInd","CostInd","qs","ProfLeo","CostLeo")

ImpactAnalysis=list(Leontief=ProfCost2, MyImpact=MacierzMy, OnMeImpact=MacierzMe, MeanImpact=MacierzMyMe)
} # kon impact


AttribPict=function(path,tofile,AttribIdent,CollabPotential)
{ # pocz pict
DoPliku=tofile
Odpowiedzi3=AttribIdent
Odpowiedzi4=CollabPotential

# tworzenie wykresu
# ustalenie skali na osi pionowej
maxy=1000
miny=1
# ustalenie skali na osi poziomej
maxx=1000
minx=1
doskali=minx:maxx
# srednice okregow, srodek duzego okregu
xx10=350  
xx20=650  
xx30=500  

yy10=650  
yy20=650  
yy30=400  

rr1=280  # xx0,yy0, rr0 to srodek i srednica
rr2=280
rr3=280

# welkosc czcionki
ps1=12  # opis kolek
ps2=10  # nazwy interesariuszy

if (DoPliku==1) pdf(file=paste(path, "AttribPict.pdf", sep=""), width=7, height=6, family="Helvetica", encoding="CP1250.enc")
par(mai=c(0.1,0.1,0.1,0.1), ps=ps1)
plot(doskali, axes=FALSE, xlab=NA, ylab=NA , type="n")
symbols(x=xx10, y=yy10, circles=rr1, inches=FALSE, add=TRUE, fg="black")
symbols(x=xx20, y=yy20, circles=rr2, inches=FALSE, add=TRUE, fg="black")
symbols(x=xx30, y=yy30, circles=rr3, inches=FALSE, add=TRUE, fg="black")
text(x=350, y=990, labels="Power", adj=0.5)
text(x=650, y=990, labels="Legitimacy", adj=0.5)
text(x=500, y=60, labels="Urgency", adj=0.5)

# klasyfikowanie do grup
# wspolrzedne poczatkow wpisywania tekstu
x_Po=250
y_Po=850
x_Le=800
y_Le=850
x_PoLe=500
y_PoLe=850
x_PoLeAtt=500
y_PoLeAtt=650
x_PoAtt=330
y_PoAtt=550
x_LeAtt=670
y_LeAtt=550
x_Att=500
y_Att=330
x_OutOfClass=120
y_OutOfClass=300
shiftY=33
#i=1
par(ps=ps2)
for (i in 1:nrow(Odpowiedzi3))
{ # pocz i
pom01=Odpowiedzi3[i,c(2,4,3)]   # power, legitimacy, urgency
pom01=substr(pom01, start=5, stop=5)
pom01[(pom01==0)]="-"

if (sum(pom01==c("+","-","-"))==3)
{ # pocz if
text(x=x_Po, y=y_Po, labels=row.names(Odpowiedzi4)[i], adj=0.5)
y_Po=y_Po-shiftY
} # kon if

if (sum(pom01==c("-","+","-"))==3)
{ # pocz if
text(x=x_Le, y=y_Le, labels=row.names(Odpowiedzi4)[i], adj=0.5)
y_Le=y_Le-shiftY
} # kon if

if (sum(pom01==c("-","-","+"))==3)
{ # pocz if
text(x=x_Att, y=y_Att, labels=row.names(Odpowiedzi4)[i], adj=0.5)
y_Att=y_Att-shiftY
} # kon if

if (sum(pom01==c("+","+","-"))==3)
{ # pocz if
text(x=x_PoLe, y=y_PoLe, labels=row.names(Odpowiedzi4)[i], adj=0.5)
y_PoLe=y_PoLe-shiftY
} # kon if

if (sum(pom01==c("+","+","+"))==3)
{ # pocz if
text(x=x_PoLeAtt, y=y_PoLeAtt, labels=row.names(Odpowiedzi4)[i], adj=0.5)
y_PoLeAtt=y_PoLeAtt-shiftY
} # kon if

if (sum(pom01==c("+","-","+"))==3)
{ # pocz if
text(x=x_PoAtt, y=y_PoAtt, labels=row.names(Odpowiedzi4)[i], adj=0.5)
y_PoAtt=y_PoAtt-shiftY
} # kon if

if (sum(pom01==c("-","+","+"))==3)
{ # pocz if
text(x=x_LeAtt, y=y_LeAtt, labels=row.names(Odpowiedzi4)[i], adj=0.5)
y_LeAtt=y_LeAtt-shiftY
} # kon if

if (sum(pom01==c("-","-","-"))==3)
{ # pocz if
text(x=x_OutOfClass, y=y_OutOfClass, labels=row.names(Odpowiedzi4)[i], adj=0.5)
y_OutOfClass=y_OutOfClass-shiftY
} # kon if

} # kon i
if (DoPliku==1) dev.off()

} # kon pic



RelationPict=function(path,tofile,MeanImpact,StakeholdClassif)
{ # pocz RelationPict
DoPliku=tofile
MacierzMyMe=MeanImpact
Odpowiedzi5=StakeholdClassif

# tabela klasyfikacji do wykresu
klasyf=matrix(NA, nrow(MacierzMyMe), 2)
klasyf=data.frame(klasyf, row.names=row.names(MacierzMyMe))
names(klasyf)=c("classif","attitude")
for (i in 1:nrow(klasyf))
{ # pocz i
#i=1
if (Odpowiedzi5[i,1]=="Definitive") klasyf[i,1]="Definitive"
if (Odpowiedzi5[i,1]=="Dormant" | Odpowiedzi5[i,1]=="Discretionary" | Odpowiedzi5[i,1]=="Demanding") klasyf[i,1]="Latent"
if (Odpowiedzi5[i,1]=="Dominant" | Odpowiedzi5[i,1]=="Dependant" | Odpowiedzi5[i,1]=="Dangerous") klasyf[i,1]="Expectant"
if (Odpowiedzi5[i,1]=="Non-Stakeholder") klasyf[i,1]="Unidentified"
klasyf[i,2]=Odpowiedzi5[i,2]
} # kon i
#klasyf

# tworzenie wykresu
# ustalenie skali na osi pionowej
maxy=1000
miny=1
# ustalenie skali na osi poziomej
maxx=1000
minx=1
doskali=minx:maxx
# ustalanie kolorow
zielony="darkgreen"
czerwony="brown2"
zolty="gold3"
bialy="gainsboro"
fioletowy="darkorchid"

# srednice okregow, srodek duzego okregu
xx0=600  
yy0=500  
rr0=350  # xx0,yy0, rr0 to srodek i srednica duzego okregu po ktorym rozkladane sa male koleczka
rD=80    # srednica okregu Definitive
rE=65    # srednica okregu Expectant
rL=50    # srednica okregu Latent
rN=35    # srednica okregu Unidentified
# grubosci strzalek; przesuniecie wobec srodka (dla dwoch strzalek z petla); przesuniecie znacznika strzalki wobec srodka odcinka laczacego
lwdHigh=4
lwdMed=2
lwdLow=1
shift1=20    # przesuwa strzalki w gore/dol dla zaleznosci cyklicznych A na B i B na A, tak by strzalki sie nie pokrywaly
shift2=0.65  # to jest parametr t w rownaniu parametrycznym odcinka wyznaczajacy pozycje konca strzalki
# welkosc czcionki
ps1=10  # opis kolek
ps2=10  # legenda

if (DoPliku==1) pdf(file=paste(path,"ImpactPict.pdf", sep=""), width=7, height=6, family="Helvetica", encoding="CP1250.enc")

par(mai=c(0.1,0.1,0.1,0.1), ps=ps1)
plot(doskali, axes=FALSE, xlab=NA, ylab=NA , type="n")

# ustalanie srodkow okregow z rownania parametrycznego okregu
tt=seq(from=0, to=2*pi, by=2*pi/nrow(klasyf))
xx=xx0+rr0*cos(tt)
yy=yy0+rr0*sin(tt)

# macierze pomocnicze wskazujace
# czy strzalki od 1 do 2 i od 2 do 1 zostaly juz narysowane, 0=brak narysowania
A=MacierzMyMe  # stara nazwa dla macierzy zaleznosci
A1221=matrix(0,nrow(A),ncol(A)) 
# obiekt zachowujacy wspolrzedne wszystkich strzalek
wspA=NULL
# rysowanie strzalek
for (i in 1:ncol(A))
{ # pocz i
for (j in 1:ncol(A))
{ # pocz j

pomA12=A[i,j]
pomA21=A[j,i]

if (pomA12>0 & A1221[i,j]!=1)
{ # pocz if
# ustalenie grubosci strzalki/odcinka lwdS
if (pomA12>2/3) lwdS=lwdHigh
if (pomA12>1/3 & pomA12<=2/3) lwdS=lwdMed
if (pomA12<=1/3) lwdS=lwdLow
# wspolrzedne odcinka patametycznie (do ustalenia konca strzalki)
# watosc patametru t=shift2 wyznacza koniec strzalki
x3=xx[i]+(xx[j]-xx[i])*shift2
y3=yy[i]+(yy[j]-yy[i])*shift2
# ustalanie przesuniecia strzalek wzgledem srodka okregu
if (pomA21==0) shift=0 else shift=shift1
# rysowanie strzalek
arrows(x0=xx[i], y0=yy[i]+shift, x1=x3, y1=y3+shift, code=2, length=0.1, lwd=lwdS)
wspA_pom=t(as.matrix(c(xx[i],yy[i]+shift,x3,y3+shift,i,j)))
wspA=rbind(wspA, wspA_pom)
lines(x=c(xx[i], xx[j]), y=c(yy[i]+shift, yy[j]+shift), type="l", lwd=lwdS)
# oznacenie, ze skrzalka juz zostala narysowana
A1221[i,j]=1
} # kon if

if (pomA21>0 & A1221[j,i]!=1)
{ # pocz if
# ustalenie grubosci strzalki/odcinka lwdS
if (pomA21>2/3) lwdS=lwdHigh
if (pomA21>1/3 & pomA21<=2/3) lwdS=lwdMed
if (pomA21<=1/3) lwdS=lwdLow
# wspolrzedne odcinka patametycznie
# watosc patametru t=shift2 wyznacza koniec strzalki
x3=xx[j]+(xx[i]-xx[j])*shift2
y3=yy[j]+(yy[i]-yy[j])*shift2
# ustalanie przesuniecia strzalek wzgledem srodka okregu
if (pomA12==0) shift=0 else shift=-shift1
# rysowanie strzalek
arrows(x0=xx[j], y0=yy[j]+shift, x1=x3, y1=y3+shift, code=2, length=0.1, lwd=lwdS)
wspA_pom=t(as.matrix(c(xx[i],yy[i]+shift,x3,y3+shift,i,j)))
wspA=rbind(wspA, wspA_pom)
lines(x=c(xx[i], xx[j]), y=c(yy[i]+shift, yy[j]+shift), type="l", lwd=lwdS)
# oznacenie, ze skrzalka juz zostala narysowana
A1221[j,i]=1
} # kon if

} # kon j
} # kon i


# rysowanie kolek
for (i in 1:(length(xx)-1))
{ # pocz i
# ustalenie promienia okregu
r_pom=klasyf[i,1]
if (r_pom=="Definitive") rr=rD
if (r_pom=="Expectant") rr=rE
if (r_pom=="Latent") rr=rL
if (r_pom=="Unidentified") rr=rN

# ustalenie koloru
kol_pom=klasyf[i,2]
if (kol_pom=="Supportive") kol=zielony
if (kol_pom=="Non-supportive") kol=czerwony
if (kol_pom=="Mixed") kol=zolty
if (kol_pom=="Neutral") kol=bialy
if (kol_pom=="Insignificant") kol=fioletowy

symbols(x=xx[i], y=yy[i], circles=rr, inches=FALSE, add=TRUE, fg="white", bg=kol)
text(x=xx[i], y=yy[i], labels=row.names(klasyf)[i], adj=0.5, col="black")
} # kon i


# legenda
par(ps=ps2)
symbols(x=45, y=940, circles=rD, inches=FALSE, add=TRUE, fg="black")
text(x=45, y=940, labels="Definitive", adj=0.5, col="black")
symbols(x=45, y=750, circles=rE, inches=FALSE, add=TRUE, fg="black")
text(x=45, y=750, labels="Expectant", adj=0.5, col="black")
symbols(x=45, y=600, circles=rL, inches=FALSE, add=TRUE, fg="black")
text(x=45, y=600, labels="Latent", adj=0.5, col="black")
symbols(x=45, y=480, circles=rN, inches=FALSE, add=TRUE, fg="black")
text(x=45, y=480, labels="Unidentified", adj=0.5, col="black")
symbols(x=45, y=380, circles=rN, inches=FALSE, add=TRUE, fg="white", bg=zielony)
text(x=45, y=380, labels="Supportive", adj=0.5, col="black")
symbols(x=45, y=290, circles=rN, inches=FALSE, add=TRUE, fg="white", bg=zolty)
text(x=45, y=290, labels="Mixed", adj=0.5, col="black")
symbols(x=45, y=200, circles=rN, inches=FALSE, add=TRUE, fg="white", bg=czerwony)
text(x=45, y=200, labels="Non-supportive", adj=0.4, col="black")
symbols(x=45, y=110, circles=rN, inches=FALSE, add=TRUE, fg="white", bg=bialy)
text(x=45, y=110, labels="Neutral", adj=0.5, col="black")
symbols(x=45, y=20, circles=rN, inches=FALSE, add=TRUE, fg="white", bg=fioletowy)
text(x=45, y=20, labels="Insignificant", adj=0.5, col="black")

arrows(x0=250, y0=25, x1=300, y1=25, code=2, length=0.1, lwd=lwdHigh)
text(x=310, y=25, labels="High", adj=0, col="black")

arrows(x0=500, y0=25, x1=550, y1=25, code=2, length=0.1, lwd=lwdMed)
text(x=560, y=25, labels="Medium", adj=0, col="black")

arrows(x0=750, y0=25, x1=800, y1=25, code=2, length=0.1, lwd=lwdLow)
text(x=810, y=25, labels="Low", adj=0, col="black")
if (DoPliku==1) dev.off()

} # kon RelationPict


Histograms=function(path,tofile,CountResponses)
{ # pocz hist
Odpowiedzi=CountResponses
DoPliku=tofile

NazwyAtrybutow4=c("attitude", "power", "urgency", "legitimacy", "benefits", "costs")
for (i in 1:nrow(Odpowiedzi))
{ # pocz i
NaHist01=Odpowiedzi[i,]
NaHistAt=NaHist01[1:5]
NaHistPo=NaHist01[6:10]
NaHistUr=NaHist01[11:15]
NaHistLe=NaHist01[16:20]
NaHistPr=NaHist01[21:25]
NaHistCo=NaHist01[26:30]
NaHist02=rbind(as.matrix(NaHistAt), as.matrix(NaHistPo), as.matrix(NaHistUr), as.matrix(NaHistLe), as.matrix(NaHistPr), as.matrix(NaHistCo))
if (DoPliku==1) pdf(file=paste(path, "Hist", row.names(NaHist01),".pdf", sep=""), width=7, height=6, family="Helvetica", encoding="CP1250.enc")
par(mfrow=c(2,3), cex=0.7, pin=c(1.7,1.5), bty="l") # zdefiniowanie ustawien pudelka wykresow
for (j in 1:nrow(NaHist02))
{ # pocz j
#j=1
NaHist03=as.numeric(NaHist02[j,])
NaHist04=c(rep(1,NaHist03[1]),rep(2,NaHist03[2]),rep(3,NaHist03[3]),rep(4,NaHist03[4]),rep(5,NaHist03[5]))
hist(x=NaHist04, breaks=seq(from=0.5, to=5.5, by=1), main=NazwyAtrybutow4[j], xlab=NULL)
} # kon j
mtext(text=paste("stakeholder: ",row.names(NaHist01)), side=3, line=-1.5, outer=TRUE, cex=0.8)
if (DoPliku==1) dev.off()
} # kon i

# Odpowiedzi
NaHist01=apply(X=Odpowiedzi, MARGIN=2, FUN=sum)
NaHistAt=NaHist01[1:5]
NaHistPo=NaHist01[6:10]
NaHistUr=NaHist01[11:15]
NaHistLe=NaHist01[16:20]
NaHistPr=NaHist01[21:25]
NaHistCo=NaHist01[26:30]
NaHist02=rbind(t(as.matrix(NaHistAt)), t(as.matrix(NaHistPo)), t(as.matrix(NaHistUr)), t(as.matrix(NaHistLe)), t(as.matrix(NaHistPr)), t(as.matrix(NaHistCo)))
if (DoPliku==1) pdf(file=paste(path, "HistOverall.pdf", sep=""), width=7, height=6, family="Helvetica", encoding="CP1250.enc")
par(mfrow=c(2,3), cex=0.7, pin=c(1.5,1.5), bty="l") # zdefiniowanie ustawien pudelka wykresow
for (j in 1:nrow(NaHist02))
{ # pocz j
#j=1
NaHist03=as.numeric(NaHist02[j,])
NaHist04=c(rep(1,NaHist03[1]),rep(2,NaHist03[2]),rep(3,NaHist03[3]),rep(4,NaHist03[4]),rep(5,NaHist03[5]))
hist(x=NaHist04, breaks=seq(from=0.5, to=5.5, by=1), main=NazwyAtrybutow4[j], xlab=NULL)
} # kon j
mtext(text="Overall results", side=3, line=-1.5, outer=TRUE, cex=0.8)
if (DoPliku==1) dev.off()
} # kon hist
