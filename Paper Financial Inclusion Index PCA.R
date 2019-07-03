library(zoo)
library(readstata13)
library(dplyr)
library(reshape)
library(reshape2)
library(purrr)
library(tidyverse)
library(data.table)
library(funprog)

##Setting up the data
getwd()

##Getting the FAS DataSet
FASDataset<-read.dta13("R:/DOC/FI/FAS/FAST/Data/Clean/Latest Complete File for Analysis/CompleteLatestFAS_022719.dta")
colnames(FASDataset)


##Subsetting FAS dataset
Paperdataset<-FASDataset[,c("economy","year","group_region","group_income","group_IMF","denom_gdp","denom_adultpop","denom_landarea",
                          "i_ATMs_km2","i_branches_km2_A1","i_deposits_A1_GDP","i_loans_A1_GDP","i_depositors_A1_pop",
                                "i_deposit_acc_A1_pop","i_borrowers_A1_pop","i_loan_acc_A1_pop")]


##Using deposit accounts where depositors is not available
Paperdataset<-
Paperdataset %>%
  mutate(Depositors = coalesce(i_depositors_A1_pop, i_deposit_acc_A1_pop))

##Using loan accounts where borrowers is not available
Paperdataset<-
  Paperdataset %>%
  mutate(Borrowers = coalesce(i_borrowers_A1_pop, i_loan_acc_A1_pop))

Analysis<-
Paperdataset%>%
  filter(!is.na(Borrowers) &!is.na(Depositors) &!is.na(i_ATMs_km2) &!is.na(i_branches_km2_A1))
Analysis<-
Analysis%>%
  filter(year>2008)

Analysisset<-Analysis[,c("economy","year","group_region","group_income","group_IMF","denom_gdp",
                         "denom_adultpop","denom_landarea",
                          "i_ATMs_km2","i_branches_km2_A1","Depositors","Borrowers")]

View(Analysisset)


for (i in 2009:2017){
  Analysisset$ATMsnorm1<-Analysisset$i_ATMs_km2/max(Analysisset$i_ATMs_km2)
}

for (i in 2009:2017){
  Analysisset$branchesnorm1<-Analysisset$i_branches_km2_A1/max(Analysisset$i_branches_km2_A1)
}

for (i in 2009:2017){
  Analysisset$Depositorsnorm1<-Analysisset$Depositors/max(Analysisset$Depositors)
}

for (i in 2009:2017){
  Analysisset$Borrowersnorm1<-Analysisset$Borrowers/max(Analysisset$Borrowers)
}
##Counting the countries for which we have yearly data
countofcountries<-Analysisset %>%
   group_by(year)%>%
  summarise(A=n_distinct(economy))

##Codes for PCA
pca<-Analysisset[,c( "ATMsnorm1","branchesnorm1","Depositorsnorm1","Borrowersnorm1")]


##Codes for the table
prin_comp <- prcomp(na.omit(pca),na.rm=T)
summary(prin_comp)
library(stargazer)
stargazer(prin_comp$rotation,type="text")
names(prin_comp)

##Codes for the visualization
pr.var=prin_comp$sdev^2
biplot(prin_comp,scale=0)
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component",ylab="Proportion of Variance Explained")
plot(pve, xlab="Principal Component",ylab="Proportion of Variance Explained",type="b")
plot(cumsum(pve),xlab="Principal Component",ylab="Cumulative Proportion of Variance Explained",type="b")
prin_comp$rotation


View(Analysisset)
Analysisset$FII<-.2694292^2*Analysisset$ATMsnorm1+.2679066^2*Analysisset$branchesnorm1 +0.5994601^2*Analysisset$Depositorsnorm1+0.7044725^2*Analysisset$Borrowersnorm1


Results<-Analysisset[,c("economy","year","FII")]
Results<-reshape(Results, idvar = "economy", timevar = "year", direction = "wide")
write.csv(Results,"OnestagePCAresult.csv")
View(Analysisset)

Year2009<-Analysisset%>%
  filter(year==2009)%>%
  select(economy,FII)%>%
  arrange(desc(FII))
View(Year2009)



