library(zoo)
library(readstata13)
library(dplyr)
library(reshape)
library(reshape2)
library(purrr)
library(tidyverse)
library(BBmisc)



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


pca1<-Analysisset[,c( "ATMsnorm1","branchesnorm1")]


##Codes for the table
prin_comp1 <- prcomp(na.omit(pca1),na.rm=T,scale=T,center=T)
summary(prin_comp1)
library(stargazer)
stargazer(prin_comp1$rotation,type="text")
names(prin_comp1)

##Codes for the visualization
pr.var1=prin_comp1$sdev^2
biplot(prin_comp1,scale=0)
pve1=pr.var1/sum(pr.var1)
pve1
plot(pve1, xlab="Principal Component",ylab="Proportion of Variance Explained")
plot(pve1, xlab="Principal Component",ylab="Proportion of Variance Explained",type="b")
plot(cumsum(pve1),xlab="Principal Component",ylab="Cumulative Proportion of Variance Explained",type="b")
prin_comp1$rotation

Analysisset$Access<-.70^2*Analysisset$ATMsnorm1+.70^2*Analysisset$branchesnorm1


pca2<-Analysisset[,c("Depositorsnorm1","Borrowersnorm1")]


##Codes for the table
prin_comp2 <- prcomp(na.omit(pca2),na.rm=T,scale=T,center=T)
summary(prin_comp2)
library(stargazer)
stargazer(prin_comp2$rotation,type="text")
names(prin_comp2)

##Codes for the visualization
pr.var2=prin_comp2$sdev^2
biplot(prin_comp2,scale=0)
pve2=pr.var2/sum(pr.var2)
pve2
plot(pve2, xlab="Principal Component",ylab="Proportion of Variance Explained")
plot(pve2, xlab="Principal Component",ylab="Proportion of Variance Explained",type="b")
plot(cumsum(pve2),xlab="Principal Component",ylab="Cumulative Proportion of Variance Explained",type="b")
prin_comp2$rotation

Analysisset$Usage<-.70^2*Analysisset$Depositorsnorm1+.70^2*Analysisset$Borrowersnorm1

pca3<-Analysisset[,c( "Access","Usage")]
prin_comp3 <- prcomp(na.omit(pca3),na.rm=T,scale=T,center=T)
summary(prin_comp3)
library(stargazer)
stargazer(prin_comp3$rotation,type="text")
names(prin_comp3)

##Codes for the visualization
pr.var3=prin_comp3$sdev^2
biplot(prin_comp3,scale=0)
pve3=pr.var3/sum(pr.var3)
pve3
plot(pve3, xlab="Principal Component",ylab="Proportion of Variance Explained")
plot(pve3, xlab="Principal Component",ylab="Proportion of Variance Explained",type="b")
plot(cumsum(pve3),xlab="Principal Component",ylab="Cumulative Proportion of Variance Explained",type="b")
prin_comp3$rotation

Analysisset$Index<-.70^2*Analysisset$Access+.70^2*Analysisset$Usage

Results<-Analysisset[,c("economy","year","Index")]
Results<-reshape(Results, idvar = "economy", timevar = "year", direction = "wide")
write.csv(Results,"TwostagePCAresult.csv")

View(Results)
