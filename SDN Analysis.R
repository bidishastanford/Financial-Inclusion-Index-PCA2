getwd()
library(zoo)
library(readstata13)
library(dplyr)
library(data.table)


##Getting the FAS DataSet
FASDataset<-read.dta13("R:/DOC/FI/FAS/FAST/Data/Clean/Latest Complete File for Analysis/CompleteLatestFAS_052219.dta")
colnames(FASDataset)

##Debit/credit cards
Cards<-FASDataset[,c("economy","year","group_region","group_income","group_IMF","denom_gdp","denom_adultpop","denom_landarea",
                     "s_cards_credit","s_cards_debit")]

###Create the indicators for debit and credit cards
Cards<-
  Cards %>%
  mutate(i_credit_pop = (s_cards_credit/denom_adultpop)*100000)
View(Cards)
Cards<-
  Cards %>%
  mutate(i_debit_pop = (s_cards_debit/denom_adultpop)*100000)
View(Cards)

##Cards Regional Classification
Creditregionalgroups<-
  Cards%>%
  group_by(group_region,year)%>%
  summarise(Creditcards=weighted.mean(i_credit_pop,denom_adultpop,na.rm = T))
View(Creditregionalgroups)

Debitregionalgroups<-
  Cards%>%
  group_by(group_region,year)%>%
  summarise(Debitcards=weighted.mean(i_debit_pop,denom_adultpop,na.rm = T))
View(Debitregionalgroups)


###Cards Income Analysis

####Number of countries with data
Creditcarddatincome<-
  Cards%>%
  filter(!is.na(i_credit_pop))%>%
  group_by(group_income,year)%>%
  summarise(Numofcountries=n_distinct(economy))
View(Creditcarddatincome)

Debitcarddatincome<-
  Cards5years%>%
  filter(!is.na(i_debit_pop_latest))%>%
  group_by(group_income,year)%>%
  summarise(Numofcountries=n_distinct(economy))
View(Debitcarddatincome)

###Income classification analysis
Credit_income<-
  Cards%>%
  group_by(group_income,year)%>%
  summarise(Creditcards=weighted.mean(i_credit_pop,denom_adultpop,na.rm = T))
View(Credit_income)

Debit_income<-
  Cards%>%
  group_by(group_income,year)%>%
  summarise(Debitcards=weighted.mean(i_debit_pop,denom_adultpop,na.rm = T))
View(Debit_income)


##Cards data after 2009
Cards5years <- subset(Cards, year > 2009)
View(Cards5years)
setDT(Cards5years)
##creating the most recent year available for debit or credit cards
Cards5years[,i_debit_pop_latest:= na.locf(i_debit_pop, na.rm = FALSE),by = economy]
Cards5years[,i_credit_pop_latest:= na.locf(i_credit_pop, na.rm = FALSE), by = economy]

##Analysis with latest data only
Credit_income_latest<-
  Cards5years%>%
  group_by(group_income,year)%>%
  summarise(Creditcards=weighted.mean(i_credit_pop_latest,denom_adultpop,na.rm = T))
View(Credit_income_latest)

Debit_income_latest<-
  Cards5years%>%
  group_by(group_income,year)%>%
  summarise(Debitcards=weighted.mean(i_debit_pop_latest,denom_adultpop,na.rm = T))
View(Debit_income_latest)


##SMEs
SME<-FASDataset[,grepl("economy|year|group|_sme|denom_adultpop|denom_landarea|denom_gdp",names(FASDataset))]
SME1<-FASDataset[,grepl("economy|year|group|A1_sme|denom_adultpop|denom_landarea|denom_gdp",names(FASDataset))]
colnames(SME1)

###Count the number of countries for each commercial bank indicator of SMEs
count<-
  SME200917%>%
  filter(!is.na(i_loans_A1_sme_GDP))%>%
  group_by(group_region,year)%>%
  summarise(Numofcountries=n_distinct(economy))
View(count)

##SME loans analysis
SME200917<-subset(SME1, year >=2009)
View(SME200917)

SMEloansGDP<-
  SME200917%>%
  group_by(group_region,year)%>%
  summarise(SMEloansGDP=weighted.mean(i_loans_A1_sme_GDP,denom_gdp,na.rm = T))
View(SMEloansGDP)

CountrieswithSMEloansGDP<-
  SME200917%>%
  filter(!is.na(i_loans_A1_sme_GDP))
View(CountrieswithSMEloansGDP)
write.csv(CountrieswithSMEloansGDP,"SMEloans.csv")

###SME borrowers analysis

SMEloanacc<-
  SME200917%>%
  group_by(group_region,year)%>%
  summarise(SMEloansacc=weighted.mean(i_loans_A1_sme_GDP,denom_gdp,na.rm = T))
View(SMEloansGDP)

CountrieswithSMEloanaccountoutofcorp<-
  SME200917%>%
  filter(!is.na(i_loan_acc_A1_sme_perNFC))
View(CountrieswithSMEloanaccountoutofcorp)
write.csv(CountrieswithSMEloanaccountoutofcorp,"SMEloanacc.csv")


##Insurance policies
Insurance<-FASDataset[,grepl("economy|year|group|_B2|denom_adultpop|denom_landarea|denom_gdp",names(FASDataset))]
colnames(Insurance)
Insurance200917<-subset(Insurance, year >=2009)
View(Insurance200917)
##Create insurance policy holder per 1000 adults
Insurance200917<-
  Insurance200917 %>%
  mutate(i_life_holder_pop = (s_policyholders_B2_life/denom_adultpop)*1000)
View(Insurance200917)

Insurance200917<-
  Insurance200917 %>%
  mutate(i_pol_pop = (s_policies_B2/denom_adultpop)*1000)
View(Insurance200917)

Insurance200917<-
  Insurance200917 %>%
  mutate(i_corp_pop = (s_institutions_B2/denom_adultpop)*100000)
View(Insurance200917)
write.csv(Insurance200917,"Insurancepolicyholders.csv")

##count the number of couuntries with insurance policies data
countlifepolicyholders<-
  Insurance200917%>%
  filter(!is.na(i_life_holder_pop))%>%
  group_by(group_region)%>%
  summarise(Numofcountries=n_distinct(economy))
View(countlifepolicyholders)

insurancepolicies<-
  Insurance200917%>%
  group_by(group_income,year)%>%
  summarise(insurpol=weighted.mean(i_pol_pop,denom_adultpop,na.rm = T))
View(insurancepolicies)


