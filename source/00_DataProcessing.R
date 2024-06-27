
## -----------------------------------------------------------    
## pull and format covariates for measles forecasting
## last updated: 21 June 2024

## -----------------------------------------------------------   
## load required packages

library(openxlsx)
library(data.table)
library(wbstats)
library(rgho)
library(ggplot2)
#install_github("PPgp/wpp2022")
library(wpp2022)
library(dplyr)
library(googledrive)
library(readxl)

data_downloads_path <- '~/Google Drive/My Drive/Gates_MeaslesForecast/updates_jhu/data_downloads/'

## -----------------------------------------------------------   
## -----------------------------------------------------------   
## prepare inputs - setting up ISO3 codes

df_raw_covariates <- data.table()

iso3_lookup <- read.xlsx("https://cdn.who.int/media/docs/default-source/immunization/immunization-coverage/measlescasesbycountrybymonth.xlsx?sfvrsn=1d9af577_111",sheet=2)
setnames(iso3_lookup, old=c("Region", "ISO3", "Country", "Year", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), 
         new = c("region", "iso3", "country", "year", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
iso3_lookup <- subset(iso3_lookup, select=c("iso3", "country"))
iso3_lookup <- unique(iso3_lookup)

## -----------------------------------------------------------   
## monthly "provisional" reported measles cases

monthly_cases <- read.xlsx("https://cdn.who.int/media/docs/default-source/immunization/immunization-coverage/measlescasesbycountrybymonth.xlsx?sfvrsn=1d9af577_111",sheet=2)
setnames(monthly_cases, old=c("Region", "ISO3", "Country", "Year", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), 
         new = c("region", "iso3", "country", "year", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

monthly_cases_long <- reshape2::melt(monthly_cases, id.vars=c("region", "iso3", "country", "year"))
setnames(monthly_cases_long, old=c('variable', 'value'), new = c('month', 'reported_cases'))
monthly_cases_long$month <- as.numeric(monthly_cases_long$month)

df_raw_covariates <- cbind(df_raw_covariates, monthly_cases_long)
monthly_cases_long <- NULL


## -----------------------------------------------------------   
## total population size

data(pop1dt, package = "wpp2022", envir = environment())
remove(popAge1dt)

pop_size <- subset(pop1dt, select=c("name", "year", "pop"))
pop_size <- subset(pop_size, year > 1979)
setnames(pop_size, old = c("name","pop"), new=c("country","pop_size"))

pop_size <- merge(pop_size, iso3_lookup, by = 'country')

# #### for now assume all annual data is set in month 1
pop_size$month <- 1

df_raw_covariates <- merge(df_raw_covariates, pop_size, all.x=T, all.y=T, by=c('iso3', 'country','year', 'month'))
pop_size <- NULL
pop1dt <- NULL


## -----------------------------------------------------------   
## annual mcv1 coverage

mcv1 <- wbstats::wb_data('SH.IMM.MEAS')
mcv1 <- data.table(subset(mcv1, !is.na(SH.IMM.MEAS)))
mcv1 <- subset(mcv1, select=c("iso3c", "date", "SH.IMM.MEAS"))

setnames(mcv1, old = c("iso3c", "date", "SH.IMM.MEAS"),
         new=c("iso3", "year", "mcv1"))

#### for now assume all annual data is set in month 1
mcv1$month <- 1

df_raw_covariates <- merge(df_raw_covariates, mcv1, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
mcv1 <- NULL


## -----------------------------------------------------------   
## annual mcv2 coverage

mcv2 <- wbstats::wb_data('SH.IMM.MEA2')
mcv2 <- data.table(subset(mcv2, !is.na(SH.IMM.MEA2)))
mcv2 <- subset(mcv2, select=c("iso3c", "date", "SH.IMM.MEA2"))

setnames(mcv2, old = c("iso3c", "date", "SH.IMM.MEA2"),
         new=c("iso3", "year", "mcv2"))

#### for now assume all annual data is set in month 1
mcv2$month <- 1

df_raw_covariates <- merge(df_raw_covariates, mcv2, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
mcv2 <- NULL


## -----------------------------------------------------------   
## crude birth & death rates

data("misc1dt", package = "wpp2022", envir = environment())

birth_death_rate <- subset(misc1dt, select=c('name', 'year', 'cbr', 'cdr'))
birth_death_rate <- subset(birth_death_rate, year > 1979)               
setnames(birth_death_rate, old=c("name", "cbr", "cdr"), new = c("country", "birth_rate", "death_rate"))

birth_death_rate <- merge(birth_death_rate, iso3_lookup, by = 'country')

#### for now assume all annual data is set in month 1
birth_death_rate$month <- 1

df_raw_covariates <- merge(df_raw_covariates, birth_death_rate, all.x=T, all.y=T, by=c('iso3', 'country','year', 'month'))
birth_death_rate <- NULL
misc1dt <- NULL


## -----------------------------------------------------------   
## net migration

data("migration1dt", package = "wpp2022", envir = environment())
net_migration <- subset(migration1dt, select=c('name', 'year', 'mig'))
net_migration <- subset(net_migration, year > 1979 & year < 2025)    
setnames(net_migration, old = c("name", "mig"), new=c("country", "net_migration"))

net_migration <- merge(net_migration, iso3_lookup, by = 'country')

#### for now assume all annual data is set in month 1
net_migration$month <- 1

df_raw_covariates <- merge(df_raw_covariates, net_migration, all.x=T, all.y=T, by=c('iso3', 'country','year', 'month'))
net_migration <- NULL


## -----------------------------------------------------------   
## population density

pop_density <- wbstats::wb_data('EN.POP.DNST')
pop_density <- data.table(subset(pop_density, !is.na(EN.POP.DNST)))
pop_density <- subset(pop_density, select=c("iso3c", "date", "EN.POP.DNST"))

setnames(pop_density, old = c("iso3c", "date", "EN.POP.DNST"),
         new=c("iso3", "year", "pop_density"))
pop_density <- subset(pop_density, year > 1979)

#### for now assume all annual data is set in month 1
pop_density$month <- 1

df_raw_covariates <- merge(df_raw_covariates, pop_density, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
pop_density <- NULL


## -----------------------------------------------------------   
## age distribution

#### average age, % under 5, % under 10
data('popAge1dt', package = "wpp2022", envir = environment())

pop_size <- subset(popAge1dt, select=c("name", "age", "year", "pop"))
pop_size <- subset(pop_size, year > 1979)
setnames(pop_size, old = c("name","pop"), new=c("country","pop_size"))

pop_size_under5 <- subset(pop_size, age < 5)
pop_size_under10 <- subset(pop_size, age < 10)

counts_under5 <- aggregate(pop_size_under5$pop_size, by=list(pop_size_under5$country, pop_size_under5$year), FUN = 'sum')
colnames(counts_under5) <- c("country", "year", "under5")

counts_under10 <- aggregate(pop_size_under10$pop_size, by=list(pop_size_under10$country, pop_size_under10$year), FUN = 'sum')
colnames(counts_under10) <- c("country", "year", "under10")

counts_total <- aggregate(pop_size$pop_size, by=list(pop_size$country, pop_size$year), FUN = 'sum')
colnames(counts_total) <- c("country", "year", "total")

counts_df <- merge(counts_under5, counts_under10, by=c('country', 'year'))
counts_df <- merge(counts_df, counts_total, by=c('country', 'year'))

counts_df$proportion_under5 <- counts_df$under5 / counts_df$total
counts_df$proportion_under10 <- counts_df$under10 / counts_df$total

av_age <- pop_size %>%
  group_by(country, year) %>%
  summarise(weighted_mean = sum(age * pop_size) / sum(pop_size))
colnames(av_age) <- c('country', 'year', 'av_age')

age_dist_covs <- merge(counts_df, av_age, by=c('country', 'year'))
age_dist_covs <- subset(age_dist_covs, select=c('country', 'year', 'proportion_under5', 'proportion_under10', 'av_age'))

age_dist_covs <- merge(age_dist_covs, iso3_lookup, by = 'country')

age_dist_covs$month <- 1

df_raw_covariates <- merge(df_raw_covariates, age_dist_covs, all.x=T, all.y=T, by=c('iso3', 'year', 'month', 'country'))
age_dist_covs <- NULL
av_age <- NULL
popAge1dt <- NULL
pop_size <- NULL


## -----------------------------------------------------------   
## gdp per capita

gdp_pc <- wbstats::wb_data('6.0.GDPpc_constant')
gdp_pc <- data.table(subset(gdp_pc, !is.na(`6.0.GDPpc_constant`)))
gdp_pc <- subset(gdp_pc, select=c("iso3c", "date", "6.0.GDPpc_constant"))

setnames(gdp_pc, old = c("iso3c", "date", "6.0.GDPpc_constant"),
         new=c("iso3", "year", "gdp_pc"))
gdp_pc <- subset(gdp_pc, year > 1979)

#### for now assume all annual data is set in month 1
gdp_pc$month <- 1

df_raw_covariates <- merge(df_raw_covariates, gdp_pc, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
gdp_pc <- NULL


## -----------------------------------------------------------   
## under-5 mortality rate

u5mr <- wbstats::wb_data('HF.DYN.MORT')
u5mr <- data.table(subset(u5mr, !is.na(HF.DYN.MORT)))
u5mr <- subset(u5mr, select=c("iso3c", "date", "HF.DYN.MORT"))

setnames(u5mr, old = c("iso3c", "date", "HF.DYN.MORT"),
         new=c("iso3", "year", "u5mr"))
u5mr <- subset(u5mr, year > 1979)

#### for now assume all annual data is set in month 1
u5mr$month <- 1

df_raw_covariates <- merge(df_raw_covariates, u5mr, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
u5mr <- NULL


## -----------------------------------------------------------   
## hiv prevalence

hiv <- wbstats::wb_data('SH.DYN.AIDS.HG.ZS')
hiv <- data.table(subset(hiv, !is.na(SH.DYN.AIDS.HG.ZS)))
hiv <- subset(hiv, select=c("iso3c", "date", "SH.DYN.AIDS.HG.ZS"))

setnames(hiv, old = c("iso3c", "date", "SH.DYN.AIDS.HG.ZS"),
         new=c("iso3", "year", "hiv"))
hiv <- subset(hiv, year > 1979)

#### for now assume all annual data is set in month 1
hiv$month <- 1

df_raw_covariates <- merge(df_raw_covariates, hiv, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
hiv <- NULL


## -----------------------------------------------------------   
## malaria incidence

malaria <- wbstats::wb_data('SH.MLR.INCD.P3')
malaria <- data.table(subset(malaria, !is.na(SH.MLR.INCD.P3)))
malaria <- subset(malaria, select=c("iso3c", "date", "SH.MLR.INCD.P3"))

setnames(malaria, old = c("iso3c", "date", "SH.MLR.INCD.P3"),
         new=c("iso3", "year", "malaria"))
malaria <- subset(malaria, year > 1979)

#### for now assume all annual data is set in month 1
malaria$month <- 1

df_raw_covariates <- merge(df_raw_covariates, malaria, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
malaria <- NULL


## -----------------------------------------------------------   
## human development index (hdi)

hdi <- wbstats::wb_data('UNDP.HDI.XD')
hdi <- data.table(subset(hdi, !is.na(UNDP.HDI.XD)))
hdi <- subset(hdi, select=c("iso3c", "date", "UNDP.HDI.XD"))

setnames(hdi, old = c("iso3c", "date", "UNDP.HDI.XD"),
         new=c("iso3", "year", "hdi"))
hdi <- subset(hdi, year > 1979)

#### for now assume all annual data is set in month 1
hdi$month <- 1

df_raw_covariates <- merge(df_raw_covariates, hdi, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
hdi <- NULL


## -----------------------------------------------------------   
## stunting prevalence

stunting <- wbstats::wb_data('HF.STA.STNT.ZS')
stunting <- data.table(subset(stunting, !is.na(HF.STA.STNT.ZS)))
stunting <- subset(stunting, select=c("iso3c", "date", "HF.STA.STNT.ZS"))

setnames(stunting, old = c("iso3c", "date", "HF.STA.STNT.ZS"),
         new=c("iso3", "year", "stunting"))
stunting <- subset(stunting, year > 1979)

#### for now assume all annual data is set in month 1
stunting$month <- 1

df_raw_covariates <- merge(df_raw_covariates, stunting, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## land dispute resolution index

land_dispute_index <- wbstats::wb_data('IC.REG.PRRT.LAND.DISP.XD.08.DB1619')
land_dispute_index <- data.table(subset(land_dispute_index, !is.na(IC.REG.PRRT.LAND.DISP.XD.08.DB1619)))
land_dispute_index <- subset(land_dispute_index, select=c("iso3c", "date", "IC.REG.PRRT.LAND.DISP.XD.08.DB1619"))

setnames(land_dispute_index, old = c("iso3c", "date", "IC.REG.PRRT.LAND.DISP.XD.08.DB1619"),
         new=c("iso3", "year", "land_dispute_index"))
land_dispute_index <- subset(land_dispute_index, year > 1979)

#### for now assume all annual data is set in month 1
land_dispute_index$month <- 1

df_raw_covariates <- merge(df_raw_covariates, land_dispute_index, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## battle-related mortality rate

battle_mr <- wbstats::wb_data('VC.BTL.DETH')
battle_mr <- data.table(subset(battle_mr, !is.na(VC.BTL.DETH)))
battle_mr <- subset(battle_mr, select=c("iso3c", "date", "VC.BTL.DETH"))

setnames(battle_mr, old = c("iso3c", "date", "VC.BTL.DETH"),
         new=c("iso3", "year", "battle_mr"))
battle_mr <- subset(battle_mr, year > 1979)

#### for now assume all annual data is set in month 1
battle_mr$month <- 1

df_raw_covariates <- merge(df_raw_covariates, battle_mr, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
df_raw_covariates$battle_mr <- df_raw_covariates$battle_mr / df_raw_covariates$pop_size


## -----------------------------------------------------------   
## idp rate

idp <- wbstats::wb_data('VC.IDP.TOCV')
idp <- data.table(subset(idp, !is.na(VC.IDP.TOCV)))
idp <- subset(idp, select=c("iso3c", "date", "VC.IDP.TOCV"))

setnames(idp, old = c("iso3c", "date", "VC.IDP.TOCV"),
         new=c("iso3", "year", "idp"))
idp <- subset(idp, year > 1979)

#### for now assume all annual data is set in month 1
idp$month <- 1

df_raw_covariates <- merge(df_raw_covariates, idp, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
df_raw_covariates$idp <- df_raw_covariates$idp / df_raw_covariates$pop_size


## -----------------------------------------------------------   
## wasting prevalence

wasting <- wbstats::wb_data('SH.STA.WAST.ZS')
wasting <- data.table(subset(wasting, !is.na(SH.STA.WAST.ZS)))
wasting <- subset(wasting, select=c("iso3c", "date", "SH.STA.WAST.ZS"))

setnames(wasting, old = c("iso3c", "date", "SH.STA.WAST.ZS"),
         new=c("iso3", "year", "wasting"))
wasting <- subset(wasting, year > 1979)

#### for now assume all annual data is set in month 1
wasting$month <- 1

df_raw_covariates <- merge(df_raw_covariates, wasting, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## underweight prevalence

underweight <- wbstats::wb_data('HF.STA.MALN.ZS')
underweight <- data.table(subset(underweight, !is.na(HF.STA.MALN.ZS)))
underweight <- subset(underweight, select=c("iso3c", "date", "HF.STA.MALN.ZS"))

setnames(underweight, old = c("iso3c", "date", "HF.STA.MALN.ZS"),
         new=c("iso3", "year", "underweight"))
underweight <- subset(underweight, year > 1979)

#### for now assume all annual data is set in month 1
underweight$month <- 1

df_raw_covariates <- merge(df_raw_covariates, underweight, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## health expenditure

health_expenditure <- wbstats::wb_data('SH.XPD.CHEX.GD.ZS')
health_expenditure <- data.table(subset(health_expenditure, !is.na(SH.XPD.CHEX.GD.ZS)))
health_expenditure <- subset(health_expenditure, select=c("iso3c", "date", "SH.XPD.CHEX.GD.ZS"))

setnames(health_expenditure, old = c("iso3c", "date", "SH.XPD.CHEX.GD.ZS"),
         new=c("iso3", "year", "health_expenditure"))
health_expenditure <- subset(health_expenditure, year > 1979)

#### for now assume all annual data is set in month 1
health_expenditure$month <- 1

df_raw_covariates <- merge(df_raw_covariates, health_expenditure, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## RCV introduction

rcv_intro <- readxl::read_excel(paste0(data_downloads_path, '/Introduction of Rubella vaccine 2024-022-03 16-58 UTC.xlsx'))
rcv_intro$DESCRIPTION <- NULL
rcv_intro$COUNTRYNAME <- NULL
rcv_intro$WHO_REGION <- NULL

colnames(rcv_intro) <- c("iso3", 'year', 'rcv_intro')
rcv_intro$rcv_intro <- ifelse(rcv_intro$rcv_intro %in% c('Yes', 'Yes (P)'), 1, 
                              ifelse(rcv_intro$rcv_intro == 'No', 0, NA))
rcv_intro$month <- 1
df_raw_covariates <- merge(df_raw_covariates, rcv_intro, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## MCV2 introduction

mcv2_intro <- readxl::read_excel(paste0(data_downloads_path,'Introduction of Measles-containing vaccine 2nd dose 2024-022-03 16-59 UTC.xlsx'))
mcv2_intro$DESCRIPTION <- NULL
mcv2_intro$COUNTRYNAME <- NULL
mcv2_intro$WHO_REGION <- NULL

colnames(mcv2_intro) <- c("iso3", 'year', 'mcv2_intro')
mcv2_intro$mcv2_intro <- ifelse(mcv2_intro$mcv2_intro %in% c('Yes', 'Yes (P)'), 1, 
                                ifelse(mcv2_intro$mcv2_intro == 'No', 0, NA))
mcv2_intro$month <- 1
df_raw_covariates <- merge(df_raw_covariates, mcv2_intro, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## vaccine supply 

vaccine_supply <- readxl::read_excel(paste0(data_downloads_path,'Vaccine supply and logistics 2024-025-03 13-0 UTC.xlsx'))
vaccine_supply$INDCAT_DESCRIPTION <- NULL
vaccine_supply$COUNTRYNAME <- NULL
vaccine_supply$WHO_REGION <- NULL

#### district stock-out
district_stockout <- subset(vaccine_supply, DESCRIPTION == 'Did a vaccine stock-out occur at the district level of Measles containing vaccines?')
district_stockout <- subset(district_stockout, select=c('ISO_3_CODE', 'YEAR', 'VALUE'))
colnames(district_stockout) <- c("iso3", 'year', 'district_stockout')
district_stockout$district_stockout <- ifelse(district_stockout$district_stockout == 'Yes', 1, 
                                              ifelse(district_stockout$district_stockout == 'No', 0, NA))
district_stockout$month <- 1
df_raw_covariates <- merge(df_raw_covariates, district_stockout, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

#### national stock-out
national_stockout <- subset(vaccine_supply, DESCRIPTION == 'Was there a stock-out at the national level of Measles containing vaccines?')
national_stockout <- subset(national_stockout, select=c('ISO_3_CODE', 'YEAR', 'VALUE'))
colnames(national_stockout) <- c("iso3", 'year', 'national_stockout')
national_stockout$national_stockout <- ifelse(national_stockout$national_stockout == 'Yes', 1, 
                                              ifelse(national_stockout$national_stockout == 'No', 0, NA))
national_stockout$month <- 1
df_raw_covariates <- merge(df_raw_covariates, national_stockout, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

#### stock-out duration
stockout_duration <- subset(vaccine_supply, DESCRIPTION == 'What was the duration of stock-out in months of Measles containing vaccines?')
stockout_duration <- subset(stockout_duration, select=c('ISO_3_CODE', 'YEAR', 'VALUE'))
colnames(stockout_duration) <- c("iso3", 'year', 'stockout_duration')
stockout_duration$stockout_duration <- ifelse(stockout_duration$stockout_duration == '4-6', 6, stockout_duration$stockout_duration)
stockout_duration$stockout_duration <- as.numeric(stockout_duration$stockout_duration)
stockout_duration$month <- 1
df_raw_covariates <- merge(df_raw_covariates, stockout_duration, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## vitamin A 

vitA <- readxl::read_excel(paste0(data_downloads_path, 'Vitamin A 2024-022-03 17-1 UTC.xlsx'))
vitA$INDCAT_DESCRIPTION <- NULL
vitA$COUNTRYNAME <- NULL
vitA$WHO_REGION <- NULL

vitA_routine <- subset(vitA, DESCRIPTION == 'Are Vitamin A supplements distributed with routine services?')
vitA_routine <- subset(vitA_routine, select = c('ISO_3_CODE', 'YEAR', 'VALUE'))
colnames(vitA_routine) <- c("iso3", 'year', 'vitA_routine')
vitA_routine$vitA_routine <- ifelse(vitA_routine$vitA_routine == 'Yes', 1, 
                                    ifelse(vitA_routine$vitA_routine == 'No', 0, NA))
vitA_routine$month <- 1
df_raw_covariates <- merge(df_raw_covariates, vitA_routine, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
vitA_routine <- NULL

vitA_campgain <- subset(vitA, DESCRIPTION == 'Are Vitamin A supplements integrated with campaigns?')
vitA_campgain <- subset(vitA_campgain, select = c('ISO_3_CODE', 'YEAR', 'VALUE'))
colnames(vitA_campgain) <- c("iso3", 'year', 'vitA_campgain')
vitA_campgain$vitA_campgain <- ifelse(vitA_campgain$vitA_campgain == 'Yes', 1, 
                                      ifelse(vitA_campgain$vitA_campgain == 'No', 0, NA))
vitA_campgain$month <- 1
df_raw_covariates <- merge(df_raw_covariates, vitA_campgain, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
vitA_campgain <- NULL
vitA <- NULL

#### add GBD covariates
vitA_GBD <- fread(paste0(data_downloads_path,'/IHME-GBD_2019_DATA-4a7f259d-1.csv'))

vitA_deaths <- subset(vitA_GBD, measure == "Deaths" & metric == "Rate")
vitA_deaths <- subset(vitA_deaths, select = c('location', 'year', 'val'))
colnames(vitA_deaths) <- c('country', 'year', 'vitA_death_rate')

vitA_dalys <- subset(vitA_GBD, measure == "DALYs (Disability-Adjusted Life Years)" & metric == "Rate")
vitA_dalys <- subset(vitA_dalys, select = c('location', 'year', 'val'))
colnames(vitA_dalys) <- c('country', 'year', 'vitA_daly_rate')

vitA_GBD <- merge(vitA_deaths, vitA_dalys, by=c('country', 'year'))
vitA_GBD <- merge(vitA_GBD, iso3_lookup, by = 'country')
vitA_GBD$month <- 1
vitA_GBD$country <- NULL

df_raw_covariates <- merge(df_raw_covariates, vitA_GBD, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
vitA_GBD <- NULL
vitA_deaths <- NULL
vitA_dalys <- NULL


## -----------------------------------------------------------   
## school-based immunization 

school_based_immunization <- readxl::read_excel(paste0(data_downloads_path,'Immunization_at_school.xlsx'), sheet = 2)
school_based_immunization <- subset(school_based_immunization, VACCINE %in% c("M/R", "MCV", "MCV1", "MCV2", "MCV4", "Measles", "Measles /Rubella",                                                                                                                                                                                                          
                                                                              "Measles and mumps vaccine", "Measles Mumps Rubella",  "Measles Rubella",  "MEASLES RUBELLA",   
                                                                              "Measles, mumps and rubella vaccine", "Measles/Rubella", "Measles2", "MMR", "MMR  GP Administered",                                                                                                                                                                                                      
                                                                              "MMR  Public Health Administered", "MMR (2nd dose)", "MMR (catch-up)", "MMR (Priorix)", 
                                                                              "MMR (secind dose)", "MMR 2", "MMR 2nd dose", "MMR 2nd dose (MMR2)", "MMR- grade1", "MMR-2",                                                                                                                                                                                                                     
                                                                              "MMR-GRADE 1",  "MMR, IPV, Td",   "MMR/MEASLES", "MMR/VARICELLA", "MMR2",  "MMR2 (catch-up pgm)",                                                                                                                                                                                                       
                                                                              "MMR2 (combined vaccine)", "MMRV", "MR", "MR 2", "MR 2 dose", "MR 2nd dose", "MR2", "NB. MMR2 Catch Up"))          

school_based_immunization <- subset(school_based_immunization, as.numeric(VACCINATED_IN_SCHL) > 0)
school_based_immunization <- subset(school_based_immunization, select=c('COUNTRY', 'YEAR'))
colnames(school_based_immunization) <- c('iso3', 'year')
school_based_immunization <- unique(school_based_immunization)
school_based_immunization$school_based_immunization <- 1

school_based_immunization$month <- 1

df_raw_covariates <- merge(df_raw_covariates, school_based_immunization, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

df_raw_covariates$school_based_immunization <- ifelse(is.na(df_raw_covariates$school_based_immunization) & df_raw_covariates$month == 1 & df_raw_covariates$year > 2007, 0, df_raw_covariates$school_based_immunization)

school_based_immunization <- NULL


## -----------------------------------------------------------   
## estimated measles infections

estimated_measles_cases <- fread(paste0(data_downloads_path,'long_cases_using_orig_WHO22_best_models.csv'))

#### estimated infections
estimated_measles_cases <- aggregate(estimated_measles_cases$cases, by=list(estimated_measles_cases$country, estimated_measles_cases$year), FUN = 'sum')
colnames(estimated_measles_cases) <- c('iso3', 'year', 'estimated_infections')
estimated_measles_cases$month <- 1

df_raw_covariates <- merge(df_raw_covariates, estimated_measles_cases, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

#### proportion of estimated infections < 5 yo
estimated_measles_cases <- fread(paste0(data_downloads_path,'long_cases_using_orig_WHO22_best_models.csv'))
estimated_measles_cases$agecat <- ifelse(estimated_measles_cases$age < 5, 1, 2)

estimated_measles_cases_under5 <- subset(estimated_measles_cases, agecat == 1)
estimated_measles_cases_over5 <- subset(estimated_measles_cases, agecat == 2)

estimated_measles_cases_under5 <- aggregate(estimated_measles_cases_under5$cases, by=list(estimated_measles_cases_under5$country, estimated_measles_cases_under5$year), FUN = 'sum')
colnames(estimated_measles_cases_under5) <- c('iso3', 'year', 'estimated_infections_under5')

estimated_measles_cases_over5 <- aggregate(estimated_measles_cases_over5$cases, by=list(estimated_measles_cases_over5$country, estimated_measles_cases_over5$year), FUN = 'sum')
colnames(estimated_measles_cases_over5) <- c('iso3', 'year', 'estimated_infections_over5')

estimated_measles_cases_agecat5 <- merge(estimated_measles_cases_over5, estimated_measles_cases_under5, by=c('iso3', 'year'))
estimated_measles_cases_agecat5$prop_estimated_infections_under5 <- estimated_measles_cases_agecat5$estimated_infections_under5 / (estimated_measles_cases_agecat5$estimated_infections_under5 + estimated_measles_cases_agecat5$estimated_infections_over5)
estimated_measles_cases_agecat5 <- subset(estimated_measles_cases_agecat5, select= c('iso3', 'year', 'prop_estimated_infections_under5'))
estimated_measles_cases_agecat5$month <- 1

df_raw_covariates <- merge(df_raw_covariates, estimated_measles_cases_agecat5, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

#### proportion of estimated infections < 10 yo
estimated_measles_cases <- fread(paste0(data_downloads_path,'long_cases_using_orig_WHO22_best_models.csv'))
estimated_measles_cases$agecat <- ifelse(estimated_measles_cases$age < 10, 1, 2)

estimated_measles_cases_under10 <- subset(estimated_measles_cases, agecat == 1)
estimated_measles_cases_over10 <- subset(estimated_measles_cases, agecat == 2)

estimated_measles_cases_under10 <- aggregate(estimated_measles_cases_under10$cases, by=list(estimated_measles_cases_under10$country, estimated_measles_cases_under10$year), FUN = 'sum')
colnames(estimated_measles_cases_under10) <- c('iso3', 'year', 'estimated_infections_under10')

estimated_measles_cases_over10 <- aggregate(estimated_measles_cases_over10$cases, by=list(estimated_measles_cases_over10$country, estimated_measles_cases_over10$year), FUN = 'sum')
colnames(estimated_measles_cases_over10) <- c('iso3', 'year', 'estimated_infections_over10')

estimated_measles_cases_agecat10 <- merge(estimated_measles_cases_over10, estimated_measles_cases_under10, by=c('iso3', 'year'))
estimated_measles_cases_agecat10$prop_estimated_infections_under10 <- estimated_measles_cases_agecat10$estimated_infections_under10 / (estimated_measles_cases_agecat10$estimated_infections_under10 + estimated_measles_cases_agecat10$estimated_infections_over10)
estimated_measles_cases_agecat10 <- subset(estimated_measles_cases_agecat10, select= c('iso3', 'year', 'prop_estimated_infections_under10'))
estimated_measles_cases_agecat10$month <- 1

df_raw_covariates <- merge(df_raw_covariates, estimated_measles_cases_agecat10, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
estimated_measles_cases <- NULL

## -----------------------------------------------------------   
## estimated measles susceptibility

data('popAge1dt', package = "wpp2022", envir = environment())
estimated_susceptibility <- fread(paste0(data_downloads_path, 'long_susc_using_orig_WHO22_best_models.csv'))

#### proportion < 5 yo susceptible
estimated_susceptibility <- subset(estimated_susceptibility, age < 5)
estimated_susceptibility <- aggregate(estimated_susceptibility$susceptibles, by=list(estimated_susceptibility$country, estimated_susceptibility$year), FUN = 'sum')
colnames(estimated_susceptibility) <- c('iso3', 'year', 'estimated_susceptibility')

under5s <- subset(popAge1dt, age < 5)
under5s <- aggregate(under5s$pop, by=list(under5s$name, under5s$year), FUN = 'sum')
colnames(under5s) <- c('country', 'year', 'pop')
under5s <- merge(under5s, iso3_lookup, by='country')

estimated_susceptibility <- merge(estimated_susceptibility, under5s, by=c('iso3', 'year'))
estimated_susceptibility$prop_susceptible_under5 <- estimated_susceptibility$estimated_susceptibility / estimated_susceptibility$pop
estimated_susceptibility <- subset(estimated_susceptibility, select = c('iso3', 'year', 'prop_susceptible_under5'))
estimated_susceptibility$month <- 1

df_raw_covariates <- merge(df_raw_covariates, estimated_susceptibility, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

#### proportion < 10 yo susceptible
estimated_susceptibility <- fread(paste0(data_downloads_path, 'long_susc_using_orig_WHO22_best_models.csv'))
estimated_susceptibility <- subset(estimated_susceptibility, age < 10)
estimated_susceptibility <- aggregate(estimated_susceptibility$susceptibles, by=list(estimated_susceptibility$country, estimated_susceptibility$year), FUN = 'sum')
colnames(estimated_susceptibility) <- c('iso3', 'year', 'estimated_susceptibility')

under10s <- subset(popAge1dt, age < 10)
under10s <- aggregate(under10s$pop, by=list(under10s$name, under10s$year), FUN = 'sum')
colnames(under10s) <- c('country', 'year', 'pop')
under10s <- merge(under10s, iso3_lookup, by='country')

estimated_susceptibility <- merge(estimated_susceptibility, under10s, by=c('iso3', 'year'))
estimated_susceptibility$prop_susceptible_under10 <- estimated_susceptibility$estimated_susceptibility / estimated_susceptibility$pop
estimated_susceptibility <- subset(estimated_susceptibility, select = c('iso3', 'year', 'prop_susceptible_under10'))
estimated_susceptibility$month <- 1

df_raw_covariates <- merge(df_raw_covariates, estimated_susceptibility, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
estimated_susceptibility <- NULL
popAge1dt <- NULL

## -----------------------------------------------------------   
## MCV1 admin1 range 

mcv1_adm1 <- readxl::read_excel(paste0(data_downloads_path, 'MCV1_coverage_ad1_ad2_2000-2022.xlsx'), sheet = 1)

mcv1_admin1_range <- data.table()
for(c in unique(mcv1_adm1$ISO3)){
  for(y in unique(mcv1_adm1$Year)){
    mcv1_temp <- subset(mcv1_adm1, ISO3 == c & Year == y)
    
    to_add <- cbind(c, y, range(mcv1_temp$`MCV1 - Mean`)[2] - range(mcv1_temp$`MCV1 - Mean`)[1])
    mcv1_admin1_range <- rbind(mcv1_admin1_range, to_add)
  }
}
setnames(mcv1_admin1_range, old = c('c', 'y', 'V3'), new = c('iso3', 'year', 'mcv1_adm1_range'))
mcv1_admin1_range$month <- 1
mcv1_admin1_range$year <- as.numeric(mcv1_admin1_range$year)

df_raw_covariates <- merge(df_raw_covariates, mcv1_admin1_range, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
mcv1_admin1_range <- NULL

## -----------------------------------------------------------   
## MCV2 admin1 range

mcv2_adm1 <- readxl::read_excel(paste0(data_downloads_path, 'MCV2_coverage_ad1_ad2_2000-2022.xlsx'), sheet = 1)

mcv2_admin1_range <- data.table()
for(c in unique(mcv2_adm1$ISO3)){
  for(y in unique(mcv2_adm1$Year)){
    mcv2_temp <- subset(mcv2_adm1, ISO3 == c & Year == y)
    
    to_add <- cbind(c, y, range(mcv2_temp$`MCV2 - Mean`)[2] - range(mcv2_temp$`MCV2 - Mean`)[1])
    mcv2_admin1_range <- rbind(mcv2_admin1_range, to_add)
  }
}
setnames(mcv2_admin1_range, old = c('c', 'y', 'V3'), new = c('iso3', 'year', 'mcv2_adm1_range'))
mcv2_admin1_range$month <- 1
mcv2_admin1_range$year <- as.numeric(mcv2_admin1_range$year)

df_raw_covariates <- merge(df_raw_covariates, mcv2_admin1_range, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
mcv2_admin1_range <- NULL

## -----------------------------------------------------------   
## school term

school_term <- fread(paste0(data_downloads_path, 'school_terms.csv'), header = T)
school_term$Notes <- NULL
school_term <- melt(school_term, id.vars = 'country')
colnames(school_term) <- c('country', 'month', 'school_term')
school_term$month <- as.integer(school_term$month)
school_term <- merge(school_term, iso3_lookup, by='country')
school_term$country <- NULL

school_term_all <- data.table()
for(y in 1980:2024){
  school_term_yr <- copy(school_term)
  school_term_yr$year <- y
  
  school_term_all <- rbind(school_term_all, school_term_yr)
}
  
df_raw_covariates <- merge(df_raw_covariates, school_term_all, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
school_term_all <- NULL

## -----------------------------------------------------------   
## position on canonical path

load(paste0(data_downloads_path, 'position_path_upto2023.RData'))

position_canonical_path <- copy(d1)
position_canonical_path <- subset(position_canonical_path, select=c('Country', 'Year', 'closest'))
colnames(position_canonical_path) <- c('country', 'year', 'position_canonical_path')

position_canonical_path <- merge(position_canonical_path, iso3_lookup, by='country')
position_canonical_path$country <- NULL

position_canonical_path$month <- 1

df_raw_covariates <- merge(df_raw_covariates, position_canonical_path, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
position_canonical_path <- NULL

# ## -----------------------------------------------------------   
# ## precipitation
# 
# library(raster)
# library(geodata)
# library(terra)
# 
# all_month_yr <- data.table()
# for(y in 1981:2024){
#   message(y)
# 
#   if(y < 1999){
#     pop <- raster(paste0(data_downloads_path, "worldpop_rasters/ppp_2000_1km_Aggregated.tif"))
#   }else if(y >= 2000 & y < 2021){
#     pop <- raster(paste0(data_downloads_path, "worldpop_rasters/ppp_", y, "_1km_Aggregated.tif"))
#   }else if(y > 2020){
#     pop <- raster(paste0(data_downloads_path, "worldpop_rasters/ppp_2020_1km_Aggregated.tif"))
#   }
#   
#   if(y < 2024){
#     for(m in 1:12){
#       if(m < 10){
#         precip <- raster(paste0(data_downloads_path, "chirps_monthly_precipitation_rasters/chirps-v2.0.", y, ".0", m,".tif"))
#       }else{
#         precip <- raster(paste0(data_downloads_path, "chirps_monthly_precipitation_rasters/chirps-v2.0.", y, ".", m,".tif"))
#       }
#       
#       precip <- precip[[1]]
#       values(precip)[values(precip) < 0] = NA
#       
#       shp_ad0 <- (world(resolution=5, level=0, 'shp_ad0', version="latest"))
#       shp_ad0 <- as(shp_ad0, 'Spatial')
#       
#       pop2 <- crop(pop, extent(precip))
#       extent(precip) <- extent(pop2)
#       
#       values(pop2)[is.na(values(pop2))] = 0
#       
#       shp_ad0$total_precipitation <- exactextractr::exact_extract(precip, shp_ad0, 'sum')
#       shp_ad0$mean_precipitation <- exactextractr::exact_extract(x = precip, y = shp_ad0, fun = 'weighted_mean', weights = pop2)
#       shp_ad0 <- as.data.table(subset(shp_ad0, select= c("GID_0", "total_precipitation", "mean_precipitation")))
#       colnames(shp_ad0) <- c('iso3', 'total_precipitation', 'mean_precipitation')
#       
#       shp_ad0$mean_precipitation <- ifelse(shp_ad0$total_precipitation== 0, 0, shp_ad0$mean_precipitation)
#       
#       shp_ad0$year <- y
#       shp_ad0$month <- m
#       all_month_yr <- rbind(all_month_yr, shp_ad0)
#     }  
#   }
#   
#   if(y == 2024){
#     for(m in 1:2){
#       if(m < 10){
#         precip <- raster(paste0(data_downloads_path, "chirps_monthly_precipitation_rasters/chirps-v2.0.", y, ".0", m,".tif"))
#       }else{
#         precip <- raster(paste0(data_downloads_path, "chirps_monthly_precipitation_rasters/chirps-v2.0.", y, ".", m,".tif"))
#       }
#       
#       precip <- precip[[1]]
#       values(precip)[values(precip) < 0] = NA
#       
#       shp_ad0 <- (world(resolution=5, level=0, 'shp_ad0', version="latest"))
#       shp_ad0 <- as(shp_ad0, 'Spatial')
#       
#       pop2 <- crop(pop, extent(precip))
#       extent(precip) <- extent(pop2)
#       
#       values(pop2)[is.na(values(pop2))] = 0
#       
#       shp_ad0$total_precipitation <- exactextractr::exact_extract(precip, shp_ad0, 'sum')
#       shp_ad0$mean_precipitation <- exactextractr::exact_extract(x = precip, y = shp_ad0, fun = 'weighted_mean', weights = pop2)
#       shp_ad0 <- as.data.table(subset(shp_ad0, select= c("GID_0", "total_precipitation", "mean_precipitation")))
#       colnames(shp_ad0) <- c('iso3', 'total_precipitation', 'mean_precipitation')
#       
#       shp_ad0$mean_precipitation <- ifelse(shp_ad0$total_precipitation== 0, 0, shp_ad0$mean_precipitation)
#       
#       shp_ad0$year <- y
#       shp_ad0$month <- m
#       all_month_yr <- rbind(all_month_yr, shp_ad0)
#     }  
#   }
# }
# 
# df_raw_covariates <- merge(df_raw_covariates, all_month_yr, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
# precip <- NULL
# pop <- NULL
# pop2 <- NULL
# 
# ## -----------------------------------------------------------   
# ## temperature
# library(ncdf4) 
# 
# all_month_yr <- data.table()
# for(y in 1980:2024){
#   message(y)
#   
#   temp_nc <- nc_open(paste0(data_downloads_path, "copernicus_temperature_data/data_", y, ".nc"))
#   temp <- ncvar_get(temp_nc, "t2m")
#   lon <- as.vector(ncvar_get(temp_nc, "longitude")) #- 180
#   lat <- ncvar_get(temp_nc, "latitude")
#   time <- ncvar_get(temp_nc, 'time')
#   lat <- sort(lat, decreasing = TRUE)  # Ensure latitudes are sorted in the correct order
#   
#   if(y < 1999){
#     pop <- raster(paste0(data_downloads_path, "worldpop_rasters/ppp_2000_1km_Aggregated.tif"))
#   }else if(y >= 2000 & y < 2021){
#     pop <- raster(paste0(data_downloads_path, "worldpop_rasters/ppp_", y, "_1km_Aggregated.tif"))
#   }else if(y > 2020){
#     pop <- raster(paste0(data_downloads_path, "worldpop_rasters/ppp_2020_1km_Aggregated.tif"))
#   }
#   pop3 <- aggregate(pop, fact = 10, fun=sum)
#   
#   if(y < 2024){
#     for(m in 1:12){
#       message(m)
#       r <- raster(t(temp[,,m]), xmn = 0, xmx = 360, ymn = min(lat), ymx = max(lat))
#       
#       r2 <- crop(r, extent(c(180,360,-90,90)))
#       r3 <- crop(r, extent(c(0.1,180,-90,90)))
#       extent(r2) <- c(-180,0,-90,90)
#       temperature <- do.call(merge, c(r2,r3))
#       
#       temperature <- temperature[[1]]
#       values(temperature)[values(temperature) < 0] = NA
#       crs(temperature) <- crs(pop)
#       
#       shp_ad0 <- (world(resolution=5, level=0, 'shp_ad0', version="latest"))
#       shp_ad0 <- as(shp_ad0, 'Spatial')
#       
#       temperature <- crop(temperature, extent(pop3))
#       extent(temperature) <- extent(pop3)
#       
#       temp2 <- terra::resample(temperature, pop3)
#       values(pop3)[is.na(values(pop3))] = 0
#       
#       shp_ad0$mean_temperature <- exactextractr::exact_extract(x = temp2, y = shp_ad0, fun = 'weighted_mean', weights = pop3)
#       shp_ad0 <- as.data.table(subset(shp_ad0, select= c("GID_0", "mean_temperature")))
#       colnames(shp_ad0) <- c('iso3', 'mean_temperature')
#       shp_ad0$mean_temperature <- shp_ad0$mean_temperature - 273.2
#       
#       shp_ad0$year <- y
#       shp_ad0$month <- m
#       all_month_yr <- rbind(all_month_yr, shp_ad0)    
#     }  
#   }
#   
#   if(y == 2024){
#     for(m in 1:4){
#       message(m)
#       r <- raster(t(temp[,,m]), xmn = 0, xmx = 360, ymn = min(lat), ymx = max(lat))
#       
#       r2 <- crop(r, extent(c(180,360,-90,90)))
#       r3 <- crop(r, extent(c(0.1,180,-90,90)))
#       extent(r2) <- c(-180,0,-90,90)
#       temperature <- do.call(merge, c(r2,r3))
#       
#       temperature <- temperature[[1]]
#       values(temperature)[values(temperature) < 0] = NA
#       crs(temperature) <- crs(pop)
#       
#       shp_ad0 <- (world(resolution=5, level=0, 'shp_ad0', version="latest"))
#       shp_ad0 <- as(shp_ad0, 'Spatial')
#       
#       temperature <- crop(temperature, extent(pop3))
#       extent(temperature) <- extent(pop3)
#       
#       temp2 <- terra::resample(temperature, pop3)
#       values(pop3)[is.na(values(pop3))] = 0
#       
#       shp_ad0$mean_temperature <- exactextractr::exact_extract(x = temp2, y = shp_ad0, fun = 'weighted_mean', weights = pop3)
#       shp_ad0 <- as.data.table(subset(shp_ad0, select= c("GID_0", "mean_temperature")))
#       colnames(shp_ad0) <- c('iso3', 'mean_temperature')
#       shp_ad0$mean_temperature <- shp_ad0$mean_temperature - 273.2
#       
#       shp_ad0$year <- y
#       shp_ad0$month <- m
#       all_month_yr <- rbind(all_month_yr, shp_ad0)    
#     }  
#   }
# }


## -----------------------------------------------------------   
## SIA
#### a. binary, "was there an SIA"
#### b. age start
#### c. age end
#### d. age range
#### e. age campaign coverage

sia <- readxl::read_excel(paste0(data_downloads_path, 'Summary_MR_SIA.xlsx'), sheet = 1, col_names = T)
colnames(sia) <- sia[1,]
sia <- sia[-1,]

sia$year <- sia$Year
sia$start_month <- as.numeric(format(as.Date(sia$`Start date`), "%m"))
sia$end_month <- as.numeric(format(as.Date(sia$`End date`), "%m"))

source('~/Google Drive/My Drive/Gates_MeaslesForecast/updates_jhu/convertAgeSIA.R')
sia$age_start <- NA
sia$age_end <- NA
for(i in 1:dim(sia)[1]){
  message(i)
  sia$age_start[i] <-  convertAgeSIA(sia$`Age group`[i])[1]
  sia$age_end[i] <-  convertAgeSIA(sia$`Age group`[i])[2]
}

sia$age_range <- sia$age_end - sia$age_start
sia$sia_coverage <- sia$`% coverage`

sia <- subset(sia, select = c('ISO code', 'year', 'age_start', 'age_end', 'age_range', 'sia_coverage', 'start_month', 'end_month'))
sia$end_month <- ifelse(is.na(sia$end_month), sia$start_month, sia$end_month)
sia$sia_id <- 1:dim(sia)[1]

`%!in%` = Negate(`%in%`)
sia_long <- data.table()

# Create a sequence of months between start and end months
sia_long <- data.table()
sia <- subset(sia, !is.na(start_month))
for(i in 1:dim(sia)[1]){
  message(i)
  months <- seq(sia$start_month[i], sia$end_month[i])
  
  # Create df_long
  df_long_to_add <- data.frame(iso3 = sia$`ISO code`[i],
                               year = sia$year[i],
                               month = months,
                               age_start = sia$age_start[i],
                               age_end = sia$age_end[i],
                               sia_coverage = sia$sia_coverage[i])  
  
  sia_long <- rbind(sia_long, df_long_to_add)
}


sia_long_final <- data.table()
for(c in 1:length(unique(sia_long$iso3))){
  sia_long_c <- subset(sia_long, iso3 == unique(sia_long$iso3)[c])
  
  for(y in unique(sia_long_c$year)){
    sia_long_c_y <- subset(sia_long_c, year == y)
    
    if(length(which(duplicated(sia_long_c_y$month))) >0){
      
      for(m in unique(sia_long_c_y$month)){
        
        sia_long_c_y_m <- subset(sia_long_c_y, month == m)
        sia_long_c_y_m <- data.frame(cbind(unique(sia_long_c_y_m$iso3), unique(sia_long_c_y_m$year), unique(sia_long_c_y_m$month), min(sia_long_c_y_m$age_start), max(sia_long_c_y_m$age_end), max(sia_long_c_y_m$sia_coverage)))
        colnames(sia_long_c_y_m) <- c('iso3', 'year', 'month', 'age_start', 'age_end', 'sia_coverage')
        sia_long_final <- rbind(sia_long_final, sia_long_c_y_m, fill = T)
      }
    }else{
      colnames(sia_long_c_y) <- c('iso3', 'year', 'month', 'age_start', 'age_end', 'sia_coverage')
      sia_long_final <- rbind(sia_long_final, sia_long_c_y, fill = T)
    }
  }
}

colnames(sia_long_final) <- c('iso3', 'year', 'month', 'sia_age_start', 'sia_age_end', 'sia_coverage')
sia_long_final$sia_occured <- 1

sia_long_final$sia_age_end <- as.numeric(sia_long_final$sia_age_end)
sia_long_final$sia_age_start <- as.numeric(sia_long_final$sia_age_start)
sia_long_final$sia_age_range <- sia_long_final$sia_age_end - sia_long_final$sia_age_start

sia_long_final$year <- as.numeric(sia_long_final$year)
sia_long_final$month <- as.numeric(sia_long_final$month)
df_raw_covariates <- merge(df_raw_covariates, sia_long_final, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## COVID-19 months
df_raw_covariates$covid_months <- 0

df_raw_covariates$covid_months <-ifelse(df_raw_covariates$year %in% c(2020,2021,2022), 1, df_raw_covariates$covid_months)
df_raw_covariates$covid_months <-ifelse(df_raw_covariates$year %in% c(2023) & df_raw_covariates$month %in% c(1,2,3,4,5), 1, df_raw_covariates$covid_months)


## -----------------------------------------------------------   
## save file
df_raw_covariates$region <- NULL
fwrite(df_raw_covariates, file = '~/Google Drive/My Drive/Gates_MeaslesForecast/updates_jhu/df_raw_covariates.csv')

