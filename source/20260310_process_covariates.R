
## -----------------------------------------------------------    
## pull and format covariates for measles forecasting
## last updated: 2 March 2026
##
## -----------------------------------------------------------   
## load required packages
library(openxlsx)
library(data.table)
library(wbstats)
library(rgho)
library(ggplot2)
library(wpp2022)
library(dplyr)
library(googledrive)
library(readxl)
library(data.table)
library(tidyr)
library(scales)
library(DescTools)
library(sf)
library(spdep)
library(giscoR)
library(lubridate)
library("rnaturalearth")
library("rnaturalearthdata")
library(circular)

`%!in%` = Negate(`%in%`)

data_downloads_path <- '~/Google Drive/My Drive/Gates_MeaslesForecast/updates_jhu/data_downloads/'

options(timeout = max(5000, getOption("timeout")))
## -----------------------------------------------------------   
## -----------------------------------------------------------   
## prepare inputs

df_raw_covariates <- data.table()

iso3_lookup <- read.xlsx("https://immunizationdata.who.int/docs/librariesprovider21/measles-and-rubella/407-table-web-measles-cases-by-month.xlsx?sfvrsn=41bda8f6_1",sheet=2)
setnames(iso3_lookup, old=c("Region", "ISO3", "Country", "Year", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), 
         new = c("region", "iso3", "country", "year", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
iso3_lookup <- subset(iso3_lookup, select=c("iso3", "country"))
iso3_lookup <- unique(iso3_lookup)

## -----------------------------------------------------------   
## monthly "provisional" reported measles cases

# monthly_cases <- read.xlsx("https://cdn.who.int/media/docs/default-source/immunization/immunization-coverage/measlescasesbycountrybymonth.xlsx?sfvrsn=1d9af577_111",sheet=2)
monthly_cases <- read.xlsx("https://immunizationdata.who.int/docs/librariesprovider21/measles-and-rubella/407-table-web-measles-cases-by-month.xlsx?sfvrsn=41bda8f6_1",sheet=2)
setnames(monthly_cases, old=c("Region", "ISO3", "Country", "Year", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), 
         new = c("region", "iso3", "country", "year", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

monthly_cases_long <- reshape2::melt(monthly_cases, id.vars=c("region", "iso3", "country", "year"))
setnames(monthly_cases_long, old=c('variable', 'value'), new = c('month', 'reported_cases'))
monthly_cases_long$month <- as.numeric(monthly_cases_long$month)
monthly_cases_long$reported_cases <- abs(as.numeric(monthly_cases_long$reported_cases))

monthly_cases_long$country <- NULL
df_raw_covariates <- cbind(df_raw_covariates, monthly_cases_long)
monthly_cases_long <- NULL


## -----------------------------------------------------------   
## total population size -- with projections

data("pop1dt", package = "wpp2022", envir = environment())
data("popproj1dt", package = "wpp2022", envir = environment())

pop1dt <- rbind(pop1dt, popproj1dt, fill = T)
pop1dt <- subset(pop1dt, year < 2026)

pop_size <- subset(pop1dt, select=c("name", "year", "pop"))
pop_size <- subset(pop_size, year > 1979)
setnames(pop_size, old = c("name","pop"), new=c("country","pop_size"))

pop_size$country <- ifelse(pop_size$country == 'Turkiye', 'Türkiye', pop_size$country)
pop_size$country <- ifelse(pop_size$country == 'Netherlands', 'Netherlands (Kingdom of the)', pop_size$country)
pop_size$country <- ifelse(pop_size$country == "Cote d'Ivoire", "Côte d'Ivoire", pop_size$country)
pop_size$country <- ifelse(pop_size$country == 'United Kingdom', 'United Kingdom of Great Britain and Northern Ireland', pop_size$country)

pop_size <- merge(pop_size, iso3_lookup, by = 'country')
pop_size$country <- NULL

pop_size$month <- 1 #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, pop_size, all.x=T, all.y=T, by=c('iso3','year', 'month'))
pop_size <- NULL
pop1dt <- NULL


## -----------------------------------------------------------   
## annual mcv1 coverage

mcv1 <- readxl::read_excel(paste0(data_downloads_path, 'jrf_release_2025/Measles vaccination coverage 2025-30-07 12-30 UTC.xlsx'))
mcv1$GROUP <- NULL
mcv1$NAME <- NULL
mcv1$ANTIGEN_DESCRIPTION <- NULL
mcv1$COVERAGE_CATEGORY_DESCRIPTION <- NULL
mcv1$TARGET_NUMBER <- NULL
mcv1$DOSES <- NULL

mcv1 <- data.table(subset(mcv1, ANTIGEN == 'MCV1' & COVERAGE_CATEGORY == 'WUENIC'))
mcv1 <- subset(mcv1, select=c("CODE", "YEAR", "COVERAGE"))

setnames(mcv1, old = c("CODE", "YEAR", "COVERAGE"), new=c("iso3", "year", "mcv1"))

mcv1$month <- 1 #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, mcv1, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
mcv1 <- NULL


## -----------------------------------------------------------   
## annual mcv2 coverage

mcv2 <- readxl::read_excel(paste0(data_downloads_path, 'jrf_release_2025/Measles vaccination coverage 2025-30-07 12-30 UTC.xlsx'))
mcv2$GROUP <- NULL
mcv2$NAME <- NULL
mcv2$ANTIGEN_DESCRIPTION <- NULL
mcv2$COVERAGE_CATEGORY_DESCRIPTION <- NULL
mcv2$TARGET_NUMBER <- NULL
mcv2$DOSES <- NULL

mcv2 <- data.table(subset(mcv2, ANTIGEN == 'MCV2' & COVERAGE_CATEGORY == 'WUENIC'))
mcv2 <- subset(mcv2, select=c("CODE", "YEAR", "COVERAGE"))

setnames(mcv2, old = c("CODE", "YEAR", "COVERAGE"), new=c("iso3", "year", "mcv2"))

mcv2$month <- 1 #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, mcv2, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
mcv2 <- NULL


## -----------------------------------------------------------   
## crude birth & death rates

data("misc1dt", package = "wpp2022", envir = environment())

birth_death_rate <- subset(misc1dt, select=c('name', 'year', 'cbr', 'cdr'))
birth_death_rate <- subset(birth_death_rate, year > 1979)               
setnames(birth_death_rate, old=c("name", "cbr", "cdr"), new = c("country", "birth_rate", "death_rate"))

birth_death_rate <- merge(birth_death_rate, iso3_lookup, by = 'country')
birth_death_rate$country <- NULL

birth_death_rate$month <- 1  #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, birth_death_rate, all.x=T, all.y=T, by=c('iso3','year', 'month'))
birth_death_rate <- NULL
misc1dt <- NULL


## -----------------------------------------------------------   
## net migration

data("migration1dt", package = "wpp2022", envir = environment())
net_migration <- subset(migration1dt, select=c('name', 'year', 'mig'))
net_migration <- subset(net_migration, year > 1979 & year < 2025)    
setnames(net_migration, old = c("name", "mig"), new=c("country", "net_migration"))

net_migration <- merge(net_migration, iso3_lookup, by = 'country')
net_migration$country <- NULL

net_migration$month <- 1  #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, net_migration, all.x=T, all.y=T, by=c('iso3','year', 'month'))
net_migration <- NULL


## -----------------------------------------------------------   
## population density

pop_density <- wbstats::wb_data('EN.POP.DNST')
pop_density <- data.table(subset(pop_density, !is.na(EN.POP.DNST)))
pop_density <- subset(pop_density, select=c("iso3c", "date", "EN.POP.DNST"))

setnames(pop_density, old = c("iso3c", "date", "EN.POP.DNST"), new=c("iso3", "year", "pop_density"))
pop_density <- subset(pop_density, year > 1979)

pop_density$month <- 1  #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, pop_density, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
pop_density <- NULL


## -----------------------------------------------------------   
## age distribution 

df_raw_covariates$proportion_under5 <- NULL
df_raw_covariates$proportion_under10 <- NULL
df_raw_covariates$av_age <- NULL

#### average age, % under 5, % under 10
data('popAge1dt', package = "wpp2022", envir = environment())
data("popprojAge1dt", package = "wpp2022", envir = environment())

popAge1dt <- rbind(popAge1dt, popprojAge1dt, fill = T)
popAge1dt <- subset(popAge1dt, year < 2025)

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

age_dist_covs$month <- 1  #assume all annual data is set in month 1
age_dist_covs$year <- as.numeric(age_dist_covs$year)
age_dist_covs$country <- NULL
df_raw_covariates <- merge(df_raw_covariates, age_dist_covs, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
age_dist_covs <- NULL
av_age <- NULL
popAge1dt <- NULL
pop_size <- NULL


## -----------------------------------------------------------   
## gdp per capita

gdp_pc <- wbstats::wb_data("NY.GDP.PCAP.CD")
gdp_pc <- subset(gdp_pc, select = c('iso3c', 'date', 'NY.GDP.PCAP.CD'))
setnames(gdp_pc, old=c("iso3c", "date", "NY.GDP.PCAP.CD"), new=c("iso3", "year", "gdp_pc"))

gdp_pc <- subset(gdp_pc, year > 1979)

gdp_pc$month <- 1  #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, gdp_pc, by=c('iso3', 'year', 'month'), all.x=T, all.y=F)
gdp_pc <- NULL


## -----------------------------------------------------------   
## under-5 mortality rate

u5mr <- wbstats::wb_data("SH.DYN.MORT")
u5mr <- subset(u5mr, select = c('iso3c', 'date', 'SH.DYN.MORT'))
setnames(u5mr, c("iso3c", "date", "SH.DYN.MORT"), c("iso3", "year", "u5mr"))

u5mr <- subset(u5mr, year > 1979)

u5mr$month <- 1  #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, u5mr, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
u5mr <- NULL


## -----------------------------------------------------------   
## hiv prevalence (note for documentation -- confirmed updated with most recent from WB website)

hiv <- wbstats::wb_data('SH.DYN.AIDS.ZS')
hiv <- data.table(subset(hiv, !is.na(SH.DYN.AIDS.ZS)))
hiv <- subset(hiv, select=c("iso3c", "date", "SH.DYN.AIDS.ZS"))

setnames(hiv, old = c("iso3c", "date", "SH.DYN.AIDS.ZS"), new=c("iso3", "year", "hiv"))
hiv <- subset(hiv, year > 1979)

hiv$month <- 1  #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, hiv, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
hiv <- NULL


## -----------------------------------------------------------   
## malaria incidence 

malaria <- wbstats::wb_data('SH.MLR.INCD.P3')
malaria <- data.table(subset(malaria, !is.na(SH.MLR.INCD.P3)))
malaria <- subset(malaria, select=c("iso3c", "date", "SH.MLR.INCD.P3"))

setnames(malaria, old = c("iso3c", "date", "SH.MLR.INCD.P3"), new=c("iso3", "year", "malaria"))
malaria <- subset(malaria, year > 1979)

malaria$month <- 1 #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, malaria, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
malaria <- NULL


## -----------------------------------------------------------   
## human development index (hdi) -- now downloading from here: https://hdr.undp.org/data-center/documentation-and-downloads 

hdi <- fread(paste0(data_downloads_path, 'HDR25_Composite_indices_complete_time_series.csv')) 
hdi <- hdi[c(1:195),c(1, 6:39)]#data.table(subset(hdi, !is.na(UNDP.HDI.XD)))
hdi <- melt(hdi, id.vars='iso3')
hdi$year <- gsub("hdi_", "", hdi$variable)
hdi$variable <- NULL
setnames(hdi, old=c('value'), new=c('hdi'))

hdi$month <- 1  #assume all annual data is set in month 1
hdi$year <- as.numeric(hdi$year)

df_raw_covariates <- merge(df_raw_covariates, hdi, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
hdi <- NULL


## -----------------------------------------------------------   
## stunting prevalence 

stunting <- wbstats::wb_data('HF.STA.STNT.ZS')
stunting <- data.table(subset(stunting, !is.na(HF.STA.STNT.ZS)))
stunting <- subset(stunting, select=c("iso3c", "date", "HF.STA.STNT.ZS"))

setnames(stunting, old = c("iso3c", "date", "HF.STA.STNT.ZS"), new=c("iso3", "year", "stunting"))
stunting <- subset(stunting, year > 1979)

stunting$month <- 1  #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, stunting, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## land dispute resolution index 

land_dispute_index <- wbstats::wb_data('IC.REG.PRRT.LAND.DISP.XD.08.DB1619')
land_dispute_index <- data.table(subset(land_dispute_index, !is.na(IC.REG.PRRT.LAND.DISP.XD.08.DB1619)))

land_dispute_index <- subset(land_dispute_index, country %!in% c("Beijing", "Shanghai", "Rio de Janeiro", "Sao Paulo",
                                                                 "Chittagong", "Dhaka", "Jakarta", "Surabaya", "Mumbai", 
                                                                 "Delhi", "Tokyo", "Osaka", "Mexico City", "Monterrey",
                                                                 "Lagos", "Kano", "Karachi", "Lahore", "Moscow", 
                                                                 "St. Petersburg", "Los Angeles", "New York"))

land_dispute_index <- subset(land_dispute_index, select=c("iso3c", "date", "IC.REG.PRRT.LAND.DISP.XD.08.DB1619"))

setnames(land_dispute_index, old = c("iso3c", "date", "IC.REG.PRRT.LAND.DISP.XD.08.DB1619"), new=c("iso3", "year", "land_dispute_index"))
land_dispute_index <- subset(land_dispute_index, year > 1979)

land_dispute_index$month <- 1  #assume all annual data is set in month 1

df_raw_covariates <- unique(df_raw_covariates)
df_raw_covariates <- merge(df_raw_covariates, land_dispute_index, by = c('iso3', 'year', 'month'), all.x=T, all.y=T)


## -----------------------------------------------------------   
## battle-related mortality rate 

battle_mr <- wbstats::wb_data('VC.BTL.DETH')
battle_mr <- data.table(subset(battle_mr, !is.na(VC.BTL.DETH)))
battle_mr <- subset(battle_mr, select=c("iso3c", "date", "VC.BTL.DETH"))

setnames(battle_mr, old = c("iso3c", "date", "VC.BTL.DETH"), new=c("iso3", "year", "battle_mr"))
battle_mr <- subset(battle_mr, year > 1979)

battle_mr$month <- 1  #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, battle_mr, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
df_raw_covariates$battle_mr <- df_raw_covariates$battle_mr / df_raw_covariates$pop_size


## -----------------------------------------------------------   
## idp rate -- NOTE FOR DOCUMENTATION THAT THIS IS A NEW INDICATOR (new people?) ARCHIVED INDICATOR __ not available

idp <- wbstats::wb_data('VC.IDP.NWCV')
idp <- data.table(subset(idp, !is.na(VC.IDP.NWCV)))
idp <- subset(idp, select=c("iso3c", "date", "VC.IDP.NWCV"))

setnames(idp, old = c("iso3c", "date", "VC.IDP.NWCV"), new=c("iso3", "year", "idp"))
idp <- subset(idp, year > 1979)

idp$month <- 1  #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, idp, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
df_raw_covariates$idp <- ifelse(is.na(df_raw_covariates$idp) & df_raw_covariates$year > 2009, 0, df_raw_covariates$idp )
df_raw_covariates$idp <- df_raw_covariates$idp / df_raw_covariates$pop_size


## -----------------------------------------------------------   
## wasting prevalence

wasting <- wbstats::wb_data('SH.STA.WAST.ZS')
wasting <- data.table(subset(wasting, !is.na(SH.STA.WAST.ZS)))
wasting <- subset(wasting, select=c("iso3c", "date", "SH.STA.WAST.ZS"))

setnames(wasting, old = c("iso3c", "date", "SH.STA.WAST.ZS"), new=c("iso3", "year", "wasting"))
wasting <- subset(wasting, year > 1979)

wasting$month <- 1  #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, wasting, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## underweight prevalence 

underweight <- wbstats::wb_data('HF.STA.MALN.ZS')
underweight <- data.table(subset(underweight, !is.na(HF.STA.MALN.ZS)))
underweight <- subset(underweight, select=c("iso3c", "date", "HF.STA.MALN.ZS"))

setnames(underweight, old = c("iso3c", "date", "HF.STA.MALN.ZS"), new=c("iso3", "year", "underweight"))
underweight <- subset(underweight, year > 1979)

underweight$month <- 1  #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, underweight, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## health expenditure 

health_expenditure <- wbstats::wb_data('SH.XPD.CHEX.GD.ZS')
health_expenditure <- data.table(subset(health_expenditure, !is.na(SH.XPD.CHEX.GD.ZS)))
health_expenditure <- subset(health_expenditure, select=c("iso3c", "date", "SH.XPD.CHEX.GD.ZS"))

setnames(health_expenditure, old = c("iso3c", "date", "SH.XPD.CHEX.GD.ZS"), new=c("iso3", "year", "health_expenditure"))
health_expenditure <- subset(health_expenditure, year > 1979)

health_expenditure$month <- 1  #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, health_expenditure, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## RCV introduction

rcv_intro <- readxl::read_excel(paste0(data_downloads_path, 'jrf_release_2025/Introduction of Rubella vaccine 2025-30-07 15-14 UTC.xlsx'))
rcv_intro$DESCRIPTION <- NULL
rcv_intro$COUNTRYNAME <- NULL
rcv_intro$WHO_REGION <- NULL
rcv_intro$ANTIGEN <- NULL

setnames(rcv_intro, old = c("ISO_3_CODE", "YEAR", "INTRO"), new=c("iso3", 'year', 'rcv_intro'))

rcv_intro$rcv_intro <- ifelse(rcv_intro$rcv_intro %in% c('Yes', 'Yes (P)'), 1, 
                              ifelse(rcv_intro$rcv_intro == 'No', 0, NA))

rcv_intro$month <- 1  #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, rcv_intro, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## MCV2 introduction

mcv2_intro <- read_excel(paste0(data_downloads_path,'jrf_release_2025/Introduction of Measles-containing vaccine 2nd dose 2025-30-07 14-44 UTC.xlsx'))
mcv2_intro$DESCRIPTION <- NULL
mcv2_intro$COUNTRYNAME <- NULL
mcv2_intro$WHO_REGION <- NULL
mcv2_intro$ANTIGEN <- NULL

setnames(mcv2_intro, old = c("ISO_3_CODE", "YEAR", "INTRO"), new=c("iso3", 'year', 'mcv2_intro'))

mcv2_intro$mcv2_intro <- ifelse(mcv2_intro$mcv2_intro %in% c('Yes', 'Yes (P)'), 1, 
                                ifelse(mcv2_intro$mcv2_intro == 'No', 0, NA))
mcv2_intro$month <- 1
df_raw_covariates <- merge(df_raw_covariates, mcv2_intro, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## vaccine supply 

vaccine_supply <- readxl::read_excel(paste0(data_downloads_path,'jrf_release_2025/Vaccine supply and logistics 2025-30-07 13-04 UTC.xlsx'))
vaccine_supply$INDCAT_DESCRIPTION <- NULL
vaccine_supply$COUNTRYNAME <- NULL
vaccine_supply$WHO_REGION <- NULL

#### district stock-out
district_stockout <- subset(vaccine_supply, DESCRIPTION == 'Was there a vaccine stock-out at the district level of Measles containing vaccines?')
district_stockout <- subset(district_stockout, select=c('ISO_3_CODE', 'YEAR', 'VALUE'))
colnames(district_stockout) <- c("iso3", 'year', 'district_stockout')
district_stockout$district_stockout <- ifelse(district_stockout$district_stockout == 'Yes', 1, 
                                              ifelse(district_stockout$district_stockout == 'No', 0, NA))
district_stockout$month <- 1  #assume all annual data is set in month 1
df_raw_covariates <- merge(df_raw_covariates, district_stockout, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

#### national stock-out
national_stockout <- subset(vaccine_supply, DESCRIPTION == 'Was there a stock-out at the national level of Measles containing vaccines?')
national_stockout <- subset(national_stockout, select=c('ISO_3_CODE', 'YEAR', 'VALUE'))
colnames(national_stockout) <- c("iso3", 'year', 'national_stockout')
national_stockout$national_stockout <- ifelse(national_stockout$national_stockout == 'Yes', 1, 
                                              ifelse(national_stockout$national_stockout == 'No', 0, NA))
national_stockout$month <- 1  #assume all annual data is set in month 1
df_raw_covariates <- merge(df_raw_covariates, national_stockout, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

#### stock-out duration
stockout_duration <- subset(vaccine_supply, DESCRIPTION == 'What was the duration of stock-out in months of Measles containing vaccines?')
stockout_duration <- subset(stockout_duration, select=c('ISO_3_CODE', 'YEAR', 'VALUE'))
colnames(stockout_duration) <- c("iso3", 'year', 'stockout_duration')
stockout_duration$stockout_duration <- ifelse(stockout_duration$stockout_duration == '4-6', 6, stockout_duration$stockout_duration)
stockout_duration$stockout_duration <- as.numeric(stockout_duration$stockout_duration)
stockout_duration$month <- 1  #assume all annual data is set in month 1
df_raw_covariates <- merge(df_raw_covariates, stockout_duration, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))


## -----------------------------------------------------------   
## vitamin A 

vitA <- readxl::read_excel(paste0(data_downloads_path, 'jrf_release_2025/Vitamin A 2025-30-07 15-23 UTC.xlsx'))
vitA$INDCAT_DESCRIPTION <- NULL
vitA$COUNTRYNAME <- NULL
vitA$WHO_REGION <- NULL

vitA_routine <- subset(vitA, DESCRIPTION == 'Are Vitamin A supplements distributed with routine services?')
vitA_routine <- subset(vitA_routine, select = c('ISO_3_CODE', 'YEAR', 'VALUE'))
colnames(vitA_routine) <- c("iso3", 'year', 'vitA_routine')
vitA_routine$vitA_routine <- ifelse(vitA_routine$vitA_routine == 'Yes', 1, 
                                    ifelse(vitA_routine$vitA_routine == 'No', 0, NA))
vitA_routine$month <- 1  #assume all annual data is set in month 1
df_raw_covariates <- merge(df_raw_covariates, vitA_routine, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
vitA_routine <- NULL

vitA_campaign <- subset(vitA, DESCRIPTION == 'Are Vitamin A supplements integrated with campaigns?')
vitA_campaign <- subset(vitA_campaign, select = c('ISO_3_CODE', 'YEAR', 'VALUE'))
colnames(vitA_campaign) <- c("iso3", 'year', 'vitA_campaign')
vitA_campaign$vitA_campaign <- ifelse(vitA_campaign$vitA_campaign == 'Yes', 1, 
                                      ifelse(vitA_campaign$vitA_campaign == 'No', 0, NA))
vitA_campaign$month <- 1  #assume all annual data is set in month 1
df_raw_covariates <- merge(df_raw_covariates, vitA_campaign, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
vitA_campaign <- NULL
vitA <- NULL

#### add GBD covariates
vitA_GBD <- fread(paste0(data_downloads_path,'/IHME-GBD_2021_DATA-c45b250c-1.csv')) 

vitA_deaths <- subset(vitA_GBD, measure == "Deaths" & metric == "Rate")
vitA_deaths <- subset(vitA_deaths, select = c('location', 'year', 'val'))
colnames(vitA_deaths) <- c('country', 'year', 'vitA_death_rate')

vitA_dalys <- subset(vitA_GBD, measure == "DALYs (Disability-Adjusted Life Years)" & metric == "Rate")
vitA_dalys <- subset(vitA_dalys, select = c('location', 'year', 'val'))
colnames(vitA_dalys) <- c('country', 'year', 'vitA_daly_rate')

vitA_GBD <- merge(vitA_deaths, vitA_dalys, by=c('country', 'year'))
vitA_GBD <- merge(vitA_GBD, iso3_lookup, by = 'country')
vitA_GBD$month <- 1 #assume all annual data is set in month 1
vitA_GBD$country <- NULL

df_raw_covariates <- merge(df_raw_covariates, vitA_GBD, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
vitA_GBD <- NULL
vitA_deaths <- NULL
vitA_dalys <- NULL


## -----------------------------------------------------------   
## school-based immunization -- NOTE THE DATA LOOK VERY DIFFERENT NOW FOR DOCUMENTATION

school_based_immunization <- readxl::read_excel(paste0(data_downloads_path,'jrf_release_2025/School vaccination 2025-30-07 15-25 UTC.xlsx'), sheet = 1)
# school_based_immunization <- subset(school_based_immunization, VACCINECODE %in% c("M/R", "MCV", "MCV1", "MCV2", "MCV4", "Measles", "Measles /Rubella",                                                                                                                                                                                                          
#                                                                               "Measles and mumps vaccine", "Measles Mumps Rubella",  "Measles Rubella",  "MEASLES RUBELLA",   
#                                                                               "Measles, mumps and rubella vaccine", "Measles/Rubella", "Measles2", "MMR", "MMR  GP Administered",                                                                                                                                                                                                      
#                                                                               "MMR  Public Health Administered", "MMR (2nd dose)", "MMR (catch-up)", "MMR (Priorix)", 
#                                                                               "MMR (secind dose)", "MMR 2", "MMR 2nd dose", "MMR 2nd dose (MMR2)", "MMR- grade1", "MMR-2",                                                                                                                                                                                                                     
#                                                                               "MMR-GRADE 1",  "MMR, IPV, Td",   "MMR/MEASLES", "MMR/VARICELLA", "MMR2",  "MMR2 (catch-up pgm)",                                                                                                                                                                                                       
#                                                                               "MMR2 (combined vaccine)", "MMRV", "MR", "MR 2", "MR 2 dose", "MR 2nd dose", "MR2", "NB. MMR2 Catch Up"))          

school_based_immunization <- subset(school_based_immunization, INDCODE == "SCHOOL_DELIVERED")
school_based_immunization <- subset(school_based_immunization, select=c('ISO_3_CODE', 'YEAR', "VALUE"))
colnames(school_based_immunization) <- c('iso3', 'year', 'school_immunization')
school_based_immunization <- unique(school_based_immunization)
school_based_immunization$school_based_immunization <- ifelse(school_based_immunization$school_immunization == 'Yes', 1, 0)
school_based_immunization$school_immunization <- NULL
school_based_immunization$month <- 1  #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, school_based_immunization, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

df_raw_covariates$school_based_immunization <- ifelse(is.na(df_raw_covariates$school_based_immunization) & df_raw_covariates$month == 1 & df_raw_covariates$year > 2007, 0, df_raw_covariates$school_based_immunization)

school_based_immunization <- NULL


## -----------------------------------------------------------   
## estimated measles infections

estimated_measles_cases <- fread(paste0(data_downloads_path,'long_cases_WHO24_best_models.csv'))

#### estimated infections
estimated_measles_cases <- aggregate(estimated_measles_cases$cases, by=list(estimated_measles_cases$country, estimated_measles_cases$year), FUN = 'sum')
colnames(estimated_measles_cases) <- c('iso3', 'year', 'estimated_infections')
estimated_measles_cases$month <- 1 #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, estimated_measles_cases, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

#### proportion of estimated infections < 5 yo
estimated_measles_cases <- fread(paste0(data_downloads_path, 'estimated_measles_cases.csv'))
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
estimated_measles_cases_agecat5$month <- 1 #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, estimated_measles_cases_agecat5, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

#### proportion of estimated infections < 10 yo
estimated_measles_cases <- fread(paste0(data_downloads_path, 'estimated_measles_cases.csv'))
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
estimated_measles_cases_agecat10$month <- 1 #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, estimated_measles_cases_agecat10, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
estimated_measles_cases <- NULL

## -----------------------------------------------------------   
## estimated measles susceptibility

data('popAge1dt', package = "wpp2022", envir = environment())
estimated_susceptibility <- fread(paste0(data_downloads_path, 'long_susc_WHO24_best_models.csv'))

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
estimated_susceptibility$month <- 1 #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, estimated_susceptibility, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

#### proportion < 10 yo susceptible
estimated_susceptibility <- fread(paste0(data_downloads_path, 'long_susc_WHO24_best_models.csv'))
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
estimated_susceptibility$month <- 1 #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, estimated_susceptibility, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
estimated_susceptibility <- NULL
popAge1dt <- NULL

## -----------------------------------------------------------   
## MCV1 admin1 range 

mcv1_adm1 <- readxl::read_excel(paste0(data_downloads_path, 'MCV1_coverage_ad1_ad2_2000-2024.xlsx'), sheet = 1) 

mcv1_admin1_range <- data.table()
for(c in unique(mcv1_adm1$ISO3)){
  for(y in unique(mcv1_adm1$Year)){
    mcv1_temp <- subset(mcv1_adm1, ISO3 == c & Year == y)
    
    to_add <- cbind(c, y, range(mcv1_temp$`MCV1 - Mean`)[2] - range(mcv1_temp$`MCV1 - Mean`)[1])
    mcv1_admin1_range <- rbind(mcv1_admin1_range, to_add)
  }
}
setnames(mcv1_admin1_range, old = c('c', 'y', 'V3'), new = c('iso3', 'year', 'mcv1_adm1_range'))
mcv1_admin1_range$month <- 1 #assume all annual data is set in month 1
mcv1_admin1_range$year <- as.numeric(mcv1_admin1_range$year)
mcv1_admin1_range$mcv1_adm1_range <- as.numeric(mcv1_admin1_range$mcv1_adm1_range)

df_raw_covariates <- merge(df_raw_covariates, mcv1_admin1_range, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
mcv1_admin1_range <- NULL

## -----------------------------------------------------------   
## MCV2 admin1 range -- TO UPDATE: after next IHME data release (not updated as of 26 August 2025)

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
mcv2_admin1_range$month <- 1 #assume all annual data is set in month 1
mcv2_admin1_range$year <- as.numeric(mcv2_admin1_range$year)
mcv2_admin1_range$mcv2_adm1_range <- as.numeric(mcv2_admin1_range$mcv2_adm1_range)

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
for(y in 1980:2025){
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

position_canonical_path$month <- 1 #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, position_canonical_path, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
position_canonical_path <- NULL

## -----------------------------------------------------------   
## precipitation 

precipitation_old <- fread('~/Google Drive/My Drive/Gates_MeaslesForecast/updates_jhu/df_raw_covariates_20250923.csv')
precipitation_old <- subset(precipitation_old, select=c('iso3', 'month', 'year', 'total_precipitation', 'mean_precipitation'))
precipitation_old <- subset(precipitation_old, !is.na(mean_precipitation) & !is.na(total_precipitation))
### this dataset covers jan 1981 to dec 2024

### this code below updates from jan 2025 to jan 2026
### this same code was used to previously generate estimates from jan 1981 to dec 2024

library(raster)
library(geodata)
library(terra)

all_month_yr <- data.table()
for(y in 2025:2026){
  message(y)

  if(y < 1999){
    pop <- raster(paste0(data_downloads_path, "worldpop_rasters/ppp_2000_1km_Aggregated.tif"))
  }else if(y >= 2000 & y < 2021){
    if(y == 2001){
    	pop <- raster(paste0(data_downloads_path, "worldpop_rasters/ppp_", y, "_1km_Aggregated.tif"))
    }else{
    	pop <- raster(paste0(data_downloads_path, "worldpop_rasters/ppp_", y, "_1km_Aggregated.tif"))
    }
  }else if(y > 2020){
    pop <- raster(paste0(data_downloads_path, "worldpop_rasters/ppp_2020_1km_Aggregated.tif"))
  }
  pop[pop > 50000] <- NA

  if(y < 2026){
    for(m in 1:12){
      message(m)
      if(m < 10){
        precip <- raster(paste0(data_downloads_path, "chirps_monthly_precipitation_rasters/chirps-v2.0.", y, ".0", m,".tif"))
      }else{
        precip <- raster(paste0(data_downloads_path, "chirps_monthly_precipitation_rasters/chirps-v2.0.", y, ".", m,".tif"))
      }

      precip <- precip[[1]]
      #values(precip)[values(precip) < 0] = NA
      precip[precip > 50000] <- NA
      #values(pop)[values(pop) > 50000] = NA

      shp_ad0 <- (world(resolution=5, level=0, 'shp_ad0', version="latest"))
      shp_ad0 <- as(shp_ad0, 'Spatial')

      pop2 <- crop(pop, extent(precip))
      extent(precip) <- extent(pop2)

      #values(pop2)[is.na(values(pop2))] = 0
      pop2[is.na(pop2)] <- 0

      shp_ad0$total_precipitation <- exactextractr::exact_extract(precip, shp_ad0, 'sum')
      shp_ad0$mean_precipitation <- exactextractr::exact_extract(x = precip, y = shp_ad0, fun = 'weighted_mean', weights = pop2)
      shp_ad0 <- as.data.table(subset(shp_ad0, select= c("GID_0", "total_precipitation", "mean_precipitation")))
      colnames(shp_ad0) <- c('iso3', 'total_precipitation', 'mean_precipitation')

      shp_ad0$mean_precipitation <- ifelse(shp_ad0$total_precipitation== 0, 0, shp_ad0$mean_precipitation)

      shp_ad0$year <- y
      shp_ad0$month <- m
      all_month_yr <- rbind(all_month_yr, shp_ad0)

      precip <- NULL
     # pop <- NULL
      pop2 <- NULL
    }
  }

  if(y == 2026){
    for(m in 1:1){
      message(m)
      if(m < 10){
        precip <- raster(paste0(data_downloads_path, "chirps_monthly_precipitation_rasters/chirps-v2.0.", y, ".0", m,".tif"))
      }else{
        precip <- raster(paste0(data_downloads_path, "chirps_monthly_precipitation_rasters/chirps-v2.0.", y, ".", m,".tif"))
      }


      precip <- precip[[1]]
      precip[precip > 50000] <- NA

      shp_ad0 <- (world(resolution=5, level=0, 'shp_ad0', version="latest"))
      shp_ad0 <- as(shp_ad0, 'Spatial')

      pop2 <- crop(pop, extent(precip))
      extent(precip) <- extent(pop2)

      #values(pop2)[is.na(values(pop2))] = 0
      pop2[is.na(pop2)] <- 0

      shp_ad0$total_precipitation <- exactextractr::exact_extract(precip, shp_ad0, 'sum')
      shp_ad0$mean_precipitation <- exactextractr::exact_extract(x = precip, y = shp_ad0, fun = 'weighted_mean', weights = pop2)
      shp_ad0 <- as.data.table(subset(shp_ad0, select= c("GID_0", "total_precipitation", "mean_precipitation")))
      colnames(shp_ad0) <- c('iso3', 'total_precipitation', 'mean_precipitation')

      shp_ad0$mean_precipitation <- ifelse(shp_ad0$total_precipitation== 0, 0, shp_ad0$mean_precipitation)

      shp_ad0$year <- y
      shp_ad0$month <- m
      all_month_yr <- rbind(all_month_yr, shp_ad0)
      precip <- NULL
      pop2 <- NULL
    }
  }
}

precipitation_all <- rbind(precipitation_old, all_month_yr)

df_raw_covariates <- merge(df_raw_covariates, precipitation_all, by=c('iso3', 'month', 'year'), all.x=T, all.y=T)

## -----------------------------------------------------------   
## temperature 

temperature_old <- fread('~/Google Drive/My Drive/Gates_MeaslesForecast/updates_jhu/df_raw_covariates_20250923.csv')
temperature_old <- subset(temperature_old, select=c('iso3', 'month', 'year', 'mean_temperature'))
temperature_old <- subset(temperature_old, !is.na(mean_temperature))
### this dataset covers jan 1981 to dec 2024

### this code below updates from jan 2025 to jan 2026
### this same code was used to previously generate estimates from jan 1981 to dec 2024

library(ncdf4)

all_month_yr <- data.table()
for(y in 2025:2026){
  message(y)

  temp_nc <- nc_open(paste0(data_downloads_path, "copernicus_temperature_data/data_", y, ".nc"))
  temp <- ncvar_get(temp_nc, "t2m")
  lon <- as.vector(ncvar_get(temp_nc, "longitude")) #- 180
  lat <- ncvar_get(temp_nc, "latitude")
  #time <- ncvar_get(temp_nc, 'time')
  lat <- sort(lat, decreasing = TRUE)  # Ensure latitudes are sorted in the correct order

  if(y < 1999){
    pop <- raster(paste0(data_downloads_path, "worldpop_rasters/ppp_2000_1km_Aggregated.tif"))
  }else if(y >= 2000 & y < 2021){
    if(y == 2001){
    	pop <- raster(paste0(data_downloads_path, "worldpop_rasters/ppp_", y, "_1km_Aggregated.tif"))
    }else{
    	pop <- raster(paste0(data_downloads_path, "worldpop_rasters/ppp_", y, "_1km_Aggregated.tif"))
    }
  }else if(y > 2020){
    pop <- raster(paste0(data_downloads_path, "worldpop_rasters/ppp_2020_1km_Aggregated.tif"))
  }

  pop[pop < 0] = NA
  pop[pop > 50000] = NA
  pop3 <- raster::aggregate(pop, fact = 10, fun=sum)

  if(y == 2025){
    for(m in 1:12){
      message(m)
      r <- raster(t(temp[,,m]), xmn = 0, xmx = 360, ymn = min(lat), ymx = max(lat))

      r2 <- crop(r, extent(c(180,360,-90,90)))
      r3 <- crop(r, extent(c(0.1,180,-90,90)))
      extent(r2) <- c(-180,0,-90,90)
      temperature <- do.call(merge, c(r2,r3))

      temperature <- temperature[[1]]
      values(temperature)[values(temperature) < 0] = NA
      crs(temperature) <- crs(pop)

      shp_ad0 <- (world(resolution=5, level=0, 'shp_ad0', version="latest"))
      shp_ad0 <- as(shp_ad0, 'Spatial')

      temperature <- crop(temperature, extent(pop3))
      extent(temperature) <- extent(pop3)

      temp2 <- terra::resample(temperature, pop3)
      values(pop3)[is.na(values(pop3))] = 0

      shp_ad0$mean_temperature <- exactextractr::exact_extract(x = temp2, y = shp_ad0, fun = 'weighted_mean', weights = pop3, default_weight=0)
      shp_ad0 <- as.data.table(subset(shp_ad0, select= c("GID_0", "mean_temperature")))
      colnames(shp_ad0) <- c('iso3', 'mean_temperature')
      shp_ad0$mean_temperature <- shp_ad0$mean_temperature - 273.2

      shp_ad0$year <- y
      shp_ad0$month <- m
      all_month_yr <- rbind(all_month_yr, shp_ad0)
    }
  }

  if(y == 2026){
    for(m in 1:1){
      message(m)
      r <- raster(t(temp), xmn = 0, xmx = 360, ymn = min(lat), ymx = max(lat))

      r2 <- crop(r, extent(c(180,360,-90,90)))
      r3 <- crop(r, extent(c(0.1,180,-90,90)))
      extent(r2) <- c(-180,0,-90,90)
      temperature <- do.call(merge, c(r2,r3))

      temperature <- temperature[[1]]
      values(temperature)[values(temperature) < 0] = NA
      crs(temperature) <- crs(pop)

      shp_ad0 <- (world(resolution=5, level=0, 'shp_ad0', version="latest"))
      shp_ad0 <- as(shp_ad0, 'Spatial')

      temperature <- crop(temperature, extent(pop3))
      extent(temperature) <- extent(pop3)

      temp2 <- terra::resample(temperature, pop3)
      values(pop3)[is.na(values(pop3))] = 0

      shp_ad0$mean_temperature <- exactextractr::exact_extract(x = temp2, y = shp_ad0, fun = 'weighted_mean', weights = pop3, default_weight=0)
      shp_ad0 <- as.data.table(subset(shp_ad0, select= c("GID_0", "mean_temperature")))
      colnames(shp_ad0) <- c('iso3', 'mean_temperature')
      shp_ad0$mean_temperature <- shp_ad0$mean_temperature - 273.2

      shp_ad0$year <- y
      shp_ad0$month <- m
      all_month_yr <- rbind(all_month_yr, shp_ad0)
    }
  }
}

temperature_all <- rbind(temperature_old, all_month_yr)

df_raw_covariates <- merge(df_raw_covariates, temperature_all, by=c('iso3', 'month', 'year'), all.x=T, all.y=T)

## -----------------------------------------------------------   
## COVID-19 months
df_raw_covariates$covid_months <- 0

df_raw_covariates$covid_months <-ifelse(df_raw_covariates$year %in% c(2020,2021,2022), 1, df_raw_covariates$covid_months)
df_raw_covariates$covid_months <-ifelse(df_raw_covariates$year %in% c(2023) & df_raw_covariates$month %in% c(1,2,3,4,5), 1, df_raw_covariates$covid_months)

## -----------------------------------------------------------   
## gini coefficient -- TO UPDATE: after next IHME data release, ask Emily H to re-run

gini <- fread(paste0(data_downloads_path, 'gini_coefficients_mcv1_2024_06_21.csv'))

gini <- subset(gini, select=c('iso3', 'year', 'gini'))
colnames(gini) <- c('iso3', 'year', 'gini')

df_raw_covariates <- merge(df_raw_covariates, gini, by=c('iso3', 'year'), all.x=T, all.y=T)
gini <- NULL

## -----------------------------------------------------------   
## temperature extremes: more than 2 or 3 sd above or below seasonal baseline

df_country_month_temp <- data.table()
for(c in unique(df_raw_covariates$iso3)){
  message(c)
  for(m in 1:12){
    df_temp <- subset(df_raw_covariates, iso3 == c & month == m)
   
    to_add <- data.table(cbind(c,m,mean(df_temp$mean_temperature, na.rm=T), sd(df_temp$mean_temperature, na.rm=T)))
    df_country_month_temp <- rbind(df_country_month_temp, to_add)
  }
}
colnames(df_country_month_temp) <- c('iso3', 'month', 'mean_seasonal_temp', 'sd_seasonal_temp')
df_country_month_temp <- data.table(df_country_month_temp)

df_country_month_temp$mean_seasonal_temp <- as.numeric(df_country_month_temp$mean_seasonal_temp)
df_country_month_temp$sd_seasonal_temp <- as.numeric(df_country_month_temp$sd_seasonal_temp)
df_country_month_temp$month <- as.integer(df_country_month_temp$month)

df_country_month_temp$high_extreme_3SD <- df_country_month_temp$mean_seasonal_temp + (3 * df_country_month_temp$sd_seasonal_temp)
df_country_month_temp$low_extreme_3SD <- df_country_month_temp$mean_seasonal_temp - (3 * df_country_month_temp$sd_seasonal_temp)

df_country_month_temp$high_extreme_2SD <- df_country_month_temp$mean_seasonal_temp + (2 * df_country_month_temp$sd_seasonal_temp)
df_country_month_temp$low_extreme_2SD <- df_country_month_temp$mean_seasonal_temp - (2 * df_country_month_temp$sd_seasonal_temp)

df_raw_covariates <- merge(df_raw_covariates, df_country_month_temp, by=c('iso3', 'month'))  

df_raw_covariates$extreme_temperature_high_3SD <- ifelse(df_raw_covariates$mean_temperature > df_raw_covariates$high_extreme_3SD, 1,
                                                     ifelse(df_raw_covariates$mean_temperature <= df_raw_covariates$high_extreme_3SD, 0, NA))

df_raw_covariates$extreme_temperature_low_3SD <- ifelse(df_raw_covariates$mean_temperature < df_raw_covariates$low_extreme_3SD, 1,
                                                     ifelse(df_raw_covariates$mean_temperature >= df_raw_covariates$low_extreme_3SD, 0, NA))

df_raw_covariates$extreme_temperature_high_2SD <- ifelse(df_raw_covariates$mean_temperature > df_raw_covariates$high_extreme_2SD, 1,
                                                     ifelse(df_raw_covariates$mean_temperature <= df_raw_covariates$high_extreme_2SD, 0, NA))

df_raw_covariates$extreme_temperature_low_2SD <- ifelse(df_raw_covariates$mean_temperature < df_raw_covariates$low_extreme_2SD, 1,
                                                     ifelse(df_raw_covariates$mean_temperature >= df_raw_covariates$low_extreme_2SD, 0, NA))

df_raw_covariates$mean_seasonal_temp <- NULL
df_raw_covariates$sd_seasonal_temp <- NULL
df_raw_covariates$high_extreme_3SD <- NULL
df_raw_covariates$low_extreme_3SD <- NULL
df_raw_covariates$high_extreme_2SD <- NULL
df_raw_covariates$low_extreme_2SD <- NULL

## -----------------------------------------------------------   
## total precipitation extremes: more than 2 or 3 sd above or below seasonal baseline

df_country_month_temp <- data.table()
for(c in unique(df_raw_covariates$iso3)){
  message(c)
  for(m in 1:12){
    df_temp <- subset(df_raw_covariates, iso3 == c & month == m)
    
    to_add <- data.table(cbind(c,m,mean(df_temp$total_precipitation, na.rm=T), sd(df_temp$total_precipitation, na.rm=T)))
    df_country_month_temp <- rbind(df_country_month_temp, to_add)
  }
}
colnames(df_country_month_temp) <- c('iso3', 'month', 'mean_seasonal_precip', 'sd_seasonal_precip')
df_country_month_temp <- data.table(df_country_month_temp)

df_country_month_temp$mean_seasonal_precip <- as.numeric(df_country_month_temp$mean_seasonal_precip)
df_country_month_temp$sd_seasonal_precip <- as.numeric(df_country_month_temp$sd_seasonal_precip)
df_country_month_temp$month <- as.integer(df_country_month_temp$month)

df_country_month_temp$high_extreme_3SD <- df_country_month_temp$mean_seasonal_precip + (3 * df_country_month_temp$sd_seasonal_precip)
df_country_month_temp$low_extreme_3SD <- df_country_month_temp$mean_seasonal_precip - (3 * df_country_month_temp$sd_seasonal_precip)

df_country_month_temp$high_extreme_2SD <- df_country_month_temp$mean_seasonal_precip + (2 * df_country_month_temp$sd_seasonal_precip)
df_country_month_temp$low_extreme_2SD <- df_country_month_temp$mean_seasonal_precip - (2 * df_country_month_temp$sd_seasonal_precip)

df_raw_covariates <- merge(df_raw_covariates, df_country_month_temp, by=c('iso3', 'month'))  

df_raw_covariates$extreme_precipitation_high_3SD <- ifelse(df_raw_covariates$total_precipitation > df_raw_covariates$high_extreme_3SD, 1,
                                                     ifelse(df_raw_covariates$total_precipitation <= df_raw_covariates$high_extreme_3SD, 0, NA))

df_raw_covariates$extreme_precipitation_low_3SD <- ifelse(df_raw_covariates$total_precipitation < df_raw_covariates$low_extreme_3SD, 1,
                                                    ifelse(df_raw_covariates$total_precipitation >= df_raw_covariates$low_extreme_3SD, 0, NA))

df_raw_covariates$extreme_precipitation_high_2SD <- ifelse(df_raw_covariates$total_precipitation > df_raw_covariates$high_extreme_2SD, 1,
                                                     ifelse(df_raw_covariates$total_precipitation <= df_raw_covariates$high_extreme_2SD, 0, NA))

df_raw_covariates$extreme_precipitation_low_2SD <- ifelse(df_raw_covariates$total_precipitation < df_raw_covariates$low_extreme_2SD, 1,
                                                    ifelse(df_raw_covariates$total_precipitation >= df_raw_covariates$low_extreme_2SD, 0, NA))

df_raw_covariates$mean_seasonal_precip <- NULL
df_raw_covariates$sd_seasonal_precip <- NULL
df_raw_covariates$high_extreme_3SD <- NULL
df_raw_covariates$low_extreme_3SD <- NULL
df_raw_covariates$high_extreme_2SD <- NULL
df_raw_covariates$low_extreme_2SD <- NULL

## -----------------------------------------------------------   
## SIA indicators -- round 1

sia <- fread(paste0(data_downloads_path, 'jrf_release_2025/V_SIA_MAIN_MR.csv'))

sia$year <- sia$YEAR
sia$START_DATE <- ifelse(nchar(sia$START_DATE)<5 & nchar(sia$START_DATE)>0, paste0(sia$START_DATE, '-01-01'), sia$START_DATE)
sia$start_month <- as.numeric(format(as.Date(sia$`START_DATE`), "%m"))
sia$end_month <- as.numeric(format(as.Date(sia$`END_DATE`), "%m"))

sia$`Age group` <- sia$AGEGROUP
sia$`% Adm coverage` <- sia$ADMIN_COVERAGE
sia$`Reached population` <- sia$DOSES

source(paste0(data_downloads_path, 'convertAgeSIA.R'))
sia$age_start <- NA
sia$age_end <- NA
for(i in 1:dim(sia)[1]){
  message(i)
  sia$age_start[i] <-  convertAgeSIA(sia$`Age group`[i])[1]
  sia$age_end[i] <-  convertAgeSIA(sia$`Age group`[i])[2]
}

sia$age_range <- sia$age_end - sia$age_start
sia$sia_coverage <- sia$`% Adm coverage`
sia$reached_population <- sia$`Reached population`

sia <- subset(sia, select = c('COUNTRY', 'year', 'age_start', 'age_end', 'age_range', 'sia_coverage', 'start_month', 'end_month', 'reached_population'))
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
  df_long_to_add <- data.frame(iso3 = sia$`COUNTRY`[i],
                               year = sia$year[i],
                               month = months,
                               age_start = sia$age_start[i],
                               age_end = sia$age_end[i],
                               sia_coverage = sia$sia_coverage[i],
                               reached_population = sia$reached_population[i])  
  
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
        sia_long_c_y_m <- data.frame(cbind(unique(sia_long_c_y_m$iso3), unique(sia_long_c_y_m$year), unique(sia_long_c_y_m$month), min(sia_long_c_y_m$age_start), max(sia_long_c_y_m$age_end), max(sia_long_c_y_m$sia_coverage), max(sia_long_c_y_m$reached_population)))
        colnames(sia_long_c_y_m) <- c('iso3', 'year', 'month', 'age_start', 'age_end', 'sia_coverage', 'reached_population')
        sia_long_final <- rbind(sia_long_final, sia_long_c_y_m, fill = T)
      }
    }else{
      colnames(sia_long_c_y) <- c('iso3', 'year', 'month', 'age_start', 'age_end', 'sia_coverage', 'reached_population')
      sia_long_final <- rbind(sia_long_final, sia_long_c_y, fill = T)
    }
  }
}

colnames(sia_long_final) <- c('iso3', 'year', 'month', 'sia_age_start', 'sia_age_end', 'sia_coverage', 'reached_population')
sia_long_final$sia_occured <- 1

sia_long_final$sia_age_end <- as.numeric(sia_long_final$sia_age_end)
sia_long_final$sia_age_start <- as.numeric(sia_long_final$sia_age_start)
sia_long_final$sia_age_range <- sia_long_final$sia_age_end - sia_long_final$sia_age_start

sia_long_final$year <- as.numeric(sia_long_final$year)
sia_long_final$month <- as.numeric(sia_long_final$month)

#### average age, % under 5, % under 10
data('popAge1dt', package = "wpp2022", envir = environment())

pop_size <- subset(popAge1dt, select=c("name", "age", "year", "pop"))
pop_size <- subset(pop_size, year > 1979)
setnames(pop_size, old = c("name","pop"), new=c("country","pop_size"))

pop_size_under5 <- subset(pop_size, age < 5)
pop_size_under15 <- subset(pop_size, age < 15)

counts_under5 <- aggregate(pop_size_under5$pop_size, by=list(pop_size_under5$country, pop_size_under5$year), FUN = 'sum')
colnames(counts_under5) <- c("country", "year", "under5")

counts_under15 <- aggregate(pop_size_under15$pop_size, by=list(pop_size_under15$country, pop_size_under15$year), FUN = 'sum')
colnames(counts_under15) <- c("country", "year", "under15")

counts_df <- merge(counts_under5, counts_under15, by=c('country', 'year'))

counts_df$under5 <- counts_df$under5 * 1000
counts_df$under15 <- counts_df$under15 * 1000

counts_df <- merge(counts_df, iso3_lookup, by = 'country')

sia_long_final <- merge(sia_long_final, counts_df, by =c('iso3', 'year'))

sia_long_final2 <- data.table()
for(i in 1:dim(sia_long_final)[1]){
  message(i)  
  sia_long_final_i <- (sia_long_final[i,])
  
  
  age_min_yr <- floor(sia_long_final_i$sia_age_start / 12)
  age_max_yr <- floor(sia_long_final_i$sia_age_end / 12)
  
  if(is.na(age_max_yr) & is.na(age_min_yr)) {
    age_max_yr <- 4
    age_min_yr <- 0
  } 
  if(is.na(age_max_yr)) age_max_yr = age_min_yr
  pop_temp <- subset(pop_size, country == sia_long_final_i$country & year == sia_long_final_i$year)
  
  pop_temp <- subset(pop_temp, age %in% c(age_min_yr:age_max_yr))
  
  sia_long_final_i$calc_target_pop <-   sum(pop_temp$pop_size) * 1000
   
  sia_long_final2 <- rbind(sia_long_final2, sia_long_final_i)
}

sia_long_final2$calc_reached_population <- (as.numeric(sia_long_final2$sia_coverage) /100) * sia_long_final2$calc_target_pop 

sia_long_final2$sia_coverage_under5 <- ifelse(sia_long_final2$sia_age_end < 59, as.numeric(sia_long_final2$calc_reached_population / sia_long_final2$under5), (as.numeric(sia_long_final2$sia_coverage) /100) )
sia_long_final2$sia_coverage_under15 <- ifelse(sia_long_final2$sia_age_end < 180, as.numeric(sia_long_final2$calc_reached_population / sia_long_final2$under15), (as.numeric(sia_long_final2$sia_coverage) /100) )
sia_long_final2 <- subset(sia_long_final2, select=c('iso3',  'year', 'month', 'sia_coverage_under5', 'sia_coverage_under15' ))

df_raw_covariates <- merge(df_raw_covariates, sia_long_final2, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
df_raw_covariates$sia_coverage_under5 <- ifelse(is.na(df_raw_covariates$sia_coverage_under5),0,df_raw_covariates$sia_coverage_under5)
df_raw_covariates$sia_coverage_under15 <- ifelse(is.na(df_raw_covariates$sia_coverage_under15),0,df_raw_covariates$sia_coverage_under15)

## -----------------------------------------------------------   
## proportion of suspected cases with IgM positivity 

table1 <- fread(paste0(data_downloads_path, 'data_from_Seb_20250903/Table 1.csv'))

table1$IGMPOSMEA <- ifelse(table1$IGMPOSMEA == 'NULL', 0, table1$IGMPOSMEA)
table1$IGMPOSRUB <- ifelse(table1$IGMPOSRUB == 'NULL', 0, table1$IGMPOSRUB)
table1$IGMNEG <- ifelse(table1$IGMNEG == 'NULL', 0, table1$IGMNEG)

table1_ctry_suspected <- aggregate(table1$SUSPECT, by = list(table1$COUNTRY, table1$ONSET_MONTH, table1$ONSET_YEAR), FUN = sum)
table1_ctry_igm_measles <- aggregate(as.numeric(table1$IGMPOSMEA), by = list(table1$COUNTRY, table1$ONSET_MONTH, table1$ONSET_YEAR), FUN = sum)
table1_ctry_igm_rubella <- aggregate(as.numeric(table1$IGMPOSRUB), by = list(table1$COUNTRY, table1$ONSET_MONTH, table1$ONSET_YEAR), FUN = sum)
table1_ctry_igm_negative <- aggregate(as.numeric(table1$IGMNEG), by = list(table1$COUNTRY, table1$ONSET_MONTH, table1$ONSET_YEAR), FUN = sum)

colnames(table1_ctry_suspected) <- c("iso3", "month", "year", "suspected_cases")
colnames(table1_ctry_igm_measles) <- c("iso3", "month", "year", "measles_igm_positive_cases")
colnames(table1_ctry_igm_rubella) <- c("iso3", "month", "year", "rubella_igm_positive_cases")
colnames(table1_ctry_igm_negative) <- c("iso3", "month", "year", "igm_negative_cases")

table1_ctry <- merge(table1_ctry_suspected, table1_ctry_igm_measles, by=c('iso3', 'month', 'year'))
table1_ctry <- merge(table1_ctry, table1_ctry_igm_rubella, by=c('iso3', 'month', 'year'))
table1_ctry <- merge(table1_ctry, table1_ctry_igm_negative, by=c('iso3', 'month', 'year'))

table1_ctry$proportion_igm_measles_pos <- table1_ctry$measles_igm_positive_cases / table1_ctry$suspected_cases
table1_ctry$proportion_igm_rubella_pos <- table1_ctry$rubella_igm_positive_cases / table1_ctry$suspected_cases
table1_ctry$proportion_igm_mr_neg <- table1_ctry$igm_negative_cases / table1_ctry$suspected_cases

table1_ctry$month <- as.numeric(table1_ctry$month)
table1_ctry$year <- as.numeric(table1_ctry$year)

table1_ctry <- subset(table1_ctry, !is.na(month))

df_raw_covariates <- merge(df_raw_covariates, table1_ctry, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

## -----------------------------------------------------------   
## average age of infection of suspected cases 

table2 <- fread(paste0(data_downloads_path, 'data_from_Seb_20250903/Table 2.csv'))

table2_age <- aggregate(table2$COUNT, by=list(table2$COUNTRY, table2$ONSET_MONTH, table2$ONSET_YEAR, table2$AGE_YEARS), FUN = 'sum')
colnames(table2_age) <- c('iso3', 'month', 'year', 'age_years', 'count')

table2_age$age_years <- as.numeric(table2_age$age_years)
table2_age <- subset(table2_age, age_years < 100 & age_years > 0)

table2_dt <- data.table(table2_age)
table2_dt <- table2_dt[,list(average_age_suspected_cases = weighted.mean(as.numeric(age_years),as.numeric(count))),by=list(iso3, month, year)]

table2_dt$month <- as.numeric(table2_dt$month)
table2_dt$year <- as.numeric(table2_dt$year)

table2_dt <- subset(table2_dt, !is.na(month))

df_raw_covariates <- merge(df_raw_covariates, table2_dt, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

## -----------------------------------------------------------   
## proportion of cases with documented vaccination, when vaccination data is available 

table2 <- fread(paste0(data_downloads_path, 'data_from_Seb_20250903/Table 2.csv'))
table2 <- subset(table2, PREV_VACC %in% c("YES", "NO"))

table2_age <- aggregate(table2$COUNT, by=list(table2$COUNTRY, table2$ONSET_MONTH, table2$ONSET_YEAR, table2$PREV_VACC), FUN = 'sum')
colnames(table2_age) <- c('iso3', 'month', 'year', 'prev_vacc', 'count')

table2_no <- subset(table2_age, prev_vacc == 'NO')
table2_yes <- subset(table2_age, prev_vacc == 'YES')

table2_no$prev_vacc <- NULL
table2_yes$prev_vacc <- NULL

colnames(table2_no) <- c('iso3', 'month', 'year', 'cases_unvacc')
colnames(table2_yes) <- c('iso3', 'month', 'year', 'cases_vacc')

table2_vax <- merge(table2_no, table2_yes, by=c('iso3', 'month', 'year'), all.x=T, all.y=T)
table2_vax$cases_unvacc <- ifelse(is.na(table2_vax$cases_unvacc), 0, table2_vax$cases_unvacc)
table2_vax$cases_vacc <- ifelse(is.na(table2_vax$cases_vacc), 0, table2_vax$cases_vacc)

table2_vax$total <- table2_vax$cases_unvacc + table2_vax$cases_vacc

table2_vax$proportion_vax_suspected_cases <- table2_vax$cases_vacc / table2_vax$total

table2_vax <- subset(table2_vax, select=c("iso3", "month", "year", "proportion_vax_suspected_cases"))

table2_vax$month <- as.numeric(table2_vax$month)

table2_vax <- subset(table2_vax, !is.na(month))

df_raw_covariates <- merge(df_raw_covariates, table2_dt, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

df_raw_covariates$average_age_suspected_cases <- df_raw_covariates$average_age_suspected_cases.x
df_raw_covariates$average_age_suspected_cases.x <- NULL
df_raw_covariates$average_age_suspected_cases.y <- NULL


## -----------------------------------------------------------   
## estimated measles susceptibility

df_raw_covariates$prop_susceptible_under5 <- NULL
df_raw_covariates$prop_susceptible_under10 <- NULL

data('popAge1dt', package = "wpp2022", envir = environment())
data("popprojAge1dt", package = "wpp2022", envir = environment())

popAge1dt <- rbind(popAge1dt, popprojAge1dt, fill = T)
popAge1dt <- subset(popAge1dt, year < 2025)

estimated_susceptibility <- fread(paste0(data_downloads_path, 'long_susc_WHO24_best_models.csv'))

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
estimated_susceptibility$month <- 1 #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, estimated_susceptibility, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

#### proportion < 10 yo susceptible
estimated_susceptibility <- fread(paste0(data_downloads_path, 'long_susc_WHO24_best_models.csv'))
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
estimated_susceptibility$month <- 1 #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, estimated_susceptibility, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
estimated_susceptibility <- NULL
popAge1dt <- NULL


## -----------------------------------------------------------   
## create new SIA variables -- early, on-time, late & evaluate predictive power of SIAs ___ MORE SIA VARIABLES TO UPDATE

df_raw_covariates$country <- NULL

#### boolean SIA took place in that month and year
df_raw_covariates$sia_occured <- ifelse(df_raw_covariates$sia_coverage_under5 == 0, 0, 1)

#### add birth cohort size as of the year of the last SIA... and keep constant across months in a given year and country within interval b/w SIAs
data('popAge1dt', package = "wpp2022", envir = environment())
data("popprojAge1dt", package = "wpp2022", envir = environment())

popAge1dt <- rbind(popAge1dt, popprojAge1dt, fill = T)
popAge1dt <- subset(popAge1dt, year < 2025)
popAge1dt <- subset(popAge1dt, age == 0 & year > 1979)
popAge1dt <- subset(popAge1dt, select = c('name', 'year', 'pop'))
colnames(popAge1dt) <- c('country', 'year', 'birth_cohort_size_annual')

popAge1dt$country <- ifelse(popAge1dt$country == 'Turkiye', 'Türkiye', popAge1dt$country)
popAge1dt$country <- ifelse(popAge1dt$country == 'Netherlands', 'Netherlands (Kingdom of the)', popAge1dt$country)
popAge1dt$country <- ifelse(popAge1dt$country == "Cote d'Ivoire", "Côte d'Ivoire", popAge1dt$country)
popAge1dt$country <- ifelse(popAge1dt$country == 'United Kingdom', 'United Kingdom of Great Britain and Northern Ireland', popAge1dt$country)

popAge1dt <- merge(popAge1dt, iso3_lookup, by='country')
popAge1dt$country <- NULL
popAge1dt$year <- as.integer(popAge1dt$year)

df_raw_covariates <- merge(df_raw_covariates,popAge1dt, by=c('iso3', 'year'), all.x=T)

popAge1dt <- NULL

#### Estimate number of susceptibles age 0 each month and country with MR2 scaling
df_raw_covariates_2 <- subset(df_raw_covariates, year < 2000)
df_raw_covariates_2$birth_cohort_size_sia <- NA

# Function to calculate cumulative sums excluding values where y == 1
cumulative_sum_excluding_1s <- function(x, y, C) {
  # Initialize variables
  result <- numeric(length(x))  # Vector to store results
  current_sum <- 0             # Track the running cumulative sum
  
  # Loop through the data
  for (i in seq_along(x)) {
    if (y[i] == 1) {
      # Multiply the current cumulative sum by C
      current_sum <- current_sum * (1 - C[i])
    } else {
      # Add to the cumulative sum
      current_sum <- current_sum + x[i]
    }
    # Store the result
    result[i] <- current_sum
  }
  
  return(result)
}

df_raw_covariates_annual_mcv <- data.table()
for(c in 1:length(unique(df_raw_covariates$iso3))){
  
  for(y in 1:length(unique(df_raw_covariates$year))){
    temp <- subset(df_raw_covariates, iso3 == unique(df_raw_covariates$iso3)[c] & year == unique(df_raw_covariates$year)[y])
    temp <- unique(temp)
    temp$mcv1_annual <- subset(temp, month == 1)$mcv1 / 100
    temp$mcv2_annual <- ifelse(is.na(unique(subset(temp, month == 1)$mcv2)), 0, subset(temp, month == 1)$mcv2 / 100)
    
    df_raw_covariates_annual_mcv <- rbind(df_raw_covariates_annual_mcv, temp)
  }
}

df_raw_covariates <- df_raw_covariates_annual_mcv

df_raw_covariates$sia_coverage_under5_v2 <- ifelse(df_raw_covariates$sia_coverage_under5 > 1, 1, df_raw_covariates$sia_coverage_under5)

df_raw_covariates$sia_coverage_under5_v2 <- ifelse(is.na(df_raw_covariates$sia_coverage_under5_v2), 0, df_raw_covariates$sia_coverage_under5_v2)
df_raw_covariates$sia_occured <- ifelse(is.na(df_raw_covariates$sia_occured), 0, df_raw_covariates$sia_occured)

df_raw_covariates_2_to_add <- data.table()
for(c in unique(df_raw_covariates$iso3)){
  
  temp <- subset(df_raw_covariates, iso3 == c & year > 1999)
  temp$birth_cohort_size_sia <- cumulative_sum_excluding_1s(temp$birth_cohort_size_annual/12 * ( 1 - (temp$mcv1_annual * (0.84 + (0.09 * temp$mcv2_annual)))), temp$sia_occured, temp$sia_coverage_under5_v2)
  
  df_raw_covariates_2_to_add <- rbind(df_raw_covariates_2_to_add, temp)
}

###### add NAs if no SIA ever
for(i in 1:length(unique(df_raw_covariates_2_to_add$iso3))){
  if( min(subset(df_raw_covariates_2_to_add, iso3 == unique(df_raw_covariates_2_to_add$iso3)[i] & sia_occured == 1)$year ) == Inf){
    df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == unique(df_raw_covariates_2_to_add$iso3)[i], NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
  }
}

###### add NAs if no SIA before 2011
for(i in 1:length(unique(df_raw_covariates_2_to_add$iso3))){
  if( min(subset(df_raw_covariates_2_to_add, iso3 == unique(df_raw_covariates_2_to_add$iso3)[i]& sia_occured == 1)$year ) < 2025 & 
      min(subset(df_raw_covariates_2_to_add, iso3 == unique(df_raw_covariates_2_to_add$iso3)[i]& sia_occured == 1)$year ) > 2010){
    message(unique(df_raw_covariates_2_to_add$iso3)[i])
    message("... ",min(subset(df_raw_covariates_2_to_add, iso3 == unique(df_raw_covariates_2_to_add$iso3)[i]& sia_occured == 1)$year))
    
  }
}

###
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "ARE" & df_raw_covariates_2_to_add$year < 2015, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "DMA" & df_raw_covariates_2_to_add$year < 2019, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "DNK" & df_raw_covariates_2_to_add$year < 2012, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "ISL" & df_raw_covariates_2_to_add$year < 2019, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "KWT" & df_raw_covariates_2_to_add$year < 2017, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "MHL" & df_raw_covariates_2_to_add$year < 2019, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "OMN" & df_raw_covariates_2_to_add$year < 2016, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "ROU" & df_raw_covariates_2_to_add$year < 2016, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "RUS" & df_raw_covariates_2_to_add$year < 2014, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "THA" & df_raw_covariates_2_to_add$year < 2015, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "TON" & df_raw_covariates_2_to_add$year < 2019, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "VCT" & df_raw_covariates_2_to_add$year < 2016, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)

df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "ARE" & df_raw_covariates_2_to_add$year == 2015 & df_raw_covariates_2_to_add$month < 11, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "DMA" & df_raw_covariates_2_to_add$year == 2019 & df_raw_covariates_2_to_add$month < 4, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "DNK" & df_raw_covariates_2_to_add$year == 2012 & df_raw_covariates_2_to_add$month < 4, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "ISL" & df_raw_covariates_2_to_add$year == 2019 & df_raw_covariates_2_to_add$month < 2, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "KWT" & df_raw_covariates_2_to_add$year == 2017 & df_raw_covariates_2_to_add$month < 2, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "MHL" & df_raw_covariates_2_to_add$year == 2019 & df_raw_covariates_2_to_add$month < 9, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "OMN" & df_raw_covariates_2_to_add$year == 2016 & df_raw_covariates_2_to_add$month < 8, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "ROU" & df_raw_covariates_2_to_add$year == 2016 & df_raw_covariates_2_to_add$month < 12, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "RUS" & df_raw_covariates_2_to_add$year == 2014 & df_raw_covariates_2_to_add$month < 5, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "THA" & df_raw_covariates_2_to_add$year == 2015 & df_raw_covariates_2_to_add$month < 5, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "TON" & df_raw_covariates_2_to_add$year == 2019 & df_raw_covariates_2_to_add$month < 1, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)
df_raw_covariates_2_to_add$birth_cohort_size_sia <- ifelse(df_raw_covariates_2_to_add$iso3 == "VCT" & df_raw_covariates_2_to_add$year == 2016 & df_raw_covariates_2_to_add$month < 4, NA, df_raw_covariates_2_to_add$birth_cohort_size_sia)

#### boolean cumsum > birth cohort size 
df_raw_covariates_2_to_add$sia_size_larger_birth_cohort <- ifelse(df_raw_covariates_2_to_add$birth_cohort_size_annual < df_raw_covariates_2_to_add$birth_cohort_size_sia, 1, 0)
df_raw_covariates_2$sia_size_larger_birth_cohort <- NA

df_raw_covariates <- rbind(df_raw_covariates_2, df_raw_covariates_2_to_add, fill = T)

#### write file by SIA 

sia_long_final$sia_timing <- NA
sia_long_final$months_late <- NA
sia_long_final$outbreak_occured <- NA
sia_long_final2 <- data.table()
for(i in unique(sia_long_final$iso3)){
  message(i)
  ctry_sia <- subset(sia_long_final, iso3 == i)

  for(s in 1:dim(ctry_sia)[1]){
    
    if(ctry_sia$year[s] >= 2011){
      
      year_last_sia <- (ctry_sia$year[s-1])
      month_last_sia <- (ctry_sia$month[s-1])
      
      year_this_sia <- (ctry_sia$year[s])
      month_this_sia <- (ctry_sia$month[s])
      
      df_raw_covariates_temp <- subset(df_raw_covariates, iso3 == i & year >= year_last_sia & year <= year_this_sia  )
      df_raw_covariates_temp$month_year <- df_raw_covariates_temp$year + (df_raw_covariates_temp$month / 12)
      df_raw_covariates_temp <- subset(df_raw_covariates_temp, month_year >= (year_last_sia + (month_last_sia / 12)) & month_year < (year_this_sia + (month_this_sia / 12))  )
      
      if(dim(df_raw_covariates_temp)[1]> 0){
        
        df_raw_covariates_temp_2 <- subset(df_raw_covariates, iso3 == i & year >= year_last_sia & year <= year_this_sia  )
        df_raw_covariates_temp_2$month_year <- df_raw_covariates_temp_2$year + (df_raw_covariates_temp_2$month / 12)
        df_raw_covariates_temp_2 <- subset(df_raw_covariates_temp_2, month_year >= (year_last_sia + (month_last_sia / 12)) & month_year < (year_this_sia + (month_this_sia / 12) + 1)  )
        
        if(length(df_raw_covariates_temp_2$reported_cases) >=12){
          result <- sapply(1:(length(df_raw_covariates_temp_2$reported_cases) - 11), function(i) sum(df_raw_covariates_temp_2$reported_cases[i:(i + 11)], na.rm=T))
          
          if(max(result / (min(df_raw_covariates_temp_2$pop_size * 1000, na.rm=T) / 1000000)) >= 20){
            ctry_sia$outbreak_occured[s] <- 1
          }else{
            ctry_sia$outbreak_occured[s] <- 0
          }  
        }else{
          ctry_sia$outbreak_occured[s] <- 0
        }
        
        if(1 %in% unique(df_raw_covariates_temp$sia_size_larger_birth_cohort)){
          
          cumsum_12_extra_months <- cumsum(df_raw_covariates_temp_2$birth_cohort_size_annual/12 * ( 1 - (df_raw_covariates_temp_2$mcv1_annual * (0.84 + (0.09 * df_raw_covariates_temp_2$mcv2_annual)))))
          ctry_sia$months_late[s] <- length(which(cumsum_12_extra_months < min(df_raw_covariates_temp_2$birth_cohort_size_annual) ))
         
          if(min(df_raw_covariates_temp_2$birth_cohort_size_annual) > max(cumsum_12_extra_months)){
            ctry_sia$sia_timing[s] <- 'on time'
          }else{
            ctry_sia$sia_timing[s] <- 'late'
          }

        }else{
          ctry_sia$sia_timing[s] <- 'early'
          ctry_sia$months_late[s] <- 0
        
        }  
      }
    }
  }
  
  sia_long_final2 <- rbind(sia_long_final2, ctry_sia)
}

sia_long_final3 <- subset(sia_long_final2, !is.na(outbreak_occured) & !is.na(sia_timing))

#### SIA covariates
sia_long_final3_short <- subset(sia_long_final3, select = c('iso3', 'year', 'month', 'sia_timing', 'months_late', 'outbreak_occured'))
colnames(sia_long_final3_short) <-  c('iso3', 'year', 'month', 'sia_timing', 'months_late', 'sia_outbreak_occurred')

df_raw_covariates <- merge(df_raw_covariates, sia_long_final3_short, by = c('year', 'iso3', 'month'), all.x=T, all.y=T)

#### additional SIA covariates 
df_raw_covariates$birth_cohort_size_sia_snapshot <- ifelse(df_raw_covariates$month == 1, df_raw_covariates$birth_cohort_size_sia, NA)

###### 
ctry_year_class <- data.table()
for(i in sort(unique(df_raw_covariates$iso3))){
  message(i)
  for(y in 2011:2023){
    
    test <- subset(df_raw_covariates, iso3 == i & year == y)
    
    vec_test <- ifelse(test$birth_cohort_size_sia> test$birth_cohort_size_annual, 'late', 'early')

    if(i %!in% c('AND', 'ARE', 'ATG', 'AUS', 'AUT', 'BEL', 'BGR', 'BHS', 'BIH', 'BRN',
                 'CAN', 'CHE', 'CZE', 'DEU', 'DMA', 'DNK', 'ESP', 'EST', 'FIN', 'FRA',
                 'GRC', 'GRD', 'HRV', 'HUN', 'ISL', 'ISR', 'ITA', 'JPN', 'KNA', 'KWT',
                 'LCA', 'LTU', 'LUX', 'LVA', 'MCO', 'MHL', 'MKD', 'MLT', 'MNE', 'MUS',
                 'NOR', 'NRU', 'NZL', 'OMN', 'PLW', 'POL', 'PRT', 'ROU', 'RUS', 'SGP',
                 'SMR', 'SRB', 'SSD', 'SVK', 'SVN', 'SWE', 'SYC', 'THA', 'TLS', 'TON',
                 'TTO', 'USA', 'VCT', 
                 'ABW', 'ALA', 'ASM', 'ATA', 'ATF', 'BES', 'BMU', 'CHI', 'CIV', 'CUW',
                 'CYM', 'ESH', 'FLK', 'FRO', 'FSM', 'GBR', 'GIB', 'GLP', 'GRL', 'GUF',
                 'GUM', 'HKG', 'HMD', 'IMN', 'JEY', 'LIE', 'MAC', 'MAF', 'MNP', 'MSR',
                 'MTQ', 'MYT', 'NCL', 'NLD', 'PRI', 'PRK', 'PSE', 'PYF', 'REU', 'SGS',
                 'SHN', 'SJM', 'SPM', 'SXM', 'TCA', 'TUR', 'TWN', 'VGB', 'VIR', 'XAD',
                 'XCA', 'XKO', 'XNC', 'AIA', 'ANT', 'TKL', 'WLF', 'XKX')){
    
      if(length(unique(vec_test)) == 1){
        if(isTRUE(unique(vec_test) == 'early')){
          marker <- 'early'
        }else if(isTRUE(unique(vec_test) == 'late')){
          marker <- 'late'
        }else{
          marker <- NA
        }  
      }else if(length(unique(vec_test)) == 2){
        marker <- 'mixed'
      }else{
        marker <- NA
      }
      
    }else{
      marker = 'no SIA'
    }
      
    ctry_year_class <- rbind(ctry_year_class, cbind(i,y,marker))
  }
}
colnames(ctry_year_class) <- c('iso3', 'year', 'annual_birth_cohort_timing')

ctry_year_class$month <- 1 #assume all annual data is set in month 1

ctry_year_class$year <- as.integer(ctry_year_class$year)

df_raw_covariates <- merge(df_raw_covariates, ctry_year_class, by = c('year', 'iso3', 'month'), all.x=T, all.y=T)

df_raw_covariates <- unique(df_raw_covariates)

## remove intermediate SIA variables that are not of interest
df_raw_covariates$proportion_of_birth_cohort_suscpetible <- df_raw_covariates$birth_cohort_size_sia / df_raw_covariates$birth_cohort_size_annual

df_raw_covariates$sia_timing <- NULL
df_raw_covariates$sia_coverage_under15 <- NULL
df_raw_covariates$sia_coverage_under5_v2 <- NULL
df_raw_covariates$sia_occured <- NULL
df_raw_covariates$months_late <- NULL
df_raw_covariates$birth_cohort_size_sia_snapshot <- NULL
df_raw_covariates$annual_birth_cohort_timing <- NULL
df_raw_covariates$sia_outbreak_occurred <- NULL
df_raw_covariates$birth_cohort_size_annual <- NULL


## -----------------------------------------------------------   
## "minor leakage" measles incidence (note: 2016 file does not exist)

#### 2014
file2014 <- fread('~/Google Drive/My Drive/Gates_MeaslesForecast/data_from_matt/all burden estimates/output-estimated_incidence_2014.csv', header = T)
file2014 <- melt(file2014, id.vars = 'V1')
colnames(file2014) <- c('iso3', 'year', 'cases')

#### 2015 file saved as 2016 instead 
file2015 <- fread('~/Google Drive/My Drive/Gates_MeaslesForecast/data_from_matt/all burden estimates/output-estimated_incidence_2016.csv', header = T)
file2015 <- melt(file2015, id.vars = 'V1')
colnames(file2015) <- c('iso3', 'year', 'cases')
file2015 <- subset(file2015, year == 2015)

#### 2017
file2017 <- fread('~/Google Drive/My Drive/Gates_MeaslesForecast/data_from_matt/all burden estimates/output-estimated_incidence_2017.csv', header = T)
file2017 <- melt(file2017, id.vars = 'V1')
colnames(file2017) <- c('iso3', 'year', 'cases')
file2017 <- subset(file2017, year %in% c(2016:2017))

#### 2018
file2018 <- fread('~/Google Drive/My Drive/Gates_MeaslesForecast/data_from_matt/all burden estimates/output-estimated_incidence_2018.csv', header = T)
file2018 <- melt(file2018, id.vars = 'V1')
colnames(file2018) <- c('iso3', 'year', 'cases')
file2018 <- subset(file2018, year == 2018)

#### 2019
file2019 <- fread('~/Google Drive/My Drive/Gates_MeaslesForecast/data_from_matt/all burden estimates/output-estimated_incidence_2019.csv', header = T)
file2019 <- melt(file2019, id.vars = 'V1')
colnames(file2019) <- c('iso3', 'year', 'cases')
file2019 <- subset(file2019, year == 2019)

#### 2020 
file2020 <- fread('~/Google Drive/My Drive/Gates_MeaslesForecast/data_from_matt/all burden estimates/output-estimated_incidence_2020.csv', header = T)
file2020 <- melt(file2020, id.vars = 'V1')
colnames(file2020) <- c('iso3', 'year', 'cases')
file2020 <- subset(file2020, year == 2020)

#### 2021
file2021 <- fread('~/Google Drive/My Drive/Gates_MeaslesForecast/data_from_matt/all burden estimates/cases_and_deaths_estimates_2021.csv')
file2021 <- subset(file2021, select=c('country', 'year', 'cases')) #melt(file2017, id.vars = 'V1')
colnames(file2021) <- c('iso3', 'year', 'cases')
file2021 <- subset(file2021, year == 2021)

#### 2022
file2022 <- fread('~/Google Drive/My Drive/Gates_MeaslesForecast/data_from_matt/all burden estimates/cases_and_deaths_estimates_2022.csv')
file2022 <- subset(file2022, select=c('country', 'year', 'cases')) #melt(file2017, id.vars = 'V1')
colnames(file2022) <- c('iso3', 'year', 'cases')
file2022 <- subset(file2022, year == 2022)

#### 2023
file2023 <- fread('~/Google Drive/My Drive/Gates_MeaslesForecast/data_from_matt/all burden estimates/cases_and_deaths_estimates_2023.csv')
file2023 <- subset(file2023, select=c('country', 'year', 'cases')) #melt(file2017, id.vars = 'V1')
colnames(file2023) <- c('iso3', 'year', 'cases')
file2023 <- subset(file2023, year == 2023)

#### 2024
file2024 <- fread(paste0(data_downloads_path, 'long_cases_WHO24_best_models.csv'))
file2024 <- subset(file2024, select=c('country', 'year', 'cases')) #melt(file2017, id.vars = 'V1')
colnames(file2024) <- c('iso3', 'year', 'cases')
file2024 %>% group_by(iso3, year) %>% summarise(cases = sum(cases)) -> file2024
file2024 <- subset(file2024, year == 2024)

minor_leakage <- rbind(file2014, file2015, file2017, file2018, file2019, file2020, file2021, file2022, file2023, file2024)
colnames(minor_leakage) <- c('iso3', 'year', 'estimated_infections_minor_leakage')

minor_leakage$month <- 1 #assume all annual data is set in month 1
minor_leakage$year <- as.integer(as.character(minor_leakage$year))

df_raw_covariates <- merge(df_raw_covariates, minor_leakage, by=c('iso3', 'year', 'month'), all.x=T)


## -----------------------------------------------------------   
## canonical path no leakage

cp_no_leakage <- fread(paste0(data_downloads_path, 'data_4alyssa_bmgfgrant_noleakage (1).csv')) 
setnames(cp_no_leakage, c("position_canonical_path"), c("position_canonical_path_no_leakage"))

cp_no_leakage$month <- 1 #assume all annual data is set in month 1

df_raw_covariates <- merge(df_raw_covariates, cp_no_leakage, by=c('iso3', 'year', 'month'), all.x=T, all.y=F)


## -----------------------------------------------------------   
## SIA coverage *********without outbreak response********** 

sia <- fread(paste0(data_downloads_path, 'jrf_release_2025/V_SIA_MAIN_MR.csv'))

sia$year <- sia$YEAR
sia$START_DATE <- ifelse(nchar(sia$START_DATE)<5 & nchar(sia$START_DATE)>0, paste0(sia$START_DATE, '-01-01'), sia$START_DATE)
sia$start_month <- as.numeric(format(as.Date(sia$`START_DATE`), "%m"))
sia$end_month <- as.numeric(format(as.Date(sia$`END_DATE`), "%m"))


sia <- subset(sia, `ACTIVITY_TYPE` %!in% c('OR','FollowUp/OR','Emergency campaign', 'CaseResponse','FollowUp/Outbreak response', 'Outbreak response', 'Case response'))

sia$`Age group` <- sia$AGEGROUP
sia$`% Adm coverage` <- sia$ADMIN_COVERAGE
sia$`Reached population` <- sia$DOSES

source(paste0(data_downloads_path, 'convertAgeSIA.R'))
sia$age_start <- NA
sia$age_end <- NA
for(i in 1:dim(sia)[1]){
  message(i)
  sia$age_start[i] <-  convertAgeSIA(sia$`Age group`[i])[1]
  sia$age_end[i] <-  convertAgeSIA(sia$`Age group`[i])[2]
}

sia$age_range <- sia$age_end - sia$age_start
sia$sia_coverage <- sia$`% Adm coverage`
sia$reached_population <- sia$`Reached population`

sia <- subset(sia, select = c('COUNTRY', 'year', 'age_start', 'age_end', 'age_range', 'sia_coverage', 'start_month', 'end_month', 'reached_population'))
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
  df_long_to_add <- data.frame(iso3 = sia$`COUNTRY`[i],
                               year = sia$year[i],
                               month = months,
                               age_start = sia$age_start[i],
                               age_end = sia$age_end[i],
                               sia_coverage = sia$sia_coverage[i],
                               reached_population = sia$reached_population[i])  
  
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
        sia_long_c_y_m <- data.frame(cbind(unique(sia_long_c_y_m$iso3), unique(sia_long_c_y_m$year), unique(sia_long_c_y_m$month), min(sia_long_c_y_m$age_start), max(sia_long_c_y_m$age_end), max(sia_long_c_y_m$sia_coverage), max(sia_long_c_y_m$reached_population)))
        colnames(sia_long_c_y_m) <- c('iso3', 'year', 'month', 'age_start', 'age_end', 'sia_coverage', 'reached_population')
        sia_long_final <- rbind(sia_long_final, sia_long_c_y_m, fill = T)
      }
    }else{
      colnames(sia_long_c_y) <- c('iso3', 'year', 'month', 'age_start', 'age_end', 'sia_coverage', 'reached_population')
      sia_long_final <- rbind(sia_long_final, sia_long_c_y, fill = T)
    }
  }
}

colnames(sia_long_final) <- c('iso3', 'year', 'month', 'sia_age_start', 'sia_age_end', 'sia_coverage', 'reached_population')
sia_long_final$sia_occured <- 1

sia_long_final$sia_age_end <- as.numeric(sia_long_final$sia_age_end)
sia_long_final$sia_age_start <- as.numeric(sia_long_final$sia_age_start)
sia_long_final$sia_age_range <- sia_long_final$sia_age_end - sia_long_final$sia_age_start

sia_long_final$year <- as.numeric(sia_long_final$year)
sia_long_final$month <- as.numeric(sia_long_final$month)

#### average age, % under 5, % under 10
data('popAge1dt', package = "wpp2022", envir = environment())

pop_size <- subset(popAge1dt, select=c("name", "age", "year", "pop"))
pop_size <- subset(pop_size, year > 1979)
setnames(pop_size, old = c("name","pop"), new=c("country","pop_size"))

pop_size_under5 <- subset(pop_size, age < 5)
pop_size_under15 <- subset(pop_size, age < 15)

counts_under5 <- aggregate(pop_size_under5$pop_size, by=list(pop_size_under5$country, pop_size_under5$year), FUN = 'sum')
colnames(counts_under5) <- c("country", "year", "under5")

counts_under15 <- aggregate(pop_size_under15$pop_size, by=list(pop_size_under15$country, pop_size_under15$year), FUN = 'sum')
colnames(counts_under15) <- c("country", "year", "under15")

counts_df <- merge(counts_under5, counts_under15, by=c('country', 'year'))

counts_df$under5 <- counts_df$under5 * 1000
counts_df$under15 <- counts_df$under15 * 1000

counts_df <- merge(counts_df, iso3_lookup, by = 'country')

sia_long_final <- merge(sia_long_final, counts_df, by =c('iso3', 'year'))

sia_long_final2 <- data.table()
for(i in 1:dim(sia_long_final)[1]){
  message(i)  
  sia_long_final_i <- (sia_long_final[i,])
  
  age_min_yr <- floor(sia_long_final_i$sia_age_start / 12)
  age_max_yr <- floor(sia_long_final_i$sia_age_end / 12)
  
  if(is.na(age_max_yr) & is.na(age_min_yr)) {
    age_max_yr <- 4
    age_min_yr <- 0
  } 
  if(is.na(age_max_yr)) age_max_yr = age_min_yr
  pop_temp <- subset(pop_size, country == sia_long_final_i$country & year == sia_long_final_i$year)
  
  pop_temp <- subset(pop_temp, age %in% c(age_min_yr:age_max_yr))
  
  sia_long_final_i$calc_target_pop <-   sum(pop_temp$pop_size) * 1000
  
  sia_long_final2 <- rbind(sia_long_final2, sia_long_final_i)
}

sia_long_final2$calc_reached_population <- (as.numeric(sia_long_final2$sia_coverage) /100) * sia_long_final2$calc_target_pop 

sia_long_final2$sia_coverage_under5_no_outbreak <- ifelse(sia_long_final2$sia_age_end < 59, as.numeric(sia_long_final2$calc_reached_population / sia_long_final2$under5), (as.numeric(sia_long_final2$sia_coverage) /100) )
sia_long_final2$sia_coverage_under15_no_outbreak <- ifelse(sia_long_final2$sia_age_end < 180, as.numeric(sia_long_final2$calc_reached_population / sia_long_final2$under15), (as.numeric(sia_long_final2$sia_coverage) /100) )
sia_long_final2 <- subset(sia_long_final2, select=c('iso3',  'year', 'month', 'sia_coverage','sia_coverage_under5_no_outbreak', 'sia_coverage_under15_no_outbreak'))

sia_long_final2$sia_coverage_no_outbreak <- sia_long_final2$sia_coverage
sia_long_final2$sia_coverage <- NULL
sia_long_final2$sia_age_start <- NULL
sia_long_final2$sia_age_end <- NULL

df_raw_covariates <- merge(df_raw_covariates, sia_long_final2, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
df_raw_covariates$sia_coverage_no_outbreak <- ifelse(is.na(df_raw_covariates$sia_coverage_no_outbreak),0,df_raw_covariates$sia_coverage_no_outbreak)
df_raw_covariates$sia_coverage_under5_no_outbreak <- ifelse(is.na(df_raw_covariates$sia_coverage_under5_no_outbreak),0,df_raw_covariates$sia_coverage_under5_no_outbreak)
df_raw_covariates$sia_coverage_under15_no_outbreak <- ifelse(is.na(df_raw_covariates$sia_coverage_under15_no_outbreak),0,df_raw_covariates$sia_coverage_under15_no_outbreak)

## -----------------------------------------------------------   
## SIA coverage *********ONLY outbreak response********** 

sia <- fread(paste0(data_downloads_path, 'jrf_release_2025/V_SIA_MAIN_MR.csv'))

sia$year <- sia$YEAR
sia$START_DATE <- ifelse(nchar(sia$START_DATE)<5 & nchar(sia$START_DATE)>0, paste0(sia$START_DATE, '-01-01'), sia$START_DATE)
sia$start_month <- as.numeric(format(as.Date(sia$`START_DATE`), "%m"))
sia$end_month <- as.numeric(format(as.Date(sia$`END_DATE`), "%m"))


sia <- subset(sia, `ACTIVITY_TYPE` %in% c('OR','FollowUp/OR','Emergency campaign', 'CaseResponse','FollowUp/Outbreak response', 'Outbreak response', 'Case response'))

sia$`Age group` <- sia$AGEGROUP
sia$`% Adm coverage` <- sia$ADMIN_COVERAGE
sia$`Reached population` <- sia$DOSES

source(paste0(data_downloads_path, 'convertAgeSIA.R'))
sia$age_start <- NA
sia$age_end <- NA
for(i in 1:dim(sia)[1]){
  message(i)
  sia$age_start[i] <-  convertAgeSIA(sia$`Age group`[i])[1]
  sia$age_end[i] <-  convertAgeSIA(sia$`Age group`[i])[2]
}

sia$age_range <- sia$age_end - sia$age_start
sia$sia_coverage <- sia$`% Adm coverage`
sia$reached_population <- sia$`Reached population`

sia <- subset(sia, select = c('COUNTRY', 'year', 'age_start', 'age_end', 'age_range', 'sia_coverage', 'start_month', 'end_month', 'reached_population'))
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
  df_long_to_add <- data.frame(iso3 = sia$`COUNTRY`[i],
                               year = sia$year[i],
                               month = months,
                               age_start = sia$age_start[i],
                               age_end = sia$age_end[i],
                               sia_coverage = sia$sia_coverage[i],
                               reached_population = sia$reached_population[i])  
  
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
        sia_long_c_y_m <- data.frame(cbind(unique(sia_long_c_y_m$iso3), unique(sia_long_c_y_m$year), unique(sia_long_c_y_m$month), min(sia_long_c_y_m$age_start), max(sia_long_c_y_m$age_end), max(sia_long_c_y_m$sia_coverage), max(sia_long_c_y_m$reached_population)))
        colnames(sia_long_c_y_m) <- c('iso3', 'year', 'month', 'age_start', 'age_end', 'sia_coverage', 'reached_population')
        sia_long_final <- rbind(sia_long_final, sia_long_c_y_m, fill = T)
      }
    }else{
      colnames(sia_long_c_y) <- c('iso3', 'year', 'month', 'age_start', 'age_end', 'sia_coverage', 'reached_population')
      sia_long_final <- rbind(sia_long_final, sia_long_c_y, fill = T)
    }
  }
}

colnames(sia_long_final) <- c('iso3', 'year', 'month', 'sia_age_start', 'sia_age_end', 'sia_coverage', 'reached_population')
sia_long_final$sia_occured <- 1

sia_long_final$sia_age_end <- as.numeric(sia_long_final$sia_age_end)
sia_long_final$sia_age_start <- as.numeric(sia_long_final$sia_age_start)
sia_long_final$sia_age_range <- sia_long_final$sia_age_end - sia_long_final$sia_age_start

sia_long_final$year <- as.numeric(sia_long_final$year)
sia_long_final$month <- as.numeric(sia_long_final$month)

#### average age, % under 5, % under 10
data('popAge1dt', package = "wpp2022", envir = environment())

pop_size <- subset(popAge1dt, select=c("name", "age", "year", "pop"))
pop_size <- subset(pop_size, year > 1979)
setnames(pop_size, old = c("name","pop"), new=c("country","pop_size"))

pop_size_under5 <- subset(pop_size, age < 5)
pop_size_under15 <- subset(pop_size, age < 15)

counts_under5 <- aggregate(pop_size_under5$pop_size, by=list(pop_size_under5$country, pop_size_under5$year), FUN = 'sum')
colnames(counts_under5) <- c("country", "year", "under5")

counts_under15 <- aggregate(pop_size_under15$pop_size, by=list(pop_size_under15$country, pop_size_under15$year), FUN = 'sum')
colnames(counts_under15) <- c("country", "year", "under15")

counts_df <- merge(counts_under5, counts_under15, by=c('country', 'year'))

counts_df$under5 <- counts_df$under5 * 1000
counts_df$under15 <- counts_df$under15 * 1000

counts_df <- merge(counts_df, iso3_lookup, by = 'country')

sia_long_final <- merge(sia_long_final, counts_df, by =c('iso3', 'year'))

sia_long_final2 <- data.table()
for(i in 1:dim(sia_long_final)[1]){
  message(i)  
  sia_long_final_i <- (sia_long_final[i,])
  
  
  age_min_yr <- floor(sia_long_final_i$sia_age_start / 12)
  age_max_yr <- floor(sia_long_final_i$sia_age_end / 12)
  
  if(is.na(age_max_yr) & is.na(age_min_yr)) {
    age_max_yr <- 4
    age_min_yr <- 0
  } 
  if(is.na(age_max_yr)) age_max_yr = age_min_yr
  pop_temp <- subset(pop_size, country == sia_long_final_i$country & year == sia_long_final_i$year)
  
  pop_temp <- subset(pop_temp, age %in% c(age_min_yr:age_max_yr))
  
  sia_long_final_i$calc_target_pop <-   sum(pop_temp$pop_size) * 1000
  
  sia_long_final2 <- rbind(sia_long_final2, sia_long_final_i)
}

sia_long_final2$calc_reached_population <- (as.numeric(sia_long_final2$sia_coverage) /100) * sia_long_final2$calc_target_pop 

sia_long_final2$sia_coverage_under5_only_outbreak <- ifelse(sia_long_final2$sia_age_end < 59, as.numeric(sia_long_final2$calc_reached_population / sia_long_final2$under5), (as.numeric(sia_long_final2$sia_coverage) /100) )
sia_long_final2$sia_coverage_under15_only_outbreak <- ifelse(sia_long_final2$sia_age_end < 180, as.numeric(sia_long_final2$calc_reached_population / sia_long_final2$under15), (as.numeric(sia_long_final2$sia_coverage) /100) )
sia_long_final2 <- subset(sia_long_final2, select=c('iso3',  'year', 'month', 'sia_coverage','sia_coverage_under5_only_outbreak', 'sia_coverage_under15_only_outbreak'))

sia_long_final2$sia_coverage_only_outbreak <- sia_long_final2$sia_coverage
sia_long_final2$sia_coverage <- NULL
sia_long_final2$sia_age_start <- NULL
sia_long_final2$sia_age_end <- NULL

df_raw_covariates <- merge(df_raw_covariates, sia_long_final2, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))
df_raw_covariates$sia_coverage_only_outbreak <- ifelse(is.na(df_raw_covariates$sia_coverage_only_outbreak),0,df_raw_covariates$sia_coverage_only_outbreak)
df_raw_covariates$sia_coverage_under5_only_outbreak <- ifelse(is.na(df_raw_covariates$sia_coverage_under5_only_outbreak),0,df_raw_covariates$sia_coverage_under5_only_outbreak)
df_raw_covariates$sia_coverage_under15_only_outbreak <- ifelse(is.na(df_raw_covariates$sia_coverage_under15_only_outbreak),0,df_raw_covariates$sia_coverage_under15_only_outbreak)

## -----------------------------------------------------------   
## clean up intermediate SIA variables no longer needed

#### only binary for outbreak reponse campaign
df_raw_covariates$outbreak_reponse_SIA <- ifelse(df_raw_covariates$sia_coverage_only_outbreak > 0,1,0)

#### drop other campaign coverage info
df_raw_covariates$sia_coverage_only_outbreak <- NULL
df_raw_covariates$sia_coverage_under5_only_outbreak <- NULL
df_raw_covariates$sia_coverage_under15_only_outbreak <- NULL
df_raw_covariates$sia_coverage_no_outbreak <- NULL
df_raw_covariates$sia_coverage_under5_no_outbreak <- NULL
df_raw_covariates$sia_coverage_under15_no_outbreak <- NULL

df_raw_covariates$birth_cohort_size_sia <- NULL

## ----------------------------------------------------------- 
## add modal months

df_sine_max <- fread('~/Google Drive/My Drive/Gates_MeaslesForecast/updates_jhu/df_sine_modal_months.csv')
head(df_sine_max)
df_sine_max <- subset(df_sine_max, select = c("iso3", "month_to_add"))

df_raw_covariates2 <- df_sine_max %>%
  left_join(df_raw_covariates, by = "iso3") %>%
  mutate(outbreak_mode = ifelse(month == month_to_add, 1, 0)) %>%
  dplyr::select(-month_to_add) 

df_raw_covariates <- copy(df_raw_covariates2)


## -----------------------------------------------------------   
## average age of infection of suspected cases

table1 <- fread(paste0(data_downloads_path, 'data_from_Seb_20250903/Table 1.csv')) 

table1 %>% group_by(COUNTRY, ONSET_YEAR, ONSET_MONTH) %>% summarise(min_cases_adm1 = min(SUSPECT), max_cases_adm1 = max(SUSPECT), cv_cases = CoefVar(SUSPECT)) -> heterogeneity_df

setnames(heterogeneity_df, old = c("COUNTRY", "ONSET_YEAR", "ONSET_MONTH" ), new = c("iso3", "year", "month"))

heterogeneity_df$case_range_adm1 <- heterogeneity_df$max_cases_adm1 - heterogeneity_df$min_cases_adm1

heterogeneity_df <- subset(heterogeneity_df, select = c('iso3', 'year', 'month', 'case_range_adm1', 'cv_cases'))
heterogeneity_df$year <- as.numeric(heterogeneity_df$year)
heterogeneity_df$month <- as.numeric(heterogeneity_df$month)

df_raw_covariates <- merge(df_raw_covariates, heterogeneity_df, by = c('iso3', 'year', 'month'), all.x = T, all.y=F)

df_raw_covariates <- subset(df_raw_covariates, year < 2025)


## -----------------------------------------------------------   
## add lat and long by country

countries <- gisco_get_countries(year = "2016", epsg = "4326", resolution = "20")

#### calculate centroid & extract coordinates
centroids <- st_centroid(countries)
coords <- st_coordinates(centroids)

#### add latitude and longitude to the original object
countries$longitude <- coords[, 1]
countries$latitude  <- coords[, 2]

#### format and merge
countries <- data.frame(subset(countries, select = c('ISO3_CODE', 'latitude', 'longitude')))
setnames(countries, old=c('ISO3_CODE'), new=c('iso3'))
countries$geometry <- NULL

df_raw_covariates <- merge(df_raw_covariates, countries, by = 'iso3', all.x=T, all.y=F)


## -----------------------------------------------------------   
## radial month means and standard deviations

df_raw_covariates %>% group_by(iso3, year, month) %>% summarise(cases=sum(suspected_cases, na.rm=T)) -> cases_iso3_month
cases_iso3_month$degrees <- (as.numeric(cases_iso3_month$month) / 12) * 360

# Create a circular object
cases_iso3_month$radial  <- circular(cases_iso3_month$degrees, type = "angles", units = "degrees", modulo = "2pi")

max_cases_month <- cases_iso3_month %>%
  group_by(iso3) %>%
  summarise(mean_radial = weighted.mean(as.numeric(radial), w=cases, na.rm=T), sd_radial=sqrt(Hmisc::wtd.var(as.numeric(radial), weights=cases,na.rm=T))) %>%
  ungroup()

max_cases_month$month <- as.numeric((max_cases_month$mean_radial/360) * 12) 
max_cases_month$month <- ifelse(max_cases_month$month==0,0.0005,max_cases_month$month )

max_cases_month$sd_month <- as.numeric((max_cases_month$sd_radial/360) * 12) 

max_cases_month <- subset(max_cases_month, select=c('iso3', 'month', 'sd_month'))
setnames(max_cases_month, old=c('month', 'sd_month'), new=c('radial_mean_month', 'radial_sd_month'))

df_raw_covariates <- merge(df_raw_covariates, max_cases_month, by='iso3', all.x=T) 


## -----------------------------------------------------------   
## serology systematic review

serology <- read_xlsx(paste0(data_downloads_path, 'IHME_MEASLES_SEROPREVALENCE_SYSTEMATIC_REVIEW_Y2024M05D28_0.XLSX'), sheet='extraction_main')

#### remove studies with no specific year information
serology <- subset(serology, !is.na(year_start) | !is.na(year_end))
serology <- subset(serology, year_start != "NA" | year_end != "NA")

#### only keep necessary variables for now
serology <- subset(serology, select=c('iso3', 'year_start', 'month_start', 'year_end', 'month_end', 
                                      'age_start', 'age_start_units','age_end', 'age_end_units', 'age_specific_n', 'vaccinated_status', 'sex', 'special_population', 'estimate', 'measure'))

#### convert all ages to years
serology$age_start <- ifelse(serology$age_start_units %in% c('month','months'), as.numeric(serology$age_start) / 12, serology$age_start)
serology$age_start_units <- ifelse(serology$age_start_units %in% c('month','months'), 'years', serology$age_start_units)

serology$age_start <- ifelse(serology$age_start_units %in% c('weeks'), as.numeric(serology$age_start) / 52, serology$age_start)
serology$age_start_units <- ifelse(serology$age_start_units %in% c('weeks'), 'years', serology$age_start_units)

serology$age_start <- ifelse(serology$age_start_units %in% c('day','days'), as.numeric(serology$age_start) / 365, serology$age_start)
serology$age_start_units <- ifelse(serology$age_start_units %in% c('day','days'), 'years', serology$age_start_units)

serology$age_end <- ifelse(serology$age_end_units %in% c('month','months'), as.numeric(serology$age_end) / 12, serology$age_end)
serology$age_end_units <- ifelse(serology$age_end_units %in% c('month','months'), 'years', serology$age_end_units)

serology$age_end <- ifelse(serology$age_end_units %in% c('weeks'), as.numeric(serology$age_end) / 52, serology$age_end)
serology$age_end_units <- ifelse(serology$age_end_units %in% c('weeks'), 'years', serology$age_end_units)

serology$age_end <- ifelse(serology$age_end_units %in% c('day','days'), as.numeric(serology$age_end) / 365, serology$age_end)
serology$age_end_units <- ifelse(serology$age_end_units %in% c('day','days'), 'years', serology$age_end_units)


#### determine if each entry is among under 5s or over 5s or both
serology$age_start <- as.numeric(serology$age_start)
serology$age_end <- as.numeric(serology$age_end)

serology$under5 <- ifelse(serology$age_start < 5 | serology$age_end < 5 , 1, ifelse(serology$age_start >= 5, 0, NA))
serology$over5 <- ifelse(serology$age_end >= 5, 1, ifelse(serology$age_end < 5, 0, NA))

serology <- subset(serology, select=c('iso3', 'year_start', 'month_start', 'year_end', 'month_end', 'under5', 'over5',
                                      'age_specific_n', 'vaccinated_status', 'sex', 'special_population', 'estimate', 'measure'))

#### fix disparate measures
serology$estimate <- ifelse(serology$measure == 'seronegative', 1-serology$estimate, serology$estimate)
serology$measure <- ifelse(serology$measure == 'seronegative', 'seropositive', serology$measure)

###### for now, only keep obviously positive observations
serology <- subset(serology, measure %in% c('seropositive', 'protective', 'seropositive, adjusted for age distribution', 'weighted seropositive'))

#### assign specific months
expand_months <- function(ys, ms, ye, me) {
  ys <- as.integer(ys)
  ms <- as.integer(ms)
  ye <- as.integer(ye)
  me <- as.integer(me)
  
  start <- as.Date(sprintf("%04d-%02d-01", ys, ms))
  end <- as.Date(sprintf("%04d-%02d-01", ye, me))
  if(start !=end){
    seq(start, end, by = "month")    
  }else{
    start
  }
}

serology$month_end <- ifelse(is.na(as.numeric(serology$month_end)) & is.na(as.numeric(serology$year_end)), 12, serology$month_end)
serology$year_end <- ifelse(is.na(as.numeric(serology$year_end)), serology$year_start, serology$year_end)

serology$year_start <- ifelse(is.na(as.numeric(serology$year_start)), serology$year_end, serology$year_start)
serology$month_start <- ifelse(is.na(as.numeric(serology$month_start)), 1, serology$month_start)
serology$month_end <- ifelse(is.na(as.numeric(serology$month_end)), 12, serology$month_end)

serology$month_start <- ifelse((as.numeric(serology$month_start == 0)), 1, serology$month_start)

serology_v2 <- rbindlist(lapply(1:nrow(serology), function(i) {
  #message(i)
  row <- serology[i,]
  dates <- expand_months(row$year_start, row$month_start, row$year_end, row$month_end)
  
  data.table(
    iso3 = row$iso3,
    age_specific_n = row$age_specific_n,
    vaccinated_status = row$vaccinated_status,
    sex = row$sex,
    special_population = row$special_population,
    estimate = row$estimate,
    measure = row$measure,
    under5 = row$under5,
    over5 = row$over5,
    year = year(dates),
    month = month(dates)
  )
}))

subset(serology_v2, under5 ==1) %>% group_by(iso3, year, month, under5) %>% summarise(mean=weighted.mean(as.numeric(estimate), as.numeric(age_specific_n), na.rm=T)) -> serology_collapsed_under5
head(serology_collapsed_under5)

serology_collapsed_under5 <- subset(serology_collapsed_under5, !is.na(mean) & !is.na(as.numeric(year)) & !is.na(as.numeric(month)))

subset(serology_v2, over5 ==1) %>% group_by(iso3, year, month, over5) %>% summarise(mean=weighted.mean(as.numeric(estimate), as.numeric(age_specific_n), na.rm=T)) -> serology_collapsed_over5
head(serology_collapsed_over5)

serology_collapsed_over5 <- subset(serology_collapsed_over5, !is.na(mean) & !is.na(as.numeric(year))& !is.na(as.numeric(month)))

setnames(serology_collapsed_under5, old=c('mean'), new=c('seroprevalence_under5s'))
setnames(serology_collapsed_over5, old=c('mean'), new=c('seroprevalence_over5s'))
serology_collapsed_under5$under5 <- NULL
serology_collapsed_over5$over5 <- NULL

serology_to_add <- merge(serology_collapsed_under5, serology_collapsed_over5, by=c('iso3', 'year', 'month'), all.x=T, all.y=T)
serology_to_add$year <- as.numeric(serology_to_add$year)
serology_to_add$month <- as.numeric(serology_to_add$month)

df_raw_covariates <- merge(df_raw_covariates, serology_to_add, by=c('iso3', 'year', 'month'), all.x=T)


## -----------------------------------------------------------   
## add additional outcome variable (based on suspected preliminary cases from monthly IVB data)

table1 <- fread(paste0(data_downloads_path, 'data_from_Seb_20250903/Table 1.csv'))

table1_ctry_suspected <- aggregate(table1$SUSPECT, by = list(table1$COUNTRY, table1$ONSET_MONTH, table1$ONSET_YEAR), FUN = sum)
colnames(table1_ctry_suspected) <- c("iso3", "month", "year", "outcome_ivb")

table1_ctry_suspected$month <- as.numeric(table1_ctry_suspected$month)
table1_ctry_suspected$year <- as.numeric(table1_ctry_suspected$year)

table1_ctry_suspected$temp <- table1_ctry_suspected$year + (table1_ctry_suspected$month / 12)
  
table1_ctry_suspected  <- subset(table1_ctry_suspected, temp <= 2025.5) # subset to everything before and from june 2025
table1_ctry_suspected$temp <- NULL

table1_ctry_suspected <- subset(table1_ctry_suspected, !is.na(month))

df_raw_covariates <- merge(df_raw_covariates, table1_ctry_suspected, all.x=T, all.y=T, by=c('iso3', 'year', 'month'))

df_raw_covariates$outcome_ivb <- ifelse(df_raw_covariates$year >=1996 & is.na(df_raw_covariates$outcome_ivb), 0, df_raw_covariates$outcome_ivb)


## -----------------------------------------------------------   
## months since SIA 

df_raw_covariates <- df_raw_covariates %>%
  arrange(iso3, year, month) %>%
  group_by(iso3) %>%
  mutate(
    sia_occurred = ifelse(is.na(sia_coverage_under5), 0, ifelse(sia_coverage_under5 > 0, 1, 0)),
    sia_group = cumsum(sia_occurred),
    months_since_sia = ifelse(
      sia_group == 0, NA,
      ave(sia_occurred, sia_group, FUN = cumsum)
    )
  ) %>%
  ungroup() %>%
  dplyr::select(-sia_group, -sia_occurred)

library(data.table)
setDT(df_raw_covariates)
setorder(df_raw_covariates, iso3, year, month)

df_raw_covariates[, sia_occurred := ifelse(is.na(sia_coverage_under5), 0, ifelse(sia_coverage_under5 > 0, 1, 0))]
df_raw_covariates[, sia_group := cumsum(sia_occurred), by = iso3]

# within each sia_group, count rows since the SIA (row number within group)
df_raw_covariates[, months_since_sia := ifelse(sia_group == 0, NA_real_, rowid(sia_group)), by = iso3]
df_raw_covariates$months_since_sia <- df_raw_covariates$months_since_sia - 1 # so that if there is an SIA that month, the count is 0 not 1

# clean up
df_raw_covariates[, c("sia_occurred", "sia_group") := NULL]


## -----------------------------------------------------------   
## fix issue with missing countries 

df_raw_covariates$country <- NULL
df_raw_covariates$mcv1_annual <- NULL
df_raw_covariates$mcv2_annual <- NULL

fwrite(df_raw_covariates, file = paste0('~/Google Drive/My Drive/Gates_MeaslesForecast/updates_jhu/df_raw_covariates_20260310.csv'))

