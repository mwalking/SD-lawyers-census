###--------------------------------------------------
### SD Census data of lawyers data processing file
### Mason Walker
### 10/15/23
### https://www.census.gov/programs-surveys/acs
###--------------------------------------------------

library(tidyverse)
library(tidycensus)
library(srvyr)


# Get the number of lawyers in SD county from 2012-2022
# Data comes from ACS PUMA data
# Universe:  Full-time, year-round civilian employed
# Population 22 years and over for various law professions


# Create San Diego County PUMAS
sd_pumas_10 <- paste0('0', c(7301:7322))
sd_pumas_20 <- paste0('0', c(7301:7330))


# Years that correspond to each Puma
years_10 <- c(2012:2019)
years_20 <- c(2021,2022)


# Pulling data for years 2012-2019
lawyers_num_10 <- map(
  years_10,
  ~ get_pums(
    variables = c('OCCP',
                  'AGEP',
                  'ESR',
                  'NAICSP'),
    state = 'CA',
    survey = 'acs1',
    puma = sd_pumas_10,
    year = .x,
    key = Sys.getenv("CENSUSAPI"),
    rep_weights = 'person',
    show_call = TRUE
    ) |>
  filter((OCCP %in% c('2100', # Lawyers
                      '2105', # Judicial law clerks
                      '2145')) & # Paralegals and legal clerks 
           AGEP >= 22 &
           (ESR == '1' |
              ESR == '2')
         )
  ) |>
  map2(years_10, ~ mutate(.x, year = .y))  # add year as id variable


# Pulling data for years 2021-2022
lawyers_num_20 <- map(
  years_20,
  ~ get_pums(
    variables = c('OCCP',
                  'AGEP',
                  'ESR',
                  'NAICSP'),
    state = 'CA',
    survey = 'acs1',
    puma = sd_pumas_20,
    year = .x,
    key = Sys.getenv("CENSUSAPI"),
    rep_weights = 'person',
    show_call = TRUE
  ) |>
    filter((OCCP %in% c('2100', # Lawyers
                        '2105', # Judicial law clerks
                        '2145') & # Paralegals and legal clerks
             AGEP >= 22 &
             (ESR == '1' |
                ESR == '2'))
    )
  ) |>
  map2(years_20, ~ mutate(.x, year = .y))  # add year as id variable


# Combine lists into single list
combined_list <- c(lawyers_num_10, lawyers_num_20)


# Create collapsed dataframe
lawyer_nums <- do.call(rbind, lapply(combined_list, data.frame))


# Create survey design object
lawyer_cnt_design <- to_survey(
  lawyer_nums,
  type = "person",
  class = "srvyr",
  design = "rep_weights"
)

# Bind estimates together
lawyer_cnt <- data.frame()
occupation <- c('2145',
                '2100')

for (i in occupation) {
  lawyers_jobs <- lawyer_cnt_design |>
  filter(OCCP == i) |>
  group_by(year) |>
  survey_count(OCCP,
               vartype = c("se", "ci"),
               level = .95)
  
  lawyer_cnt <- rbind(lawyer_cnt,lawyers_jobs)
}

lawyer_cnt <- lawyer_cnt |>
  mutate(oCCP_desc = case_when(OCCP %in% '2145' ~ 'Paralegals & Assistants',
                               OCCP %in% '2100'~ 'Lawyers'))

###--------------------------------------------------
# Get a detailed look of the demographics of lawyers
# Data comes from ACS PUMA data
# Universe:  Full-time, year-round civilian employed
# Population 22 years and over for various law professions

# Create variable viewer
pums_vars <- pums_variables |>
  filter(year == 2021,
         survey == 'acs5',
         level == 'person')


# Variables 
per_vars_21 <- c('ADJINC', # Income adjustment factor
                 'AGEP',   # Age
                 'COW',    # Class of worker
                 'FER',    # Gave birth last 12 months
                 'HINS1',  # Insurance current employer
                 'JWMNP',  # Travel time work
                 'JWTRNS', # Means of trans.
                 'LANX',   # Lang. other than English @ home
                 'MAR',    # Marital status
                 'MARHD',  # Divorced last 12 months
                 'MARHM',  # Number of times married
                 'MIL',    # Served military
                 'SEX',    # Sex
                 'WAGP',   # Wages
                 'WKHP',   # Hours worked per week
                 'DIS',    # Disabled
                 'ESR',    # Employment status
                 'FOD1P',  # Field of degree
                 'HICOV',  # Health insurance
                 'HISP',   # Hispanic origin
                 'INDP',   # Industry
                 'MIGSP',  # Migration
                 'MSP',    # Married
                 'NAICSP', # NAICS codes
                 'NATIVITY', # Native born
                 'OC',     # Have children
                 'PINCP',  # Income
                 'RAC1P',  # Recoded race
                 'SOCP',   # Occupation recode
                 'OCCP',  # Occupation
                 'PUMA'
              )


# Pulling data for San Diego law professions
sd_law_demos <- data.frame()

for (i in occupation) {
sd_law_pums <- get_pums(variables = per_vars_21,
                      state = 'CA',
                      survey = 'acs5',
                      puma = sd_pumas_20,
                      year = 2021,
                      key = Sys.getenv("CENSUSAPI"),
                      rep_weights = 'person',
                      show_call = TRUE,
                      variables_filter = list(OCCP = (i))
                      )

sd_law_demos <- rbind(sd_law_demos, sd_law_pums)

}


# Pulling data for all of U.S. law professions
us_law_demos <- data.frame()

for (i in occupation) {
  us_law_pums <- get_pums(variables = per_vars_21,
                          survey = 'acs5',
                          state = 'all',
                          year = 2021,
                          key = Sys.getenv("CENSUSAPI"),
                          rep_weights = 'person',
                          show_call = TRUE,
                          variables_filter = list(OCCP = (i))
  ) |>
    filter(!(PUMA %in% sd_pumas_20))
  
  us_law_demos <- rbind(us_law_demos, us_law_pums)
  
}


###--------------------------------------------------
# Recoding the ACS 5-year file
# Apply recoding rules to both US and SD data

sd_law_demos_r <- sd_law_demos |>
  mutate(
    AGE_R = case_when(AGEP < 18 ~ 1,
                      AGEP >= 18 & AGEP < 30 ~ 2,
                      AGEP >= 30 & AGEP < 50 ~ 3,
                      AGEP >= 50 & AGEP < 65 ~ 4,
                      AGEP >= 65 ~ 5,
                      TRUE ~ 6),
    AGE_R2 = case_when(AGEP < 18 ~ as.numeric(NA),
                       AGEP >= 18 & AGEP <= 24 ~ 1,
                       AGEP >= 25 & AGEP <= 34 ~ 2,
                       AGEP >= 35 & AGEP <= 44 ~ 3,
                       AGEP >= 45 & AGEP <= 54 ~ 4,
                       AGEP >= 55 & AGEP <= 64 ~ 5,
                       AGEP >= 65 & AGEP <= 74 ~ 6,
                       TRUE ~ 7),
    RACEETHN_R = case_when(RAC1P == "1" & HISP == "01" ~ "White", # White non-Hispanic
                           RAC1P == "2" & HISP == "01" ~ "Black", # Black non-Hispanic
                           RAC1P == "6" & HISP == "01" ~ "Asian", # Asian non-Hispanic
                           RAC1P %in% c("3", "4", "5", "7", "8", "9") & HISP == "01" ~ "Other non-Hispanic", # Other non-Hispanic
                           TRUE ~ "Hispanic"),
    TRAVEL = case_when(JWMNP <= 15 ~ 1, # Travel time of 15min or less to work
                       JWMNP > 15 & JWMNP <= 30 ~ 2, # Travel time of 15min-30min to work
                       JWMNP > 30 & JWMNP <= 45 ~ 3, # Travel time of 30min-45min to work
                       JWMNP > 45 & JWMNP <= 60 ~ 4, # Travel time of 45min-60min to work
                       JWMNP > 60 & JWMNP <= 90 ~ 5, # Travel time of 60min-90min to work
                       TRUE ~ 6), # Travel time of 90+min to work
    WORK_HRS = case_when(WKHP <= 10 ~ 1, # Works 10 or less hrs
                         WKHP > 10 & WKHP <= 20 ~ 2, # Works 10-20 hrs
                         WKHP > 20 & WKHP <= 30 ~ 3, # Works 20-30 hrs
                         WKHP > 30 & WKHP <= 40 ~ 4, # Works 30-40 hrs
                         WKHP > 40 & WKHP <= 50 ~ 5, # Works 40-50 hrs
                         WKHP > 50 & WKHP <= 60 ~ 6, # Works 50-60 hrs
                         TRUE ~ 7), # Works 60+ hrs
    ))
  
    ))))



###--------------------------------------------------
# Export the data as csv's for report 


write.csv(lawyer_cnt, "lawyers_time_series.csv")
write.csv("sd_lawyers_demos.csv")
write.csv()

