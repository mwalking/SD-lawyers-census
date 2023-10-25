###--------------------------------------------------
### SD Census data of lawyers data processing file
### Mason Walker
### 10/15/23
### https://www.census.gov/programs-surveys/acs
###--------------------------------------------------

library(tidyverse)
library(tidycensus)

# Create variable viewer

pums_vars <- pums_variables |>
  filter(year == 2021,
         survey == 'acs5',
         level == 'person')


# Create San Diego County PUMAS

sd_pumas_21 <- paste0('0', c(7301:7330))
sd_pumas_16 <- paste0('0', c(7301:7322))


# Variables 

per_vars <- c('ADJINC', # Income adjustment factor
              'AGEP',   # Age
              'COW',    # Class of worker
              'FER',    # Gave birth last 12 months
              'HINS1',  # Insurance current employer
              'JWMNP',  # Travel time work
              'JWTR',   # Means of trans.
              'LANX',   # Lang. other than English @ home
              'MAR',    # Marital status
              'MARHD',  # Divorced last 12 months
              'MARHM',  # Number of times married
              'MIL',    # Served military
              'SEX',    # Sex
              'WAGP',   # Wages
              'WKHP',   # Hours worked per week
              )





