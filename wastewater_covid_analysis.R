# ===================================================
# Project: SARS-CoV-2 Wastewater and COVID-19 Case Trend Analysis
#
# Description:
#   This script processes and visualizes SARS-CoV-2 wastewater
#   viral concentration data alongside weekly COVID-19 case counts
#   for Toronto in 2024.
#   The workflow includes data loading, date standardization,
#   filtering for relevant dates and locations,
#   normalization of metrics, and time series visualization
#   to explore the relationship between wastewater signals
#   and reported COVID-19 cases.
# ===================================================

##_ Prepare environment -------------
install.packages ('tidyverse')
install.packages ('readxl')
install.packages ('dplyr')
library('tidyverse')
library('readxl')
library ('dplyr')

##_ Load data sets ------------

cases_covid <- read_excel ('cases.xlsx')
#Weekly recordings of SARS-CoV-2 cases in Toronto

data_zone <- read_excel ('surveillance.xlsx')
#Daily recordings of concentration of SARS-CoV-2 gene copies (Standardized).

data_province <- read_csv('aggregate.csv')
#Weekly recordings representing  average viral concentrations of SARS-CoV-2.

##_ Data manipulation ---------

##__ Standardization of date format ----------

cases_covid$'Week start date' <- as.Date(cases_covid$'Week start date', format = '%Y-%m-%d')

data_zone$'Sample Date' <- as.Date(data_zone$'Sample Date', format = '%Y-%m-%d')

data_province$weekstart <- as.Date(data_province$weekstart, format = '%Y-%m-%d')
#To ensure that the numerical values within the date column in each of the data set is on the same format for downstream analyses.

cases_covid <-cases_covid %>% 
  rename(SampleDate = `Week start date`)

data_zone <- data_zone %>%
  rename(SampleDate = `Sample Date`)

data_province <- data_province %>%
  rename(SampleDate = `weekstart`)
#To ensure that all the columns are called the same. To facilitate downstream analysis

##__ Data Filtering ----------

##___ Filtering by date ----------

cases_covid_filtered <- cases_covid %>% filter(format(SampleDate, "%Y") == '2024' )

data_zone_filtered <- data_zone %>% filter(format(SampleDate, "%Y" ) == '2024')

data_city_filtered <- data_province %>% filter (format(SampleDate, "%Y" ) == '2024')
#Only retained data from 2024 because it was the most recent year for which there was an almost complete year-round data set.

##___ Filtering by relevant columns ----------

cases_covid_filtered <- cases_covid_filtered %>% filter (`Public health unit`== 'Toronto Public Health', Disease=='COVID-19')
cases_covid_filtered <- cases_covid_filtered %>% select ('Public health unit','Disease','# of cases', 'Population', 'Cases per 100,000 population', 'SampleDate')

data_zone_filtered <- data_zone_filtered %>% select ('SampleDate', 'Province', 'GTA')

data_city_filtered <- data_city_filtered %>% filter(city == 'Toronto', measureid == 'covN2')
data_city_filtered <- data_city_filtered %>% select ('province', 'SampleDate', 'w_avg')
#Filtered the data set to reduce the noise from irrelevant columns and to ensure that they only contained information from Toronto or the GTA.

##___ Quality check ----------

cases_covid_filtered <- na.omit (cases_covid_filtered)

data_zone_filtered <-na.omit (data_zone_filtered)

data_city_filtered <-na.omit (data_city_filtered)
#Remove all NA value to prevent downstream errors

##___ Data set rename ----------

cases_covid_final <- cases_covid_filtered

zone_conc_final <- data_zone_filtered

city_final <- data_city_filtered

##_ Data analysis and visualization ----------

##__ Line plot of daily SARS-CoV-2 Concentration in GTA  ----------

ggplot (zone_conc_final, aes(x=SampleDate, y=GTA)) +
        geom_line(color='steelblue') +
          labs(
            x='Date',
            y='Standardized, Log-Transformed SARS-CoV-2 Signal',
            title= 'Daily Wastewater SARS-CoV-2 Concentration in the GTA (2024)'
          ) +
          theme_minimal () +
          theme (
            plot.title = element_text(hjust=0.5)
          )

##__ Line plot of weekly SARS-CoV-2 Concentration in Toronto  ----------

ggplot (city_final, aes(x=SampleDate, y=w_avg)) +
          geom_line(color='steelblue') +
          labs(x='Date',
               y='Avg SARS-CoV-2 Concentration (Unspecific units)',
               title='Weekly Average Wastewater SARS-CoV-2 Concentration in Toronto (2024)') +
          theme_minimal () +
          theme(plot.title=element_text(hjust=0.5)
                )

##__ Line plot of weekly COVID-19 cases in Toronto   ----------

ggplot (cases_covid_final,aes(x=SampleDate, y=`Cases per 100,000 population`))  +
        geom_line(color='steelblue') +
          labs(x='Date',
               y='Cases per 100,000 Population',
               title='Weekly Reported COVID-19 Cases in Toronto (2024)')  +
          theme_minimal () +
          theme(plot.title=element_text(hjust=0.5))

##__ Line plot of weekly COVID-19 cases vs. SARS-CoV-2 Concentration in Toronto ----------

weekly_combined <- left_join(
  cases_covid_final,
  city_final,
  by = "SampleDate"
)
#Joined the weekly COVID-19 case data with wastewater data by matching the sample dates, enabling comparison over time.

weekly_combined <- weekly_combined %>%
  mutate(
    cases_norm = (`Cases per 100,000 population` - min(`Cases per 100,000 population`, na.rm = TRUE)) /
      (max(`Cases per 100,000 population`, na.rm = TRUE) - min(`Cases per 100,000 population`, na.rm = TRUE)),
    wastewater_norm = (w_avg - min(w_avg, na.rm = TRUE)) /
      (max(w_avg, na.rm = TRUE) - min(w_avg, na.rm = TRUE))
  )
#Normalized both metrics using min-max scaling (0 to 1)

weekly_long <- weekly_combined %>%
  select(SampleDate, cases_norm, wastewater_norm) %>%
  pivot_longer(cols = c(cases_norm, wastewater_norm), names_to = "Metric", values_to = "Value")
#Reshaped to long format for ggplot

weekly_long$Metric <- recode(weekly_long$Metric,
                             cases_norm = "COVID-19 Cases",
                             wastewater_norm = "Wastewater Viral Load")
#Replaced variable names for cleaner legend

ggplot(weekly_long, aes(x = SampleDate, y = Value, color = Metric)) +
  geom_line(size = 1.1) +
  labs(
    title = "Normalized Weekly Trends: COVID-19 Cases vs. Wastewater SARS-CoV-2 in Toronto (2024)",
    x = "Date",
    y = "Normalized Value (0–1)",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
#Plotted both trends together






