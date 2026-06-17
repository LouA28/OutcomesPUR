## Load Library

library(dplyr)
library(tidyr)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(readxl)
library(plotly)
library(treemap)
library(treemapify)
library(data.table)
library(DT)
library(openxlsx)
library(stringr)
library(png)
library(scales)
library(purrr)
library(lubridate)
library(htmlwidgets)
library(htmltools)
library(shinyjs)
library(formattable)
library(sf)
library(leaflet)
library(profvis)
library(shinycssloaders)

## Load PUR Data & UK trade agreements

PUR_data1 <- readRDS("FTA monitoring/final_importPUR2026-06-15.RDS")

source("FTA monitoring/uk_trade_agreement_columns.R")

Pref_data1 <- readRDS("FTA monitoring/PUR_type of preference2026-06-15.RDS")

#-----------------------------------------------------#

# -----------------------------------------------------------------------------
# EU MEMBER COUNTRY CODES — remove individual countries, keep "EU" bloc row
# -----------------------------------------------------------------------------

eu_country_codes <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")

# Remove individual EU member rows — the "EU" bloc row already covers them
PUR_data1 <- PUR_data1 %>%
  filter(!cooalpha %in% eu_country_codes)


## Create script for Quarterly analysis + function to generate monthly, quarterly & yearly

## 1. Create new columns that state that month, quarter and year 
  PUR_data1 <- PUR_data1 %>%
  mutate(
    # Cast to integer first — Year and Month are stored as strings in your data
    Year  = as.integer(Year),
    Month = as.integer(Month),
    
    # Quarter:   Q1 = months 1-3, Q2 = 4-6, Q3 = 7-9, Q4 = 10-12
    quarter = paste0(Year, " Q", ceiling(Month / 3)),
    
    # Half-year: H1 = months 1-6, H2 = months 7-12
    half    = paste0(Year, " H", if_else(Month <= 6, 1L, 2L))
  )

agg <- function(data, timeframe) {
  data %>%
    group_by({{ timeframe }}, country_name, Year) %>%
    summarise(
      Total_imp      = sum(Total_imp,      na.rm = TRUE),
      Pref_Trade     = sum(Pref_Trade,     na.rm = TRUE),
      Eligible_Trade = sum(Eligible_Trade, na.rm = TRUE),
      Total_volume   = sum(Total_volume,      na.rm = TRUE),
      Pref_Volume    = sum(Pref_Volume,     na.rm = TRUE),
      Eligible_Volume = sum(Eligible_Volume, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange({{ timeframe }}, country_name, Year) %>%
    add_uk_trade_cols(country_col = "country_name")
}

# 1. MONTHLY  (grouped by perref YYYYMM)

monthly <- agg(PUR_data1, Month)

# 2. QUARTERLY  (grouped by year + quarter label e.g. "2022 Q1")

quarterly <- agg(PUR_data1, quarter)

# 3. 6-MONTHLY / HALF-YEAR  (grouped by year + half label e.g. "2022 H1")

half_yearly <- agg(PUR_data1, half)

# 4. Yearly 

yearly <- PUR_data1 %>%
  group_by(Year, country_name) %>%
  summarise(
    Total_imp       = sum(Total_imp,       na.rm = TRUE),
    Pref_Trade      = sum(Pref_Trade,      na.rm = TRUE),
    Eligible_Trade  = sum(Eligible_Trade,  na.rm = TRUE),
    Total_volume    = sum(Total_volume,    na.rm = TRUE),
    Pref_Volume     = sum(Pref_Volume,     na.rm = TRUE),
    Eligible_Volume = sum(Eligible_Volume, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year, country_name) %>%
  add_uk_trade_cols(country_col = "country_name")



# =============================================================================
# BUILD BLOC ROWS
# =============================================================================


cptpp_members <- c(
  "Australia","Brunei","Canada","Chile","Japan","Malaysia",
  "Mexico","New Zealand","Peru","Singapore","Vietnam"
)

dcts_all <- c(
  "Afghanistan","Angola","Bangladesh","Benin","Bhutan","Burkina Faso",
  "Burundi","Cambodia","Central African Republic","Chad","Comoros",
  "Democratic Republic of the Congo","Djibouti","East Timor","Eritrea",
  "Ethiopia","Gambia","Guinea","Guinea-Bissau","Haiti","Kiribati",
  "Laos","Lesotho","Liberia","Madagascar","Malawi","Mali","Mauritania",
  "Mozambique","Myanmar","Nepal","Niger","Rwanda","Sao Tome and Principe",
  "Senegal","Sierra Leone","Solomon Islands","Somalia","South Sudan",
  "Sudan","Tanzania","Togo","Tuvalu","Uganda","Yemen","Zambia",
  "Algeria","Bolivia","Cape Verde","Congo","Cook Islands",
  "Federated States of Micronesia","Kyrgyzstan","Mongolia","Nigeria",
  "Niue","Pakistan","Philippines","Sri Lanka","Syria","Tajikistan",
  "Uzbekistan","Vanuatu","India","Indonesia"
)

# =============================================================================
# EXCLUDE UNWANTED COUNTRIES
# =============================================================================

exclude_countries <- c(
  "PEM","Ceuta","Western Sahara",
  "United States Minor Outlying Islands"
)

monthly   <- monthly   %>% filter(!country_name %in% exclude_countries)
quarterly <- quarterly %>% filter(!country_name %in% exclude_countries)
yearly    <- yearly    %>% filter(!country_name %in% exclude_countries)

monthly     <- monthly     %>% filter(!country_name %in% c("CPTPP", "DCTS"))
quarterly   <- quarterly   %>% filter(!country_name %in% c("CPTPP", "DCTS"))
half_yearly <- half_yearly %>% filter(!country_name %in% c("CPTPP", "DCTS"))
yearly      <- yearly      %>% filter(!country_name %in% c("CPTPP", "DCTS"))

build_bloc <- function(data, members, bloc_name) {
  members <- members[members %in% data$country_name]
  if (length(members) == 0) return(NULL)
  
  time_cols <- intersect(
    c("x_label","sort_date","Year","Month","quarter","half"),
    names(data)
  )
  
  data %>%
    filter(country_name %in% members) %>%
    group_by(across(all_of(time_cols))) %>%
    summarise(
      Total_imp       = sum(Total_imp,       na.rm = TRUE),
      Pref_Trade      = sum(Pref_Trade,      na.rm = TRUE),
      Eligible_Trade  = sum(Eligible_Trade,  na.rm = TRUE),
      Total_volume    = sum(Total_volume,    na.rm = TRUE),
      Pref_Volume     = sum(Pref_Volume,     na.rm = TRUE),
      Eligible_Volume = sum(Eligible_Volume, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      country_name       = bloc_name,
      uk_trade_agreement = TRUE,
      uk_agreement_name  = case_when(
        bloc_name == "CPTPP" ~
          "Comprehensive and Progressive Agreement for Trans-Pacific Partnership",
        bloc_name == "DCTS"  ~ "Developing Countries Trading Scheme",
        TRUE                 ~ bloc_name
      ),
      uk_dcts      = (bloc_name == "DCTS"),
      uk_dcts_tier = ifelse(bloc_name == "DCTS", "Mixed", NA_character_)
    )
}


monthly_with_blocs <- bind_rows(monthly,
                                build_bloc(monthly,   cptpp_members, "CPTPP"),
                                build_bloc(monthly,   dcts_all,      "DCTS"))

quarterly_with_blocs <- bind_rows(quarterly,
                                  build_bloc(quarterly, cptpp_members, "CPTPP"),
                                  build_bloc(quarterly, dcts_all,      "DCTS"))

yearly_with_blocs <- bind_rows(yearly,
                               build_bloc(yearly, cptpp_members, "CPTPP"),
                               build_bloc(yearly, dcts_all,      "DCTS"))


month_labels <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")

Pref_data1_clean <- Pref_data1 %>%
  mutate(
    perref    = as.character(perref),
    Year      = as.integer(substr(perref, 1, 4)),
    Month     = as.integer(substr(perref, 5, 6)),
    sort_date = as.Date(paste0(Year, "-", formatC(Month, width=2, flag="0"), "-01")),
    x_label   = paste(month_labels[Month], Year)
  )


headline_cache <- lapply(sort(unique(
  as.integer(substr(as.character(Pref_data1$perref), 1, 4))
)), function(yr) {
  s <- Pref_data1 %>%
    mutate(Year = as.integer(substr(as.character(perref), 1, 4))) %>%
    filter(Year == yr,
           !cooalpha %in% c("EU", "CPTPP", "PEM", "DCTS")) %>%
    summarise(
      total      = sum(statvalue, na.rm = TRUE),
      mfn_zero   = sum(statvalue[use_name == "MFN zero (u10)"],  na.rm = TRUE),
      fta_zero   = sum(statvalue[use_name == "PTA zero (u30)"],  na.rm = TRUE),
      dcts_zero  = sum(statvalue[use_name == "GSP zero (u20)"],  na.rm = TRUE)
    ) %>%
    mutate(
      pct_tariff_free = round((mfn_zero + fta_zero + dcts_zero) / total * 100, 1),
      pct_mfn         = round(mfn_zero  / total * 100, 1),
      pct_fta         = round(fta_zero  / total * 100, 1),
      pct_dcts        = round(dcts_zero / total * 100, 1)
    )
  list(year = yr, stats = s)
})
names(headline_cache) <- sapply(headline_cache, function(x) as.character(x$year))

write.csv(monthly,     "FTA monitoring/data/trade_monthly.csv",     row.names = FALSE)
write.csv(quarterly,   "FTA monitoring/data/trade_quarterly.csv",   row.names = FALSE)
write.csv(half_yearly, "FTA monitoring/data/trade_half_yearly.csv", row.names = FALSE)
write.csv(yearly, "FTA monitoring/data/trade_yearly.csv", row.names = FALSE)
saveRDS(monthly_with_blocs,   "FTA monitoring/data/trade_monthly_blocs.RDS")
saveRDS(quarterly_with_blocs, "FTA monitoring/data/trade_quarterly_blocs.RDS")
saveRDS(headline_cache, "FTA monitoring/data/headline_cache.RDS")
saveRDS(Pref_data1_clean, "FTA monitoring/data/pref_data_clean.RDS")
write.csv(yearly_with_blocs, "FTA monitoring/data/trade_yearly.csv", row.names = FALSE)
