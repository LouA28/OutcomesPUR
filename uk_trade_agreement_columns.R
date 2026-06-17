# =============================================================================
# UK TRADE AGREEMENT LOOKUP COLUMNS
# =============================================================================
# Adds FIVE columns to any dataframe that has a country name column:
#
#   uk_trade_agreement    : TRUE / FALSE — does the UK have an agreement?
#   uk_agreement_name     : Name of the agreement (or NA)
#   uk_agreement_in_force : Date the agreement came into force (or status note)
#   uk_dcts               : TRUE / FALSE — is the country eligible for DCTS?
#   uk_dcts_tier          : DCTS tier ("Comprehensive", "Enhanced", "Standard",
#                           or NA if not eligible)
#
# USAGE:
#   1. Source this file:  source("uk_trade_agreement_columns.R")
#   2. Call the function: df <- add_uk_trade_cols(df, country_col = "your_country_column")
#
# Your country column should contain standard English country names.
# A fuzzy match is applied automatically for common variants.
# =============================================================================

library(dplyr)

# -----------------------------------------------------------------------------
# 1. REFERENCE LOOKUP TABLE
# -----------------------------------------------------------------------------

uk_trade_lookup <- tribble(
  ~country,                        ~uk_trade_agreement, ~uk_agreement_name,
  
  # EU (TCA)
  "Austria",                       TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Belgium",                       TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Bulgaria",                      TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Croatia",                       TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Cyprus",                        TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Czech Republic",                TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Denmark",                       TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Estonia",                       TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Finland",                       TRUE,  "UK-EU Trade and Cooperation Agreement",
  "France",                        TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Germany",                       TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Greece",                        TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Hungary",                       TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Ireland",                       TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Italy",                         TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Latvia",                        TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Lithuania",                     TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Luxembourg",                    TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Malta",                         TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Netherlands",                   TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Poland",                        TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Portugal",                      TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Romania",                       TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Slovakia",                      TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Slovenia",                      TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Spain",                         TRUE,  "UK-EU Trade and Cooperation Agreement",
  "Sweden",                        TRUE,  "UK-EU Trade and Cooperation Agreement",
  "European Union",                TRUE,  "UK-EU Trade and Cooperation Agreement",
  
  # Trade blocs
  
  "CPTPP",                         TRUE,  "Comprehensive and Progressive Agreement for Trans-Pacific Partnership",
  "DCTS",                          TRUE,  "Developing Countries Trading Scheme",
  
  # EEA / EFTA
  "Iceland",                       TRUE,  "UK-Iceland-Liechtenstein-Norway FTA",
  "Liechtenstein",                 TRUE,  "UK-Iceland-Liechtenstein-Norway FTA; UK-Switzerland-Liechtenstein Trade Agreement",
  "Norway",                        TRUE,  "UK-Iceland-Liechtenstein-Norway FTA",
  "Switzerland",                   TRUE,  "UK-Switzerland Trade Agreement",
  
  # Bilateral FTAs / EPAs / Association Agreements
  "Albania",                       TRUE,  "UK-Albania Partnership, Trade and Cooperation Agreement",
  "Australia",                     TRUE,  "UK-Australia Free Trade Agreement; CPTPP",
  "Cameroon",                      TRUE,  "UK-Cameroon Economic Partnership Agreement",
  "Canada",                        TRUE,  "UK-Canada Trade Continuity Agreement; CPTPP",
  "Chile",                         TRUE,  "UK-Chile Association Agreement; CPTPP",
  "Colombia",                      TRUE,  "UK-Andean Countries Trade Agreement",
  "Costa Rica",                    TRUE,  "UK-Central America Association Agreement",
  "Cote d'Ivoire",                 TRUE,  "UK-Cote d'Ivoire Stepping Stone EPA",
  "Ecuador",                       TRUE,  "UK-Andean Countries Trade Agreement",
  "Egypt",                         TRUE,  "UK-Egypt Association Agreement",
  "El Salvador",                   TRUE,  "UK-Central America Association Agreement",
  "Faroe Islands",                 TRUE,  "UK-Faroe Islands Free Trade Agreement",
  "Georgia",                       TRUE,  "UK-Georgia Strategic Partnership and Cooperation Agreement",
  "Ghana",                         TRUE,  "UK-Ghana Interim Trade Partnership Agreement",
  "Guatemala",                     TRUE,  "UK-Central America Association Agreement",
  "Honduras",                      TRUE,  "UK-Central America Association Agreement",
  "India",                         TRUE,  "UK-India Comprehensive Economic and Trade Agreement (CETA)",
  "Israel",                        TRUE,  "UK-Israel Trade and Partnership Agreement",
  "Japan",                         TRUE,  "UK-Japan Comprehensive Economic Partnership Agreement; CPTPP",
  "Jordan",                        TRUE,  "UK-Jordan Association Agreement",
  "Kenya",                         TRUE,  "UK-Kenya Economic Partnership Agreement",
  "Kosovo",                        TRUE,  "UK-Kosovo Partnership, Trade and Cooperation Agreement",
  "Lebanon",                       TRUE,  "UK-Lebanon Association Agreement",
  "Malaysia",                      TRUE,  "CPTPP",
  "Mexico",                        TRUE,  "UK-Mexico Trade Continuity Agreement; CPTPP",
  "Moldova",                       TRUE,  "UK-Moldova Strategic Partnership, Trade and Cooperation Agreement",
  "Morocco",                       TRUE,  "UK-Morocco Association Agreement",
  "Mozambique",                    TRUE,  "SACUM-UK Economic Partnership Agreement",
  "New Zealand",                   TRUE,  "UK-New Zealand Free Trade Agreement; CPTPP",
  "Nicaragua",                     TRUE,  "UK-Central America Association Agreement",
  "North Macedonia",               TRUE,  "UK-North Macedonia Partnership, Trade and Cooperation Agreement",
  "Palestinian Authority",         TRUE,  "UK-Palestinian Authority Political, Trade and Partnership Agreement",
  "Panama",                        TRUE,  "UK-Central America Association Agreement",
  "Peru",                          TRUE,  "UK-Andean Countries Trade Agreement; CPTPP",
  "Serbia",                        TRUE,  "UK-Serbia Partnership, Trade and Cooperation Agreement",
  "Singapore",                     TRUE,  "UK-Singapore Free Trade Agreement; UK-Singapore Digital Economy Agreement; CPTPP",
  "South Korea",                   TRUE,  "UK-South Korea Trade Agreement (updated Dec 2025)",
  "Tunisia",                       TRUE,  "UK-Tunisia Association Agreement",
  "Turkey",                        TRUE,  "UK-Turkey Trade Agreement",
  "Ukraine",                       TRUE,  "UK-Ukraine Political, Free Trade and Strategic Partnership Agreement; UK-Ukraine Digital Trade Agreement",
  "United States",                 TRUE,  "UK-US Economic Prosperity Deal (partial)",
  "Vietnam",                       TRUE,  "UK-Vietnam Free Trade Agreement; CPTPP",
  
  # CARIFORUM EPA
  "Antigua and Barbuda",           TRUE,  "CARIFORUM-UK Economic Partnership Agreement",
  "Bahamas",                       TRUE,  "CARIFORUM-UK Economic Partnership Agreement",
  "Barbados",                      TRUE,  "CARIFORUM-UK Economic Partnership Agreement",
  "Belize",                        TRUE,  "CARIFORUM-UK Economic Partnership Agreement",
  "Dominica",                      TRUE,  "CARIFORUM-UK Economic Partnership Agreement",
  "Dominican Republic",            TRUE,  "CARIFORUM-UK Economic Partnership Agreement",
  "Grenada",                       TRUE,  "CARIFORUM-UK Economic Partnership Agreement",
  "Guyana",                        TRUE,  "CARIFORUM-UK Economic Partnership Agreement",
  "Jamaica",                       TRUE,  "CARIFORUM-UK Economic Partnership Agreement",
  "Saint Lucia",                   TRUE,  "CARIFORUM-UK Economic Partnership Agreement",
  "St Kitts and Nevis",            TRUE,  "CARIFORUM-UK Economic Partnership Agreement",
  "St Vincent and the Grenadines", TRUE,  "CARIFORUM-UK Economic Partnership Agreement",
  "Suriname",                      TRUE,  "CARIFORUM-UK Economic Partnership Agreement",
  "Trinidad and Tobago",           TRUE,  "CARIFORUM-UK Economic Partnership Agreement",
  
  # SACUM EPA
  "Botswana",                      TRUE,  "SACUM-UK Economic Partnership Agreement",
  "Eswatini",                      TRUE,  "SACUM-UK Economic Partnership Agreement",
  "Lesotho",                       TRUE,  "SACUM-UK Economic Partnership Agreement",
  "Namibia",                       TRUE,  "SACUM-UK Economic Partnership Agreement",
  "South Africa",                  TRUE,  "SACUM-UK Economic Partnership Agreement",
  
  # ESA EPA
  "Madagascar",                    TRUE,  "ESA-UK Economic Partnership Agreement",
  "Mauritius",                     TRUE,  "ESA-UK Economic Partnership Agreement",
  "Seychelles",                    TRUE,  "ESA-UK Economic Partnership Agreement",
  "Zimbabwe",                      TRUE,  "ESA-UK Economic Partnership Agreement",
  
  # Pacific EPA
  "Fiji",                          TRUE,  "UK-Pacific Economic Partnership Agreement",
  "Papua New Guinea",              TRUE,  "UK-Pacific Economic Partnership Agreement",
  "Samoa",                         TRUE,  "UK-Pacific Economic Partnership Agreement",
  "Solomon Islands",               TRUE,  "UK-Pacific Economic Partnership Agreement"
)

# -----------------------------------------------------------------------------
# 2. DCTS LOOKUP TABLE
# -----------------------------------------------------------------------------

dcts_lookup <- tribble(
  ~country,                             ~uk_dcts_tier,
  
  # Comprehensive Preferences (LDCs) — 0% tariff on 99.8% of products
  "Afghanistan",                        "Comprehensive",
  "Angola",                             "Comprehensive",
  "Bangladesh",                         "Comprehensive",
  "Benin",                              "Comprehensive",
  "Bhutan",                             "Comprehensive",
  "Burkina Faso",                       "Comprehensive",
  "Burundi",                            "Comprehensive",
  "Cambodia",                           "Comprehensive",
  "Central African Republic",           "Comprehensive",
  "Chad",                               "Comprehensive",
  "Comoros",                            "Comprehensive",
  "Democratic Republic of the Congo",   "Comprehensive",
  "Djibouti",                           "Comprehensive",
  "East Timor",                         "Comprehensive",
  "Eritrea",                            "Comprehensive",
  "Ethiopia",                           "Comprehensive",
  "Gambia",                             "Comprehensive",
  "Guinea",                             "Comprehensive",
  "Guinea-Bissau",                      "Comprehensive",
  "Haiti",                              "Comprehensive",
  "Kiribati",                           "Comprehensive",
  "Laos",                               "Comprehensive",
  "Lesotho",                            "Comprehensive",
  "Liberia",                            "Comprehensive",
  "Madagascar",                         "Comprehensive",
  "Malawi",                             "Comprehensive",
  "Mali",                               "Comprehensive",
  "Mauritania",                         "Comprehensive",
  "Mozambique",                         "Comprehensive",
  "Myanmar",                            "Comprehensive",
  "Nepal",                              "Comprehensive",
  "Niger",                              "Comprehensive",
  "Rwanda",                             "Comprehensive",
  "Sao Tome and Principe",              "Comprehensive",
  "Senegal",                            "Comprehensive",
  "Sierra Leone",                       "Comprehensive",
  "Solomon Islands",                    "Comprehensive",
  "Somalia",                            "Comprehensive",
  "South Sudan",                        "Comprehensive",
  "Sudan",                              "Comprehensive",
  "Tanzania",                           "Comprehensive",
  "Togo",                               "Comprehensive",
  "Tuvalu",                             "Comprehensive",
  "Uganda",                             "Comprehensive",
  "Yemen",                              "Comprehensive",
  "Zambia",                             "Comprehensive",
  
  # Enhanced Preferences — 0% tariff on 92% of products
  "Algeria",                            "Enhanced",
  "Bolivia",                            "Enhanced",
  "Cape Verde",                         "Enhanced",
  "Congo",                              "Enhanced",
  "Cook Islands",                       "Enhanced",
  "Federated States of Micronesia",     "Enhanced",
  "Kyrgyzstan",                         "Enhanced",
  "Mongolia",                           "Enhanced",
  "Nigeria",                            "Enhanced",
  "Niue",                               "Enhanced",
  "Pakistan",                           "Enhanced",
  "Philippines",                        "Enhanced",
  "Sri Lanka",                          "Enhanced",
  "Syria",                              "Enhanced",
  "Tajikistan",                         "Enhanced",
  "Uzbekistan",                         "Enhanced",
  "Vanuatu",                            "Enhanced",
  
  # Standard Preferences — 0% tariff on 65% of products
  "India",                              "Standard",
  "Indonesia",                          "Standard"
)

# -----------------------------------------------------------------------------
# 3. NAME NORMALISATION — common variants mapped to lookup keys
# -----------------------------------------------------------------------------

name_variants <- c(
  # EU
  "czechia"                          = "Czech Republic",
  "czech rep"                        = "Czech Republic",
  "the netherlands"                  = "Netherlands",
  "holland"                          = "Netherlands",
  
  # Common alternates
  "united states"                    = "United States",
  "usa"                              = "United States",
  "u.s.a."                           = "United States",
  "us"                               = "United States",
  "u.s."                             = "United States",
  "united states of america"         = "United States",
  "south korea"                      = "South Korea",
  "republic of korea"                = "South Korea",
  "korea, rep."                      = "South Korea",
  "viet nam"                         = "Vietnam",
  "cote d ivoire"                    = "Cote d'Ivoire",
  "ivory coast"                      = "Cote d'Ivoire",
  "cote divoire"                     = "Cote d'Ivoire",
  "cote d'ivoire"                    = "Cote d'Ivoire",
  "côte d'ivoire"                    = "Cote d'Ivoire",
  "côte d ivoire"                    = "Cote d'Ivoire",
  "côte-d'ivoire"                    = "Cote d'Ivoire",
  "côte d’ivoire"                    = "Cote d'Ivoire",
  "timor leste"                      = "East Timor",
  "timor-leste"                      = "East Timor",
  "european union"                   = "European Union",
  "dr congo"                         = "Democratic Republic of the Congo",
  "drc"                              = "Democratic Republic of the Congo",
  "congo, dem. rep."                 = "Democratic Republic of the Congo",
  "democratic republic of congo"     = "Democratic Republic of the Congo",
  "congo - kinshasa"                 = "Democratic Republic of the Congo",
  "congo, rep."                      = "Congo",
  "republic of the congo"            = "Congo",
  "congo - brazzaville"              = "Congo",
  "swaziland"                        = "Eswatini",
  "north macedonia"                  = "North Macedonia",
  "macedonia"                        = "North Macedonia",
  "kyrgyz republic"                  = "Kyrgyzstan",
  "lao pdr"                          = "Laos",
  "lao"                              = "Laos",
  "burma"                            = "Myanmar",
  "myanmar (burma)"                  = "Myanmar",
  "sao tome & principe"              = "Sao Tome and Principe",
  "st kitts and nevis"               = "St Kitts and Nevis",
  "saint kitts and nevis"            = "St Kitts and Nevis",
  "st. kitts and nevis"              = "St Kitts and Nevis",
  "st vincent and the grenadines"    = "St Vincent and the Grenadines",
  "saint vincent and the grenadines" = "St Vincent and the Grenadines",
  "st. vincent & grenadines"         = "St Vincent and the Grenadines",
  "st. vincent and the grenadines"   = "St Vincent and the Grenadines",
  "antigua & barbuda"                = "Antigua and Barbuda",
  "trinidad & tobago"                = "Trinidad and Tobago",
  "brunei darussalam"                = "Brunei",
  "great britain"                    = "United Kingdom",
  "palestine"                        = "Palestinian Authority",
  # St. with period variants
  "st. lucia"                        = "Saint Lucia",
  "st lucia"                         = "Saint Lucia",
  "st. kitts & nevis"                = "St Kitts and Nevis",
  "st. kitts and nevis"              = "St Kitts and Nevis"
)

# -----------------------------------------------------------------------------
# 4. HELPER — normalise a country name vector
# -----------------------------------------------------------------------------

normalise_country <- function(x) {
  trimmed <- trimws(x)
  lower   <- tolower(trimmed)
  ifelse(lower %in% names(name_variants), name_variants[lower], trimmed)
}

# -----------------------------------------------------------------------------
# 5. MAIN FUNCTION
# -----------------------------------------------------------------------------
#
# add_uk_trade_cols(df, country_col = "country")
#
# Arguments:
#   df          : Your dataframe
#   country_col : Name of the column containing country names (string)
#
# Returns the same dataframe with five new columns appended:
#   uk_trade_agreement    (logical  : TRUE if UK has any trade agreement with this country)
#   uk_agreement_name     (character: name(s) of the agreement, or NA)
#   uk_agreement_in_force (character: date/status the agreement came into force, or NA)
#   uk_dcts               (logical  : TRUE if country is eligible under the DCTS scheme)
#   uk_dcts_tier          (character: "Comprehensive" / "Enhanced" / "Standard" / NA)

add_uk_trade_cols <- function(df, country_col = "country") {
  
  if (!country_col %in% names(df)) {
    stop(paste0("Column '", country_col, "' not found in dataframe. ",
                "Set country_col to the correct column name."))
  }
  
  df %>%
    mutate(.norm_country = normalise_country(.data[[country_col]])) %>%
    left_join(uk_trade_lookup, by = c(".norm_country" = "country")) %>%
    left_join(dcts_lookup,     by = c(".norm_country" = "country")) %>%
    mutate(
      uk_trade_agreement    = if_else(is.na(uk_trade_agreement), FALSE, uk_trade_agreement),
      uk_agreement_name     = if_else(uk_trade_agreement, uk_agreement_name, NA_character_),
      uk_dcts               = !is.na(uk_dcts_tier),
      uk_dcts_tier          = uk_dcts_tier
    ) %>%
    select(-.norm_country)
}

# =============================================================================
# HOW TO USE IN YOUR OWN SCRIPT
# =============================================================================
#
# Step 1 — source this file once at the top of your script:
#   source("uk_trade_agreement_columns.R")
#
# Step 2 — call the function on your dataframe:
#   your_df <- add_uk_trade_cols(your_df, country_col = "your_country_column")
#
# If your country column is already called "country" you can omit the argument:
#   your_df <- add_uk_trade_cols(your_df)
#
