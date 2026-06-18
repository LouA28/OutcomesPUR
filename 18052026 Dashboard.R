
## Load library 

library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(DT)

## Load data 

monthly   <- readRDS("data/trade_monthly_blocs.RDS")
quarterly <- readRDS("data/trade_quarterly_blocs.RDS")
yearly    <- read.csv("data/trade_yearly.csv")
Pref_data1 <- readRDS("data/pref_data_clean.RDS")
headline_cache <- readRDS("data/headline_cache.RDS")

month_labels <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")

monthly <- monthly %>%
  mutate(
    Month     = as.integer(Month),
    Year      = as.integer(Year),
    sort_date = as.Date(paste(Year, Month, "01", sep = "-")),
    x_label   = paste(month_labels[Month], Year)
  )

quarterly <- quarterly %>%
  mutate(
    Year      = as.integer(Year),
    x_label   = quarter,
    sort_date = as.Date(paste0(
      substr(quarter, 1, 4), "-",
      (as.integer(substr(quarter, 7, 7)) - 1) * 3 + 1, "-01"
    ))
  )

yearly <- yearly %>%
  mutate(
    Year      = as.integer(Year),
    x_label   = paste0("Y", Year),
    sort_date = as.Date(paste0(Year, "-01-01"
    ))
  )

all_countries <- sort(unique(monthly$country_name[
  !monthly$country_name %in% c("European Union","CPTPP","DCTS")
]))
all_years <- sort(unique(monthly$Year))

summary_countries <- c(
  "CPTPP","Japan",
  "India","Australia","New Zealand", "United States"
)

# =============================================================================
# AGREEMENT DATES
# =============================================================================

agreement_dates <- list(
  "CPTPP"          = as.Date("2024-12-24"),
  "DCTS"           = as.Date("2023-06-19"),
  "European Union" = as.Date("2021-05-01"),
  "Australia"      = as.Date("2023-05-31"),
  "New Zealand"    = as.Date("2023-05-31"),
  "Albania"        = as.Date("2021-05-03"),
  "Cameroon"       = as.Date("2021-07-09"),
  "Canada"         = as.Date("2021-04-01"),
  "Ghana"          = as.Date("2023-08-01"),
  "Iceland"        = as.Date("2023-02-01"),
  "Liechtenstein"  = as.Date("2022-09-01"),
  "Norway"         = as.Date("2022-09-01"),
  "Singapore"      = as.Date("2021-02-11"),
  "Madagascar"     = as.Date("2024-08-01"),
  "India"          = as.Date("2026-03-01"),
  "United States"  = as.Date("2025-06-30"),
  "Chile"          = as.Date("2021-01-01"),
  "Colombia"       = as.Date("2022-06-28"),
  "Ecuador"        = as.Date("2021-01-01"),
  "Egypt"          = as.Date("2021-01-01"),
  "Faroe Islands"  = as.Date("2021-01-01"),
  "Georgia"        = as.Date("2021-01-01"),
  "Israel"         = as.Date("2021-01-01"),
  "Japan"          = as.Date("2021-01-01"),
  "Jordan"         = as.Date("2021-05-01"),
  "Kenya"          = as.Date("2021-03-22"),
  "Kosovo"         = as.Date("2021-01-01"),
  "Lebanon"        = as.Date("2020-12-31"),
  "Mexico"         = as.Date("2021-06-01"),
  "Moldova"        = as.Date("2021-01-01"),
  "Morocco"        = as.Date("2021-01-01"),
  "Mozambique"     = as.Date("2021-01-01"),
  "Nicaragua"      = as.Date("2021-01-01"),
  "Panama"         = as.Date("2021-01-01"),
  "Peru"           = as.Date("2021-01-01"),
  "Serbia"         = as.Date("2021-07-15"),
  "South Korea"    = as.Date("2021-01-01"),
  "Switzerland"    = as.Date("2021-01-01"),
  "Tunisia"        = as.Date("2021-01-01"),
  "Turkey"         = as.Date("2021-04-20"),
  "Ukraine"        = as.Date("2021-01-01"),
  "Vietnam"        = as.Date("2021-05-01"),
  "Botswana"       = as.Date("2021-01-01"),
  "Eswatini"       = as.Date("2021-01-01"),
  "Lesotho"        = as.Date("2021-01-01"),
  "Namibia"        = as.Date("2021-01-01"),
  "South Africa"   = as.Date("2021-01-01"),
  "Mauritius"      = as.Date("2020-12-06"),
  "Seychelles"     = as.Date("2021-01-01"),
  "Zimbabwe"       = as.Date("2021-01-01"),
  "Costa Rica"     = as.Date("2021-01-01"),
  "El Salvador"    = as.Date("2021-01-01"),
  "Guatemala"      = as.Date("2021-01-01"),
  "Honduras"       = as.Date("2021-01-01")
)

cptpp_accession_dates <- list(
  "Australia"   = as.Date("2023-05-31"),  # already in agreement_dates
  "New Zealand" = as.Date("2023-05-31"),  # already in agreement_dates
  "Canada"      = as.Date("2021-04-01"),  # already in agreement_dates
  "Singapore"   = as.Date("2021-02-11"),  # already in agreement_dates
  "Japan"       = as.Date("2021-01-01"),  # already in agreement_dates
  "Vietnam"     = as.Date("2021-05-01"),  # already in agreement_dates
  "Mexico"      = as.Date("2021-06-01"),  # already in agreement_dates
  "Peru"        = as.Date("2021-01-01"),  # already in agreement_dates
  "Chile"       = as.Date("2021-01-01"),  # already in agreement_dates
  "Malaysia"    = NA,                     # no UK agreement yet
  "Brunei"      = NA                      # no UK agreement yet
)

# Combocodes to show in the breakdown chart
selected_combocodes <- c("e1u11","e2u11","e2u21","e3u11","e3u31","e3u30")

# Colour palette — one per combocode
combocode_colours <- c(
  "e1u11" = "#1B2A5E",
  "e2u11" = "#2E75B6",
  "e2u21" = "#70AD47",
  "e3u11" = "#ED7D31",
  "e3u31" = "#FFC000",
  "e3u30" = "#e74c3c"
)

combocode_labels <- c(
  "e1u11" = "MFN Only: MFN non-zero",
  "e2u11" = "GSP: FN non-zero",
  "e2u21" = "GSP: GSP non-zero",     
  "e3u11" = "PTA: MFN non-zero",
  "e3u31" = "PTA: PTA non-zero",
  "e3u30" = "PTA: PTA zero"
)

## Takes the raw trade data for one country, collapses it into one row per time period
## calculates how much of the available preference was used versus unused ready for the chart.

agg_country <- function(df, country) {
  df %>%
    filter(country_name == country) %>%
    group_by(x_label, sort_date) %>%
    summarise(
      Pref_Trade      = sum(Pref_Trade,      na.rm = TRUE),
      Eligible_Trade  = sum(Eligible_Trade,  na.rm = TRUE),
      Pref_Volume     = sum(Pref_Volume,     na.rm = TRUE),
      Eligible_Volume = sum(Eligible_Volume, na.rm = TRUE),
      Total_imp       = sum(Total_imp,       na.rm = TRUE),
      Total_volume    = sum(Total_volume,    na.rm = TRUE),
      # Keep first value of non-numeric cols
      country_name          = first(country_name),
      uk_trade_agreement    = first(uk_trade_agreement),
      uk_agreement_name     = first(uk_agreement_name),
      uk_dcts               = first(uk_dcts),
      uk_dcts_tier          = first(uk_dcts_tier),
      .groups = "drop"
    ) %>%
    arrange(sort_date) %>%
    mutate(
      used_val    = Pref_Trade,
      unused_val  = pmax(Eligible_Trade  - Pref_Trade,  0),
      used_vol    = Pref_Volume,
      unused_vol  = pmax(Eligible_Volume - Pref_Volume, 0),
      pur_val_pct = ifelse(Eligible_Trade  > 0,
                           round((Pref_Trade  / Eligible_Trade)  * 100), NA_real_),
      pur_vol_pct = ifelse(Eligible_Volume > 0,
                           round((Pref_Volume / Eligible_Volume) * 100), NA_real_)
    )
}


## Builds charts for each individual country/trade bloc

make_mini_chart <- function(df, country, used_col, unused_col,
                            pct_col, divisor, y_label) {
  
  df <- agg_country(df, country)
  
  if (nrow(df) == 0) return(plotly_empty())
  
  x_order <- unique(df$x_label)
  
  df <- df %>%
    mutate(
      used_plot   = .data[[used_col]]   / divisor,
      unused_plot = .data[[unused_col]] / divisor,
      pct_label   = ifelse(!is.na(.data[[pct_col]]),
                           paste0(.data[[pct_col]], "%"), "")
    )
  
  shapes <- list()
  no_data_rows <- which(df$used_plot == 0 & df$unused_plot == 0)
  if (length(no_data_rows) > 0) {
    start <- no_data_rows[1]
    end   <- no_data_rows[length(no_data_rows)]
    shapes[[length(shapes) + 1]] <- list(
      type      = "rect", xref = "x", yref = "paper",
      x0        = start - 1.5, x1 = end - 0.5,
      y0        = 0, y1 = 1,
      fillcolor = "rgba(180,180,180,0.25)",
      line      = list(width = 0), layer = "below"
    )
  }
  
  source_id <- paste0("mini_", gsub("[^a-zA-Z0-9]", "_", country))
  
  plot_ly(df,
          source = source_id,
          x      = ~factor(x_label, levels = x_order),
          y      = ~used_plot,
          name   = "Pref used",
          type   = "bar",
          marker           = list(color = "#1B2A5E"),
          text             = ~pct_label,
          textposition     = "inside",
          insidetextanchor = "middle",
          textfont         = list(color = "white", size = 8),
          hovertemplate    = paste0(
            "<b>%{x}</b><br>Pref used: %{y:,.0f} ", y_label,
            "<br>PUR: %{text}<extra></extra>"),
          showlegend = FALSE
  ) %>%
    add_trace(
      y             = ~unused_plot,
      name          = "Not used",
      marker        = list(color = "#B8C9E8"),
      text          = "",
      textposition  = "none",
      hovertemplate = paste0(
        "<b>%{x}</b><br>Not used: %{y:,.0f} ", y_label,
        "<extra></extra>"),
      showlegend = FALSE
    ) %>%
    layout(
      barmode = "stack",
      shapes  = if (length(shapes) > 0) shapes else NULL,
      xaxis = list(
        title         = "",
        tickangle     = -45,
        tickfont      = list(size = 8),
        categoryorder = "array",
        categoryarray = x_order
      ),
      yaxis = list(
        title     = y_label,
        tickfont  = list(size = 10),
        rangemode = "tozero",
        tickformat = ","
      ),
      margin        = list(t = 5, b = 70, l = 55, r = 5),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      hovermode     = "closest"
    )
}

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  
  title = "Defra Outcomes - EUIT PUR",
  
  tags$head(tags$style(HTML("
    body { background-color: #f4f6f9; font-family: Arial, sans-serif; }

    .main-header {
      background-color: #1B2A5E; color: white;
      padding: 14px 20px; margin-bottom: 16px; border-radius: 4px;
    }
    .main-header h3 { margin: 0; font-size: 20px; }
    .main-header p  { margin: 2px 0 0 0; font-size: 12px; color: #B8C9E8; }

    .controls-bar {
      background: white; padding: 12px 16px; border-radius: 4px;
      margin-bottom: 14px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      display: flex; align-items: flex-end; gap: 16px; flex-wrap: wrap;
    }
    .controls-bar .form-group { margin-bottom: 0; }
    .controls-bar label { font-weight: bold; font-size: 12px; color: #555; }

    .mini-box {
      background: white; border-radius: 4px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.12);
      padding: 10px 12px 4px 12px; margin-bottom: 14px;
      cursor: pointer; transition: box-shadow 0.2s;
    }
    .mini-box:hover { box-shadow: 0 3px 10px rgba(27,42,94,0.3); }
    .mini-box h5 {
      color: #1B2A5E; margin: 0 0 4px 0;
      font-size: 13px; font-weight: bold;
    }

    .chart-panel {
      background: white; border-radius: 4px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      padding: 14px; margin-bottom: 14px;
    }
    .chart-panel h4 {
      color: #1B2A5E; margin: 0 0 2px 0;
      font-size: 15px; font-weight: bold;
    }
    .chart-subtitle { color: #888; font-size: 12px; margin-bottom: 10px; }

    .section-label {
      font-size: 12px; font-weight: bold; color: #1B2A5E;
      letter-spacing: 0.5px;
      margin: 0 0 12px 0; padding: 6px 10px;
      background: #f0f4ff; border-left: 3px solid #1B2A5E;
      border-radius: 2px;
    }

    .back-bar {
      background: #fff3cd; border-left: 4px solid #ffc107;
      padding: 8px 14px; border-radius: 2px; margin-bottom: 10px;
      display: flex; align-items: center; gap: 12px;
    }
    .back-bar p { margin: 0; font-size: 13px; }

    .pre-note {
      background: #f8d7da; border-left: 4px solid #e74c3c;
      padding: 7px 12px; border-radius: 2px;
      margin-bottom: 10px; font-size: 12px;
    }

    .btn-back {
      background-color: #1B2A5E; color: white; border: none;
      padding: 5px 14px; border-radius: 3px;
      font-size: 12px; cursor: pointer;
    }
    .btn-back:hover { background-color: #2E75B6; color: white; }

    .data-table-panel {
      background: white; border-radius: 4px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1); padding: 14px;
    }

    .legend-bar {
      display: flex; gap: 20px; align-items: center;
      margin-bottom: 8px; font-size: 12px; color: #555;
    }
    .legend-swatch {
      width: 13px; height: 13px; border-radius: 2px;
      display: inline-block; margin-right: 4px; vertical-align: middle;
    }
    
    .yr-btn { 
  padding:4px 10px; border-radius:3px; cursor:pointer; 
  font-size:12px; background:white; color:#1B2A5E; 
  border:1px solid #1B2A5E; 
}
.yr-btn.active { 
  background:#1B2A5E; color:white; 
}
  "))),
  
  div(class = "main-header",
      h3("Defra Outcomes - EUIT PUR"),
      p("Click any chart to drill into the full time series for that country or bloc")
  ),
  
  div(class = "controls-bar",
      
      # Always visible — country selector
      div(
        tags$label("Or select any country"),
        selectInput("country", label = NULL,
                    choices  = c("Overview" = "all",
                                 "── Blocs ──"    = "",
                                 "CPTPP"          = "CPTPP",
                                 "── Countries ──" = "",
                                 "Australia"      = "Australia",
                                 "New Zealand"    = "New Zealand",
                                 "Japan"          = "Japan",
                                 "India"          = "India",
                                 "United States"  = "United States"),
                    selected = "all", width = "220px")
      ),
      
      # Only visible when a country is selected
      conditionalPanel(
        condition = "input.country != 'all' && input.country != ''",
        div(
          tags$label("Time Period"),
          selectInput("period", label = NULL,
                      choices  = c("Monthly"     = "monthly",
                                   "Quarterly"   = "quarterly",
                                   "Yearly" = "yearly"),
                      selected = "quarterly", width = "150px")
        )
      ),
      conditionalPanel(
        condition = "input.country != 'all' && input.country != ''",
        div(
          tags$label("Measure"),
          selectInput("measure", label = NULL,
                      choices  = c("Value (£)"   = "value",
                                   "Volume (t)" = "volume"),
                      selected = "value", width = "140px")
        )
      ),
      conditionalPanel(
        condition = "input.country != 'all' && input.country != '' && input.period != 'yearly'",
        div(
          tags$label("Filter by Year"),
          selectInput("year_filter", label = NULL,
                      choices  = c("All years" = "all",
                                   setNames(all_years, all_years)),
                      selected = "all", width = "130px")
        )
      )
  ),
  
  uiOutput("back_bar"),
  uiOutput("pre_agreement_note"),
  uiOutput("headline_panel"),
  uiOutput("main_content"),
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  selected_country <- reactiveVal(NULL)
  
  show_breakdown <- reactiveVal(FALSE)
  
  # Reset breakdown when country changes
  observeEvent(selected_country(), {
    show_breakdown(FALSE)
  })
  
  observeEvent(input$btn_breakdown, {
    show_breakdown(!show_breakdown())
  })
  
  observeEvent(input$country, {
    selected_country(
      if (input$country %in% c("all","")) NULL else input$country
    )
  })
  
  observeEvent(input$btn_back, {
    selected_country(NULL)
    updateSelectInput(session, "country", selected = "all")
  })
  
  # Click listeners for each mini-chart
  for (ctry in summary_countries) {
    local({
      c   <- ctry
      src <- paste0("mini_", gsub("[^a-zA-Z0-9]", "_", c))
      observeEvent(event_data("plotly_click", source = src), {
        selected_country(c)
        updateSelectInput(session, "country", selected = c)
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
    })
  }
  
  
  ## Picks which dataset (monthly/quarterly/yearly) to use based on the period dropdown
  
  active_data <- reactive({
    switch(input$period,
           monthly   = monthly,
           quarterly = quarterly,
           yearly    = yearly
    )
  })
  
  # These track whether the user wants value or volume and point every chart to the right columns and labels
  
  is_value   <- reactive({ input$measure == "value" })
  y_lbl      <- reactive({ if (is_value()) "£ million" else "Thousand tonnes" })
  divisor    <- 1e6
  used_col   <- reactive({ if (is_value()) "used_val"    else "used_vol"    })
  unused_col <- reactive({ if (is_value()) "unused_val"  else "unused_vol"  })
  pct_col    <- reactive({ if (is_value()) "pur_val_pct" else "pur_vol_pct" })
  
  period_label <- reactive({
    switch(input$period,
           monthly   = "Monthly",
           quarterly = "Quarterly",
           yearly    = "Yearly")
  })
  
  ## optionally narrows the data to a single year when the year filter dropdown is used.
  apply_year_filter <- function(df) {
    if (input$period != "yearly" &&
        !is.null(input$year_filter) &&
        input$year_filter != "all") {
      df <- df %>% filter(Year == as.integer(input$year_filter))
    }
    df
  }
  
  ## Base data is the dataset with the year filter applied, for all country charts
  
  base_data <- reactive({
    active_data() %>% apply_year_filter()
  })
  
  ## Overview data — always unfiltered, always quarterly
  overview_data <- reactive({
    quarterly
  })
  
  # ── Detail data  ──────────────────────────────────────────
  
  detail_df <- reactive({
    req(selected_country())
    ag_date <- agreement_dates[[selected_country()]]
    
    agg_country(base_data(), selected_country()) %>%
      mutate(
        # FIX 3: safe pre_agreement — handles NULL ag_date without crash
        pre_agreement = if (!is.null(ag_date) && !is.na(ag_date)) {
          sort_date < ag_date
        } else {
          rep(FALSE, n())
        }
      )
  })
  
  ## This toggles the button text between show and hide for the preference type breakdown.
  
  output$breakdown_btn_label <- renderUI({
    if (show_breakdown()) "▲ Hide preference type breakdown" else
      "▼ View preference type breakdown"
  })
  
  ## renders the collapsible panel containing the preference type breakdown chart
  
  output$breakdown_panel <- renderUI({
    req(selected_country(), show_breakdown())
    div(class = "chart-panel",
        h4(paste0("Preference type breakdown — ", selected_country())),
        div(class = "chart-subtitle",
            HTML(paste0(
              "<b>Monthly</b> trade value (£ million) for <b>selected preference types only</b>. ",
              "Other preference types exist in the data but are not shown here. ",
              "<span style='color:#e74c3c;'><b>Note:</b> This breakdown is always shown at monthly granularity, regardless of the time period selected above.</span>"
            ))),
        div(style = "background:#f8f9fa; border:1px solid #e0e0e0; border-radius:4px;
                   padding:10px 14px; margin-bottom:12px; font-size:12px; color:#444;",
            tags$details(
              tags$summary(style = "cursor:pointer; font-weight:bold; color:#1B2A5E;",
                           "▶ What do these preference types mean?"),
              div(style = "margin-top:10px;
                         display:grid; grid-template-columns:1fr 1fr; gap:6px 20px;",
                  HTML("
                  <div><b>MFN Only: MFN non-zero</b><br>The import was subject to a non-zero MFN tariff. </div>
                  <div><b>GSP: MFN non-zero</b><br>The good was eligible for a GSP preference and was imported under an MFN non zero tariff.</div>
                  <div><b>GSP: GSP non-zero</b><br>The good was eligible for a GSP preference and was imported under a GSP non zero tariff.</div>
                  <div><b>PTA: MFN non-zero</b><br>The good was eligible for a preference under a trade agreement and was imported under an MFN non zero tariff.</div>
                  <div><b>PTA: PTA non-zero</b><br>The good was eligible for a preference under a trade agreement and was imported under a preferential non zero tariff.</div>
                  <div><b>PTA: PTA zero</b><br>The good was eligible for a preference under a trade agreement and was imported at 0% under that agreement.</div>
                ")
              )
            )
        ),
        plotlyOutput("chart_breakdown", height = "380px"),
        div(style = "margin-top:10px; font-size:12px; color:#555;",
            HTML('For a deeper commodity-level breakdown, visit the
            <a href="https://dash-connect-prd.azure.defra.cloud/PUR-app/"
               target="_blank" style="color:#1B2A5E; font-weight:bold;">
               UK import PUR App
            </a>
            or the
      <a href="https://dash-connect-prd.azure.defra.cloud/marketaccessapp/"
         target="_blank" style="color:#1B2A5E; font-weight:bold;">
         Market Access App
      </a>')
        )
    )
  })
  
  ## This draws the monthly line chart breaking down trade by preference type (MFN, GSP, FTA combocodes)
  output$chart_breakdown <- renderPlotly({
    req(selected_country(), show_breakdown())
    
    # Filter to selected country and combocodes
    df <- Pref_data1 %>%
      filter(
        tolower(country_name) == tolower(selected_country()),
        combocode %in% selected_combocodes
      )
    
    df <- df %>%
      group_by(combocode, x_label, sort_date) %>%
      summarise(statvalue = sum(statvalue, na.rm = TRUE), .groups = "drop") %>%
      arrange(sort_date)
    
    # ── Fill missing months with NA so lines break at gaps ──────────────────
    all_months <- seq(min(df$sort_date), max(df$sort_date), by = "month")
    
    full_grid <- expand.grid(
      combocode = unique(df$combocode),
      sort_date = all_months,
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        Year    = as.integer(format(sort_date, "%Y")),
        Month   = as.integer(format(sort_date, "%m")),
        x_label = paste(month_labels[Month], Year)
      )
    
    df <- full_grid %>%
      left_join(df %>% select(combocode, sort_date, statvalue),
                by = c("combocode", "sort_date")) %>%
      arrange(combocode, sort_date)
    
    # Check any data exists
    if (nrow(df) == 0) {
      return(plotly_empty() %>%
               layout(title = "No combocode data available for this country"))
    }
    
    # Get combocodes actually present
    present_codes <- unique(df$combocode)
    x_order       <- unique(df$x_label)
    
    p <- plot_ly()
    
    for (code in present_codes) {
      df_code <- df %>% filter(combocode == code)
      colour  <- combocode_colours[code]
      label   <- ifelse(!is.na(combocode_labels[code]),
                        combocode_labels[code], code) 
      
      p <- p %>% add_trace(
        data          = df_code,
        x             = ~factor(x_label, levels = x_order),
        y             = ~statvalue / 1e6,
        name          = label,                          
        type          = "scatter",
        mode          = "lines+markers",
        line          = list(color = colour, width = 2),
        marker        = list(color = colour, size = 5),
        hovertemplate = paste0(
          "<b>", label, "</b><br>%{x}<br>",
          "£%{y:,.2f}m<extra></extra>")
      )
    }
    
    ag_date     <- agreement_dates[[selected_country()]]
    bd_shapes   <- list()
    bd_annotations <- list()
    
    if (!is.null(ag_date) && !is.na(ag_date)) {
      df_dates   <- df %>% distinct(x_label, sort_date) %>% arrange(sort_date)
      pre_count  <- sum(df_dates$sort_date < ag_date, na.rm = TRUE)
      
      if (pre_count > 0 && pre_count < nrow(df_dates)) {
        bd_shapes[[1]] <- list(
          type = "line", xref = "x", yref = "paper",
          x0 = pre_count - 0.5, x1 = pre_count - 0.5, y0 = 0, y1 = 1,
          line = list(color = "#e74c3c", width = 2, dash = "dash")
        )
        bd_annotations[[1]] <- list(
          xref = "x", yref = "paper",
          x = pre_count - 0.5, y = 0.97,
          text = paste0("<b>In force: ", format(ag_date, "%d %b %Y"), "</b>"),
          showarrow   = TRUE, arrowhead = 2, arrowcolor = "#e74c3c",
          ax = 60, ay = -10,
          font        = list(size = 10, color = "#e74c3c"),
          bgcolor     = "white",
          bordercolor = "#e74c3c",
          borderwidth = 1, borderpad = 3
        )
      }
    }
    
    # ── CPTPP member accession lines ─────────────────────────────────────
    if (selected_country() == "CPTPP") {
      df_dates <- df %>% distinct(x_label, sort_date) %>% arrange(sort_date)
      
      # Group members by date to avoid duplicate lines
      date_to_members <- list()
      for (member in names(cptpp_accession_dates)) {
        d <- cptpp_accession_dates[[member]]
        if (!is.null(d) && !is.na(d)) {
          d_str <- as.character(d)
          date_to_members[[d_str]] <- c(date_to_members[[d_str]], member)
        }
      }
      
      for (d_str in names(date_to_members)) {
        d   <- as.Date(d_str)
        pos <- sum(df_dates$sort_date < d, na.rm = TRUE)
        
        if (pos > 0 && pos < nrow(df_dates)) {
          n <- length(bd_shapes) + 1
          bd_shapes[[n]] <- list(
            type = "line", xref = "x", yref = "paper",
            x0 = pos - 0.5, x1 = pos - 0.5, y0 = 0, y1 = 1,
            line = list(color = "#2C2C2C", width = 1, dash = "dot")
          )
          members_label <- paste(date_to_members[[d_str]], collapse = ", ")
          bd_annotations[[length(bd_annotations) + 1]] <- list(
            xref = "x", yref = "paper",
            x = pos - 0.5, y = 0.75,
            text = paste0("<b>", members_label, "</b><br>", format(d, "%b %Y")),
            showarrow   = FALSE,
            font        = list(size = 8, color = "#2C2C2C"),
            bgcolor     = "white",
            bordercolor = "#2C2C2C",
            borderwidth = 1, borderpad = 2,
            xanchor     = "left"
          )
        }
      }
    }
    
    p %>% layout(
      shapes      = if (length(bd_shapes) > 0) bd_shapes else NULL,
      annotations = if (length(bd_annotations) > 0) bd_annotations else NULL,
      xaxis = list(
        title         = "",
        tickangle     = -45,
        tickfont      = list(size = 9),
        categoryorder = "array",
        categoryarray = x_order
      ),
      yaxis = list(
        title     = "£ million",
        rangemode = "tozero",
        tickfont  = list(size = 10),
        tickformat = ","
      ),
      legend = list(
        orientation = "h",
        x = 0.5, xanchor = "center",
        y = -0.25,          # negative pushes it below the x-axis
        yanchor = "top",
        font = list(size = 11)
      ),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      margin = list(t = 40, b = 130, l = 70, r = 20),
      hovermode = "points"
    )
  })
  
  ## the yellow bar at the top showing which country is selected and the back button.
  
  output$back_bar <- renderUI({
    req(selected_country())
    div(class = "back-bar",
        actionButton("btn_back", "← Back to Overview", class = "btn-back"),
        p(HTML(paste0("Viewing: <b>", selected_country(), "</b>")))
    )
  })
  
  ## the red info box showing how many periods predate the trade agreement.
  
  output$pre_agreement_note <- renderUI({
    req(selected_country())
    df      <- detail_df()
    ag_date <- agreement_dates[[selected_country()]]
    if (!is.null(ag_date) && !is.na(ag_date) &&
        isTRUE(any(df$pre_agreement, na.rm = TRUE))) {
      div(class = "pre-note",
          icon("info-circle"),
          HTML(paste0(
            " <b>", sum(df$pre_agreement, na.rm = TRUE),
            " period(s)</b> shown before the UK-", selected_country(),
            " agreement came into force on <b>",
            format(ag_date, "%d %B %Y"),
            "</b>"))
      )
    }
  })
  
  ##  switches between the overview page (six mini charts) and the country detail page (main chart + breakdown button)
  
  output$main_content <- renderUI({
    if (is.null(selected_country())) {
      
      measure_lbl <- if (is_value()) "Value (£ million)" else
        "Volume (thousand tonnes)"
      
      
      tagList(
        div(style = "background:white; border-radius:4px;
     box-shadow:0 1px 3px rgba(0,0,0,0.1);
     padding:10px 16px; margin-bottom:14px;",
            HTML('For a deeper analysis of preference utilisation by commodity code, visit the 
    <a href="https://dash-connect-prd.azure.defra.cloud/PUR-app/" 
       target="_blank" 
       style="color:#1B2A5E; font-weight:bold; font-size:13px;">
       UK import PUR App
    </a>')
        ),
        
        div(class = "legend-bar",
            span(span(class="legend-swatch",
                      style="background:#1B2A5E;"), "Preference used"),
            span(span(class="legend-swatch",
                      style="background:#B8C9E8;"), "Preference not used"),
            span(span(class="legend-swatch",
                      style="background:rgba(180,180,180,0.4);"), "No preference data available")
        ),
        
        div(class = "section-label",
            paste0(period_label(),
                   " preference utilisation — ", measure_lbl,
                   " | Highlighted countries & blocs only")),
        div(style = "background:#f0f4ff; border-left:3px solid #2E75B6;
     border-radius:2px; padding:7px 12px; margin-bottom:12px;
     font-size:12px; color:#444;",
            HTML("<b>Note:</b> The charts below show a selection of key trading partners and blocs.
          To view any country or bloc, use the <b>'Or select any country'</b> dropdown above.")
        ),
        div(style = "background:#fff8e1; border-left:4px solid #ffc107;
     border-radius:2px; padding:7px 12px; margin-bottom:12px;
     font-size:12px; color:#555;",
            HTML("<b>Note:</b> PUR may vary naturally from quarter to quarter due to seasonal trade patterns, 
          global events, exchange rate shifts, and changes in the most competitive sourcing country. 
          A quarter-on-quarter decline does not indicate that the outcome goal is not being met.")
        ),
        
        fluidRow(
          column(6, div(class="mini-box",
                        h5("CPTPP"),
                        plotlyOutput("mini_cptpp", height = "320px")
          )),
          column(6, div(class="mini-box",
                        h5("Australia"),
                        plotlyOutput("mini_aus", height = "320px")
          ))
        ),
        fluidRow(
          column(6, div(class="mini-box",
                        h5("New Zealand"),
                        plotlyOutput("mini_nz", height = "320px")
          )),
          column(6, div(class="mini-box",
                        h5("Japan"),
                        plotlyOutput("mini_japan", height = "320px")
          ))
        ),
        fluidRow(
          
          column(6, div(class="mini-box",
                        h5("India"),
                        plotlyOutput("mini_india", height = "320px")
          )),
          column(6, div(class="mini-box",
                        h5("United States"),
                        plotlyOutput("mini_us", height = "320px")
          ))
        )
      )
      
    } else {
      tagList(
        
        # Quarterly caveat note
        if (input$period == "quarterly") {
          div(style = "background:#fff8e1; border-left:4px solid #ffc107;
               padding:8px 14px; border-radius:2px; margin-bottom:10px;
               font-size:12px; color:#555;",
              HTML("<b>Note:</b> PUR may vary naturally from quarter to quarter due to 
           seasonal trade patterns, global events, exchange rate shifts, and 
           changes in the most competitive sourcing country. A quarter-on-quarter 
           decline does not indicate that the outcome goal is not being met.")
          )
        },
        
        div(class = "chart-panel",
            h4(paste0(period_label(), " preference utilisation — ",
                      selected_country())),
            div(class = "chart-subtitle", uiOutput("detail_subtitle")),
            plotlyOutput("chart_detail", height = "460px"),
            div(style = "margin-top: 10px;",
                actionButton("btn_breakdown",
                             label = uiOutput("breakdown_btn_label"),
                             style = "background-color:#2E75B6; color:white;
                          border:none; padding:6px 16px;
                          border-radius:3px; font-size:12px;
                          cursor:pointer;")
            )
        ),
        uiOutput("breakdown_panel")
      )
    }
    
  })
  
  ## the headline statistics box with year buttons and the MFN/FTA/DCTS percentage sentence
  
  output$headline_panel <- renderUI({
    if (!is.null(selected_country())) return(NULL)
    
    req(length(headline_cache) > 0)
    
    yr <- if (!is.null(input$headline_year)) {
      as.integer(input$headline_year)
    } else {
      available <- sort(unique(yearly$Year), decreasing = TRUE)
      available[available < max(available)][1]
    }
    
    cache_entry <- headline_cache[[as.character(yr)]]
    req(!is.null(cache_entry))
    
    s <- cache_entry$stats
    
    pct_tariff_free <- s$pct_tariff_free
    pct_mfn         <- s$pct_mfn
    pct_fta         <- s$pct_fta
    pct_dcts        <- s$pct_dcts
    
    
    available_years <- sort(unique(yearly$Year), decreasing = FALSE)
    #available_years <- available_years[available_years < max(available_years)]
    
    div(style = "background:white; border-radius:4px;
               box-shadow:0 1px 3px rgba(0,0,0,0.1);
               padding:12px 16px; margin-bottom:14px;",
        div(style = "display:flex; align-items:center; gap:10px; margin-bottom:8px;",
            span(style = "font-size:12px; font-weight:bold; color:#555;",
                 "Headline statistics for year:"),
            div(style = "display:flex; gap:8px;",
                lapply(available_years, function(y) {
                  is_partial <- y == max(available_years)
                  tags$button(
                    if (is_partial) paste0(y, "*") else as.character(y),
                    class = paste0("yr-btn", if (y == yr) " active" else ""),
                    onclick = paste0("Shiny.setInputValue('headline_year', ", y, ", {priority: 'event'})")
                  )
                })
            )
        ),
        div(style = "font-size:13px; color:#444; line-height:1.8;",
            HTML(paste0(
              "<b style='color:#1B2A5E; font-size:16px;'>", pct_tariff_free, "%</b>",
              " of goods entered the UK tariff free in <b>", yr, "</b> — ",
              "<b>", pct_mfn,  "%</b> did so under MFN terms, ",
              "<b>", pct_fta,  "%</b> did so under FTA preferences, and ",
              "<b>", pct_dcts, "%</b> did so under DCTS preferences."
            ))
        ),
        if (yr == max(available_years)) {
          div(style = "font-size:11px; color:#888; margin-top:4px;",
              "* 2026 data is partial and covers January to the latest available month only.")
        }
    )
  })
  
  ## the grey subtitle under the country chart title showing measure and agreement name.
  
  output$detail_subtitle <- renderUI({
    req(selected_country())
    m  <- if (is_value()) "Value (£ million)" else "Volume (thousand tonnes)"
    ag <- tryCatch(detail_df()$uk_agreement_name[1], error = function(e) NA)
    
    # Manual fallback for blocs whose agreement name may not be in the data
    bloc_agreements <- list(
      "CPTPP"          = "Comprehensive and Progressive Agreement for Trans-Pacific Partnership",
      "DCTS"           = "Developing Countries Trading Scheme",
      "European Union" = "UK-EU Trade and Cooperation Agreement"
    )
    
    agreement_text <- if (!is.na(ag) && ag != "NA") {
      ag
    } else if (selected_country() %in% names(bloc_agreements)) {
      bloc_agreements[[selected_country()]]
    } else if (!is.null(agreement_dates[[selected_country()]])) {
      "UK trade agreement in force"
    } else {
      "No UK trade agreement"
    }
    
    paste0(m, " | ", agreement_text)
  })
  
  ##  renders each of the six overview mini charts and registers click events so users can drill into a country
  
  render_mini <- function(country) {
    renderPlotly({
      p <- make_mini_chart(overview_data(), country,
                           "used_val", "unused_val", "pur_val_pct",
                           divisor, "£ million")
      event_register(p, "plotly_click")
    })
  }
  
  output$mini_cptpp  <- render_mini("CPTPP")
  output$mini_aus    <- render_mini("Australia")
  output$mini_nz     <- render_mini("New Zealand")
  output$mini_japan  <- render_mini("Japan")
  output$mini_india  <- render_mini("India")
  output$mini_us     <- render_mini("United States")
  
  ## the main stacked bar chart on the country detail page
  
  output$chart_detail <- renderPlotly({
    df      <- detail_df()
    ag_date <- agreement_dates[[selected_country()]]
    req(nrow(df) > 0)
    
    df      <- df %>% distinct(x_label, .keep_all = TRUE)
    x_order <- df$x_label
    
    df <- df %>%
      mutate(
        used_plot   = .data[[used_col()]]   / divisor,
        unused_plot = .data[[unused_col()]] / divisor,
        pct_label   = ifelse(!is.na(.data[[pct_col()]]),
                             paste0(.data[[pct_col()]], "%"), "")
      )
    
    shapes      <- list()
    no_data_rows <- which(df$used_plot == 0 & df$unused_plot == 0)
    if (length(no_data_rows) > 0) {
      start <- no_data_rows[1]
      end   <- no_data_rows[length(no_data_rows)]
      shapes[[length(shapes) + 1]] <- list(
        type      = "rect", xref = "x", yref = "paper",
        x0        = start - 1.5, x1 = end - 0.5,
        y0        = 0, y1 = 1,
        fillcolor = "rgba(180,180,180,0.25)",
        line      = list(width = 0), layer = "below"
      )
    }
    annotations <- list()
    
    if (input$period == "monthly" &&
        !is.null(ag_date) && !is.na(ag_date) &&
        isTRUE(any(df$pre_agreement, na.rm = TRUE))) {
      
      snap_to_period <- function(d, period) {
        m <- as.integer(format(d, "%m"))
        y <- as.integer(format(d, "%Y"))
        if (period == "quarterly") {
          q_start <- c(1, 4, 7, 10)
          nearest <- max(q_start[q_start <= m])
          return(as.Date(paste0(y, "-", formatC(nearest, width = 2, flag = "0"), "-01")))
        } else {
          return(as.Date(paste0(y, "-", formatC(m, width = 2, flag = "0"), "-01")))
        }
      }
      
      snapped_date <- snap_to_period(ag_date, input$period)
      last_pre_x   <- sum(df$sort_date < snapped_date, na.rm = TRUE)
      
      shapes[[1]] <- list(
        type = "rect", xref = "x", yref = "paper",
        x0 = -0.5, x1 = last_pre_x - 0.5, y0 = 0, y1 = 1,
        fillcolor = "rgba(180,180,180,0.2)",
        line = list(width = 0), layer = "below"
      )
      shapes[[2]] <- list(
        type = "line", xref = "x", yref = "paper",
        x0 = last_pre_x - 0.5, x1 = last_pre_x - 0.5, y0 = 0, y1 = 1,
        line = list(color = "#e74c3c", width = 2, dash = "dash")
      )
      annotations[[1]] <- list(
        xref = "x", yref = "paper",
        x = last_pre_x - 0.5, y = 0.97,
        text = paste0("<b>Agreement in force<br>",
                      format(ag_date, "%d %b %Y"), "</b>"),
        showarrow   = TRUE, arrowhead = 2, arrowcolor = "#e74c3c",
        ax = 70, ay = -10,
        font        = list(size = 11, color = "#e74c3c"),
        bgcolor     = "white",
        bordercolor = "#e74c3c",
        borderwidth = 1, borderpad = 4
      )
    }
    
    plot_ly(df,
            source = "detail",
            x      = ~factor(x_label, levels = x_order),
            y      = ~used_plot,
            name   = "Preference used",
            type   = "bar",
            marker           = list(color = "#1B2A5E"),
            text             = ~pct_label,
            textposition     = "inside",
            insidetextanchor = "middle",
            textfont         = list(color = "white", size = 11),
            hovertemplate    = paste0(
              "<b>%{x}</b><br>Pref used: £%{y:,.0f}m",
              "<br>PUR: %{text}<extra></extra>")
    ) %>%
      add_trace(
        y             = ~unused_plot,
        name          = "Preference not used",
        marker        = list(color = "#B8C9E8"),
        text          = "",
        textposition  = "none",
        hovertemplate = paste0(
          "<b>%{x}</b><br>Not used: £%{y:,.0f}m",
          "<extra></extra>")
      ) %>%
      layout(
        barmode     = "stack",
        shapes      = if (length(shapes) > 0) shapes else NULL,
        annotations = if (length(annotations) > 0) annotations else NULL,
        xaxis = list(
          title         = "",
          tickangle     = -45,
          tickfont      = list(size = 10),
          categoryorder = "array",
          categoryarray = x_order,
          type          = "category",
          tickmode      = "array",
          tickvals      = x_order,
          ticktext      = gsub("^Y(\\d{4})$", "\\1", x_order)
        ),
        yaxis = list(
          title     = y_lbl(),
          rangemode = "tozero",
          tickfont  = list(size = 10),
          tickformat = ","
        ),
        legend = list(
          orientation = "h", x = 0.5, xanchor = "center",
          y = 1.04, font = list(size = 11)
        ),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin = list(t = 40, b = 90, l = 70, r = 30)
      ) %>%
      event_register("plotly_click")
  })
  
}

## Run App

shinyApp(ui = ui, server = server)