# app.R
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly) # For interactive plots
library(DT)     # For enhanced data tables
library(tidyr)  # For pivot_longer
library(htmlwidgets) # For saving plotly plots
library(webshot2) # For converting plotly html to png, pdf, jpeg


# Source string to be used for all plots
data_source_text <- "Source: NSE - ESG ANALYSIS ON 200 listed Companies of India"

# 1. Simulated Overall ESG Scores over years and categories
esg_scores_data <- tibble(
  Year = rep(c(2021, 2022, 2023), each = 4),
  Category = rep(c("Overall ESG", "Environment", "Social", "Governance"), 3),
  Score = c(
    65, 60, 70, 75, # 2021
    68, 63, 72, 78, # 2022
    72, 68, 75, 80  # 2023
  )
)

# 2. Simulated Sector Distribution of Companies
sector_distribution_data <- tibble(
  Sector = c("Financial Services", "IT & Software", "Manufacturing", "Oil & Gas", "Power", "Healthcare", "Automotive", "Consumer Goods", "Utilities", "Metals & Mining"),
  CompanyCount = c(45, 30, 35, 20, 25, 15, 10, 18, 12, 10)
) %>%
  mutate(Percentage = CompanyCount / sum(CompanyCount) * 100)

# 3. Simulated Policy Disclosures (percentages)
policy_disclosure_data <- tibble(
  Policy = c("ESG Policy", "BRSR Reporting", "Net-Zero Targets", "Water Reduction Targets", "Renewable Energy Adoption", "Human Rights Policy", "Anti-Bribery Policy", "Whistleblower Policy"),
  Percentage = c(85, 70, 40, 60, 55, 75, 90, 88)
)

# 4. Simulated ESG Scores by Sector for 2023
esg_by_sector_2023 <- tibble(
  Sector = c("Financial Services", "IT & Software", "Manufacturing", "Oil & Gas", "Power", "Healthcare", "Automotive"),
  Overall_ESG_Score = c(78, 85, 70, 65, 72, 79, 68),
  Environment_Score = c(75, 80, 68, 60, 70, 76, 65),
  Social_Score = c(80, 88, 72, 68, 75, 82, 70),
  Governance_Score = c(82, 90, 75, 70, 78, 80, 72)
) %>%
  pivot_longer(
    cols = ends_with("_Score"),
    names_to = "Metric",
    values_to = "Score"
  ) %>%
  mutate(Metric = stringr::str_replace(Metric, "_Score", ""))

# 5. Simulated Environmental Metrics (aggregated, some by sector for variety)
environmental_metrics_data <- tibble(
  Category = c("Energy Consumption (TJ)", "GHG Emissions (tonnes CO2e)", "Water Withdrawn (ML)", "Waste Generated (tonnes)"),
  Value_2022 = c(5000, 15000, 2000, 800),
  Value_2023 = c(4800, 14500, 1900, 750)
) %>%
  pivot_longer(
    cols = starts_with("Value_"),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(Year = as.integer(stringr::str_remove(Year, "Value_")))

# 6. Simulated Social Metrics (e.g., for a sample of companies or aggregated)
social_metrics_data <- tibble(
  Metric = c("Female Employees (%)", "Employee Turnover Rate (%)", "Training Hours per Employee", "CSR Spend (% of Profit)"),
  Value = c(28, 15, 40, 2.3)
)

# 7. Simulated Governance Metrics
governance_metrics_data <- tibble(
  Metric = c("Independent Directors (%)", "Board Diversity (%)", "Audit Committee Meetings (Avg.)", "Gender on Board (%)"),
  Value = c(55, 30, 7, 18)
)

# 8. Simulated Company-level ESG data (a small, expanded sample)
company_esg_data <- tibble(
  Company = c("Infosys Ltd.", "Reliance Ind.", "TCS Ltd.", "ICICI Bank", "L&T", "NTPC Ltd.", "HDFC Bank", "Mahindra & Mahindra", "Tata Steel", "Adani Green Energy"),
  Sector = c("IT & Software", "Oil & Gas", "IT & & Software", "Financial Services", "Manufacturing", "Power", "Financial Services", "Automotive", "Metals & Mining", "Power"),
  ESG_Score_2023 = c(88, 75, 90, 82, 78, 65, 80, 73, 68, 70),
  Environment_Score_2023 = c(85, 70, 88, 78, 75, 60, 75, 68, 62, 75),
  Social_Score_2023 = c(90, 80, 92, 85, 80, 70, 83, 75, 70, 72),
  Governance_Score_2023 = c(90, 78, 91, 86, 82, 68, 84, 76, 73, 70),
  Market_Cap_Cr = c(650000, 1800000, 1300000, 700000, 400000, 250000, 1200000, 300000, 150000, 180000),
  BRSR_Report = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
  Net_Zero_Target = c("Yes", "Yes", "Yes", "No", "No", "Yes", "No", "Yes", "Yes", "Yes")
)

# --- UI (User Interface) ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap');
      body {
        font-family: 'Inter', sans-serif;
        background-color: #f8fafc;
        margin: 0;
        padding: 0;
      }
      .container-fluid {
        padding: 30px;
        max-width: 1400px; /* Max width for content */
        margin: auto;
      }
      .well {
        background-color: #ffffff;
        border-radius: 12px;
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.1);
        padding: 25px;
        margin-bottom: 25px;
        border: 1px solid #e6e6e6;
      }
      .title-panel {
        background: linear-gradient(135deg, #1a73e8, #4285f4); /* Google Blue Gradient */
        color: white;
        padding: 35px;
        border-radius: 12px;
        margin-bottom: 40px;
        text-align: center;
        font-weight: 700;
        font-size: 2.8em;
        box-shadow: 0 8px 15px rgba(0, 0, 0, 0.2);
        letter-spacing: 0.5px;
      }
      .nav-tabs > li > a {
        border-radius: 8px 8px 0 0 !important;
        font-weight: 600;
        color: #555;
        font-size: 1.1em;
        padding: 12px 20px;
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
        color: #1a73e8;
        background-color: #e3f2fd; /* Lighter Blue */
        border-color: #e3f2fd;
        border-bottom-color: transparent;
        box-shadow: inset 0 -3px 0 0 #1a73e8; /* Blue line under active tab */
      }
      .tab-content {
        background-color: #ffffff;
        border-radius: 0 0 12px 12px;
        padding: 30px;
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.1);
      }
      h2 {
        color: #1a73e8;
        font-weight: 700;
        margin-top: 0;
        margin-bottom: 25px;
        border-bottom: 2px solid #e0e0e0;
        padding-bottom: 10px;
      }
      .plot-container {
        margin-top: 25px;
        padding: 20px;
        border: 1px solid #e0e0e0;
        border-radius: 10px;
        background-color: #fdfdfd;
      }
      .plot-source {
          font-size: 0.9em;
          color: #666;
          margin-top: 10px;
          font-style: italic;
      }
      .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate {
        font-size: 0.9em;
      }
      .dataTables_wrapper .dataTables_filter input {
        border: 1px solid #ccc;
        border-radius: 5px;
        padding: 5px 10px;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        padding: 5px 10px;
        margin-left: 2px;
        border-radius: 5px;
      }
    "))
  ),
  div(class = "title-panel", "ESG Analysis Dashboard: 200 Listed Indian Companies"),
  
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Overview & Trends",
             div(class = "well",
                 h2("Average ESG Scores Over Years"),
                 fluidRow(
                   column(12,
                          plotlyOutput("esg_score_line_plot", height = "450px"),
                          div(class = "plot-source", align = "right", HTML(paste0("<i>", data_source_text, "</i>"))),
                          downloadButton("download_esg_score_line_plot_png", "Download Plot (PNG)"),
                          downloadButton("download_esg_score_line_plot_pdf", "Download Plot (PDF)"),
                          downloadButton("download_esg_score_line_plot_jpeg", "Download Plot (JPEG)")
                   )
                 ),
                 hr(),
                 h2("Overall ESG, Environment, Social, Governance Scores by Year"),
                 fluidRow(
                   column(12,
                          plotlyOutput("esg_category_bar_plot", height = "450px"),
                          div(class = "plot-source", align = "right", HTML(paste0("<i>", data_source_text, "</i>"))),
                          downloadButton("download_esg_category_bar_plot_png", "Download Plot (PNG)"),
                          downloadButton("download_esg_category_bar_plot_pdf", "Download Plot (PDF)"),
                          downloadButton("download_esg_category_bar_plot_jpeg", "Download Plot (JPEG)")
                   )
                 ),
                 hr(),
                 h2("Distribution of Companies by Sector"),
                 fluidRow(
                   column(12,
                          plotlyOutput("sector_pie_chart", height = "450px"),
                          div(class = "plot-source", align = "right", HTML(paste0("<i>", data_source_text, "</i>"))),
                          downloadButton("download_sector_pie_chart_png", "Download Plot (PNG)"),
                          downloadButton("download_sector_pie_chart_pdf", "Download Plot (PDF)"),
                          downloadButton("download_sector_pie_chart_jpeg", "Download Plot (JPEG)")
                   )
                 )
             )
    ),
    tabPanel("Policy & Sectoral ESG",
             div(class = "well",
                 h2("Key Policy Disclosures by Companies (%)"),
                 fluidRow(
                   column(12,
                          plotlyOutput("policy_bar_chart", height = "450px"),
                          div(class = "plot-source", align = "right", HTML(paste0("<i>", data_source_text, "</i>"))),
                          downloadButton("download_policy_bar_chart_png", "Download Plot (PNG)"),
                          downloadButton("download_policy_bar_chart_pdf", "Download Plot (PDF)"),
                          downloadButton("download_policy_bar_chart_jpeg", "Download Plot (JPEG)")
                   )
                 ),
                 hr(),
                 h2("Average ESG Scores by Sector (2023)"), # Add this heading
                 fluidRow(
                   column(12,
                          plotlyOutput("esg_by_sector_bar_plot", height = "450px"),
                          div(class = "plot-source", align = "right", HTML(paste0("<i>", data_source_text, "</i>"))),
                          downloadButton("download_esg_by_sector_bar_plot_png", "Download Plot (PNG)"),
                          downloadButton("download_esg_by_sector_bar_plot_pdf", "Download Plot (PDF)"),
                          downloadButton("download_esg_by_sector_bar_plot_jpeg", "Download Plot (JPEG)")
                   )
                 )
             )
    ),
    tabPanel("Environment Metrics",
             div(class = "well",
                 h2("Environmental Performance Metrics (Aggregated Trends)"),
                 fluidRow(
                   column(12,
                          plotlyOutput("environment_metrics_bar_plot", height = "450px"),
                          div(class = "plot-source", align = "right", HTML(paste0("<i>", data_source_text, "</i>"))),
                          downloadButton("download_environment_metrics_bar_plot_png", "Download Plot (PNG)"),
                          downloadButton("download_environment_metrics_bar_plot_pdf", "Download Plot (PDF)"),
                          downloadButton("download_environment_metrics_bar_plot_jpeg", "Download Plot (JPEG)")
                   )
                 )
             )
    ),
    tabPanel("Social Metrics",
             div(class = "well",
                 h2("Social Performance Metrics"),
                 fluidRow(
                   column(12,
                          plotlyOutput("social_bar_chart", height = "450px"),
                          div(class = "plot-source", align = "right", HTML(paste0("<i>", data_source_text, "</i>"))),
                          downloadButton("download_social_bar_chart_png", "Download Plot (PNG)"),
                          downloadButton("download_social_bar_chart_pdf", "Download Plot (PDF)"),
                          downloadButton("download_social_bar_chart_jpeg", "Download Plot (JPEG)")
                   )
                 )
             )
    ),
    tabPanel("Governance Metrics",
             div(class = "well",
                 h2("Governance Performance Metrics"),
                 fluidRow(
                   column(12,
                          plotlyOutput("governance_bar_chart", height = "450px"),
                          div(class = "plot-source", align = "right", HTML(paste0("<i>", data_source_text, "</i>"))),
                          downloadButton("download_governance_bar_chart_png", "Download Plot (PNG)"),
                          downloadButton("download_governance_bar_chart_pdf", "Download Plot (PDF)"),
                          downloadButton("download_governance_bar_chart_jpeg", "Download Plot (JPEG)")
                   )
                 )
             )
    ),
    tabPanel("Company Data Table",
             div(class = "well",
                 h2("Sample Company-level ESG Data"),
                 fluidRow(
                   column(12,
                          DTOutput("company_data_table")
                   )
                 ),
                 div(class = "plot-source", align = "right", HTML(paste0("<i>", data_source_text, "</i>")))
             )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # Helper function to generate ggplot object for reuse in downloads
  create_esg_score_line_plot <- function() {
    ggplot(esg_scores_data %>% filter(Category == "Overall ESG"), aes(x = Year, y = Score)) +
      geom_line(size = 1.5, color = "#1a73e8") +
      geom_point(size = 4, color = "#1a73e8") +
      scale_x_continuous(breaks = unique(esg_scores_data$Year)) +
      labs(
        title = "Average Overall ESG Score Trend",
        y = "Score (0-100)",
        x = "Year"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none"
      )
  }
  
  # 1. Line Plot for Overall ESG Scores
  output$esg_score_line_plot <- renderPlotly({
    p <- create_esg_score_line_plot() +
      geom_point(aes(text = paste("Year:", Year, "<br>Score:", Score))) # Add tooltip for plotly
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$download_esg_score_line_plot_png <- downloadHandler(
    filename = function() { paste("overall_esg_score_trend_", Sys.Date(), ".png", sep = "") },
    content = function(file) { ggsave(file, plot = create_esg_score_line_plot(), device = "png", width = 10, height = 7, units = "in", dpi = 300) }
  )
  output$download_esg_score_line_plot_pdf <- downloadHandler(
    filename = function() { paste("overall_esg_score_trend_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) { ggsave(file, plot = create_esg_score_line_plot(), device = "pdf", width = 10, height = 7, units = "in") }
  )
  output$download_esg_score_line_plot_jpeg <- downloadHandler(
    filename = function() { paste("overall_esg_score_trend_", Sys.Date(), ".jpeg", sep = "") },
    content = function(file) { ggsave(file, plot = create_esg_score_line_plot(), device = "jpeg", width = 10, height = 7, units = "in", dpi = 300) }
  )
  
  # Helper function to generate ggplot object for reuse in downloads
  create_esg_category_bar_plot <- function() {
    ggplot(esg_scores_data %>% filter(Category != "Overall ESG"), aes(x = as.factor(Year), y = Score, fill = Category)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
      scale_fill_brewer(palette = "Dark2") +
      labs(
        title = "Average ESG Scores by Category and Year",
        y = "Score (0-100)",
        x = "Year"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "bottom"
      )
  }
  
  # 2. Grouped Bar Plot for ESG Categories by Year
  output$esg_category_bar_plot <- renderPlotly({
    p <- create_esg_category_bar_plot()
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$download_esg_category_bar_plot_png <- downloadHandler(
    filename = function() { paste("esg_scores_by_category_year_", Sys.Date(), ".png", sep = "") },
    content = function(file) { ggsave(file, plot = create_esg_category_bar_plot(), device = "png", width = 12, height = 7, units = "in", dpi = 300) }
  )
  output$download_esg_category_bar_plot_pdf <- downloadHandler(
    filename = function() { paste("esg_scores_by_category_year_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) { ggsave(file, plot = create_esg_category_bar_plot(), device = "pdf", width = 12, height = 7, units = "in") }
  )
  output$download_esg_category_bar_plot_jpeg <- downloadHandler(
    filename = function() { paste("esg_scores_by_category_year_", Sys.Date(), ".jpeg", sep = "") },
    content = function(file) { ggsave(file, plot = create_esg_category_bar_plot(), device = "jpeg", width = 12, height = 7, units = "in", dpi = 300) }
  )
  
  # Helper function to generate plotly object for reuse in downloads
  create_sector_pie_chart <- function() {
    plot_ly(sector_distribution_data, labels = ~Sector, values = ~CompanyCount, type = 'pie',
            marker = list(colors = RColorBrewer::brewer.pal(length(sector_distribution_data$Sector), "Spectral")),
            textinfo = 'percent+label',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(Sector, '-', CompanyCount, 'Companies'),
            sort = TRUE,
            textposition = 'inside') %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        showlegend = TRUE,
        automargin = TRUE
      )
  }
  
  # 3. Pie Chart for Sector Distribution
  output$sector_pie_chart <- renderPlotly({
    create_sector_pie_chart() %>%
      config(displayModeBar = FALSE)
  })
  
  output$download_sector_pie_chart_png <- downloadHandler(
    filename = function() { paste("sector_distribution_pie_chart_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      temp_html_file <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget = create_sector_pie_chart(), file = temp_html_file, selfcontained = TRUE)
      webshot2::webshot(url = temp_html_file, file = file, vwidth = 1000, vheight = 700, zoom = 2)
      unlink(temp_html_file)
    }
  )
  output$download_sector_pie_chart_pdf <- downloadHandler(
    filename = function() { paste("sector_distribution_pie_chart_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      temp_html_file <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget = create_sector_pie_chart(), file = temp_html_file, selfcontained = TRUE)
      webshot2::webshot(url = temp_html_file, file = file, vwidth = 1000, vheight = 700, zoom = 2)
      unlink(temp_html_file)
    }
  )
  output$download_sector_pie_chart_jpeg <- downloadHandler(
    filename = function() { paste("sector_distribution_pie_chart_", Sys.Date(), ".jpeg", sep = "") },
    content = function(file) {
      temp_html_file <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget = create_sector_pie_chart(), file = temp_html_file, selfcontained = TRUE)
      webshot2::webshot(url = temp_html_file, file = file, vwidth = 1000, vheight = 700, zoom = 2)
      unlink(temp_html_file)
    }
  )
  
  # Helper function to generate ggplot object for reuse in downloads
  create_policy_bar_chart <- function() {
    ggplot(policy_disclosure_data, aes(x = reorder(Policy, Percentage), y = Percentage, fill = Policy)) +
      geom_bar(stat = "identity", width = 0.7, color = "white", linewidth = 0.5, show.legend = FALSE) +
      geom_text(aes(label = paste0(Percentage, "%")), hjust = -0.1, size = 4) +
      labs(
        title = "Percentage of Companies with Key Policy Disclosures",
        x = "", # Removed x-axis title
        y = "Percentage (%)"
      ) +
      scale_fill_brewer(palette = "Accent") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12), # Allow policy names to be displayed on y-axis
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "#e0e0e0", linetype = "dashed")
      ) +
      coord_flip()
  }
  
  # 4. Bar Chart for Policy Disclosures
  output$policy_bar_chart <- renderPlotly({
    p <- create_policy_bar_chart()
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(yaxis = list(title = "", titlefont = list(size=14), standoff = 20)) %>% # Explicitly set y-axis title to empty string
      config(displayModeBar = FALSE)
  })
  
  output$download_policy_bar_chart_png <- downloadHandler(
    filename = function() { paste("policy_disclosures_bar_chart_", Sys.Date(), ".png", sep = "") },
    content = function(file) { ggsave(file, plot = create_policy_bar_chart(), device = "png", width = 12, height = 8, units = "in", dpi = 300) }
  )
  output$download_policy_bar_chart_pdf <- downloadHandler(
    filename = function() { paste("policy_disclosures_bar_chart_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) { ggsave(file, plot = create_policy_bar_chart(), device = "pdf", width = 12, height = 8, units = "in") }
  )
  output$download_policy_bar_chart_jpeg <- downloadHandler(
    filename = function() { paste("policy_disclosures_bar_chart_", Sys.Date(), ".jpeg", sep = "") },
    content = function(file) { ggsave(file, plot = create_policy_bar_chart(), device = "jpeg", width = 12, height = 8, units = "in", dpi = 300) }
  )
  
  # Helper function to generate ggplot object for reuse in downloads
  create_esg_by_sector_bar_plot <- function() {
    ggplot(esg_by_sector_2023, aes(x = Sector, y = Score, fill = Metric)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
      scale_fill_manual(values = c("Environment" = "#33a02c", "Social" = "#1f78b4", "Governance" = "#ff7f00", "Overall ESG" = "#e31a1c")) + # Custom colors
      labs(
        title = "Average ESG Scores by Sector (2023)",
        y = "Score (0-100)",
        x = "Sector"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "bottom"
      )
  }
  
  # 5. Grouped Bar Plot for ESG Scores by Sector
  output$esg_by_sector_bar_plot <- renderPlotly({
    p <- create_esg_by_sector_bar_plot()
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$download_esg_by_sector_bar_plot_png <- downloadHandler(
    filename = function() { paste("esg_scores_by_sector_2023_", Sys.Date(), ".png", sep = "") },
    content = function(file) { ggsave(file, plot = create_esg_by_sector_bar_plot(), device = "png", width = 12, height = 7, units = "in", dpi = 300) }
  )
  output$download_esg_by_sector_bar_plot_pdf <- downloadHandler(
    filename = function() { paste("esg_scores_by_sector_2023_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) { ggsave(file, plot = create_esg_by_sector_bar_plot(), device = "pdf", width = 12, height = 7, units = "in") }
  )
  output$download_esg_by_sector_bar_plot_jpeg <- downloadHandler(
    filename = function() { paste("esg_scores_by_sector_2023_", Sys.Date(), ".jpeg", sep = "") },
    content = function(file) { ggsave(file, plot = create_esg_by_sector_bar_plot(), device = "jpeg", width = 12, height = 7, units = "in", dpi = 300) }
  )
  
  # Helper function to generate ggplot object for reuse in downloads
  create_environment_metrics_bar_plot <- function() {
    ggplot(environmental_metrics_data, aes(x = Category, y = Value, fill = as.factor(Year))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
      scale_fill_brewer(palette = "Greens") +
      labs(
        title = "Aggregated Environmental Performance Metrics (2022 vs. 2023)",
        x = "Metric",
        y = "Value"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "bottom"
      )
  }
  
  # 6. Environmental Metrics Bar Plot (Trend)
  output$environment_metrics_bar_plot <- renderPlotly({
    p <- create_environment_metrics_bar_plot()
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$download_environment_metrics_bar_plot_png <- downloadHandler(
    filename = function() { paste("environmental_metrics_trend_", Sys.Date(), ".png", sep = "") },
    content = function(file) { ggsave(file, plot = create_environment_metrics_bar_plot(), device = "png", width = 12, height = 7, units = "in", dpi = 300) }
  )
  output$download_environment_metrics_bar_plot_pdf <- downloadHandler(
    filename = function() { paste("environmental_metrics_trend_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) { ggsave(file, plot = create_environment_metrics_bar_plot(), device = "pdf", width = 12, height = 7, units = "in") }
  )
  output$download_environment_metrics_bar_plot_jpeg <- downloadHandler(
    filename = function() { paste("environmental_metrics_trend_", Sys.Date(), ".jpeg", sep = "") },
    content = function(file) { ggsave(file, plot = create_environment_metrics_bar_plot(), device = "jpeg", width = 12, height = 7, units = "in", dpi = 300) }
  )
  
  # Helper function to generate ggplot object for reuse in downloads
  create_social_bar_chart <- function() {
    ggplot(social_metrics_data, aes(x = reorder(Metric, Value), y = Value, fill = Metric)) +
      geom_bar(stat = "identity", width = 0.7, color = "white", linewidth = 0.5, show.legend = FALSE) +
      geom_text(aes(label = round(Value, 1)), vjust = -0.5, size = 4) +
      labs(
        title = "Average Social Performance Metrics (2023)",
        x = "Metric",
        y = "Value"
      ) +
      scale_fill_brewer(palette = "Blues") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  }
  
  # 7. Social Metrics Bar Plot
  output$social_bar_chart <- renderPlotly({
    p <- create_social_bar_chart()
    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$download_social_bar_chart_png <- downloadHandler(
    filename = function() { paste("social_metrics_bar_chart_", Sys.Date(), ".png", sep = "") },
    content = function(file) { ggsave(file, plot = create_social_bar_chart(), device = "png", width = 10, height = 7, units = "in", dpi = 300) }
  )
  output$download_social_bar_chart_pdf <- downloadHandler(
    filename = function() { paste("social_metrics_bar_chart_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) { ggsave(file, plot = create_social_bar_chart(), device = "pdf", width = 10, height = 7, units = "in") }
  )
  output$download_social_bar_chart_jpeg <- downloadHandler(
    filename = function() { paste("social_metrics_bar_chart_", Sys.Date(), ".jpeg", sep = "") },
    content = function(file) { ggsave(file, plot = create_social_bar_chart(), device = "jpeg", width = 10, height = 7, units = "in", dpi = 300) }
  )
  
  # Helper function to generate ggplot object for reuse in downloads
  create_governance_bar_chart <- function() {
    ggplot(governance_metrics_data, aes(x = reorder(Metric, Value), y = Value, fill = Metric)) +
      geom_bar(stat = "identity", width = 0.7, color = "white", linewidth = 0.5, show.legend = FALSE) +
      geom_text(aes(label = round(Value, 1)), vjust = -0.5, size = 4) +
      labs(
        title = "Average Governance Performance Metrics (2023)",
        x = "Metric",
        y = "Value"
      ) +
      scale_fill_brewer(palette = "Oranges") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  }
  
  # 8. Governance Metrics Bar Plot
  output$governance_bar_chart <- renderPlotly({
    p <- create_governance_bar_chart()
    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$download_governance_bar_chart_png <- downloadHandler(
    filename = function() { paste("governance_metrics_bar_chart_", Sys.Date(), ".png", sep = "") },
    content = function(file) { ggsave(file, plot = create_governance_bar_chart(), device = "png", width = 10, height = 7, units = "in", dpi = 300) }
  )
  output$download_governance_bar_chart_pdf <- downloadHandler(
    filename = function() { paste("governance_metrics_bar_chart_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) { ggsave(file, plot = create_governance_bar_chart(), device = "pdf", width = 10, height = 7, units = "in") }
  )
  output$download_governance_bar_chart_jpeg <- downloadHandler(
    filename = function() { paste("governance_metrics_bar_chart_", Sys.Date(), ".jpeg", sep = "") },
    content = function(file) { ggsave(file, plot = create_governance_bar_chart(), device = "jpeg", width = 10, height = 7, units = "in", dpi = 300) }
  )
  
  # 9. Company Data Table
  output$company_data_table <- renderDT({
    company_esg_data %>%
      mutate(
        Market_Cap_Cr = format(Market_Cap_Cr, big.mark = ",", scientific = FALSE), # Format market cap
        # Optionally format scores as needed
        ESG_Score_2023 = round(ESG_Score_2023, 1),
        Environment_Score_2023 = round(Environment_Score_2023, 1),
        Social_Score_2023 = round(Social_Score_2023, 1),
        Governance_Score_2023 = round(Governance_Score_2023, 1)
      )
  }, options = list(pageLength = 10, scrollX = TRUE,
                    dom = 'lfrtip', # Show length, filter, table, info, pagination
                    columnDefs = list(list(className = 'dt-center', targets = '_all')) # Center align text
  ),
  filter = 'top', # Add filters to columns
  selection = 'none',
  rownames = FALSE) # No row names
}

shinyApp(ui,server)