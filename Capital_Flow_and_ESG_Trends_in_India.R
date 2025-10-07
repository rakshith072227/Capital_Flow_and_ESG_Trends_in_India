library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(scales)
library(readr)
library(shinydashboard)
library(shinyWidgets)
library(htmltools)
library(shinycssloaders)

if (!file.exists("revenue_cleaned_long.csv")) {
  warning("revenue_cleaned_long.csv not found. Generating dummy revenue data.")
  revenue_data <- data.frame(
    Company = rep(c("RELIANCE.BO", "TCS.BO", "HDFCBANK.BO", "ICICIBANK.BO", "INFY.BO",
                    "HINDUNILVR.BO", "SBIN.BO", "BAJFINANCE.BO", "BHARTIARTL.BO",
                    "ASIANPAINT.BO", "ITC.BO", "LT.BO", "KOTAKBANK.BO", "AXISBANK.BO",
                    "MARUTI.BO", "SUNPHARMA.BO", "NESTLEIND.BO", "ULTRACEMCO.BO",
                    "TITAN.BO", "ADANIPORTS.BO"), each = 11),
    Year = rep(2014:2024, 20),
    Revenue = round(runif(220, 10000, 500000), 0)
  )
} else {
  revenue_data <- read_csv("revenue_cleaned_long.csv")
}


set.seed(123)
companies_list <- unique(revenue_data$Company)
years <- 2014:2024

company_industry_map <- list(
  "RELIANCE.BO" = "Oil & Gas", "TCS.BO" = "IT Services", "HDFCBANK.BO" = "Financial Services",
  "ICICIBANK.BO" = "Financial Services", "INFY.BO" = "IT Services", "HINDUNILVR.BO" = "FMCG",
  "SBIN.BO" = "Financial Services", "BAJFINANCE.BO" = "Financial Services", "BHARTIARTL.BO" = "Telecom",
  "ASIANPAINT.BO" = "Chemicals", "ITC.BO" = "FMCG", "LT.BO" = "Capital Goods",
  "KOTAKBANK.BO" = "Financial Services", "AXISBANK.BO" = "Financial Services", "MARUTI.BO" = "Automobile",
  "SUNPHARMA.BO" = "Pharmaceuticals", "NESTLEIND.BO" = "FMCG", "ULTRACEMCO.BO" = "Building Materials",
  "TITAN.BO" = "Consumer Discretionary", "ADANIPORTS.BO" = "Logistics"
)

esg_data <- expand.grid(Company = companies_list, Year = years) %>%
  rowwise() %>%
  mutate(
    ESG_Score = max(1, min(100, runif(1, 40, 80) + (Year - 2014) * 0.5 + rnorm(1, 1, 5))),
    Industry = company_industry_map[[Company]]
  ) %>%
  ungroup()

combined_data <- esg_data %>%
  left_join(revenue_data, by = c("Company", "Year"))


custom_styles <- tags$head(
  tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;700&family=Montserrat:wght@400;700&family=Orbitron:wght@400;700&display=swap", rel = "stylesheet"),
  tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
  tags$style(HTML("
    body {
      font-family: 'Roboto', sans-serif;
      background-color: #1A202C;
      color: #E2E8F0;
    }

    .main-banner {
      background: linear-gradient(rgba(0,0,0,0.6), rgba(0,0,0,0.6)), url('https://images.unsplash.com/photo-1611974789855-9c2a0a7d5b8b?q=80&w=2940&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D') no-repeat center center;
      background-size: cover;
      padding: 60px 0;
      color: white;
      text-shadow: 2px 2px 8px rgba(0,0,0,0.8);
      border-radius: 15px;
      margin-bottom: 30px;
      box-shadow: 0 8px 30px rgba(0,0,0,0.5);
      animation: fadeIn 1s ease-out;
    }
    .main-banner h1 {
      font-family: 'Orbitron', sans-serif;
      color: #00BF63 !important;
      font-size: 4em;
      margin-bottom: 10px;
      letter-spacing: 2px;
      text-transform: uppercase;
    }
    .main-banner p {
      font-size: 1.4em;
      margin-top: 0;
      font-weight: 300;
    }

    .kpi-box {
      text-align: center;
      padding: 30px;
      margin-bottom: 25px;
      border-radius: 18px;
      background: linear-gradient(145deg, #2D3748, #1A202C);
      box-shadow: 8px 8px 20px #11141a, -8px -8px 20px #2a3442;
      transition: all 0.3s ease-in-out;
      border: 1px solid #4A5568;
    }
    .kpi-box:hover {
      transform: translateY(-8px) scale(1.02);
      box-shadow: 12px 12px 30px #11141a, -12px -12px 30px #2a3442;
    }
    .kpi-title {
      font-size: 18px;
      color: #CBD5E0;
      margin-bottom: 10px;
      font-weight: 400;
    }
    .kpi-value {
      font-size: 36px;
      font-weight: bold;
      color: #00BF63;
      letter-spacing: 1px;
    }
    .kpi-icon {
      font-size: 28px;
      margin-right: 12px;
      color: #63B3ED;
    }

    .panel {
      background-color: #2D3748;
      padding: 30px;
      border-radius: 20px;
      margin-top: 30px;
      box-shadow: 0 10px 30px rgba(0,0,0,0.4);
      border: 1px solid #4A5568;
    }
    h1, h2, h3, h4 {
      font-family: 'Montserrat', sans-serif;
      color: #E2E8F0;
      border-bottom: 2px solid #4A5568;
      padding-bottom: 12px;
      margin-bottom: 25px;
    }

    .plotly .modebar {
      background: rgba(45, 55, 72, 0.9);
      border-radius: 10px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.5);
    }
    .plotly .modebar-btn {
      color: #E2E8F0 !important;
    }
    .plotly .modebar-btn.active {
      color: #00BF63 !important;
    }

    .industry-infographic, .plot-container {
      padding: 25px;
      background: #2D3748;
      border-radius: 20px;
      box-shadow: 0 8px 25px rgba(0,0,0,0.3);
      margin-top: 25px;
      border: 1px solid #4A5568;
    }
    .plot-title-container {
      margin-bottom: 20px;
    }
    .plot-subtitle {
      font-size: 15px;
      color: #A0AEC0;
      margin-top: -10px;
    }

    .form-control {
      background-color: #4A5568;
      color: #E2E8F0;
      border: 1px solid #63B3ED;
      border-radius: 8px;
    }
    .selectize-dropdown-content {
      background-color: #4A5568;
      color: #E2E8F0;
    }
    .selectize-dropdown .option.active {
      background-color: #63B3ED;
      color: white;
    }
    .irs--shiny .irs-bar {
      background: #00BF63;
      border-top: 1px solid #00BF63;
      border-bottom: 1px solid #00BF63;
    }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
      background: #00BF63;
      color: white;
    }
    .irs--shiny .irs-handle {
      border: 1px solid #00BF63;
      background-color: #2D3748;
      box-shadow: 0 1px 3px rgba(0,0,0,0.5);
    }

    @keyframes fadeIn {
      from { opacity: 0; }
      to { opacity: 1; }
    }
    @keyframes slideInUp {
      from { transform: translateY(20px); opacity: 0; }
      to { transform: translateY(0); opacity: 1; }
    }
    .kpi-box { animation: slideInUp 0.5s ease-out forwards; }
    .plot-container { animation: slideInUp 0.6s ease-out forwards; }
    .panel { animation: slideInUp 0.7s ease-out forwards; }

    .js-plotly-plot .plotly .cursor-pointer {
      cursor: pointer !important;
    }
    .main-svg .cartesianlayer .plot .xaxislayer .x.grid,
    .main-svg .cartesianlayer .plot .yaxislayer .y.grid {
      stroke: #4A5568;
      stroke-width: 0.5px;
    }
  "))
)

ui <- fluidPage(
  custom_styles,
  
  fluidRow(
    column(12, align = "center",
           div(class = "main-banner",
               h1("Capital Flow & ESG Trends in India"),
               p("Unlock deep insights into market performance and corporate sustainability.")
           )
    )
  ),
  
  fluidRow(
    column(4,
           div(class = "kpi-box",
               div(class = "kpi-title", tags$i(class = "fas fa-seedling kpi-icon"), "Avg. ESG Score (All Companies)"),
               div(class = "kpi-value", textOutput("avg_esg_kpi"))
           )),
    column(4,
           div(class = "kpi-box",
               div(class = "kpi-title", tags$i(class = "fas fa-chart-line kpi-icon"), "Total Revenue (₹ Cr)"),
               div(class = "kpi-value", textOutput("total_revenue_kpi"))
           )),
    column(4,
           div(class = "kpi-box",
               div(class = "kpi-title", tags$i(class = "fas fa-building kpi-icon"), "Total Companies Analyzed"),
               div(class = "kpi-value", textOutput("total_companies_kpi"))
           ))
  ),
  
  sidebarLayout(
    sidebarPanel(class = "panel",
                 h3("Dashboard Controls"),
                 selectInput("selected_company", "Select Company:", choices = unique(combined_data$Company)),
                 selectInput("esg_plot_type", "ESG Plot Type:", choices = c("Line", "Bar", "Area")),
                 selectInput("revenue_plot_type", "Revenue Plot Type:", choices = c("Line", "Bar", "Area")),
                 selectInput("scatter_type", "ESG vs Revenue Plot:", choices = c("Scatter", "Line with Points")),
                 sliderInput("esg_range", "ESG Score Range:", min = 0, max = 100, value = c(0, 100)),
                 sliderInput("revenue_range", "Revenue Range:", min = 0, max = max(combined_data$Revenue, na.rm = TRUE),
                             value = c(0, max(combined_data$Revenue, na.rm = TRUE)), step = 10000)
    ),
    
    mainPanel(
      div(class = "panel",
          div(class = "plot-container",
              div(class = "plot-title-container",
                  h3("ESG Score Trend Over Time"),
                  p(class = "plot-subtitle", "Track the Environmental, Social, and Governance performance year-over-year.")
              ),
              plotlyOutput("esgPlot", height = "400px") %>% withSpinner(type = 6, color = "#00BF63")
          ),
          div(class = "plot-container",
              div(class = "plot-title-container",
                  h3("Revenue Growth Trend"),
                  p(class = "plot-subtitle", "Visualize the financial revenue growth in ₹ Crore.")
              ),
              plotlyOutput("revenuePlot", height = "400px") %>% withSpinner(type = 6, color = "#00BF63")
          ),
          div(class = "plot-container",
              div(class = "plot-title-container",
                  h3("ESG Score vs. Revenue Correlation"),
                  p(class = "plot-subtitle", "Examine the relationship between ESG scores and revenue generation across years.")
              ),
              plotlyOutput("relationshipPlot", height = "450px") %>% withSpinner(type = 6, color = "#00BF63")
          )
      )
    )
  )
)
server <- function(input, output) {
  output$avg_esg_kpi <- renderText({
    round(mean(combined_data$ESG_Score, na.rm = TRUE), 1)
  })
  
  output$total_revenue_kpi <- renderText({
    format(round(sum(combined_data$Revenue, na.rm = TRUE)), big.mark = ",")
  })
  
  output$total_companies_kpi <- renderText({
    length(unique(combined_data$Company))
  })
  
  data_filtered <- reactive({
    combined_data %>% filter(Company == input$selected_company)
  })
  
  output$esgPlot <- renderPlotly({
    df <- data_filtered()
    p <- ggplot(df, aes(x = Year, y = ESG_Score,
                        text = paste("Year: ", Year, "<br>ESG Score: ", round(ESG_Score, 1)))) +
      labs(x = "Year", y = "ESG Score") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "#E2E8F0"),
            axis.title = element_text(size = 12, color = "#A0AEC0"),
            axis.text = element_text(size = 10, color = "#A0AEC0"),
            panel.grid.major.y = element_line(color = "#4A5568", linetype = "dashed", size = 0.3),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA))
    
    if (input$esg_plot_type == "Line") {
      p <- p + geom_line(color = "#00BF63", size = 1.8) + geom_point(size = 5, color = "#00BF63", alpha = 0.9)
    } else if (input$esg_plot_type == "Bar") {
      p <- p + geom_col(fill = "#00BF63", alpha = 0.9, width = 0.7)
    } else if (input$esg_plot_type == "Area") {
      p <- p + geom_area(fill = "#00BF63", alpha = 0.7, color = "#00BF63", linewidth = 1)
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "#2D3748", font = list(color = "#E2E8F0")),
             plot_bgcolor = "transparent",
             paper_bgcolor = "transparent",
             xaxis = list(gridcolor = "#4A5568", title_font = list(color = "#A0AEC0"), tickfont = list(color = "#A0AEC0")),
             yaxis = list(gridcolor = "#4A5568", title_font = list(color = "#A0AEC0"), tickfont = list(color = "#A0AEC0")))
  })
  
  output$revenuePlot <- renderPlotly({
    df <- data_filtered()
    p <- ggplot(df, aes(x = Year, y = Revenue,
                        text = paste("Year: ", Year, "<br>Revenue: ₹", format(Revenue, big.mark = ","), " Cr"))) +
      labs(x = "Year", y = "Revenue in ₹ Crore") +
      theme_minimal() + scale_y_continuous(labels = comma) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "#E2E8F0"),
            axis.title = element_text(size = 12, color = "#A0AEC0"),
            axis.text = element_text(size = 10, color = "#A0AEC0"),
            panel.grid.major.y = element_line(color = "#4A5568", linetype = "dashed", size = 0.3),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA))
    
    if (input$revenue_plot_type == "Line") {
      p <- p + geom_line(color = "#63B3ED", size = 1.8) + geom_point(size = 5, color = "#63B3ED", alpha = 0.9)
    } else if (input$revenue_plot_type == "Bar") {
      p <- p + geom_col(fill = "#63B3ED", alpha = 0.9, width = 0.7)
    } else if (input$revenue_plot_type == "Area") {
      p <- p + geom_area(fill = "#63B3ED", alpha = 0.7, color = "#63B3ED", linewidth = 1)
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "#2D3748", font = list(color = "#E2E8F0")),
             plot_bgcolor = "transparent",
             paper_bgcolor = "transparent",
             xaxis = list(gridcolor = "#4A5568", title_font = list(color = "#A0AEC0"), tickfont = list(color = "#A0AEC0")),
             yaxis = list(gridcolor = "#4A5568", title_font = list(color = "#A0AEC0"), tickfont = list(color = "#A0AEC0")))
  })
  
  output$relationshipPlot <- renderPlotly({
    df <- data_filtered() %>%
      filter(!is.na(Revenue)) %>%
      filter(ESG_Score >= input$esg_range[1], ESG_Score <= input$esg_range[2],
             Revenue >= input$revenue_range[1], Revenue <= input$revenue_range[2])
    
    p <- ggplot(df, aes(x = ESG_Score, y = Revenue, color = as.factor(Year),
                        text = paste("Year: ", Year, "<br>ESG Score: ", round(ESG_Score, 1), "<br>Revenue: ₹", format(Revenue, big.mark = ","), " Cr"))) +
      theme_minimal() + scale_y_continuous(labels = comma) +
      labs(x = "ESG Score", y = "Revenue in ₹ Crore", color = "Year") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "#E2E8F0"),
            axis.title = element_text(size = 12, color = "#A0AEC0"),
            axis.text = element_text(size = 10, color = "#A0AEC0"),
            legend.position = "bottom",
            legend.text = element_text(color = "#A0AEC0"),
            legend.title = element_text(color = "#E2E8F0"),
            panel.grid.major.y = element_line(color = "#4A5568", linetype = "dashed", size = 0.3),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA)) +
      scale_color_viridis_d(option = "plasma")
    
    if (input$scatter_type == "Scatter") {
      p <- p + geom_point(size = 5, alpha = 0.8)
    } else {
      p <- p + geom_line(size = 1.8) + geom_point(size = 5)
    }
    
    p <- p + geom_smooth(method = "lm", se = FALSE, color = "#FF3366", linetype = "dashed", size = 1.2)
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "#2D3748", font = list(color = "#E2E8F0")),
             plot_bgcolor = "transparent",
             paper_bgcolor = "transparent",
             xaxis = list(gridcolor = "#4A5568", title_font = list(color = "#A0AEC0"), tickfont = list(color = "#A0AEC0")),
             yaxis = list(gridcolor = "#4A5568", title_font = list(color = "#A0AEC0"), tickfont = list(color = "#A0AEC0")),
             legend = list(bgcolor = "transparent", bordercolor = "transparent", font = list(color = "#E2E8F0")))
  })
}

shinyApp(ui = ui, server = server)
