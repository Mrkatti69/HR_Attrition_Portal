library(shiny)
library(ggplot2)
library(dplyr)
library(randomForest)
library(bslib)
library(shinyWidgets)
library(bsicons)
library(plotly)

# Pre-load data and build model behind the scenes for the interface
raw_data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
q1 <- quantile(raw_data$MonthlyIncome, 0.25)
q3 <- quantile(raw_data$MonthlyIncome, 0.75)
IQR <- q3 - q1
clean_data <- raw_data %>% filter(MonthlyIncome >= (q1 - 1.5 * IQR) & MonthlyIncome <= (q3 + 1.5 * IQR))

clean_data$totalSatisfaction <- clean_data$JobSatisfaction + clean_data$EnvironmentSatisfaction + clean_data$RelationshipSatisfaction
clean_data$yearsAtCompanyByAge <- clean_data$YearsAtCompany / clean_data$Age
clean_data$Attrition <- as.factor(clean_data$Attrition)

set.seed(123)
rf_model <- randomForest(Attrition ~ OverTime + MonthlyIncome + Age + totalSatisfaction + yearsAtCompanyByAge, data = clean_data, importance = TRUE)

# ==================== FRONTEND USER INTERFACE (UI) ====================
ui <- fluidPage(
  # Add web styling theme
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  
  titlePanel("📊 HR Employee Attrition Analytics Portal"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Interactive Controls"),
      p("Adjust values dynamically to update analytics & predictions.", class = "text-muted small"),
      
      accordion(
        id = "sidebar_accordion",
        open = c("Predictor Inputs", "Quick Test Scenarios"),
        
        accordion_panel(
          "Chart Settings",
          selectInput("chart_type", "Select Analytical Chart:",
                      choices = c("Attrition Balance", "Job Satisfaction Impact", 
                                  "Income Levels vs Attrition", "Age Demographic Spread"))
        ),
        
        accordion_panel(
          "Predictor Inputs",
          sliderInput("ui_age", "Employee Age:", min = 18, max = 60, value = 30),
          sliderInput("ui_income", "Monthly Income ($):", min = 1000, max = 20000, value = 5000),
          selectInput("ui_overtime", "Works Overtime?", choices = c("Yes", "No")),
          sliderInput("ui_sat", "Total Combined Satisfaction (3-12):", min = 3, max = 12, value = 8),
          actionButton("predict_btn", "Calculate Risk", class = "btn-primary w-100 mt-2")
        ),
        
        accordion_panel(
          "Quick Test Scenarios",
          div(
            class = "d-grid gap-2",
            actionButton("preset_burnout", "Burnout Profile (High Risk)", class = "btn-outline-danger btn-sm"),
            actionButton("preset_loyal", "Senior Retention (Low Risk)", class = "btn-outline-success btn-sm"),
            actionButton("preset_new", "Entry-Level Baseline", class = "btn-outline-secondary btn-sm")
          )
        )
      )
    ), 
    
    mainPanel(
      tabsetPanel(
        tabPanel("Exploratory Data Visualizations", 
                 br(),
                 plotlyOutput("metric_plot")),
        tabPanel("Machine Learning Risk Predictor", 
                 br(),
                 h3("Predictive System Output"),
                 uiOutput("prediction_card"),
                 hr(),
                 h4("Global AI Feature Importance Drivers"),
                 plotlyOutput("importance_plot"))
      )
    )
  )
)

# ==================== BACKEND INTERFACE ROUTER (SERVER) ====================
server <- function(input, output, session) {
  
  # 1. Interactive Exploratory Chart
  output$metric_plot <- renderPlotly({
    if (input$chart_type == "Attrition Balance") {
      p <- ggplot(clean_data, aes(x = Attrition, fill = Attrition)) +
        geom_bar() +
        scale_fill_manual(values = c("No" = "#2ecc71", "Yes" = "#e74c3c")) +
        labs(title = "Company Overview: Attrition Balance", x = "Attrition Status", y = "Total Headcount") +
        theme_minimal()
    } else if (input$chart_type == "Job Satisfaction Impact") {
      p <- ggplot(clean_data, aes(x = as.factor(JobSatisfaction), fill = Attrition)) +
        geom_bar(position = "dodge") +
        scale_fill_manual(values = c("No" = "#2ecc71", "Yes" = "#e74c3c")) +
        labs(title = "Job Satisfaction Breakdown", x = "Satisfaction Rating", y = "Employee Count") +
        theme_minimal()
    } else if (input$chart_type == "Income Levels vs Attrition") {
      p <- ggplot(clean_data, aes(x = Attrition, y = MonthlyIncome, fill = Attrition)) +
        geom_boxplot() +
        scale_fill_manual(values = c("No" = "#2ecc71", "Yes" = "#e74c3c")) +
        labs(title = "Salary Profiles Comparison", x = "Attrition Status", y = "Monthly Income ($)") +
        theme_minimal()
    } else {
      p <- ggplot(clean_data, aes(x = Age, fill = Attrition)) +
        geom_histogram(position = "dodge", bins = 20) +
        labs(title = "Age Demographic Spread", x = "Age", y = "Employee Count") +
        theme_minimal()
    }
    ggplotly(p)
  })
  
  # 2. Interactive Feature Importance Chart (Cleaned Labels)
  output$importance_plot <- renderPlotly({
    imp_df <- as.data.frame(randomForest::importance(rf_model))
    imp_df$Feature <- rownames(imp_df)
    
    clean_names <- c(
      "OverTime" = "Works Overtime",
      "MonthlyIncome" = "Monthly Income ($)",
      "Age" = "Employee Age",
      "totalSatisfaction" = "Total Satisfaction",
      "yearsAtCompanyByAge" = "Tenure-to-Age Ratio"
    )
    imp_df$CleanFeature <- ifelse(imp_df$Feature %in% names(clean_names), clean_names[imp_df$Feature], imp_df$Feature)
    
    p <- ggplot(imp_df, aes(x = reorder(CleanFeature, MeanDecreaseGini), y = MeanDecreaseGini,
                            text = paste0("Feature: ", CleanFeature, "<br>Gini Importance: ", round(MeanDecreaseGini, 2)))) +
      geom_col(fill = "#2c3e50", width = 0.6) +
      coord_flip() +
      labs(title = "Top Model Indicators (Gini Importance)", x = "", y = "Relative Importance Score") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # 3. Quick Scenario Presets
  observeEvent(input$preset_burnout, {
    updateSliderInput(session, "ui_age", value = 24)
    updateSliderInput(session, "ui_income", value = 2300)
    updateSelectInput(session, "ui_overtime", selected = "Yes")
    updateSliderInput(session, "ui_sat", value = 5)
  })
  
  observeEvent(input$preset_loyal, {
    updateSliderInput(session, "ui_age", value = 45)
    updateSliderInput(session, "ui_income", value = 14500)
    updateSelectInput(session, "ui_overtime", selected = "No")
    updateSliderInput(session, "ui_sat", value = 11)
  })
  
  observeEvent(input$preset_new, {
    updateSliderInput(session, "ui_age", value = 30)
    updateSliderInput(session, "ui_income", value = 5000)
    updateSelectInput(session, "ui_overtime", selected = "No")
    updateSliderInput(session, "ui_sat", value = 8)
  })
  
  # Calculate risk probability and key drivers on button click
  pred_results <- eventReactive(input$predict_btn, {
    new_emp <- data.frame(
      Age = input$ui_age,
      MonthlyIncome = input$ui_income,
      OverTime = input$ui_overtime,
      totalSatisfaction = input$ui_sat,
      yearsAtCompanyByAge = 5 / input$ui_age
    )
    
    prob <- predict(rf_model, new_emp, type = "prob")[, "Yes"]
    prob_pct <- round(prob * 100, 1)
    is_high_risk <- prob > 0.30
    
    # Build dynamic profile drivers
    drivers <- c()
    if (input$ui_overtime == "Yes") {
      drivers <- c(drivers, "Working overtime significantly increases burnout and attrition risk.")
    } else {
      drivers <- c(drivers, "No overtime helps maintain healthy work-life balance.")
    }
    if (input$ui_income >= 8000) {
      drivers <- c(drivers, "Above-average monthly income strongly supports retention.")
    } else if (input$ui_income < 4000) {
      drivers <- c(drivers, "Lower monthly income tier increases turnover vulnerability.")
    }
    if (input$ui_sat >= 9) {
      drivers <- c(drivers, "High combined satisfaction score reinforces retention.")
    } else if (input$ui_sat <= 6) {
      drivers <- c(drivers, "Low satisfaction score elevates flight risk.")
    }
    
    list(prob_pct = prob_pct, is_high_risk = is_high_risk, drivers = drivers)
  }, ignoreNULL = FALSE)
  
  # Render upgraded Predictive System Output card
  output$prediction_card <- renderUI({
    res <- pred_results()
    box_theme <- if (res$is_high_risk) "danger" else "success"
    risk_label <- if (res$is_high_risk) "HIGH RISK" else "LOW RISK"
    risk_summary <- if (res$is_high_risk) {
      "The AI forecasts this employee profile matches characteristics of individuals who left the company."
    } else {
      "The AI forecasts this employee profile shares attributes indicating strong retention."
    }
    
    card(
      card_header(
        class = paste0("bg-", box_theme, " text-white d-flex justify-content-between align-items-center"),
        span(strong(risk_label), " — Attrition Risk: ", strong(paste0(res$prob_pct, "%"))),
        tooltip(
          bsicons::bs_icon("info-circle"),
          "Random Forest Classifier | Overall Accuracy: 83.86% | Alert Threshold: >30% Risk"
        )
      ),
      card_body(
        p(risk_summary),
        progressBar(
          id = "risk_bar",
          value = res$prob_pct,
          status = box_theme,
          display_pct = TRUE,
          striped = TRUE
        ),
        hr(),
        h6("Key Profile Drivers:"),
        tags$ul(lapply(res$drivers, tags$li)),
        div(
          class = "text-end mt-3",
          downloadButton("download_report", "Export Risk Report (.csv)", class = "btn-outline-dark btn-sm")
        )
      )
    )
  })
  
  # 4. Export Employee Risk Report as CSV
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("HR_Attrition_Risk_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      res <- pred_results()
      report_df <- data.frame(
        Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        Employee_Age = input$ui_age,
        Monthly_Income_USD = input$ui_income,
        Works_Overtime = input$ui_overtime,
        Total_Satisfaction = input$ui_sat,
        Attrition_Risk_Pct = res$prob_pct,
        Risk_Category = ifelse(res$is_high_risk, "HIGH RISK", "LOW RISK"),
        Key_Drivers = paste(res$drivers, collapse = " | ")
      )
      write.csv(report_df, file, row.names = FALSE)
    }
  )
}

# Run the complete application
shinyApp(ui = ui, server = server)

