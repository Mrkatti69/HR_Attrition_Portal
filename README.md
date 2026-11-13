# HR Employee Attrition Analytics & ML Risk Portal

An interactive web dashboard and machine learning risk predictor built with **R Shiny** and **Random Forest** to help HR teams identify employee turnover drivers and evaluate individual retention risk in real time.

🔗 *Live Interactive App:* https://katti-hr.shinyapps.io/HR-Attrition-Portal/

---

## Key Features

- **Real-Time Attrition Risk Predictor:** Powered by a **Random Forest Classifier** (**83.86% overall accuracy**) that evaluates employee profiles and outputs an exact attrition probability percentage with a dynamic risk gauge (`LOW RISK` vs. `HIGH RISK` at a `>30%` threshold).
- **Actionable Profile Drivers:** Automatically translates model predictions into clear business explanations (e.g., overtime burnout impact, compensation tier protection, and satisfaction scores).
- **One-Click HR Test Scenarios:** Built-in sidebar presets (**Burnout Profile**, **Senior Retention**, and **Entry-Level Baseline**) for rapid executive demonstrations.
- **Interactive Exploratory Analytics:** Powered by **Plotly** and **ggplot2** with hover tooltips across four workforce views:
  - Attrition Balance
  - Job Satisfaction Impact
  - Monthly Income Levels vs. Attrition
  - Age Demographic Spread
- **Global Feature Importance:** Visualizes the top predictive indicators (`OverTime`, `MonthlyIncome`, `Age`, `TotalSatisfaction`, and `Tenure-to-Age Ratio`) using Gini Importance scores.
- **One-Click CSV Risk Report Export:** Allows HR managers to download timestamped prediction summaries for individual employee assessments.

---

## Tech Stack

- **Language:** R
- **Web Framework & UI:** `shiny`, `bslib` (Bootstrap 5 Minty theme), `shinyWidgets`, `bsicons`
- **Machine Learning:** `randomForest`
- **Data Wrangling & Visualization:** `dplyr`, `ggplot2`, `plotly`
- **Cloud Deployment:** `rsconnect` on **shinyapps.io**

---

## Project Structure

```text
├── app.R                                   # Complete Shiny UI, Server & ML pipeline
├── WA_Fn-UseC_-HR-Employee-Attrition.csv   # IBM HR Analytics Employee Attrition dataset
├── Employee_Attrition_Analysis.R           # Exploratory modeling & validation script
└── README.md                               # Project documentation
```

---

## Run Locally

1. **Clone the repository:**
   ```bash
   git clone https://github.com
   cd hr-attrition-portal
   ```

2. **Install required R packages:**
   ```r
   install.packages(c(
     "shiny", "bslib", "shinyWidgets", "bsicons",
     "randomForest", "dplyr", "ggplot2", "plotly"
   ))
   ```

3. **Launch the application in RStudio:**
   ```r
   shiny::runApp("app.R")
   ```

---

## Model Summary

| Metric | Detail |
| :--- | :--- |
| **Algorithm** | Random Forest Classifier |
| **Overall Accuracy** | 83.86% |
| **Alert Decision Threshold** | > 30% Attrition Probability |
| **Core Predictors** | `OverTime`, `MonthlyIncome`, `Age`, `totalSatisfaction`, `yearsAtCompanyByAge` |
