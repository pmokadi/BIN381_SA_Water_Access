# 🌍 South African Water Access Dashboard

[![Shiny](https://img.shields.io/badge/Shiny-Dashboard-blue?style=flat-square&logo=r)](https://shiny.rstudio.com/)
[![R Version](https://img.shields.io/badge/R-%3E%3D%204.0-brightgreen?style=flat-square&logo=r)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/License-Educational-orange?style=flat-square)](LICENSE)

> **Data-Driven Analysis of Water Access Inequality in South Africa (2002–2023)**

This dashboard visualises water-access trends across South African provinces, using processed GHS data, interactive mapping, time-series forecasting, and predictive modelling.

---

## 📊 Project Overview

**Course:** BIN381 Project Milestone 6  
**Group:** H  
**Authors:** Tokelo Ramogase, Kaidy Edwards, Tumiso Lethabo Koee, Paballo Mokadi  
**Institution:** Belgium Campus iTversity
**Date:** October 2025

### Key Features

- 🗺️ **Interactive Choropleth Maps** - Visualize provincial water access disparities
- 📈 **Predictive Modeling** - Linear Regression (R² = 0.88) + C5.0 Decision Tree
- 🔮 **Forecasting Engine** - Project water access trends 1-10 years ahead
- 🎯 **Scenario Builder** - Test custom what-if scenarios
- 📊 **Temporal Analysis** - Track trends from 1998 to 2023
- 🔬 **Model Evaluation** - Transparent performance metrics with cross-validation

---

## 🚀 Live Demo

**Dashboard:** [View Live Demo](https://pmokadi.shinyapps.io/SA-Water-Access-GroupH/)  

---

## 📸 Screenshots

### Dashboard Overview
![Dashboard](<img width="1470" height="920" alt="Dashboard Overview (1)" src="https://github.com/user-attachments/assets/437ec9a7-e51a-464f-b7ca-83e759a77adc" />)
*Summary statistics and project overview*

### Geographic Visualization
![Map](<img width="1470" height="920" alt="Map Tab" src="https://github.com/user-attachments/assets/bff374b9-4186-4022-b092-0f27268e404d" />)
*Provincial choropleth showing AccessRatio distribution*

### Forecast Analysis
![Forecast](<img width="1470" height="920" alt="Forcast Tab" src="https://github.com/user-attachments/assets/2a728b4a-7b5f-4b5a-9688-7be4beb3a582" />)

*Time series projection for selected province*

---

## 🛠️ Installation & Setup

### Prerequisites

- **R** (version ≥ 4.0)
- **RStudio** (recommended)
- Internet connection (for deployment)

### Required R Packages

```r
# Install dependencies
install.packages(c(
  "shiny",
  "shinydashboard",
  "dplyr",
  "ggplot2",
  "leaflet",
  "sf",
  "readr",
  "DT",
  "caret",
  "C50"
))
```

### Clone Repository

```bash
git clone https://github.com/your-username/sa-water-access-dashboard.git
cd sa-water-access-dashboard
```

---

## 📂 Project Structure

```
sa-water-access-dashboard/
├── app.R                               # Main Shiny dashboard application
├── water_access_lrm.rds                # Trained Linear Regression model
├── water_access_c50.rds                # Trained C5.0 Decision Tree model
├── water_access_processed_data.rds     # Cleaned & processed dataset
├── water_access_median_ratio.rds       # Classification threshold (0.37)
├── southafrica_water_access_v2.csv     # Raw data (fallback)
├── za.json                             # South Africa GeoJSON (provinces)
├── model_training.R                    # Model development script
├── data_preprocessing.R                # Data cleaning pipeline
├── README.md                           # This file
├── SA Water Access Dashboard Usage Documentation Guide [Group H].pdf  # Comprehensive usage documentation

```

---

## 🏃 Running Locally

### Quick Start

```r
# Open RStudio, set working directory
setwd("path/to/sa-water-access-dashboard")

# Run the dashboard
shiny::runApp()
```

The dashboard will open in your default browser at `http://127.0.0.1:xxxx`

### Alternative Method

```r
# Load and run directly
library(shiny)
runApp("app.R")
```

---

## 📊 Data Sources

### Primary Dataset

- **Source:** General Household Survey (GHS), Statistics South Africa
- **Years:** 2002, 2006, 2010, 2016, 2018, 2023
- **Scope:** Provincial household water indicators (9 provinces)
- **Format:** Tab-delimited CSV

### Variables

| Variable | Description | Unit |
|----------|-------------|------|
| `SurveyYear` | Year of survey | Year (2002-2023) |
| `Province` | South African province | Categorical (9 levels) |
| `PipedWater` | Households with piped water | Thousands (k HH) |
| `ImprovedSource` | Households with improved sources | Thousands (k HH) |
| `SurfaceWater` | Households using surface water | Thousands (k HH) |
| `OtherUnspecified` | Other/unspecified sources | Thousands (k HH) |
| `AccessRatio` | Piped / Total sources | 0-1 scale |
| `AccessLevel` | High (>0.37) or Low (≤0.37) | Factor |

### Geospatial Data

- **File:** `za.json` (South Africa provincial boundaries)
- **Format:** GeoJSON FeatureCollection
- **Source:** [SimpleMaps](https://simplemaps.com) / Natural Earth Data
- **Provinces:** 9 features with `name` and `id` properties

---

## 🤖 Models

### 1. Linear Regression Model (LRM)

**Purpose:** Predict continuous AccessRatio (0-1 scale)

**Formula:**
```
AccessRatio ~ SurveyYear + ImprovedSource + SurfaceWater + OtherUnspecified + Province
```

**Performance:**
- **Adjusted R²:** 0.88
- **RMSE:** 0.046
- **MAE:** 0.039
- **Cross-Validation:** 5-fold (mean accuracy 85-90%)

**Use Cases:**
- Forecasting future water access trends
- Understanding relationships between variables
- Continuous predictions for nuanced analysis

### 2. C5.0 Decision Tree

**Purpose:** Classify provinces as High or Low access

**Configuration:**
- **Target:** AccessLevel (High/Low)
- **Boosting:** 10 trials
- **MinCases:** 2
- **Threshold:** Median AccessRatio (0.37)

**Performance:**
- **Accuracy:** 80-100% (varies by fold)
- **Sensitivity:** High (typically 85-100%)
- **Specificity:** Variable (60-100%)

**Use Cases:**
- Binary policy decisions (intervention vs. monitoring)
- Rule-based interpretability
- Classification of new scenarios

---

## 📖 Dashboard Features

### 1. Dashboard Tab

- Summary statistics (observations, provinces, years, threshold)
- Project overview and team information
- Quick reference cards

### 2. Explore Tab

- **Provincial Bar Chart:** Compare provinces for selected year/metric
- **Temporal Trend Line:** Track one province over time
- **Summary Statistics:** Latest year data + trend direction
- **Data Table:** Browse all 45 province-year records

### 3. Map Tab

- **Interactive Choropleth:** Colour-coded provincial map
- **Metric Selection:** AccessRatio, PipedWater, ImprovedSource, SurfaceWater
- **Hover Tooltips:** Province name + exact values
- **Legend:** Colour scale interpretation

### 4. Forecast Tab

- **Linear Extrapolation:** Project 1-10 years ahead
- **Province Selector:** Choose target province
- **Combined Chart:** Historical (blue) + Forecast (gold)
- **Forecast Table:** Year-by-year predictions
- **Methodology Notes:** Assumptions and limitations

### 5. Predict Tab

- **Scenario Builder:** Input custom water source values
- **Dual Predictions:** LRM (continuous) + C5.0 (classification)
- **Real-Time Output:** Instant predictions on button click
- **Threshold Explanation:** Median-based classification

### 6. Evaluation Tab

- **LRM Metrics:** Adjusted R², RMSE, F-statistic p-value
- **Diagnostic Plot:** Residuals vs. Fitted
- **C5.0 Summary:** Boosting configuration
- **Cross-Validation Note:** Reference to training script

---

## 📝 Documentation

- **[User Guide](https://github.com/user-attachments/files/23722830/SA.Water.Access.Dashboard.Usage.Documentation.Guide.Group.H.pdf)** - Comprehensive feature documentation

---

## 📄 License

**Educational Use Only**

This project was developed for academic purposes as part of BIN381 coursework. The code is provided as-is for educational reference. Data sources (Stats SA GHS) retain their original licenses.

---

## 📧 Contact

**Project Team (Group H):**

- 👤 **Tokelo Ramogase** - [GitHub](https://github.com/Tokelo-Ramogase)
- 👤 **Kaidy Edwards** - [GitHub](https://github.com/username)
- 👤 **Tumiso Lethabo Koee** - [GitHub](https://github.com/Tumi600441)
- 👤 **Paballo Keneilwe Mokadi** - [GitHub](https://github.com/pmokadi)
---

<div align="center">

**Built with ❤️ using R + Shiny**

🇿🇦 **Supporting SDG 6: Clean Water and Sanitation for All** 🇿🇦

*Making water access data accessible for informed decision-making*

</div>

---

**Version:** 1.0  
**Last Updated:** October 2025  
**Status:** ✅ Active & Deployed
