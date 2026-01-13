# UAE Used Cars – Interactive Shiny Dashboard

An interactive Shiny dashboard analyzing used car listings from the United Arab Emirates.  
The app normalizes prices to USD, supports mile/kilometer toggling, and provides dynamic filtering and visual exploration.

## Features
- AED → USD price normalization
- Miles ↔ Kilometers distance toggle
- Outlier trimming for cleaner visuals
- Interactive Plotly charts
- Filtered data download

## How to Run
```r
install.packages(c("shiny","dplyr","plotly","DT","shinydashboard","scales"))
shiny::runApp()
