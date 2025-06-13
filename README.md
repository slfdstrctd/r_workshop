# R Workshop: Statistical Analysis of Music Data

## Overview
This project is an R-based statistical analysis of a music dataset, focusing on features such as key, acousticness, and other audio characteristics. The analysis includes descriptive statistics, hypothesis testing, and visualization, and is designed for educational purposes in an R Workshop course.

## Project Structure
```
r_workshop/
├── main.R                 # Main R script with all analysis steps
├── README.md              # This file
├── data/                  # Data files
│   ├── data.csv          # Main dataset (large, not included)
│   ├── s1.csv            # Sample 1 (100 observations)
│   └── s2.csv            # Sample 2 (400 observations)
├── images/                # Generated plots
│   ├── 1.jpg             # ECDF of key with cumulative distribution
│   ├── 2.jpg             # Histogram of acousticness
│   ├── 3.jpg             # Histogram of key with normal curve
│   ├── 4.jpg             # ECDF of acousticness with uniform fit
│   └── 5.jpg             # ECDF comparison with Kolmogorov-Smirnov
├── output/                # Generated output files
│   └── rplot.pdf         # PDF containing R plot outputs
└── docs/                  # Documentation
    └── README.md         # Detailed project documentation
```

## Requirements
- R (version 4.0 or higher recommended)
- R packages:
  - `PerformanceAnalytics`
  - `samplingbook`
  - `EnvStats`

Install required packages in R:
```R
install.packages(c("PerformanceAnalytics", "samplingbook", "EnvStats"))
```

## Usage
1. Ensure all data files are in the `data/` directory
2. Run the main script in R:
   ```R
   source('main.R')
   ```
3. The script will:
   - Output summary statistics to the console
   - Generate plots in the `images/` directory
   - Create additional output files in the `output/` directory

## Analysis Steps
- **Descriptive Statistics:** Distribution, mean, variance, skewness, and kurtosis for the `key` variable
- **Empirical Distribution Function:** Plots ECDF for `key` and `acousticness`
- **Histograms:** Visualizes distributions of `acousticness` and `key`
- **Confidence Intervals:** For mean and variance using normal and t-distributions
- **Sample Size Estimation:** Calculates required sample size for given error margins
- **Normality and Goodness-of-Fit Tests:** Pearson's chi-squared, Kolmogorov-Smirnov tests
- **Hypothesis Testing:** Tests for mean and variance, and comparison between samples

## Outputs
- **Console Output:** Summary statistics, confidence intervals, test results
- **Images:** Statistical plots saved as JPEG files in `images/` directory
- **PDF Output:** Additional R plot outputs in `output/` directory