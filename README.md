# ProtectionCode

# Code for the main analysis of the paper Protection of previous SARS-CoV-2

## Analysis

The code runs the main analysis of the paper for all four outcomes: positive, hospitalization, severe disease, and death. As the original dataset is not available due to its sensitivity, a demo dataset is given.

## Software

The code is an R code and checked on "R version 4.0.2 (2020-06-22)".

The following R packages are needed

* tidyverse

* pander

* broom


##  Running the Code

To run the analyses, use the command
```{r echo=T, results='hide', warning=FALSE,message=FALSE}
source("./main_analysis.R")
```

The code creates a table "df" that holds the confidence intervals for the protection of cohort 2 with respect to cohort 0. Calculating confidence intervals for cohorts 1A, 1B, and Recovered is done similarly. The table can be printed as follows:
```{r}
pander(df)
```
