# Project RYSE - Elite LoL Performance Analytics

**Repository:** [github.com/Guille1799/ryse-publico](https://github.com/Guille1799/ryse-publico)  
**Live app (v2, English):** [ryse-lol-v2](https://guillermomartindeoliva.shinyapps.io/ryse-lol-v2/)  
*Revised build: interface translated to English and dashboard improvements over the original thesis submission.*

> **What this repository is:** the earlier **monolithic** build of the dashboard — a single
> `app.R` with a Spanish interface. The live app linked above is deployed from a separate,
> newer codebase that is not in this repository, so cloning and running this repo will not
> reproduce what the link shows.

End-to-end **R** analytics: ETL, clustering, supervised learning and a **Shiny** dashboard. Built for a postgraduate master's in **Behavioural Data Science**. It was a group thesis of five people, and I led the technical work.

The domain is **elite League of Legends** performance. The methods are not: find heterogeneous groups, model an outcome, then check what the model actually learned. Averages hide who needs different support, so the same three steps carry over to behavioural and social-impact work.

## Overview

Project RYSE analyzes behavioral and performance patterns in high-elo players (Master, Grandmaster, Challenger) using Riot Games data and an end-to-end CRISP-DM workflow.

The project combines:
- data cleaning and feature engineering,
- clustering by role,
- predictive modeling and interpretability,
- and an interactive dashboard for exploration.

## Core scope

- Build a clean analytical dataset from match-level records.
- Create performance KPIs for player profiling.
- Identify role-specific archetypes with unsupervised learning.
- Estimate drivers of win probability.
- Provide a practical scouting and comparison interface through Shiny.

## Methodology highlights

- Framework: CRISP-DM (full pipeline from data prep to interpretation).
- Data source: Riot Games API (processed and consolidated in `data/`).
- Filters:
  - elite tiers only: Master / Grandmaster / Challenger
  - invalid roles removed
  - short matches excluded
- Feature engineering:
  - `kda_ajustado`
  - economy and vision rates per minute
  - objective-related metrics
  - `oci` (Objective Control Index)
- Modeling and analysis:
  - K-Means clustering (role-wise profiles)
  - Random Forest for key victory factors
  - ALE plots for interpretability (the app explains why PDPs mislead when KPIs are correlated)
  - consistency metrics (coefficient of variation)

## Dashboard sections

The app includes multiple analysis tabs such as:
- General overview KPIs
- Role and cluster profiles
- Correlation analysis
- Key victory factors and variable impact
- Player consistency diagnostics
- Individual player analysis
- Executive report and key findings

## Tech stack

- R
- Shiny
- tidyverse
- ranger
- cluster
- pROC
- ggcorrplot
- iml

## Repository structure

```text
.
|-- app.R
|-- data/
|   `-- ryse_database.csv
`-- README.md
```

## Run locally

From the repository root (the app reads `data/ryse_database.csv` relative to it):

```r
install.packages(c(
  "shiny", "tidyverse", "janitor", "scales", "htmltools",
  "ranger", "pROC", "ggcorrplot", "iml"
))

shiny::runApp("app.R")
```

Package versions are **not** pinned — there is no `renv.lock` in this repository, so a fresh
install takes whatever CRAN currently serves.

## Notes

- This public repository is focused on analysis and portfolio presentation.
- `HEAD` no longer tracks the early-stage artifacts, the RStudio caches or the thesis PDF. Git history still holds them. A full clone therefore downloads about 20 MB, not the 4 MB the current files weigh.
- `git clone --depth 1` skips that history if you only want the code and the dataset.

## Authorship

This was a **group postgraduate master's thesis — a team of five**. I led the technical side: the data pipeline
(ETL and de-duplication), the clustering and modelling, and the Shiny dashboard. The rest of the
team covered the research framing and the written report. See the *Contributors* tab for everyone
who committed to this repository.

Guillermo Martín de Oliva  
LinkedIn: [guillermo-martin-de-oliva](https://www.linkedin.com/in/guillermo-martin-de-oliva/)
