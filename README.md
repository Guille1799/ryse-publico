# Project RYSE - Elite LoL Performance Analytics

**Repository:** [github.com/Guille1799/ryse-publico](https://github.com/Guille1799/ryse-publico)  
**Live app:** [AnalisisClusterRolyLiga](https://guillermomartindeoliva.shinyapps.io/AnalisisClusterRolyLiga/)

End-to-end **R** analytics (ETL, clustering, supervised learning, **Shiny**) developed for an MSc in **Behavioural Data Science** (group thesis, team of five; I led the technical work). The domain is **elite League of Legends** performance; the methods—heterogeneous groups, predictive modelling, interpretability—transfer to behavioural and social-impact work where averages hide who needs different support.

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
  - ALE/PDP-based interpretability workflow
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
- iml / pdp

## Repository structure

```text
.
|-- app.R
|-- data/
|   |-- ryse_database.csv
|   |-- high_elo_puuids_euw.csv
|   `-- random_sample_test.csv
`-- README.md
```

## Run locally

1) Open the project in RStudio.  
2) Install required packages (if missing).  
3) Run:

```r
shiny::runApp("app.R")
```

## Notes

- This public repository is focused on reproducible analysis and portfolio presentation.
- Some early-stage artifacts and private planning materials are intentionally excluded.

## Authorship

This was a **group MSc thesis — a team of five**. I led the technical side: the data pipeline
(ETL and de-duplication), the clustering and modelling, and the Shiny dashboard. The rest of the
team covered the research framing and the written report. See the *Contributors* tab for everyone
who committed to this repository.

Guillermo Martín de Oliva  
LinkedIn: [guillermo-martin-de-oliva](https://www.linkedin.com/in/guillermo-martin-de-oliva/)
