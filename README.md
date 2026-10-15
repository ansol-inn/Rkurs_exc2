# Digital versus On-Site Resistance Exercise in Cancer Survivors

A reproducible R and Quarto project demonstrating data management, statistical analysis, visualization, and collaborative scientific coding using Git and GitHub.

## Project Overview

This repository contains a simulated exercise oncology dataset designed for learning reproducible research workflows.

The project compares:

- Digital home-based resistance exercise
- Supervised on-site resistance exercise

in cancer survivors following treatment.

The repository is intended for a R-course with focus on:

- Git and GitHub
- Version control
- Reproducible scientific workflows
- Collaborative coding
- Quarto reporting
- R-based data analysis and visualization

The datasets are simulated and do not contain any real patient information.

---

## Repository Structure

```text
├── README.md
├── report.qmd
├── references.bib
├── apa.csl
├── .gitignore
│
├── data/
│   ├── participants.csv
│   ├── measurements.csv
│   └── training_diary.csv
│
├── R/
│   ├── 01_generate_data.R
│   ├── 02_analysis_and_figures.R
│   └── functions.R
│
├── figures/
│
└── output/
```

---

## Datasets

### 1. Participants

Participant-level information.

Variables include:

- `fp` : participant ID
- `group` : digital or onsite
- `city`
- `age`
- `sex`
- `dropout`
- `cancer_treat`
- `cancer_diag_group`

There are 150 participants.

---

### 2. Measurements

Pre- and post-intervention outcomes.

Variables include:

- `fp`
- `test` (pre/post)
- `test_type`
- `test_name`
- `test_value`

Outcomes:

- Push-ups
- 6-minute step test
- Leg press strength
- Glucose
- PHQ-9

---

### 3. Training Diary

Session-level exercise data collected during the 12-week intervention.

Variables include:

- `fp`
- `week`
- `session`
- `exercise`
- `sets_max`
- `sets_completed`
- `reps_max`
- `reps_completed`
- adherence-related variables

The intervention consists of:

- 12 weeks
- 24 sessions
- 8 exercises per session

---

## Analyses Included

The report includes:

### Descriptive Statistics

- Participant characteristics
- Baseline comparisons

### Exercise Adherence

- Group comparisons
- Adherence visualization

### Physical Performance

- Push-up performance
- Leg press performance
- 6-minute step test

### Mental Health

- PHQ-9 analysis

### Statistics

- Independent t-tests
- Chi-square tests
- ANCOVA
- Linear mixed models
- Linear regression
- Logistic regression

### Visualizations

- Boxplots
- Violin plots
- Spaghetti plots
- Histograms
- Scatterplots with regression lines

---

## Reproducing the Analysis

### 1. Generate Data

Run:

```r
source("R/01_generate_data.R")
```

This creates:

```text
participants.csv
measurements.csv
training_diary.csv
```

in the `data/` folder.

### 2. Run Analyses

```r
source("R/02_analysis_and_figures.R")
```

This script:

- Performs statistical analyses
- Produces figures
- Creates summary tables

### 3. Render the Report

From R:

```r
quarto::quarto_render("report.qmd")
```

or from the terminal:

```bash
quarto render report.qmd
```

---

## Required Packages

```r
tidyverse
lme4
lmerTest
broom
```

Install if needed:

```r
install.packages(
  c(
    "tidyverse",
    "lme4",
    "lmerTest",
    "broom"
  )
)
```

---

## Learning Objectives

This repository demonstrates:

- Reproducible data analysis
- Organization of scientific projects
- Use of Git for version control
- Collaborative workflows with GitHub
- Statistical programming in R
- Dynamic reporting with Quarto

---

## Disclaimer

All data in this repository are simulated for educational purposes only.

The dataset was created to mimic a randomized exercise intervention study in cancer survivors and should not be interpreted as real clinical data.

---

## Author

Anna Solheim

Course: HEV9009
