# Improving Access to Essential Medicines via Decision-Aware Machine Learning

[![Data: Dryad](https://img.shields.io/badge/Data-Dryad-0B7285)](https://doi.org/10.5061/dryad.h9w0vt4tw)
[![Python](https://img.shields.io/badge/Python-3.9-3776AB?logo=python&logoColor=white)](#required-software)
[![R](https://img.shields.io/badge/R-4.2.1-276DC3?logo=r&logoColor=white)](#required-software)

This repository contains the code necessary to reproduce the main analysis, robustness checks, and figures for the paper "Improving Access to Essential Medicines via Decision-Aware Machine Learning." 

<sub>**Expected runtime:** Each analysis script should run in **5–10 minutes** on a standard laptop.</sub>

---
## Citation
If you use this repository, please cite the paper:

```bibtex
@article{ChungetalAllocMedSL,
  title   = {Improving Access to Essential Medicines via Decision-Aware Machine Learning},
  author  = {},
  journal = {},
  year    = {YYYY},
  doi     = {DOI}
}
```
---

## Table of contents
- [Required software](#required-software)
- [Data access](#data-access)
- [Data required to run the code](#data-required-to-run-the-code)
- [Quickstart](#quickstart)
- [Reproducing results](#reproducing-results)
  - [1. MainAnalysis.R](#1-mainanlysisr)
  - [2. Figures.R](#2-figuresr)
  - [3. RobustnessAnalysis.R](#3-robustnessanalysisr)
  - [4. Event_Study.do](#4-event_studydo)
  - [5. LATEIV.do](#5-lateivdo)
- [Mapbox token (Figure 2 map)](#mapbox-token-figure-2-map)
- [Notes on restricted data](#notes-on-restricted-data-worldcitiescsv)

---

## Required software
- **Python 3.9**
- **R 4.2.1**
- **Stata** (to run `.do` files)

---

## Data access
All data can be accessed here (Dryad):
- https://doi.org/10.5061/dryad.h9w0vt4tw

Detailed data descriptions and data dictionaries are also available on Dryad link above.

---

## Data required to run the code

- **S1.csv:** List of public health facilities.
- **S2.csv:** Consumption and monthly facility-level stock balance data from DHIS2, covering the products we allocated.
- **S4_AlternativeData.csv:** Consumption and monthly facility-level stock balance data from DHIS2, covering control products to run alternative control analysis.
- **S5_dfImp_popbased.csv:** Population based demand for each facility across products, which will be used to run population-based imputation analysis.
- **worldcities.csv:** List of cities and corresponding latitude & longitude in Sierra Leone, which is used to run treatment impact by rural/urban. **Note:** this dataset was purchased so we do not release publicly. You can purchase from here: https://simplemaps.com/data/world-cities
- **mainData.csv:** Processed based on S1 & S2; can be obtained from `MainAnalysis.R`. 
- **IVData.csv:** Processed based on S1 & S2; can be obtained from `MainAnalysis.R`. This data is used to run `LATEIV.do` to generate IV results.

---

## Quickstart

1) Install required R packages:
```bash
Rscript InstallPackages.R
```

2) Run main analysis (also generates Stata input datasets):
```bash
Rscript "MainAnalysis.R"
```

3) Generate figures:
```bash
Rscript "Figures.R"
```

4) Run robustness checks:
```bash
Rscript "RobustnessAnalysis.R"
```

5) (Optional) Run Stata scripts (after `MainAnalysis.R`):
```bash
stata -b do "Event_Study.do"
stata -b do "LATEIV.do"
```

---

## Scripts and expected outputs

### 1. MainAnalysis.R
`MainAnalysis.R` includes:

- **SynthDiD Main Analysis**
  - Expected result: point estimate, se, and confidence interval.
- **Processed and output data to run STATA code file**
  - Output `~/data/mainData.csv` to run `Event_Study.do`
  - Output `~/data/IVData.csv` to run `LATEIV.do`

---

### 2. Figures.R
`Figures.R` includes:

- **Figure 2:** Map of treatment distribution in 2023 Q2.  
- **Figure 3 (a):** Average normalized consumption time trends based on raw data.  
- **Figure 3 (b):** Average normalized consumption time trends based on SynthDiD main result.  

---

### 3. RobustnessAnalysis.R
`RobustnessAnalysis.R` includes  
(**all the following should expect results of point estimates, se, and confidence interval**):

- Difference-in-differences (Supplement 2.6.1)  
- Substitution Analysis (Supplement 2.6.5)  
- Missing Analysis (Supplement 2.6.4)  
  - Missing as outcome  
  - Only retain products without imbalanced missing  
- SynthDiD: Use alternative control (staggered) (Supplement 2.6.6)  
- SynthDiD: Stockout as outcome (Supplement 2.6.7)  
- SynthDiD Treatment impact by facility type (Supplement 2.5)  
- SynthDiD Treatment impact by rural/urban (Supplement 2.5)  
- SynthDiD: Imputation (Avg Consumption) (Supplement 2.6.3)  
- SynthDiD: Imputation (Population Based) (Supplement 2.6.3)  
- SynthDiD: Imputation (Low Rank) (Supplement 2.6.3)  

---

### 4. Event_Study.do
`Event_Study.do` includes (requires `~/data/mainData.csv`):

- SynthDiD event study plot (Supplement 2.2)  
- Standard DiD event study plot (Supplement 2.6.1)  

---

### 5. LATEIV.do
`Deployment Evaluation Code/LATEIV.do` includes (requires `~/data/IVData.csv`):

- Complier analysis (LATE IV) (Supplement 2.4)

---

## Mapbox token
Obtain Mapbox token to run the code of producing Figure 2 map: https://docs.mapbox.com/api/accounts/tokens/

---

## Notes on restricted data 
`worldcities.csv` is a purchased dataset and is not released publicly. You can purchase it here:
- https://simplemaps.com/data/world-cities




