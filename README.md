# Replication Materials

This repository contains code and data for replicating the main findings in a  
forthcoming paper on exposure and engagement with partisan and unreliable  
news on Google Search.

To run the code available here, you will first need to download all of the code  
and data, which is available at: https://doi.org/10.7910/DVN/WANAX3  

<br>

---
## Datasets

User-level aggregated data for replicating our main findings are available in  
the `data` directory. Some columns have been removed to protect participant   
privacy. These datasets contain  merged columns from all datatypes -- exposure,  
follows, and overall engagement -- and have prefixes to delineate among them.  
Please see the Methods section of the paper for additional details and context  
on each measure. 

`data/users2018.csv`  
- Provides user-level aggregated data for participants from our 2018 study wave.  
Each row represents a participant and has a unique identifier in the `caseid`  
column that we've replaced with an autoincrement integer value in this dataset.  
The column prefixes for distinguishing datatypes are `search_` for exposure,  
`follow_` for follows, and `browse_` for overall engagement. A secondary measure  
of overall engagement is provided in columns prefixed with `history_`, representing  
participants' complete Google History.

`data/users2020.csv`  
- Provides user-level aggregated data for participants from our 2020 study wave.  
Each row represents a participant and has a unique identifier in the `user_id`  
column that we've replaced with an autoincrement integer value in this dataset.  
The column prefixes for distinguishing datatypes are `activity_gs_search_` for  
exposure, `activity_gs_follow_` for follows, and `browser_history_` for overall  
engagement.  A secondary measure of overall engagement is provided in columns  
prefixed with `activity_`, representing participants' complete Tab Activity.  

`data/coefficients.csv`  
- Provides regression coefficients, 95% CIs, *t* values, and *P*-values for  
the main regression analysis. Produced in `regressions/run_analysis.R` and  
used in `figure_coefficients.ipynb` and `table_coefficients.ipynb`.  

<br>

---
## Jupyter Notebooks

The descriptive analysis was done primarly in jupyter notebooks, which we list  
below along with brief descriptions. These notebooks import shared utility  
functions from `functions.py` for reformatting data, adjusting plots, and  
calculating statistics.  

`main_results.ipynb`:  
- This notebook contains descriptive analyses for 2018 and 2020 data. It creates  
the figures that appear in the main manuscript (excluding the diagram in Figure 1),  
the figures and tables that appear in Extended Data, and the tables that appear in  
Supplementary Information.  

`figure_coefficients.ipynb`:  
- This notebook loads, reshapes, and plots regression coefficients and CIs produced  
in `run_analysis.R`.  

`table_coefficients.ipynb`:  
- This notebook loads and reshapes the regression coefficients, CIs, t-values,  
and P-values produced in `run_analysis.R`. It outputs a latex tables of  
formatted regression results that we further edited by hand to produce  
Extended Data Tables 4-7.  

`figure_individual_level.ipynb`:  
- This notebook loads, reshapes, and plots participant-level distributions of  
partisan news exposure, follows, and engagement. The data needed to run this  
file are not publicly available because only aggregated data may be released.  

<br>

---
## Regressions

The regression analysis was done using the R scripts in `regressions/`. Below  
we list each script with a brief description.

`regressions/run_analysis.R`  
- Run regression models and produce associated output  

`regressions/helper_functions.R`  
- Helper functions for regression modeling and organizing output  

<br>

---
## Search Queries - Pivoted Text Scaling

We used pivoted text scaling to identify features in our participants' search  
queries using the R scripts in `pivot_scores/`. We do not provide the text  
data needed to regenerate these scores. Below we list each script with 
a brief description.

`pivot_scores/make_parrot_scores.R`  
- Creates pivoted text scaling scores from participants' search queries  

`pivot_scores/parrot_functions.R`  
- Pivoted text scaling helper functions and pipeline  

