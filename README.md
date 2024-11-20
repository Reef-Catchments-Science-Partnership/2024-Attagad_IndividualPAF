![RCSP LOGO_Wide 2 (1)](https://github.com/user-attachments/assets/eb142ac1-314c-4200-985d-612b2d25efb9)


# Assessing long temporal trends of time-series Potentially Affected Fraction (PAF) on the Great Barrier Reef rivers
## Project Overview

**Owner/s:**  and Ryan Turner  
**Project Name:** Assessing long temporal trends of time-series Potentially Affected Fraction (PAF) on the Great Barrier Reef rivers

**Date Started:** 19/02/2024  
**Date Last Updated:** 19/11/2024 

### Project Description
The thesis aim is to analyse and evaluate statistical trends in water quality time-series PAF sampled in GBR waterways. Three objectives are to: 

1) Calculate time-series PAF for pesticide samples collected for up to 12 years from 13 monitoring sites, 

2) Statistically analyse temporal trends of time-series PAF by implementing Change Point analysis (CPA) and Seasonal Mann-Kendall analysis (SMK), and 

3) Identify and explore any significant trends at those sites with possible contributing factors. 

## Table of Contents

1. Data
2. Scripts
3. Outputs
4. Credits and Acknowledgements
5. Citation
6. Acknowledgments
7. Contact Details

## 1. Data
There are four main codes with one additional code used in this project. The main codes are used to clean data, convert LORs to relative LORs, core PRM to output time-series PAF of each group of pesticide mixtures (i.e., total pesticides, PSII herbicides, other herbicides, and insecticides), and change point analysis for each monitoring site in the project (13 sites). The additional code is used to visualize overall trends through boxplot analysis.  

**Raw Data From Tahbil**
   - Each excel file contains the sheets of raw data, focused year (from 2011/12 to 2022/23), working on, 22 pesticides and 18 pesticides. The raw data are extracted from the Tahbil Water Data Portal and then clipped to focused year from 2011/12 to 2022/23. Each site has different monitoring years variously from eight to 12 years due to data availability.
   - The working on sheet demonstrates how to be excelling data. 22 pesticides are focused on the project, consequently extracted in csv and further worked with script 1. Clean NA.R 2. Convert LORs to Relative LORs.R 3. Core PRM code IA.R in following.  
     
**msPAF22**
- Output from `3. Core PRM code IA.R` and also used as input for `4. Boxplot and xy of PRM_HK (additional).R`. The data here, particularly total pesticides, are further adjusted to work with CPA.
- Note: Daily.Ave.PAF refers to time-series PAF of each type of pesticide mixtures.

**CSV for CPA**
- All files here are used in Change Point Analysis Scripts to output change points of each site. 

## 2. Scripts
**Clean NA.R**  
- Work with Raw Data From Tabil, extract the sheet of 22 pesticides in csv to clean up data 

**Convert LORs to Relative LORs.R** 
- Cleanup data from 1. Clean.NA.R are used to convert LORs to relative LORs  

**Core PRM code_IA.R** 
- The relative LORs data are used with the script to output time-series PAF data of each type of pesticide mixtures 

**Boxplot and xy of PRM_HK (additional).R**
- Use to visualize the trends of each group of pesticide mixtures, working with msPAF22 data  

**Change Point Analysis_CN_0.9_2.R** 
- Change point analysis for Sandy Creek, core code developed  


## 3. Outputs

**msPAF22**
Outputs from 3. Core PRM code_IA.R, derived from Raw Data From Tahbil  
- msPAF22_Insecticides_Barratta.csv -> msPAF22_Insecticides_Tully.csv
- msPAF22_Other_Herbicides_Barratta.csv -> msPAF22_Other_Herbicides_Tully.csv
- msPAF22__PSII_Barratta.csv -> msPAF22__PSII_Tully.csv
- msPAF22_Total _Barratta.csv -> msPAF22_Total_Tully.csv 



## 4. References

1. Bezzina, A., Neelamraju, C., Strauss, J., Kaminski, H., Roberts, C., Glen, J., & Dias, F. (2022). *CatchThemAll.PRM: Pesticide Risk Metric Calculations. R package*. Water Quality Monitoring & Investigations, Department of Environment and Science, Queensland Government. GitHub Repository. https://github.com/AlexWaterboyBezzina/CatchThemAll.PRM

2. Warne, M., Neelamraju, C., Strauss, J., Smith, R., Turner, R., & Mann, R. (2020). *Development of a Method for Estimating the Toxicity of Pesticide Mixtures and a Pesticide Risk Baseline for the Reef 2050 Water Quality Improvement Plan*. https://doi.org/10.13140/RG.2.2.24439.55202

3. Water Quality & Investigations. (2024a). *Tahbil - Water Quality Data Portal*. Queensland Government, Brisbane. Water Data Portal. https://apps.des.qld.gov.au/water-data-portal

4. Water Quality & Investigations. (2023). *Pesticide Risk Metric Dashboard*. Department of Environment and Science, Brisbane, Australia. PRM Dashboard. https://prmdashboard.des.qld.gov.au/

5. Water Quality & Investigations. (2023). *Pesticide Risk Metric Calculator*. Department of Environment and Science, Brisbane, Australia. PRM Calculator. https://prmcalculator.des.qld.gov.au/


## 5. Acknowledgements

Catherine Neeramjaru and Hayley Kaminski for their contributions on adapting the msPAF and CPA scripts for this analysis


## 6. Citation

Reef Catchments Science Partnership. (2024). *Attagad Pesticide Risk Prediction Sandy Creek*. GitHub. https://github.com/Reef-Catchments-Science-Partnership/2024_Attagad_IndividualPAF/


## 7. Contact Details

Please contact rcsp.info@uq.edu.au for any inquiries related to this repository.
