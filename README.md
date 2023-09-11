
# SCL Data - Data Ecosystem Working Group

[![IDB Logo](https://scldata.iadb.org/assets/iadb-7779368a000004449beca0d4fc6f116cc0617572d549edf2ae491e9a17f63778.png)](https://scldata.iadb.org)

## Welcome to IDB Social Indicators

This repository aims to provide a framework for computing social sector indicators in a clean, organized and repeatable manner. 

## Getting Started

To start contributing, first clone this repository. Then, we primarily work with two branches: 1) `main` and 2) `development`.

The `development` branch is the most current one. When contributing, pull from this branch and create a new personal branch for the specific task you are working on. For instance, if you are reviewing geographical disaggregations, create a branch named "geographic_dis_scl".

Work on the assigned tasks, commit and push the changes to the `development` branch. Make a pull request so that another team member can review it and accept it as the new version of `development`.

## Calculating indicators

To calculate indicators, create the folder Outputs and then there are two ways to calculate indicators depending on the amount of countries and years required:
1. **Specific country, source of information and year**: Open the `runningScript.R`, and modify the variables *pais*, *year*, *type* and *geoLevel*.
   - *pais*: To specify this use the ISO Alpha-3 code for the specific country (e.g. SLV for El Salvador) and for the second one add as a string the required time for the indicator.
   - *year*: Specify as a string the required time for the indicator
   - *tipo*: Specify one of the two:"censos" or "encuestas" as strings depending if census or surveys are required. 
   - *geoLevel1*: For census, specify if the indicators required at a country level or geolev1 indicating the first geographical disaggregation for the country. 
3. **Loop through different countries and years**: Open the `runningScript_loop.R` and define the variable tipo with "encuestas" or "censos" depending on the data source type required. Then the code would be executed based on all the census or surveys available according to the `Inputs/running_survey.csv` or `Inputs/running_census.csv` files. 

## Repository Structure 

This repository consists of three main parts.

1. **Intermediate Variables**: One script per division, each containing all necessary variables for computing the indicators of the corresponding division (`var_EDU.R`, `var_GDI.R`, `var_LMK.R` and `var_SOC.R`)

3. **Indicator Definitions (`idef.csv`)**: This file controls the computation of indicators. It contains the definition of each indicator.

4. **Running Scripts**: `scl_indicators.R` runs the function. `runningScript_loop.R` runs `scl_indicators.R` for a batch of countries

5. **Functions**: These scripts stablish the functions required to calculate indicators.
   - `functions.R`: contains the functions to calculate indicators for ratios (scl_pct), means (scl_mean) and gini (scl_gini). As well as a function to execute all indicators in `idef.csv` (calculate_indicators) and a function to delimit the disagregations to execute (evaluatingFilter). 
   - `directory_periods.R`: This scrips based on the type of data source survey (encuesta) or census(censos) defined in variable *tipo* returns the appropiate harmonized.
  
6. **Inputs folder** This folder includes important supporting documents such as excels showing the data available (Planeación - Armonización de Encuestas de Hogares.xlsx, Planeación - Population and Housing Censuses.xlsx, running_census.csv and running_survey.csv), dictionary of variable for both census and surveys (D.1.1.4 Diccionario microdatos encuestas de hogares.xlsx and D.7.1.3 Diccionario variables censos.xlsx) and identification files (idef.csv and idefCensos.csv) defining the indicators to calculate. 

## How to Contribute

1. Fork the repository to your GitHub account.

2. Clone the forked repository to your local machine.

3. Create a new branch for your tasks.

4. Make your changes and commit them to your branch.

5. Push the branch to your GitHub repository.

6. From the GitHub page of your forked repository, open a pull request to the `development` branch of the main repository.

