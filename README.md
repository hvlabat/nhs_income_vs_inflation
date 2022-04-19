# NHS Doctor Income vs Inflation Project
#### By Hugo Labat

## Description
This is my marked project for the Data Analysis and Visualisation module (PSY6422) of my MSc Cognitive Neuroscience and Human Neuroimaging Intercalation at the University of Sheffield. This project's purpose is to compare inflation to the income of National Health Service (NHS) doctors in various stages of training.

The data used in this project comes from two sources:
- The NHS
- The Office for National Statistics (ONS)

The NHS data is from their Staff Earnings Estimates, which includes information
on many aspects of staff pay over many staff roles. The data extracted from this
is the Mean Basic Salary per Full Time Equivalent (FTE), which is one of the
best measures of 'typical' income. The data extracted is monthly salaries.
The groups focused on in this study are:

- Consultants (represents the 'end' of official training)
- Specialist Registrars (First official specialist role; becoming an expert)
- Core Trainees (First step into specialising, a sort of pre-specialising)
- Foundation Doctors Year 2 (Second year of Foundation)
- Foundation Doctors Year 1 (First job after medical school)

These groups were selected since they represent the entirety of the official
training pathways for doctors in the NHS.


The ONS data is from the Consuer Price Index including the costs of hosing (CPIH)
This was chosen since costs of living also include costs of housing, and this
offers a more accurate display of the impact of inflation on income.


## Contents
This project was coded using R. This project contains the following files and folders, listed as seen:

**".gitignore"** - Specifies untracked files that Git should ignore and those it should focus on.

**".Rhistory"** - Contains a history of the commands entered, generally unimportant.

**"codebook.txt"** - File describing the nature of the data found in the **"refined"**" data folder of this project.

**"data"** - Contains the **"raw"**" data used in this project, the source of which is outlined above. The **"refined"**" data folder contains the processed data from this project, described below.

**"docs"** - Contains the scripts used to test the code used in the **"index.Rmd"** file. The file **"007_shiny_script.R"** is the test-code for the main visualisation of this project. However, the main visualisation itself is found in the index file, outlined beflow.

**"index.Rmd" / "index.html"** - The R Markdown file intended to be marked for the assessment of this module. **This contains the main visualisation of the project.**

**"nhs_income_vs_inflation.Rproj"** - The R Project file of this project. Must be run if the scripts in **"docs"** are to work correctly.

**"README.md" / "README.html"** - This file, in different formats.

**"renv"** - A package-management system, used to ensure the isolation, portability, and reproducibility of this project. This is the R Studio updated version of packrat.

**"renv.lock"** - Contains data on the packages stored in renv



## Instructions
If you would like to run the scripts, **please first run the R Project file titled "nhs_income_vs_inflation.Rproj"**

The scripts should ideally be run sequentially, in order of their numbers (i.e., "001_pdf_salary_extraction_script.R" before "002_csv_salary_extraction_script.R"). However, scripts 4 and 7 are designed to be able to run together (in that order) without issue.

The R Shiny script is an interactive app that allows the user to adjust the date with a slider. It enables a more intuitive and hands-on relationship with the data, and the accompanying bar chart gives an idea of the changes occurring per-month.

## Queries
If you have any queries, please direct them to hugolabat@doctors.org.uk

Thanking you kindly.


~
