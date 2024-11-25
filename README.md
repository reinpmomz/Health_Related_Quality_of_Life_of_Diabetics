# Health_Related_Quality_of_Life_of_Diabetics
Determinants of health-related Quality of life (HRQOL) among Kenyan population with diabetes in Korogocho, Viwandani and Dandora. 

## Summary



## Setup

We are assuming you have `R Software` and `Rstudio IDE` installed. If not you can download and install [**R software**](https://www.r-project.org/) then followed by [**RStudio/Posit IDE**](https://posit.co/download/rstudio-desktop/).

## Data

The data used for analysis can be accessed through [**APHRC Microdata portal**](https://microdataportal.aphrc.org/index.php/catalog/75) upon proper approvals.


## Materials

- The `wdf_baseline_recode_file.xlsx` file contains: 
    
    1. MetaData about study in the _study_title_ sheet.
    
    2. Data dictionary of the data _rename_vars_ sheet.
   
    3. _selected_vars_ sheet that contains original and new created variables during data cleaning. Guide on variables to be          analyzed and visualized.
    
    4. _drop_selected_vars_ sheet for dropped variables that won't be used in modelling.
    
    5. _model_params_ sheet with set parameters for machine learning.
    
    6. _model_names_ sheet with various regression models used.
    
    7. _data_names_train_ sheet with the analysis name to label our outcome variable. 
   
## Run

After cloning the repository or downloading the ZIP, you also need the data files (**Data used for analysis**) in the _data_ sub-folder of _Health_Related_Quality_of_Life_of_Diabetics_ folder.

Open `Rstudio` then set your working directory to the _Health_Related_Quality_of_Life_of_Diabetics_ folder. 

- Copy the below code to run all files at once in Rstudio

```
source("main.R")

```
- To run individual files, open the `main.R` script, and run from the beginning.

