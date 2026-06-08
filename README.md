# Green_Nest_Material

Welcome to the Green Nest Material project repository! 
This study has been pre-registered here: <https://doi.org/10.17605/OSF.IO/S7J6Z>

This repository contains material related to the study: 

## Why do birds use green nest material? A systematic review and meta-analysis of experiments

Many animals build nests. As external structures that can influence survival and reproduction, nests are often considered extended phenotypes. Birds are key examples of nest builders, and some species add green plant material to their nests. Yet, the adaptive value of this behaviour remains debated. Non-mutually exclusive hypotheses propose roles in courtship signalling, parasite defence, and enhancement of offspring condition through pharmacological effects independent of parasite reduction. Here, we conducted a pre-registered systematic review and meta-analysis of 28 experimental studies (26 published, 2 unpublished), spanning seven bird species and 274 effect sizes, to test whether green nest material enhances fitness and to evaluate competing functional explanations. Our meta-analysis shows that green nest material can increase fitness; however, this effect varied depending on the fitness proxy investigated, being strongest for morphological proxies. We found no compelling evidence to preferentially support the courtship, nest protection, or drug hypothesis. Nonetheless, experimental design (i.e., treatment–control comparison type) was the moderator explaining most effect size variation, challenging the traditionally held role of aromatic compounds in the fitness benefits of green nest material. Our synthesis provides evidence for the adaptive significance of green nest material and highlights the need for further research into the underlying mechanisms.


- Explicitly point out that package versions are commented inside each of the respective code files at the end.
- Following acceptance, author/contact details, publication links, and the repository citation/DOI in both the Data Statement and reference list of the main manuscript. Please also ensure the repository is public; your GitHub repository already is.


`Green_Nest_Material.Rproj` This is an R Project file for our project. We recommend using this .Rproj file after forking/downloading the repository to access the folders and scripts easily if you are using RStudio. In case of Visual Studio or Positron IDE, please open the entire folder. It sets the working directory correctly and makes folder paths in R script more accessible.

### CODE USED IN THE STUDY

- A brief list of instructions for users to run the code (e.g., explain the project workflow),

`code/` This folder contains all the R scripts associated with this project, named in chronological order (eg. 00\_...,01\_..). The later stages of data cleaning and analysis are done using .Rmd files and their .html version is shared where applicable. 

This project maintains the actual workflow of the systematic review and meta-analysis as was used in the process of conducting the study. 
 
(code/00_search_stratergy_litsearchr_code.R) : This script uses the initial library (of 15 studies) and generates a ..
(code/01_systematic_search_deduplication.R) : Once the search is conducted and all available files ([GNM_wos.bib](data/01_systematic_search/02_reference_data/web_of_science/GNM_wos.bib)and [GNM_scopus.bib](data/01_systematic_search/02_reference_data/scopus/GNM_scopus.bib)) were downloaded, these scripts were run to deduplicate the files and create relevant 
(code/02_abstract_screening.R) :
(code/03_fulltext_and_data_extraction.R) :
(code/04_search_repeat_12082024.R) :
(code/05_data_cleaning.Rmd or code/05_data_cleaning.html) : 
(code/06_data_preparation.Rmd or code/06_data_preparation.html) :
(code/07_data_analysis.Rmd or code/07_data_analysis.html) :
(code/08_figures-tables.Rmd or code/08_figures-tables.html) :

`data/` This folder contains all the data used or generated during this project. It is organized into sub-folders based on project steps:

-   `01_systematic_search/`
    -   `01_search_strategy` contains data related to developing of the search string for the systematic search.
    -   `02_reference_data` contains .bib files downloaded from the Web of Science and Scopus and the final unique_reference_list generated from combining and de-duplicating these files.
    -   `03_title_abstract_screening` contains data files with screening decisions, combined dataset after abstract screening and the selected dataset for subsequent steps.
-   `02_data_extraction/` This folder contains all the files associated with the data extraction for meta-analysis. It contains the different sets for data extraction for the three screeners. It also contains the final extracted data and the combined cleaned dataset for further steps.
-   `03_data_cleaning/`
-   `04_data_analysis/`
-   `05_data_sensitivity_analysis/`

#### Data Description

- A completed overview of the folder/file structure and contents. This would typically include descriptions of all columns/variables in the data files; however, I recognise that you have many data files, each with many columns. Instead, I suggest detailing the variable descriptions for only key data files, such as: “.data/03_data_cleaning/dataset_after_cleaning.csv”.

#### Code Desciption



### License

- licensing information (e.g., CC-BY)

This project is licensed under the \[\] - see the LICENSE.md file for details.

### Authors contribution

### Acknowledgements
