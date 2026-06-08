# Data dictionary: dataset_after_cleaning.csv

This file provides short descriptions for variables in `data/03_data_cleaning/dataset_after_cleaning.csv`. This is the file then used to calculate the effect sizes and sampling variances.

Number of unique values and missingness were calculated using the `dataMaid` codebook package.

| Variable | Description | Unique values | Missing |
|----------------|------------------------|--------------:|---------------:|
| `paper_ID` | Unique identifier for each source article or unpublished dataset included after full-text screening. | 28 | 0.00% |
| `fulltext_screening` | Final full-text screening decision for the record; retained rows are included or have missing values when not applicable. NA in this are for the unpublished dataset that did not undergo a fulltext_screening since it is not published record. | 2 | 6.36% |
| `fulltext_notes` | Notes or exclusion reason from full-text screening. In this cleaned dataset this field is empty (NA) because excluded records were removed. | 1 | 100.00% |
| `variable_note` | Additional notes about the extracted outcome value, such as whether it was log-transformed, derived from a figure, or involved shared sample sizes. | 51 | 1.06% |
| `authors` | Complete information of all authors. Each author name to be separated by ;. Example: Dave Shutler and Adam A. Campbell → “Shutler D; Campbell A A”. | 27 | 0.00% |
| `year_publication` | Publication year of the source article. Unpublished datasets have missing values (NA) for publication. | 22 | 6.36% |
| `population_location` | Geographical location of the study population extracted as provided by the authors along with any coordinates provided. If multiple sites are studied in a location, extract them as PopulationName_SiteA. | 27 | 0.00% |
| `Observation_ID` | Unique row-level identifier for each extracted observation or potential effect size. | 283 | 0.00% |
| `experiment_ID` | An identifier given to the estimates coming from the same sub-population within a location. | 5 | 0.00% |
| `group_ID` | An identifier given to the estimates coming from the same group of experimental units (e.g., males and females in a population, first clutch and second clutch). | 7 | 0.00% |
| `repeated_trait_ID` | An identifier given to the repeated measurement of the same trait within the same individuals (e.g., chick mass on day 7, 14, and 21). | 26 | 0.00% |
| `bird_species` | Scientific name of the bird species studied. | 7 | 0.00% |
| `treatment_plant_species` | Plant Species Scientific or common name of the plant species used as the green nest material (treatment group). | 31 | 0.00% |
| `control_plant_species` | Plant Species Scientific name of the plant(s) species used as control treatment(s) in the study. If the scientific name is not given, extract it as reported by the authors. If there is no material used for control, note “blank”. | 13 | 0.00% |
| `comparision_type` | Experimental comparison type used in the study. Experimental design is used to code the comparison between the two groups: 1 = non aromatic (control) vs. aromatic (treatment) 2 = no material (control) vs. aromatic (treatment) 3 = no material (control) vs. non-aromatic (treatment) | 4 | 9.89% |
| `CH` | Binary indicator for whether the outcome was classified as relevant to the courtship hypothesis. | 2 | 0.00% |
| `PCH` | Binary indicator for whether the outcome was classified as relevant to the parental-care/protection hypothesis. | 2 | 0.00% |
| `Hypothesis` | Categorical hypothesis assignment derived from CH and PCH, indicating courtship, parental care/protection, or both. | 3 | 0.00% |
| `measure_central_tendency_experiment` | Extracted mean, percentage, or other central estimate for the experimental group. | 230 | 7.07% |
| `type_measure_central_tendency_experiment` | Type of central tendency measure reported for the experimental group. | 4 | 7.07% |
| `measure_dispersion_experiment` | Extracted measure of dispersion for the experimental group, such as SD or SE. | 211 | 8.13% |
| `type_measure_dispersion_experiment` | Type of dispersion measure reported for the experimental group. | 5 | 8.48% |
| `n_experiment` | Original sample size for the experimental group. | 41 | 0.00% |
| `measure_central_tendency_control` | Extracted mean, percentage, or other central estimate for the control group. | 232 | 7.07% |
| `type_measure_central_tendency_control` | Type of central tendency measure reported for the control group. | 4 | 8.13% |
| `measure_dispersion_control` | Extracted measure of dispersion for the control group, such as SD or SE. | 203 | 8.48% |
| `type_measure_dispersion_control` | Type of dispersion measure reported for the control group. | 5 | 9.89% |
| `n_control` | Original sample size for the control group. | 36 | 0.00% |
| `fitness_proxy` | Original name of the outcome or fitness proxy as extracted from the source study. | 200 | 0.00% |
| `trait_type` | Broad category of the measured outcome, such as reproduction, morphology, physiology, behaviour, phenology, or parasite/pathogen load. | 6 | 0.00% |
| `proxy_decision` | Decision on whether the proxy was included in the main analysis, excluded, or retained for sensitivity analysis only. | 4 | 0.00% |
| `proxy_comment` | Explanation for proxy-level decisions that were made, including exclusions, sensitivity-analysis coding, or zero-effect assumptions. | 27 | 75.97% |
| `proxies_sign` | Direction multiplier sign assigned in relation to fitness: +1 if fitness increases with proxy (e.g., survival rate, body mass). −1 if fitness decreases with proxy (e.g., mortality, parasite load). | 2 | 0.00% |
| `statistics_type` | Type of inferential statistic used to calculate an effect size when means and dispersions were unavailable. | 4 | 95.41% |
| `test_statistics_type` | Specific test statistic (such as f-value, t-values) reported in the source study, where applicable. | 4 | 95.41% |
| `statistics_value` | Numerical value of the reported inferential statistic used for effect-size conversion. | 14 | 95.41% |
| `p_value` | Reported p-value associated with the inferential statistic, where applicable. | 13 | 95.41% |
| `sign_relationship` | Direction assigned to effect sizes converted from inferential statistics. | 2 | 98.59% |
| `total_sample_size` | Total sample size reported for analyses based on inferential statistics or contingency data. | 2 | 98.94% |
| `DF` | Degrees of freedom associated with the reported inferential statistic. | 3 | 98.59% |
| `data_location` | Location of the extracted data in the source, such as table, figure, text, supplement, or raw-data file. | 52 | 7.07% |
| `parasite_type` | Broad category of parasite or pathogen-related outcome, where applicable. Levels: "Arthropod", "Micro-organism" or NA when the fitness proxy did not relate to parasite/pathogen load | 3 | 69.61% |
| `time_of_gnm_addition` | Timing of green nest material addition during the nesting cycle. levels: continously through nesting phase (c), before egg hatching (b) or after egg hatching (a) | 3 | 0.00% |
| `extractor_ID` | Identifier of the person who extracted the data. | 5 | 0.00% |
| `extractor_comments` | Comments recorded during data extraction, including uncertainties or extraction decisions. | 69 | 50.53% |
| `data_checker_ID` | Identifier of the person who checked the extracted data. | 5 | 11.31% |
| `data_checker_comments` | Comments recorded during data checking or reconciliation. | 28 | 63.96% |
| `blinding` | Risk-of-bias variable indicating whether blinding was conducted — yes (y) or no (n). | 2 | 0.00% |
| `random_assignment` | Risk-of-bias variable indicating whether nests were assigned to treatment or control randomly — yes (y) or no (n). | 2 | 0.00% |
| `missing_data` | Risk-of-bias variable indicating whether some data was incompletely reported/ missing (y) or not (n) | 2 | 0.00% |
| `shared_experiment` | Number of times the same experimental group was reused across comparisons in the same study. Used to identify non-independence between shared treatment groups. Value = 1 if individuals were compared only once as a treatment group. | 2 | 0.00% |
| `shared_control` | Number of times the same control group was reused across comparisons in the same study. Used to identify non-independence between shared control groups. Value = 1 if individuals were compared only once as a control group. | 2 | 0.00% |
| `fitness_proxy_cleaned` | Standardized outcome (fitness proxy) name assigned during data cleaning for analysis across studies. | 70 | 0.00% |
