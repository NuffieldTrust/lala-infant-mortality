# Understanding differences in infant mortality rates across local areas

<b>lala-infant-mortality: Local area linked analysis of infant mortality</b>

<b>Project status: complete</b>

## Project description
This project looked at which local area characteristics are associated with infant mortality. It used data at the upper tier local authority level to look at the association between infant mortality rates and local area factors using linear regression models. 

* The final report can be found [here](https://www.nuffieldtrust.org.uk/research/understanding-differences-in-infant-mortality-rates-across-local-areas) 
* The code is available in [`r/`](https://github.com/NuffieldTrust/lala-infant-mortality/tree/main/r)

## Data sources

The infant mortality data was sourced from the [Office for National Statistics](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/childmortalitystatisticschildhoodinfantandperinatalchildhoodinfantandperinatalmortalityinenglandandwales). We calculated the rates using the number of infant deaths / the number of live births per 1,000.
The local area characteristics were sourced from a variety of different places including governement websites, see the [technical annex](https://www.nuffieldtrust.org.uk/sites/default/files/2024-02/Technical_appendix_infant%20mortality_WEB_0.pdf) for details. 
The map shapefile was sourced from [ONS Geoportal](https://geoportal.statistics.gov.uk/maps/counties-and-unitary-authorities-december-2017-ew-bfe).

<i>NOTE: For the number of deliveries to different ethnic groups the Hospital Episode Statistics (© NHS Digital 2022) were used.</i>

## Requirements
The code was written in R using version 3.6.2. The following packages are needed:
* tidyverse
* tidylog
* janitor
* psych
* tidytext
* ggforce 
* sf
* cowplot
* writexl
* ggpubr
* pacman
* fs
* epitools
* plyr
* RColorBrewer
* lm.beta
* lmtest
* ciTools
* MASS
* mctest
* relaimpo
* AER
* DescTools
* rsq

## Usage
* [01_requirements.R](https://github.com/NuffieldTrust/lala-infant-mortality/blob/main/r/01_requirements.R): Set up needed to run all code in the project - <b>run once per session</b>
* [02_load_data.R](https://github.com/NuffieldTrust/lala-infant-mortality/blob/main/r/02_load_data.R): Check for, import and set up datafiles - <b>run once per session</b>
* [03_describe_dependent_national.R](https://github.com/NuffieldTrust/lala-infant-mortality/blob/main/r/03_describe_dependent_national.R): Distribution of infant mortality rates geographically and over time
* [04_describe_dependent_local.R](https://github.com/NuffieldTrust/lala-infant-mortality/blob/main/r/04_describe_dependent_local.R): Distribution of infant mortality rates by local authority and over time
* [05_describe_independent_local.R](https://github.com/NuffieldTrust/lala-infant-mortality/blob/main/r/05_describe_independent_local.R): Summarise independent variables at local authority level
* [06_correlation_dependent_independent.R](https://github.com/NuffieldTrust/lala-infant-mortality/blob/main/r/06_correlation_dependent_independent.R): Check the relationship between infant mortality rates and independent variables
* [07_simple_glm_regression.R](https://github.com/NuffieldTrust/lala-infant-mortality/blob/main/r/07_simple_glm_regression.R): Unadjusted regression models of each independent variable indivdually with infant mortality rates
* [08_blocks_multiple_glm_regression.R](https://github.com/NuffieldTrust/lala-infant-mortality/blob/main/r/08_blocks_multiple_glm_regression.R): Adjusted negative binomial regression models based on variable themes and then final model

## Code authors
Eilís Keeble - [Twitter](https://twitter.com/eiliskeeble) - [GitHub](https://github.com/eiliskeeble)

## License
This project is licensed under the [MIT License](https://github.com/NuffieldTrust/lala-obesity-reception/blob/main/LICENSE).

## Suggested citation
Fisher E, Keeble E, Cheung R, Hargreaves D, Wortley E and Elias L (2024) Understanding differences in infant mortality rates across local areas. Research report, Nuffield Trust.
