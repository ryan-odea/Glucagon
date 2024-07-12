# Utilization and Reimbursement of Glucagon Products for Severe Hypoglycemia in Medicaid: 2012-2023
## About
This serves as the code (and some data) repository for 'Utilization and Reimbursement of Glucagon Products for Severe Hypoglycemia in Medicaid: 2012-2023' [LINK AND DOI HERE] authored by Noah M. Feder, Ryan O'Dea, Dr. Margaret Zupa, and Dr. Jing Luo.

## Using the provided Rscript and Data
Within this repository, you will find the R script [glucagon.R](https://github.com/ryan-odea/Glucagon/blob/main/glucagon.R) which will: 
1. Scrape [Medicaid State Drug Utilization Data](https://www.medicaid.gov/medicaid/prescription-drugs/state-drug-utilization-data/index.html)
2. Create [cleaned and aggregated data](https://github.com/ryan-odea/Glucagon/tree/main/Cleaned%20Data)
    1. [Total aggregated table from scrape](https://github.com/ryan-odea/Glucagon/blob/main/Cleaned%20Data/output_raw.csv)
    2. [Market Share](https://github.com/ryan-odea/Glucagon/blob/main/Cleaned%20Data/brandpct.csv) - the aggregated table of brand market share percentages by year
    3. [NDC to Brand Key](https://github.com/ryan-odea/Glucagon/blob/main/Cleaned%20Data/key.csv) - decoder for NDC codes
    3. [Yearly Medicaid Reimbursement](https://github.com/ryan-odea/Glucagon/blob/main/Cleaned%20Data/reimbursements.csv) - yearly monetary reimbursements for medicaid, corrected to 2023 dollars through the [Federal Reserve Bank of Minneapolis CPI](https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1913-)
    4. [Total scripts per year](https://github.com/ryan-odea/Glucagon/blob/main/Cleaned%20Data/total.csv)
3. Plot [figures](https://github.com/ryan-odea/Glucagon/tree/main/Figures)
## To Note:
Data from 2022 was originally scraped in August of 2023. Since then it appears that approximately half of the data has gone missing. In similar fashion, the scraping mechanism does not work with 2023 data due to API changes. We have included a [Data.zip](https://github.com/ryan-odea/Glucagon/blob/main/Data.zip) file which contains both of these datasets for maximum reproducibility. \
The rest of the data was pulled 7/11/2024 and can be accessed upon request to the corresponding author, but the provided script should allow reproducibility for all other years within the study.