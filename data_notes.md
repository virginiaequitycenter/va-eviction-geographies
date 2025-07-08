---
title: "data_notes"
output: html_document
---
#### Data Sources

The data on eviction filings are taken from [Virginia's General District Court Online Case Information System](https://eapps.courts.state.va.us/gdcourts/landing.do?landing=landing) and represent all Unlawful Detainer (eviction) cases filed in the state's Civil Courts. 
Data are provided by the Civil Court Data Initiative ([CCDI](https://www.lsc.gov/initiatives/civil-court-data-initiative)) through a partnership with the [Legal Services Corporation](https://www.lsc.gov/). 
The data presented here includes only eviction cases filed against *residential* defendants (tenants); cases filed against commercial tenants have been removed. In addition, the data here is organized around eviction filers; tenants who are defendants in eviction cases are not identified. 
Plaintiff names are cleaned and standardized by the Legal Services Corporation using their [CleanCourt](https://pypi.org/project/cleancourt/) Python library.

The data on residential characteristics are taken from the 5-year 2019-2023 [American Community Survey](https://www.census.gov/data/developers/data-sets/acs-5year.html), and is accessed using the [tidycensus](https://walker-data.com/tidycensus/) R package.

#### Variable Definitions

The summary data is grouped by **geography** and **timeframe**. The geographical groupings range from zip code, to county, to legal aid service area, and the timeframes range from pre-COVID (2018-2019), to during COVID (2020-2021), to post-COVID (2022-2023).   

For each geography and time frame grouping the data reports:

- **Filing Rate**: the number of Unlawful Detainer (eviction) cases filed per rental unit. 
- **Judgment Rate**: the rate of Judgments for Possession issued per rental unit. See an outline of the eviction process [here](https://www.dss.virginia.gov/files/division/dcse/family_engagement_programs/eviction_resources/EvixStepsTenantFlyer_Eng_oct2019.pdf). 
- **Percent Filed by Businesses**: the percentage of eviction cases filed by business entities. 
- **Percent Serial Filings**: the percentage of eviction cases filed that were characterized as being filed multiple times against the same defendants, in the same rental unit, over a 12 month period.
- **Proportion of Landlords that Engage in Serial Filing Behavior**: the proportion of landlords that have engaged in serial filing behavior. Serial eviction filings are characterized by multiple eviction filings against the same household, even if they haven't been physically evicted.
- **Median Eviction Amount**: the median dollar amount owed to the landlord by the tenant per eviction filing.
- **Rent Exploitation Ratio**: the ratio of median renter costs among renter-occupied units to median property taxes. The higher the ratio, the greater the renting household is paying compared to the value of the property.
- **Percent Renting Households**: the percentage of housing units that were renter-occupied.
- **Median Rent**: the median gross monthly rental amount per unit.
- **Percent Cost-Burdened Renters**: the percentage of renters who pay more than 30% of their income for housing. 
- **Percent Poverty**: the percentage of households living below the poverty level. 
- **Percent White**: the percentage of the population who identify as white alone.
- **Percent Black**: the percentage of the population who identify as Black alone. 
- **Percent Hispanic**: the percentage of the population who identify as Hispanic or Latino. 
- **Percent Minoritized**: the percentage of the population who do not identify as white alone. 
