# hitRcovid: Access and Visualize the HIT-COVID Database

### *Under development*

For documentation and a tutorial (to come) see: https://hopkinsidd.github.io/hitRcovid/ 

## Database background

The Health Intervention Tracking for COVID-19 (HIT-COVID) project tracks the implementation and relaxation of public health and social measures (PHSMs) taken by governments to slow transmission of SARS-COV-2 globally. Hundreds of volunteer data contributors were trained, provided with standardized field definitions and access to an online forum for asking questions and sharing ideas. Each change in policy and corresponding date is documented at the first-level administrative unit (e.g., states, districts) and nationally for all countries with more detailed geographic resolution in some locations (e.g., counties in the US).

Data are entered into a structured questionnaire with a source document(s) required for each record. Source documents from official government sources are preferred, but other sources are permitted when official sources are unavailable. For each intervention, HIT-COVID captures a suite of additional data including whether interventions are required or recommended and the particular subpopulation to which policies apply. To ensure data quality, contributors are asked to complete weekly self-audit reports, have the ability to submit corrections on past entries, and the management team performs geographic or intervention-specific audits as issues arise. 

This package can be used to easily access, filter, and visualize the HIT-COVID database. The full database including detailed documentation is available in the [hit-covid repository](https://github.com/HopkinsIDD/hit-covid) and more details can be found on the [website](https://akuko.io/post/covid-intervention-tracking). The creation of the database is also described in detail in [this paper](https://www.nature.com/articles/s41597-020-00610-2).


## Installation

You can install hitRcovid in R using the following command:

`devtools::install_github('https://github.com/HopkinsIDD/hitRcovid.git')`


## Basic Usage

Before using any of the filtering or visualization functions you first need to pull the HIT-COVID database using [hit_pull()](https://hopkinsidd.github.io/hitRcovid/reference/hit_pull.html). The default behavior of this function will use the [covidregionaldata](https://github.com/epiforecasts/covidregionaldata) package to include the date of the first case of COVID-19 and the first death from COVID-19 for each country.

`hit_data <- hit_pull()`

You can then filter the database by location and/or intervention type using [hit_filter()](https://hopkinsidd.github.io/hitRcovid/reference/hit_filter.html) function. The documentation provides details on all of the options available for this function. Here is an example looking at the mask policies across Asia:

`asia_masks <- hit_filter(hit_data, continent = "Asia", intervention_group = "mask")`

The country and admin codes to be used for filtering can be found in the [geo_lookup](https://hopkinsidd.github.io/hitRcovid/reference/geo_lookup.html) dataframe provided in the package. The intervention group codes and names can be found in the [intervention_lookup](https://hopkinsidd.github.io/hitRcovid/reference/intervention_lookup.html) dataframe provided in the package or all options can be printed using the function [get_interventions()](https://hopkinsidd.github.io/hitRcovid/reference/list_interventions.html).


## Visualizations

### Timeline of intervention updates

The package contains a plotting function, [intervention_timeline()](https://hopkinsidd.github.io/hitRcovid/reference/intervention_timeline.html), to display a timeline of all of the intervention updates in the database using [hit_filter()](https://hopkinsidd.github.io/hitRcovid/reference/hit_filter.html) to focus on the locations and/or intervention types specified. The documentation provides details about all of the filtering and visualization options. Here is an example of a plot of the interventions from India and New Zealand:

`intervention_timeline(hit, country = c("IND", "NZL"), facet_by = "country")`


### World map of status of an intervention

The package contains a plotting function, [intervention_map()](https://hopkinsidd.github.io/hitRcovid/reference/intervention_map.html), to display a world map showing the current status of a one intervention group at a date specified. The documentation provides details about all of the filtering and visualization options. Here is an example of a map of the status of school closures on September 1, 2020:

`intervention_map(hit_data, intervention_group = "school_closed", time_point = "9/01/2020")`


### Epi-curve with intervention data

The package contains a plotting funciton, [intervention_epi()](https://hopkinsidd.github.io/hitRcovid/reference/intervention_epi.html), to display a barplot of daily case counts (the epi-curve) along with a timeline showing the status of selected interventions (border closures, household confinement, universal mask mandates, restaurant closures, primary school closures, and retail store closures) over time. The main usage would be to plot the epi-curves at the country level, but admin1 level case counts are available for 11 countries (Afghanistan, Belgium, Brazil, Canada, Columbia, Germany, India, Italy, Russia, UK, and USA).

*IMPORTANT note about intervention timelines printed under the epi curves. These final bars of each timeline represent the last logged status of an intervention. For some locations, the intervention data may not have been updated which means that older policies would appear to carry to the present when they are not still active. Care should be taken when interpretting these plots without knowledge of the completeness of the intervention data of the location of interest.*

Here is an example plotting the epi-curve and intervention status for India starting from when the country had 100 total cases:

`intervention_epi(hit_data, country = "IND", case_threshold = 100)`



## Mangement Team

* Qulu Zheng, Johns Hopkins Bloomberg School of Public Health
* Sophia Zweig, Johns Hopkins Bloomberg School of Public Health
* Sarah V. Leavitt, Boston University School of Public Health
* Lawson Ung, Harvard Medical School
* Forrest K. Jones, Johns Hopkins Bloomberg School of Public Health
* Hanmeng Xu, Johns Hopkins Bloomberg School of Public Health
* Elizabeth C. Lee, Johns Hopkins Bloomberg School of Public Health
* Alain Labrique, Johns Hopkins Bloomberg School of Public Health
* David Peters, Johns Hopkins Bloomberg School of Public Health
* Andrew S. Azman, Johns Hopkins Bloomberg School of Public Health

See also the list of data [contributors](https://akuko.io/post/9862de6c-1b8b-4927-b939-3c2282397c31) who participated in this project.

## License

This dataset is licensed under the GNU General Public License v3.0 - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Our nifty [website](https://akuko.io/post/covid-intervention-tracking) was designed by Matt Berg and Dan McCarey from [ona.io](https://ona.io/home/)

Contact: Andrew Azman (azman@jhu.edu) or hit-covid@jhu.edu





