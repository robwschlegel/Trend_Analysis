# Trend_Analysis
All of the files used to produce "Effects of Natural Variability of Seawater Temperature, Time Series Length, Decadal Trend, and Instrument Precision on the Ability to Detect Temperature Trends"

## File description
* The text of the paper may be found in "Trend_Analysis.tex"
* The code used to create the meta-data and statistics referenced in the text may be found under their corresponding section and subsection headers in the file "text.R"
* The code used to perform the full analyses of the paper may be found in "trend_analysis.R"
* The code used to create the tables seen in the paper may be found in "tables.R"
* The code used to create the figures seen in the paper may be found in "figures.R"
* The "/data" folder contains the raw data used in the analyses as well as their output
* The "/func" folder contains several custom made functions used in the analyses
* The "/graph" folder contains the files necessary to create maps, a custom ggplot2 theme, as well as the output of "figures.R"
* The "/setupParams" folder contains the site list of the SACTN
* Several other files found in the root folder are used to compile the "Trend_Analysis.tex"

## 2016/07/05
* Added the basic files required to run all analyses and write up the results for publication.

## 2016/07/06
* Effects of linear interpolation of missing monthly values analysed.
* The relevant code in "trend_analysis.R" changed so that NA values are no longer interpolated or dropped.

## 2016/07/07
* GLS model re-run without linear interpolation.
* Removed the file "func/detrendFunc.R" as this analysis is now performed directly in "trend_analysis.R".
* Results and Discussion sections in "Trend_analysis.R" outlined with relevant information for completion. 
* Added the file "tables.R".
* The code for several more figures added to "figures.R"

