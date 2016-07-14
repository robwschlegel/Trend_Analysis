# Trend_Analysis
All of the files used to produce "Effects of Natural Variability of Seawater Temperature, Time Series Length, Decadal Trend, and Instrument Precision on the Ability to Detect Temperature Trends"

## File description
* The text of the paper may be found in "/LaTeX/Trend_Analysis.tex"
* The code used to create the meta-data and statistics referenced in the text may be found under their corresponding section and subsection headers in the file "5.Text.R"
* The code used to prepare the monthly data for their modelling may be found in "1.Data_assembly.R"
* The code used to fit the models may be found in "2.Model_fitting.R"
* The code used to create the figures seen in the paper may be found in "3.Figures.R"
* The code used to create the tables seen in the paper may be found in "4.Tables.R"
* The code used to perform the analyses for the stats referenced in the paper may be found in "5.Text.R" under the corresponding sub-headers
* The "/data" folder contains the raw data used in the analyses as well as their output
* The "/func" folder contains custom made functions used in the analyses
* The "/graph" folder contains the files necessary to create maps, as well as the output of "3.Figures.R"
* The "/setupParams" folder contains the site list of the SACTN and a custom ggplot2 theme
* The "/LaTeX" folder contains the files used to compile "/LaTeX/Trend_Analysis.tex", the body of the text

## 2016/07/05
* Added the basic files required to run all analyses and write up the results for publication

## 2016/07/06
* Effects of linear interpolation of missing monthly values analysed
* The relevant code in "trend_analysis.R" changed so that NA values are no longer interpolated or dropped

## 2016/07/07
* GLS model re-run without linear interpolation
* Removed the file "func/detrendFunc.R" as this analysis is now performed directly in "trend_analysis.R"
* Results and Discussion sections in "Trend_analysis.R" outlined with relevant information for completion
* Added the file "tables.R"
* The code for several more figures added to "figures.R"

## 2016/07/08
* Beginning of results section edited

## 2016/07/09
* Created "/LaTeX" folder
* Completed several more meta-analyses for results
* Results section nearly finished
* Compiled document for first time
* All figures used in "/LatEX/Trend_analysis.R" added to "/LaTeX" folder

## 2016/07/10
* Populated minor correction to model results to all figures
* Minor updates to abstract, intro and methods
* Added results of analysis of length vs precision to the results section
* Discussion section is now a complete rough draft

## 2016/07/11
* Discussion section smoothed out

## 2016/07/12
* Additional references added
* Exploratory analyses performed
* Discussion section finished

## 2016/07/13
* Compiling issues addressed
* Specific points by reviewers addressed
* Conclusion section finished

## 2016/07/14
* Addressed all reviewer comments by line item
* Added final references
* Corrected reference format for submission
* Corrected figure size and text sizes therein
* Final proofread
* Submitted paper for further review