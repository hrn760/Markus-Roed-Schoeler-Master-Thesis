*************************************************************************************************************
*
*
*  This code uses Local Projections to analyse the effect of monetary shock on American Stocks and sectors
*
*  Markus Roed Schøler - 20-12-2024
*
*
*************************************************************************************************************


*************************************************************************************************************
*
*        I Start by using a Local Projection approach looking at Fed shocks with a reference sector
*
*************************************************************************************************************
clear


* Step 1: We Load the dataset
use "C:/Users/Markus/SyNologyDrive/Drive/Desktop/Speciale/code/Finale_US.dta", clear  // Replace with the path to your saved Stata file


* Step 2: We encode the Ticker variable to convert it to numeric with labels
encode Ticker, gen(Ticker_num)


* Step 3:  We convert the date to a Stata date format
gen Date_num = date(Date, "YMD")
format Date_num %td
drop Date
rename Date_num Date


* Step 4: We destring the variables
destring Shocks_FED, replace force
destring VIX, replace force


* Step 5: We standardise the shock variable
summarize Shocks_FED
local shock_mean = r(mean)
local shock_sd = r(sd)
gen Shock_Std = (Shocks_FED - `shock_mean') / `shock_sd'


* Step 6: We encode Sector as a categorical variable
encode Sector, gen(Sector_id)


* Step 7: We generate a new date variable that doesnt have gaps because of weekends, holidays etc.
preserve
keep Date
duplicates drop
sort Date
gen DayCounter = _n
tempfile dates
save `dates', replace
restore
merge m:1 Date using `dates', keep(master match) nogen


* Step 8: We define our ticket identifier and date variable
sort Ticker_num DayCounter
xtset Ticker_num DayCounter


* Step 9: We run the Local Projection for 15 horizons
forval i = 0/15 {  // Loop through the horizons
        // Run the regression with fixed effects
        reghdfe f`i'.daily_returns Shock_Std  l.daily_returns /// 
		l2.daily_returns  i.Sector_id#c.Shock_Std  l.VIX , ///
          absorb(Ticker_num)   vce(cluster DayCounter)

}

clear

*After manually retrieving the coefficents and standard errors, we now plot the IRFs

cd "X/X/X"
import delimited "Shocks_Results_US.csv", clear


* Step 10: We make the 68 pct. and 95 pct.
forval sector = 1/11 {
    // 68% confidence interval (1-standard deviation)
    gen lower68_sector`sector' = coefficient`sector' - 1 * std_error`sector'
    gen upper68_sector`sector' = coefficient`sector' + 1 * std_error`sector'

    // 95% confidence interval (1.96 standard deviations)
    gen lower95_sector`sector' = coefficient`sector' - 1.96 * std_error`sector'
    gen upper95_sector`sector' = coefficient`sector' + 1.96 * std_error`sector'
}


* Step 11 : we define the sector names locally
// Define sector names as a local macro
local sector_names "Communication_Services Consumer_Discretionary Consumer_Staples Energy Financials Healthcare Industrials Materials Real_Estate Technology Utilities"


* Step 12 : We make the graphs and export them
forval sector = 1/11 {
    // Get the sector name
    local sector_name : word `sector' of `sector_names'

    // Create the IRF graph
    twoway ///
        (rarea lower95_sector`sector' upper95_sector`sector' horizon, color(blue%10)) ///  // 95% confidence band
        (rarea lower68_sector`sector' upper68_sector`sector' horizon, color(blue%50)) ///  // 68% confidence band
        (line coefficient`sector' horizon, lwidth(medium) lcolor(blue)) ///  // IRF line
        , yline(0, lcolor(black) lwidth(medium)) ///  // Horizontal line at 0
          title("") ///  // Title with the sector name
          xtitle("") ytitle("") ///  // X and Y axis titles
          legend(off)  // Turn off legend

    // Export the graph as a PNG file
    graph export IRF_Sector`sector'_US.png, replace
}


**************************************************************************************************************************************		  
*	
* IN THIS SECTION WE LOOK AT THE OVERALL EFFECT WITHOUT THE INTERACTIVE TERM
*
**************************************************************************************************************************************
clear

* Step 1: We Load the dataset
use "C:/Users/Markus/SyNologyDrive/Drive/Desktop/Speciale/code/Finale_US.dta", clear  // Replace with the path to your saved Stata file


* Step 2: We encode the Ticker variable to convert it to numeric with labels
encode Ticker, gen(Ticker_num)


* Step 3:  We convert the date to a Stata date format
gen Date_num = date(Date, "YMD")
format Date_num %td
drop Date
rename Date_num Date


* Step 4: We destring the variables
destring Shocks_FED, replace force
destring VIX, replace force


* Step 5: We standardise the shock variable
summarize Shocks_FED
local shock_mean = r(mean)
local shock_sd = r(sd)
gen Shock_Std = (Shocks_FED - `shock_mean') / `shock_sd'


* Step 6: We encode Sector as a categorical variable
encode Sector, gen(Sector_id)


* Step 7: We generate a new date variable that doesnt have gaps because of weekends, holidays etc.
preserve
keep Date
duplicates drop
sort Date
gen DayCounter = _n
tempfile dates
save `dates', replace
restore
merge m:1 Date using `dates', keep(master match) nogen


* Step 8: We define our ticket identifier and date variable
sort Ticker_num DayCounter
xtset Ticker_num DayCounter


* Step 9: We run the LP and save the coefficents
matrix coeffs = J(16, 1, .)
matrix stderrs = J(16, 1, .)

forval i = 0/15 {  
    reghdfe f`i'.daily_returns Shock_Std l.daily_returns ///
        l2.daily_returns  l.VIX , ///
        absorb(Ticker_num) vce(cluster DayCounter)
    
    
    matrix coeffs[`i'+1, 1] = _b[Shock_Std]
    matrix stderrs[`i'+1, 1] = _se[Shock_Std]
}



* Step 10: We change the matrices to variables
svmat coeffs, names(coefficient1)
svmat stderrs, names(std_error1)
keep coefficient1 std_error1 


* Step 11: We create variable for the horizon
gen horizon = _n - 1


* Step 12: In case there is any empty obsevations we delete them
drop if missing(coefficient1)


* Step 13: We create the confidence bands
    gen lower68_sector1 = coefficient1 - 1 * std_error1
    gen upper68_sector1 = coefficient1 + 1 * std_error1
	
    gen lower95_sector1 = coefficient1 - 1.96 * std_error1
    gen upper95_sector1 = coefficient1 + 1.96 * std_error1
	
		  
* Step 14: We make our plot and save it	
	twoway ///
        (rarea lower95_sector1 upper95_sector1 horizon, color(blue%10)) ///  // 95% confidence band
        (rarea lower68_sector1 upper68_sector1 horizon, color(blue%50)) ///  // 68% confidence band
        (line coefficient1 horizon, lwidth(medium) lcolor(blue)) ///  // IRF line
        , yline(0, lcolor(black) lwidth(medium)) ///  // Horizontal line at 0
          title("") ///
          xtitle("") ytitle("") ///
          legend(off) 
    graph export Market_US.png, replace
	
	
**************************************************************************************************************************************		  
*	
* IN THIS SECTION WE LOOK AT THE INITIAL EFFECT WITHOUT THE INTERACTIVE TERM
*
**************************************************************************************************************************************

clear

* Step 1: We Load the dataset
use "C:/Users/Markus/SyNologyDrive/Drive/Desktop/Speciale/code/Finale_US.dta", clear  // Replace with the path to your saved Stata file


* Step 2: We encode the Ticker variable to convert it to numeric with labels
encode Ticker, gen(Ticker_num)


* Step 3:  We convert the date to a Stata date format
gen Date_num = date(Date, "YMD")
format Date_num %td
drop Date
rename Date_num Date


* Step 4: We destring the variables
destring Shocks_FED, replace force
destring VIX, replace force


* Step 5: We standardise the shock variable
summarize Shocks_FED
local shock_mean = r(mean)
local shock_sd = r(sd)
gen Shock_Std = (Shocks_FED - `shock_mean') / `shock_sd'


* Step 6: We encode Sector as a categorical variable
encode Sector, gen(Sector_id)


* Step 7: We generate a new date variable that doesnt have gaps because of weekends, holidays etc.
preserve
keep Date
duplicates drop
sort Date
gen DayCounter = _n
tempfile dates
save `dates', replace
restore
merge m:1 Date using `dates', keep(master match) nogen


* Step 8: We define our ticket identifier and date variable
sort Ticker_num DayCounter
xtset Ticker_num DayCounter


* Step 9: We run the inital regression
	  reghdfe daily_returns l.daily_returns /// 
		l2.daily_returns  i.Sector_id#c.Shock_Std  l.VIX , ///
          absorb(Ticker_num)   vce(cluster DayCounter) level(68)
		  
clear

* We load the manually fetched results
cd "X/X/X"
import delimited "Shocks_Results_US_H0.csv", clear


* Step 9: We make labels for the sectors
gen sector_num = sector
label define sector_lbl 1 "Communication Services" 2 "Consumer Discretionary" 3 "Consumer Staples" 4 "Energy" 5 "Financials" 6 "Healthcare" 7 "Industrials" 8 "Materials" 9 "Real Estate" 10 "Technology" 11 "Utilities"
label values sector_num sector_lbl


* Step 10: We plot the results
twoway (rcap lower upper sector_num) ///  // Confidence intervals
       (scatter coefficent sector_num), ///  // Coefficients
	   xtitle("") ///
       xlabel(1(1)11, valuelabel angle(70)) ///
       ytitle("") ///
       title("") ///
       legend(off) 
	   graph export USA_H_zero.png, replace
	   
	   
**************************************************************************************************************************************		  	
* IN THIS SECTION WE COMBINE THE US AND DK INITIAL EFFECT
**************************************************************************************************************************************
	   
clear
// Set working directory
cd "X/X/X"

* Import US data and add a country identifier
import delimited "Shocks_Results_US_H0.csv", clear
gen country = "US"
save Shocks_Results_Combined.dta, replace

* Import Danish data and add a country identifier
import delimited "Shocks_Results_DK_H0.csv", clear
gen country = "Denmark"
append using Shocks_Results_Combined.dta


* Step 1 : We make labels for the sectors
gen sector_num = sector
label define sector_lbl 1 "Communication Services" 2 "Consumer Discretionary" 3 "Consumer Staples" 4 "Energy" 5 "Financials" 6 "Healthcare" 7 "Industrials" 8 "Materials" 9 "Real Estate" 10 "Technology" 11 "Utilities"
label values sector_num sector_lbl


* Step 2 : We combine the two graphs and export it
twoway (rcap lower upper sector_num if country == "US", lcolor(ebblue)) ///  // US confidence intervals
       (scatter coefficent sector_num if country == "US", mcolor(ebblue) msize(small)) ///  // US coefficients
       (rcap lower upper sector_num if country == "Denmark", lcolor(cranberry)) ///  // Denmark confidence intervals
       (scatter coefficent sector_num if country == "Denmark", mcolor(cranberry) msize(small)), ///  // Denmark coefficients
       xtitle("") ///
       xlabel(1(1)11, valuelabel angle(70)) ///
       legend(order(2 "US" 4 "Denmark") label(2 "US") label(4 "Denmark") pos(3) col(1))

graph export Combined_Impact_H0_with_CI.png, replace






**************************************************************************************************************************************
*		  	
* IN THIS SECTION WE CALCULATE THE INDUSTRY SHARE OF THE SECTORS
*
**************************************************************************************************************************************
clear
* Load the dataset
use "C:/Users/Markus/SyNologyDrive/Drive/Desktop/Speciale/code/Finale_US.dta", clear  // Replace with the path to your saved Stata file

* Step 1: We keep only "Ticker" and "Sector" and create a dataset of unique tickers and their sectors
keep Ticker Sector
bysort Ticker (Sector): keep if _n == 1  // Ensure unique entries per Ticker

tempfile temp_dta
save `temp_dta'


* Step 2: We Import the .csv file and merge it with the dataset
import delimited "X/X/X/Industries_US.csv", clear
rename v1 Ticker 
rename v2 Industry
merge 1:1 Ticker using `temp_dta'
drop _merge  // Clean up merge indicator if needed
drop if missing(Sector)


* Step 3: We Calculate the share of each industry within each sector
bysort Sector Industry (Ticker): gen SectorIndustryCount = _N if _n == 1
bysort Sector (Ticker): replace SectorIndustryCount = SectorIndustryCount[_n-1] if mi(SectorIndustryCount)

bysort Sector (Ticker): gen SectorTotal = _N if _n == 1
bysort Sector (Ticker): replace SectorTotal = SectorTotal[_n-1] if mi(SectorTotal)

gen IndustryShare = SectorIndustryCount / SectorTotal



* Step 4: We Remove duplicate observations of Industry, keeping the first occurrence
bysort Industry (Ticker): keep if _n == 1



* Step 5: We make a tabel for each sector
levelsof Sector, local(sector_list)  // Get a list of all unique sectors

foreach sector in `sector_list' {
    preserve  // Save the current state of the dataset
    keep if Sector == "`sector'"  // Filter for the current sector
    export delimited using "IndustryShares_`sector'_US.csv", replace  // Save as CSV
    restore  // Restore the original dataset for the next iteration
}



**************************************************************************************************************************************
*		  	
* IN THIS SECTION WE LOOK AT THE MODEL ROBUSTNESS 
*
**************************************************************************************************************************************
* Step 1: We Load the dataset
use "C:/Users/Markus/SyNologyDrive/Drive/Desktop/Speciale/code/Finale_US.dta", clear  // Replace with the path to your saved Stata file


* Step 2: We encode the Ticker variable to convert it to numeric with labels
encode Ticker, gen(Ticker_num)


* Step 3:  We convert the date to a Stata date format
gen Date_num = date(Date, "YMD")
format Date_num %td
drop Date
rename Date_num Date


* Step 4: We destring the variables
destring Shocks_FED, replace force
destring VIX, replace force


* Step 5: We standardise the shock variable
summarize Shocks_FED
local shock_mean = r(mean)
local shock_sd = r(sd)
gen Shock_Std = (Shocks_FED - `shock_mean') / `shock_sd'


* Step 6: We encode Sector as a categorical variable
encode Sector, gen(Sector_id)


* Step 7: We generate a new date variable that doesnt have gaps because of weekends, holidays etc.
preserve
keep Date
duplicates drop
sort Date
gen DayCounter = _n
tempfile dates
save `dates', replace
restore
merge m:1 Date using `dates', keep(master match) nogen


* Step 8: We define our ticket identifier and date variable
sort Ticker_num DayCounter
xtset Ticker_num DayCounter

* STep 9: We run the different models

		    // FULL MODEL
        reghdfe daily_returns Shock_Std  l.daily_returns /// 
		l2.daily_returns   i.Sector_id#c.Shock_Std  l.VIX , ///
          absorb(Ticker_num)   vce(cluster DayCounter)

		    // 4 LAGS
        reghdfe daily_returns Shock_Std  l.daily_returns /// 
		l2.daily_returns l3.daily_returns l4.daily_returns  i.Sector_id#c.Shock_Std  l.VIX , ///
          absorb(Ticker_num)   vce(cluster DayCounter)
		  
		    // 3 LAGS
        reghdfe daily_returns Shock_Std  l.daily_returns /// 
		l2.daily_returns l3.daily_returns  i.Sector_id#c.Shock_Std  l.VIX , ///
          absorb(Ticker_num)   vce(cluster DayCounter)
		  
		    // 1 LAG
        reghdfe daily_returns Shock_Std  l.daily_returns /// 
		  i.Sector_id#c.Shock_Std  l.VIX , ///
          absorb(Ticker_num)   vce(cluster DayCounter)
		  
		    // NO FE
        reghdfe daily_returns Shock_Std  l.daily_returns /// 
		l2.daily_returns  i.Sector_id#c.Shock_Std  l.VIX , ///
             vce(cluster DayCounter)
		  
		    // 2  SHOCK LAGS
        reghdfe daily_returns Shock_Std l.Shock_Std l2.Shock_Std  l.daily_returns /// 
		l2.daily_returns  i.Sector_id#c.Shock_Std  l.VIX , ///
          absorb(Ticker_num)   vce(cluster DayCounter)
		  
		    // 1 SHOCK LAG
        reghdfe daily_returns Shock_Std l.Shock_Std  l.daily_returns /// 
		l2.daily_returns  i.Sector_id#c.Shock_Std  l.VIX , ///
          absorb(Ticker_num)   vce(cluster DayCounter)
		  
		    // NO VIX
        reghdfe daily_returns Shock_Std  l.daily_returns /// 
		l2.daily_returns  i.Sector_id#c.Shock_Std  , ///
          absorb(Ticker_num)   vce(cluster DayCounter)
		  
		    // VIX INTIAL
        reghdfe daily_returns Shock_Std  l.daily_returns /// 
		l2.daily_returns  i.Sector_id#c.Shock_Std  VIX , ///
          absorb(Ticker_num)   vce(cluster DayCounter)
		  
		    // 2 VIX LAGS
        reghdfe daily_returns Shock_Std  l.daily_returns /// 
		l2.daily_returns  i.Sector_id#c.Shock_Std  l.VIX l2.VIX , ///
          absorb(Ticker_num)   vce(cluster DayCounter)
		  


**************************************************************************************************************************************
*		  	
* IN THIS SECTION WE LOOK AT THE INFFORMATION COMPONENT
*
**************************************************************************************************************************************

* Step 1: We Load the dataset
use "X/X/X/Finale_US.dta", clear  // Replace with the path to your saved Stata file


* Step 3:  We convert the date to a Stata date format
gen Date_num = date(Date, "YMD")
format Date_num %td
drop Date
rename Date_num Date


* Step 4:  We destring the variables
destring INFO_Shocks_FED, replace force
destring VIX, replace force


* Step 5: We standardise the shock variable
summarize INFO_Shocks_FED
local shock_mean = r(mean)
local shock_sd = r(sd)
gen Shock_Std = (INFO_Shocks_FED - `shock_mean') / `shock_sd'


* Step 6: We encode Sector as a categorical variable
encode Sector, gen(Sector_id)


* Step 7: We generate a new date variable that doesnt have gaps because of weekends, holidays etc.
keep Date
duplicates drop
sort Date
gen DayCounter = _n
tempfile dates
save `dates', replace
restore
merge m:1 Date using `dates', keep(master match) nogen


* Step 8: We define our ticket identifier and date variable
sort Ticker_num DayCounter
xtset Ticker_num DayCounter


 reghdfe daily_returns    l.daily_returns /// 
		l2.daily_returns Shock_Std     l.VIX , ///
          absorb(Ticker_num) level(68)   vce(cluster DayCounter)
		  
		  
 reghdfe daily_returns    l.daily_returns /// 
		l2.daily_returns Shock_Std i.Sector_id#c.Shock_Std     l.VIX , ///
          absorb(Ticker_num) level(68)   vce(cluster DayCounter)

		  
		  
**************************************************************************************************************************************
