import delimited "/Users/williamswartz/Downloads/12-19-2024---668/Data_12-19-2024---668.csv", clear
cd "/Users/williamswartz/Downloads/740/740 Research"

rename *d *
                
reshape long fipshd stabbrhd sectorhd iclevelhd localehd grrttotdrvgr grrtmdrvgr grrtwdrvgr ///
grrtandrvgr grrtapdrvgr grrtasdrvgr grrtnhdrvgr grrtbkdrvgr grrthsdrvgr grrtwhdrvgr grrt2mdrvgr grrtundrvgr ///
ret_pcfef ret_pcpef enrtotdrvef, i(uniti) j(year)

drop v240


rename *hd *
rename *drvgr *

label variable grrttot "Total Graduation Rate"
label variable grrtm "Male Graduation Rate"
label variable grrtw "Woman Graduation Rate"
label variable grrtbk "Black Graduation Rate"
label variable grrths "Hispanic Graduation Rate"
label variable grrtwh "White Graduation Rate"
label variable grrtas "Asian Graduation Rate"

drop grrtan grrtap grrtnh grrt2m grrtun

label variable ret_pcfef "Full-Time Retention Rate"
label variable ret_pcpef "Part-Time Retention Rate"
label variable enrtotdrvef "Total Enrollment"

label variable sector "Sector"
label variable iclevel "Institution Level"
label variable locale "Degree of Urbanization"

*Keep only 2-year and 4-year institutions
keep if iclevel == 1 | iclevel == 2

save "College Data", replace

********************************************************************************************************************
*MERGE WITH STATE LEVEL DEMOGRAPHIC DATA*
use "College Data", clear
merge m:1 fips year using  "demographic census data"

drop if fips == 72
drop name
keep if sector > 0
gen public = sector == 1 | sector == 4

save "MASTER DATA V2", replace


********************************************************************************************************************
*CREATE URBAN-NESS SCORE*
* Create a new variable for urban-ness score
generate urban_score = .

* Assign scores based on the locale variable
replace urban_score = 1.0 if locale == 11  // City: Large
replace urban_score = 0.9 if locale == 12  // City: Midsize
replace urban_score = 0.8 if locale == 13  // City: Small
replace urban_score = 0.7 if locale == 21  // Suburb: Large
replace urban_score = 0.6 if locale == 22  // Suburb: Midsize
replace urban_score = 0.5 if locale == 23  // Suburb: Small
replace urban_score = 0.4 if locale == 31  // Town: Fringe
replace urban_score = 0.3 if locale == 32  // Town: Distant
replace urban_score = 0.2 if locale == 33  // Town: Remote
replace urban_score = 0.15 if locale == 41 // Rural: Fringe
replace urban_score = 0.1 if locale == 42  // Rural: Distant
replace urban_score = 0.0 if locale == 43  // Rural: Remote


********************************************************************************************************************
*START GENERATING SUMMARY STATISTICS
use "MASTER DATA V2", clear


*SUMMARY STATS*
preserve 
collapse grrttot enrtotdrvef pct_hs_grad unemprate medinc , by(year fips)
asdoc sum grrttot enrtotdrvef pct_hs_grad unemprate medinc, save(summary stats.doc)
restore

*TOTAL AVERAGES*
preserve 
collapse grrttot, by(year)
sum grrttot
tab year
twoway (line grrttot year, ///
    title("Country Wide Average Graduation Rate") ///
    xtitle("Year") ///
    ytitle("Graduation Rate") ///
    xline(2020))
graph export "Country Wide Average Graduation Rate.png", replace
restore





********************************************************************************************************************
*START GENERATING FIGURES*

use "MASTER DATA V2", clear
* Generate a variable to identify Oregon
gen is_oregon = (stabbr == "OR")

*AVERAGE TOTAL GRADUATION RATE PLOT*
preserve

* Calculate average graduation rate for Oregon and other states
collapse (mean) grrttot, by(year is_oregon)

* Plot the average graduation rate over time
twoway (line grrttot year if is_oregon, lcolor(blue) lpattern(solid) lwidth(medium) ///
    title("Average Graduation Rate") ///
    xtitle("Year") ytitle("Graduation Rate") ///
    xline(2020) ///
    subtitle("Oregon vs Other States") ///
    legend(order(1 "Oregon" 2 "Other States"))) ///
       (line grrttot year if !is_oregon, lcolor(red) lpattern(dash) lwidth(medium))
graph export "Average Graduation Rate Oregon vs.png", replace
restore
*AVG GRADUATION RATE BY SECTOR*

*Public*
preserve
keep if public == 1
* Calculate average graduation rate for Oregon and other states
collapse (mean) grrttot, by(year is_oregon)

* Plot the average graduation rate over time
twoway (line grrttot year if is_oregon, lcolor(blue) lpattern(solid) lwidth(medium) ///
    title("Average Graduation Rate in Public Universities") ///
    xtitle("Year") ytitle("Graduation Rate") ///
    xline(2020) ///
    subtitle("Oregon vs Other States") ///
    legend(order(1 "Oregon" 2 "Other States"))) ///
       (line grrttot year if !is_oregon, lcolor(red) lpattern(dash) lwidth(medium))
graph export "Average Graduation Rate Public.png", replace
restore

*Private*
preserve
keep if public == 0
* Calculate average graduation rate for Oregon and other states
collapse (mean) grrttot, by(year is_oregon)

* Plot the average graduation rate over time
twoway (line grrttot year if is_oregon, lcolor(blue) lpattern(solid) lwidth(medium) ///
    title("Average Graduation Rate in Private Universities") ///
    xtitle("Year") ytitle("Graduation Rate") ///
    xline(2020) ///
    subtitle("Oregon vs Other States") ///
    legend(order(1 "Oregon" 2 "Other States"))) ///
       (line grrttot year if !is_oregon, lcolor(red) lpattern(dash) lwidth(medium))
graph export "Average Graduation Rate Private.png", replace
restore

*AVG GRADUATION RATE BY LOCALE*

*City*
preserve
keep if locale == 11 | locale == 12 | locale == 13
* Calculate average graduation rate for Oregon and other states
collapse (mean) grrttot, by(year is_oregon)

* Plot the average graduation rate over time
twoway (line grrttot year if is_oregon, lcolor(blue) lpattern(solid) lwidth(medium) ///
    title("Average Graduation Rate in "City" Universities") ///
    xtitle("Year") ytitle("Graduation Rate") ///
    xline(2020) ///
    subtitle("Oregon vs Other States") ///
    legend(order(1 "Oregon" 2 "Other States"))) ///
       (line grrttot year if !is_oregon, lcolor(red) lpattern(dash) lwidth(medium))

restore

*AVG GRADUATION RATE BY GENDER*
preserve
collapse (mean) grrtm grrtw, by(year is_oregon)

twoway (line grrtm year if is_oregon, lcolor(blue) lpattern(solid) lwidth(medium) ///
    title("Average Graduation")) line grrtw year if is_oregon, lcolor(blue) lpattern(dash) lwidth(medium)
restore




********************************************************************************************************************
*START SYNTHETIC CONTROL ANALYSIS*

*POOLED

use "MASTER DATA V2", clear

preserve
collapse grr* ret* enrtotdrvef unemprate medinc pct_hs_grad urban_score, by(year fips stabbr)
xtset fips year

global predictors grrttot(2012) grrttot(2013) grrttot(2014) grrttot(2015) grrttot(2016) grrttot(2017) grrttot(2018) grrttot(2019) ///
unemprate(2012) unemprate(2016) medinc(2012) medinc(2016) pct_hs_grad(2012) pct_hs_grad(2016) enrtotdrvef(2012) enrtotdrvef(2016) 

synth grrttot $predictors, trunit(41) trperiod(2020) unitnames(stabbr) keep(synth_gradrate.dta)replace fig
graph export "Pooled_Synthetic_Control.png", replace 
restore 

*effects graphs
preserve
use synth_gradrate.dta, clear
gen effect = _Y_treated - _Y_synthetic
tsset _time

sum effect if _time<=2020
sum effect if _time>2020


tsline effect, yline(0) xline(2020) title("Estimated Effects of Oregon's Measure on 110 Grad Rates") ///
ytitle("Graduation Rate") xtitle("Year")
graph export "Effects_Pooled.png", replace
restore

*PUBLIC ONLY

use "MASTER DATA V2", clear

preserve
keep if public == 1
collapse grr* ret* enrtotdrvef unemprate medinc pct_hs_grad urban_score, by(year fips stabbr)
xtset fips year

global predictors grrttot(2012) grrttot(2013) grrttot(2014) grrttot(2015) grrttot(2016) grrttot(2017) grrttot(2018) grrttot(2019) ///
unemprate(2012) unemprate(2016) medinc(2012) medinc(2016) pct_hs_grad(2012) pct_hs_grad(2016) enrtotdrvef(2012) enrtotdrvef(2016) 

synth grrttot $predictors, trunit(41) trperiod(2020) unitnames(stabbr) keep(synth_gradrate.dta)replace fig

restore 
mat list e(V_matrix)

*effects graphs
preserve
use synth_gradrate.dta, clear
gen effect = _Y_treated - _Y_synthetic
tsset _time

sum effect if _time<=2020
sum effect if _time>2020


tsline effect, yline(0) xline(2020)
restore


*PRIVATE ONLY

use "MASTER DATA V2", clear

preserve
keep if public == 0
collapse grr* ret* enrtotdrvef unemprate medinc pct_hs_grad urban_score, by(year fips stabbr)
xtset fips year

global predictors grrttot(2012) grrttot(2013) grrttot(2014) grrttot(2015) grrttot(2016) grrttot(2017) grrttot(2018) grrttot(2019) ///
unemprate(2012) unemprate(2016) medinc(2012) medinc(2016) pct_hs_grad(2012) pct_hs_grad(2016) enrtotdrvef(2012) enrtotdrvef(2016) 

synth grrttot $predictors, trunit(41) trperiod(2020) unitnames(stabbr) keep(synth_gradrate.dta)replace fig

restore 
mat list e(V_matrix)

*effects graphs
preserve
use synth_gradrate.dta, clear
gen effect = _Y_treated - _Y_synthetic
tsset _time

sum effect if _time<=2020
sum effect if _time>2020


tsline effect, yline(0) xline(2020)
restore



********************************************************************************************************************
*SYNTH VIA SYNTHRUNNER*
*ALL INSITUTIONS*
use "MASTER DATA V2", clear

preserve
collapse grr* ret* enrtotdrvef unemprate medinc pct_hs_grad urban_score, by(year fips stabbr)
xtset fips year

synth_runner grrttot $predictors, trunit(41) trperiod(2020) unitnames(stabbr) keep(synth_gradrate.dta)replace

use synth_gradrate.dta, clear

label variable effect "Estimated Effect"
sort fip year

gen donor_code = fip if fip!= 41

levelsof(donor_code)
local donor_pool = r(levels) 

foreach dc of local donor_pool{
		local plotline_donor  "`plotline_donor' line effect year if fip == `dc', lc(gs10) lwidth(thin) lpattern(solid)  || "
	}
twoway `plotline_donor' line effect year if fip == 41, lpattern(solid) lc(black) ||, ///
title("Placebo Tests of Oregon vs Other States") legend(off) xline(2020) yline(0) bgcolor(white) graphregion(color(white))

graph export "Placebo Tests.png", replace

// After running synth_runner

// Extract results into separate matrices
matrix b = e(b)
matrix pvals = e(pvals)
matrix pvals_std = e(pvals_std)

// Combine matrices into one
matrix Pooled_Results = b', pvals', pvals_std'

matrix colnames Pooled_Results = Estimate Pvalue Standardized_Pvalue
matrix rownames Pooled_Results = t0 t1 t2 t3 

asdoc wmat, matrix(Pooled_Results) save(Pooled_Results.doc) replace


/* MSPE ratios
gen mspe_ratio = post_rmspe/pre_rmspe
label variable mspe_ratio "MSPE Ratio"

keep if year == 2021
gsort -mspe_ratio
gen place = _n
gen qpval = place/51
sum qpval if fip==41

restore

*/

********************************************************************************************************************
*SYNTH VIA SYNTHRUNNER*
*PUBLIC INSTIUTIONS*

use "MASTER DATA V2", clear

keep if public == 1
preserve
collapse grr* ret* enrtotdrvef unemprate medinc pct_hs_grad urban_score, by(year fips stabbr)
xtset fips year

synth_runner grrttot $predictors, trunit(41) trperiod(2020) unitnames(stabbr) keep(synth_gradrate.dta)replace

use synth_gradrate.dta, clear

label variable effect "Estimated Effect"
sort fip year

gen donor_code = fip if fip!= 41

levelsof(donor_code)
local donor_pool = r(levels) 

foreach dc of local donor_pool{
		local plotline_donor  "`plotline_donor' line effect year if fip == `dc', lc(gs10) lwidth(thin) lpattern(solid)  || "
	}
twoway `plotline_donor' line effect year if fip == 41, lpattern(solid) lc(black) ||, ///
title("Placebo Tests of Oregon vs Other States") legend(off) xline(2020) yline(0) bgcolor(white) graphregion(color(white))

graph export "Placebo Tests.png", replace

// After running synth_runner

// Extract results into separate matrices
matrix b = e(b)
matrix pvals = e(pvals)
matrix pvals_std = e(pvals_std)

// Combine matrices into one
matrix Public_Results = b', pvals', pvals_std'

matrix colnames Public_Results = Estimate Pvalue Standardized_Pvalue
matrix rownames Public_Results = t0 t1 t2 t3 

asdoc wmat, matrix(Public_Results) save(Public_Results.doc) replace



********************************************************************************************************************
*SYNTH VIA SYNTHRUNNER*
*PRIVATE INSTIUTIONS*

use "MASTER DATA V2", clear

keep if public == 0
preserve
collapse grr* ret* enrtotdrvef unemprate medinc pct_hs_grad urban_score, by(year fips stabbr)
xtset fips year

synth_runner grrttot $predictors, trunit(41) trperiod(2020) unitnames(stabbr) keep(synth_gradrate.dta)replace

use synth_gradrate.dta, clear

label variable effect "Estimated Effect"
sort fip year

gen donor_code = fip if fip!= 41

levelsof(donor_code)
local donor_pool = r(levels) 

foreach dc of local donor_pool{
		local plotline_donor  "`plotline_donor' line effect year if fip == `dc', lc(gs10) lwidth(thin) lpattern(solid)  || "
	}
twoway `plotline_donor' line effect year if fip == 41, lpattern(solid) lc(black) ||, ///
title("Placebo Tests of Oregon vs Other States") legend(off) xline(2020) yline(0) bgcolor(white) graphregion(color(white))


// After running synth_runner

// Extract results into separate matrices
matrix b = e(b)
matrix pvals = e(pvals)
matrix pvals_std = e(pvals_std)

// Combine matrices into one
matrix Private_Results = b', pvals', pvals_std'

matrix colnames Private_Results = Estimate Pvalue Standardized_Pvalue
matrix rownames Private_Results = t0 t1 t2 t3 

asdoc wmat, matrix(Private_Results) save(Private_Results.doc) replace

********************************************************************************************************************
*SYNTH VIA SYNTHRUNNER*
*URBAN INSITUTIONS*

use "MASTER DATA V2", clear

keep if locale == 11 | locale == 12 | locale == 13
preserve
collapse grr* ret* enrtotdrvef unemprate medinc pct_hs_grad urban_score, by(year fips stabbr)
xtset fips year

synth_runner grrttot $predictors, trunit(41) trperiod(2020) unitnames(stabbr) keep(synth_gradrate.dta)replace

use synth_gradrate.dta, clear

label variable effect "Estimated Effect"
sort fip year

gen donor_code = fip if fip!= 41

levelsof(donor_code)
local donor_pool = r(levels) 

foreach dc of local donor_pool{
		local plotline_donor  "`plotline_donor' line effect year if fip == `dc', lc(gs10) lwidth(thin) lpattern(solid)  || "
	}
twoway `plotline_donor' line effect year if fip == 41, lpattern(solid) lc(black) ||, ///
title("Placebo Tests of Oregon vs Other States") legend(off) xline(2020) yline(0) bgcolor(white) graphregion(color(white))


// After running synth_runner

// Extract results into separate matrices
matrix b = e(b)
matrix pvals = e(pvals)
matrix pvals_std = e(pvals_std)

// Combine matrices into one
matrix Urban_Results = b', pvals', pvals_std'

matrix colnames Urban_Results = Estimate Pvalue Standardized_Pvalue
matrix rownames Urban_Results = t0 t1 t2 t3 

asdoc wmat, matrix(Urban_Results) save(Urban_Results.doc) replace