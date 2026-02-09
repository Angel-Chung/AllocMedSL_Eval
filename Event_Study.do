** SynthDiD Event Study Plot**
import delimited "~/data/MainData.csv", clear // you can obtain it from MainAnalysis.R

qui sdid consumption hf_pk quarterid treated, vce(noinference) graph g2_opt(ylab(-5(5)20) ytitle("Consumption") scheme(sj)) graph_export(groups, .pdf) 
set seed 12341
matrix list e(lambda)
matrix list e(series)

matrix lambda = e(lambda)[1..3,1]
matrix yco = e(series)[1..3,2] 
matrix ytr = e(series)[1..3,3]
matrix aux = lambda'*(ytr - yco) 
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..5,1..2] 
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o


local b = 1
local B = 10
while `b'<=`B'{
 preserve
 bsample, cluster(hf_pk) idcluster(c2)
 duplicates drop hf_pk quarterid, force
 qui count if treat== 0
 local r1 = r(N)
 qui count if treat != 0
 local r2 = r(N)
 if (`r1'!=0 & `r2'!=0) {
   qui sdid consumption hf_pk quarterid treated, vce(noinference) graph 
   matrix lambda_b = e(lambda)[1..3,1]
   matrix yco_b = e(series)[1..3,2] 
   matrix ytr_b = e(series)[1..3,3]
   matrix aux_b = lambda_b'*(ytr_b - yco_b) 
   matrix meanpre_b = J(5,1,aux_b[1,1])
   matrix d`b' = e(difference)[1..5,2] - meanpre_b
   matrix list d`b'

 }
 local ++b
 restore
}

preserve
keep time d
keep if time!=.


forval b = 1/`B' {
    confirm matrix d`b'
    if _rc == 0 {
        svmat d`b'
    }
	
}

egen rsd = rowsd(d11 - d`B'1) 
gen LCI = d + invnormal(0.025)*rsd
gen UCI = d + invnormal(0.975)*rsd 

list time d LCI UCI if time!=.

*generate plot
tw rarea UCI LCI time, color(gray%40) || scatter d time, color(blue) m(d) xtitle("Quarter",size(4)) ytitle("Consumption",size(4)) xlab(12 "2022Q3" 13 "2022Q4" 14 "2023Q1" 15 "2023Q2" 16 "2023Q3", angle(0) labsize(3.8)) legend(order(2 "Point Estimate" 1 "95% CI") bplacement(nw) ring(0) col(1) size(4)) xline(14, lc(black) lp(solid)) yline(0, lc(red) lp(shortdash)) graphregion(color(white)) plotregion(color(white))

graph export "SDID_event_main.png", width(600) height(420) replace

restore





** Event Study for DiD **

clear all
set more off

import delimited "~/data/MainData.csv", clear
xtset hf_pk quarterid

* Create relative time variables (quarters relative to treatment start)
gen rel_time = quarterid - 15

* Create event time indicators
// Note: Quarter 14 (t=-1) will be omitted as baseline
gen pre3 = (rel_time == -3)  // Quarter 12
gen pre2 = (rel_time == -2)  // Quarter 13  
gen pre1 = (rel_time == -1)  // Quarter 14 (baseline - omitted)
gen post0 = (rel_time == 0)  // Quarter 15 (treatment starts)
gen post1 = (rel_time == 1)  // Quarter 16

*Create treatment × relative time interactions
gen treat_pre3 = treat * pre3
gen treat_pre2 = treat * pre2
// treat_pre1 omitted (baseline)
gen treat_post0 = treat * post0  
gen treat_post1 = treat * post1

label var treat_pre3 "Treatment × t-3"
label var treat_pre2 "Treatment × t-2" 
label var treat_post0 "Treatment × t=0"
label var treat_post1 "Treatment × t+1"

* Main event study regression
eststo event_study: xtreg consumption treat_pre3 treat_pre2 treat_post0 treat_post1 ///
    i.quarterid, fe 

estout event_study, cells(b(star fmt(3)) se(par fmt(3))) ///
    stats(N r2_w, fmt(0 3) labels("Observations" "Within R-squared")) ///
    legend label collabels(none) ///
    title("Event Study Results")


matrix coef = e(b)
matrix se = e(V)

* Extract coefficients
scalar b_pre3 = coef[1,1]
scalar b_pre2 = coef[1,2] 
scalar b_post0 = coef[1,3]
scalar b_post1 = coef[1,4]

scalar se_pre3 = sqrt(se[1,1])
scalar se_pre2 = sqrt(se[2,2])
scalar se_post0 = sqrt(se[3,3]) 
scalar se_post1 = sqrt(se[4,4])

* Create dataset for plotting
preserve
clear
set obs 5

gen rel_time = .
gen coef = .
gen se = .
gen ci_lower = .
gen ci_upper = .

replace rel_time = -3 in 1
replace coef = b_pre3 in 1
replace se = se_pre3 in 1

replace rel_time = -2 in 2  
replace coef = b_pre2 in 2
replace se = se_pre2 in 2

replace rel_time = -1 in 3
replace coef = 0 in 3      
replace se = 0 in 3

replace rel_time = 0 in 4
replace coef = b_post0 in 4
replace se = se_post0 in 4

replace rel_time = 1 in 5
replace coef = b_post1 in 5  
replace se = se_post1 in 5

replace ci_lower = coef - 1.96*se
replace ci_upper = coef + 1.96*se

format coef ci_lower ci_upper %9.3f
list rel_time coef ci_lower ci_upper, noobs sep(0)

twoway (rcap ci_lower ci_upper rel_time, lcolor(navy)) ///
       (scatter coef rel_time, mcolor(navy) msize(medium)) ///
       (line coef rel_time, lcolor(navy) lpattern(dash)), ///
       xline(-0.5, lcolor(red) lpattern(dot)) ///
       yline(0, lcolor(black) lpattern(solid)) ///
       xlabel(-3(1)1, labsize(small)) ///
       ylabel(, labsize(small) format(%9.3f)) ///
       xtitle("Quarters Relative to Treatment", size(small)) ///
       ytitle("Treatment Effect", size(small)) ///
       title("Event Study: Treatment Effects on Consumption", size(medium)) ///
       subtitle("95% Confidence Intervals", size(small)) ///
       note("Note: Quarter t=-1 is the omitted baseline period." ///
            "Treatment begins at t=0 (2023Q3).", size(vsmall)) ///
       legend(off) ///
       graphregion(color(white)) plotregion(color(white))

*Save graph
graph export "DiD_event_study_plot.png", replace width(800) height(600)
