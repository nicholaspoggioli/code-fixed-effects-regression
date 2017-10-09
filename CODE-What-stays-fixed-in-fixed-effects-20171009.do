***=========================================
*	What Fixed in Fixed Effects?
*	By: Nicholas Poggioli (poggi005@umn.edu)
*	
*	Stata version 15.0
***=========================================

***============
*	Environment
***============
clear all
set more off
set seed 61047


***===================
*	Generate variables
***===================
***	200 firms
set obs 200

gen firm = _n
label var firm "Firm"


***	True fixed effect
gen t_mu = runiformint(2,100)
label var t_mu "True Firm Fixed Effect"

expand 7


*** False fixed effects
gen f_mu1 = runiformint(49,51)
label var f_mu1 "False Firm Fixed Effect (49-51)"

gen f_mu2 = runiformint(50,60)
label var f_mu2 "False Firm Fixed Effect (50-60)"

gen f_mu3 = runiformint(20,90)
label var f_mu3 "False Firm Fixed Effect (50-60)"


***	7 year panel variables
sort firm t_mu f_mu1 f_mu2 f_mu3
by firm: gen year = _n + 2000
label var year "Year"


***==================
*	True fixed effect
***==================
gen rd = rnormal(11,8) + t_mu
label var rd "R&D Expenditure"

gen e = 3*rnormal() + 3*t_mu
label var e "Error"

gen roa = rd + e
label var roa "ROA"

tempfile t_fe
save "`t_fe'"


***================================================================
*	False fixed effect
*
*	The higher the variance in the fixed effect, the worse the bias
*
*	TO DO: 	Adopt a variable naming convention that enables looping
*			through regressions later in the code.
***================================================================
keep firm t_mu f_mu* year

forvalues v = 1/3 {
	gen rd`v' = rnormal(11,8) + f_mu`v'
	label var rd`v' "R&D Expenditure with f_mu`v'"

	gen e`v' = 3*rnormal() + 3*f_mu`v'
	label var e`v' "Error with f_mu`v'"
}

forvalues v = 1/3 {
	gen roa`v' = rd`v' + e`v'
	label var roa`v' "ROA"
}


***============
*	Append data
***============
append using `t_fe', gen(t_fe)
label var t_fe "=1 if true fixed effect"


***===========
*	Regression
***===========

*	Pooled robust regression
est clear 
forvalues v = 0/1 {
	reg roa rd if t_fe==`v', robust
	est sto reg_`v'
}

estout reg_0 reg_1 , cells(b(star fmt(%9.3f)) se(par))                ///
        stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared))      ///
        legend label varlabels(_cons Constant rd R&D)
		


*	Pooled regression clustered by firm
forvalues v = 0(1)1 {
	reg roa rd if t_fe==`v', cluster(firm)
	est sto reg_`v'
}



xtset firm year
xtreg roa rd, fe
estimates store fe_true


xtset firm year
xtreg roa rd, fe
estimates store fe_false


estout reg_true fe_true reg_false fe_false, cells(b(star fmt(%9.3f)) se(par))                ///
        stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared))      ///
        legend label collabels(none) varlabels(_cons Constant rd R&D) ///
		ml("Reg True" "FE True" "Reg False" "FE False")

est clear
append using `data_true', gen(true)
label var true "=1 if simulated data contains true fixed effect"

sum * if true==1
sum * if true==0

corr *  if true==1, means
corr *  if true==0, means



***=====================
*	Multiple simulations
***=====================

***	True fixed effect, omitted in pooled OLS
capt program drop fe_t_ols
program define fe_t_ols, eclass
	version 13.1
	drop _all
	set obs 200
	gen mu1 = floor(rnormal(20,3))
	gen firm = _n
	expand 7
	bysort firm: gen year = _n + 2000
	gen rd = rnormal(11,8) + mu1
	gen e = 3*rnormal() + 3*mu1
	gen roa = rd + e
	reg roa rd
end

simulate _b _se, reps(2000): fe_t_ols

sum _b_rd, d

histogram _b_rd, scheme(plottig) xline(1, lw(thick)) xlab(.75(.25)1.75) percent

***	True fixed effect, accounted for with FE
capt program drop fe_t_fe
program define fe_t_fe, eclass
	version 13.1
	drop _all
	set obs 200
	gen mu1 = floor(rnormal(20,3))
	gen firm = _n
	expand 7
	bysort firm: gen year = _n + 2000
	gen rd = rnormal(11,8) + mu1
	gen e = 3*rnormal() + 3*mu1
	gen roa = rd + e
	xtset firm year
	xtreg roa rd, fe
end

simulate _b _se, reps(200): fe_t_fe

histogram _b_rd, scheme(plottig) xline(1, lw(thick)) xlab(.75(.25)1.75) percent


***	False fixed effect that changes each observation, FE regression
capt program drop fe_f_fe
program define fe_f_fe, eclass
	version 13.1
	drop _all
	set obs 200
	gen firm = _n
	expand 7
	gen mu1 = floor(rnormal(20,3))
	bysort firm: gen year = _n + 2000
	gen rd = rnormal(11,8) + mu1
	gen e = 3*rnormal() + 3*mu1
	gen roa = rd + e
	xtset firm year
	xtreg roa rd, fe
end

simulate _b _se, reps(200): fe_f_fe

histogram _b_rd, scheme(plottig) xline(1, lw(thick)) xlab(.75(.25)1.75) percent


***	False fixed effect that changes in only the last year for almost all firms, FE regression
capt program drop fe_f_fe_last
program define fe_f_fe_last, eclass
	version 13.1
	drop _all
	set obs 200
	gen firm = _n
	gen mu1 = floor(rnormal(20,3))
	expand 7
	bysort firm: replace mu1 = mu1 + floor(rnormal(3)) if _n==floor(rnormal(4,3)) & mod(firm,2)==0
	bysort firm: gen year = _n + 2000
	gen rd = rnormal(11,8) + mu1
	gen e = 3*rnormal() + 3*mu1
	gen roa = rd + e
	xtset firm year
	xtreg roa rd, fe
end

simulate _b _se, reps(2000): fe_f_fe_last

sum _b_rd, d
sum _se_rd, d

histogram _b_rd, scheme(plottig) xline(1, lw(thick)) xlab(.75(.25)1.25) percent

gen low = _b_rd - _se_rd
gen high = _b_rd + _se_rd

histogram low, scheme(plottig) xline(1, lw(thick))
histogram high, scheme(plottig) xline(1, lw(thick))
