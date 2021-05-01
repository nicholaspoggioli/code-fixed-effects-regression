capt log using fe_log.txt, text
***=========================================
*	What Stays Fixed in Fixed Effects?
*	By: Nicholas Poggioli (poggi005@umn.edu)
*	
***=========================================

***=============================================================================
/*	Outline: Demonstrate the following
	1)	Fixed effects controls for unobserved, time-invariant confounds
	2)	FE becomes less useful as confounds become more time-varying
	3)	Specify a common model in strategy: R&D on Profitability
		a)	Assume CEO effect picked up by fixed effect
		b)	Demonstrate what happens when CEO does not remain constant through panel
*/

***=============================================================================
*	Environment
***============
version

clear all
set more off
set seed 61047

***=============================================================================///	Move this inside a program for each simulation
*	Generate variables
***===================
***	200 firms
set obs 200

gen firm = _n
label var firm "Firm"


***	True fixed effect
gen fixed_true = runiformint(40,60)
label var fixed_true "True Firm Fixed Effect"

expand 5


*** False fixed effects
gen fixed_false_1 = runiformint(49,51)
label var fixed_false_1 "False Firm Fixed Effect (49-51)"

gen fixed_false_2 = runiformint(40,60)
label var fixed_false_2 "False Firm Fixed Effect (40-60)"

gen fixed_false_3 = runiformint(20,80)
label var fixed_false_3 "False Firm Fixed Effect (20-90)"


***	True fixed effect
sort firm fixed_true fixed_false_1 fixed_false_2 fixed_false_3
gen rd_true = rnormal(11,8) + fixed_true
label var rd_true "R&D with fixed_true"

gen error_true = rnormal() + fixed_true
label var error_true "Error with fixed_true"


***	Year
by firm: gen year = _n + 1999
label var year "Year"

order firm year

***	Independent variables
forvalues count = 1/3 {
	gen rd_false_`count' = rnormal(11,8) + fixed_false_`count'
	label var rd_false_`count' "R&D with mu_f`count'"

	gen error_false_`count' = 3*rnormal() + 3*fixed_false_`count'
	label var error_false_`count' "Error with mu_f`count'"
}

***	Dependent variable
gen roa_true = 1*rd_true + error_true
label var roa_true "ROA with fe_t"

forvalues count = 1/3 {
	gen roa_false_`count' = rd_false_`count' + error_false_`count'
	label var roa_false_`count' "ROA with rd_f`count'"
}

order firm year *_true *_false_1 *false_2 *false_3


***=============================================================================///	CONVERT THESE TO PROGRAMS AND SIMULATIONS
*	Fixed effects works															///		TO DEMONSTRATE SAMPLING DISTRIBUTION
***====================
*	Biased specification
reg roa_true rd_true
est sto pooled_true_bias

*	Dummy-variable approach
reg roa_true rd_t i.firm
est sto dummy

estout pooled_true_bias dummy, cells(b(star fmt(%9.3f)) se(par)) drop(*firm) ///
	stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) ///
	label varlabels(_cons Constant) ///
	numbers collabels(none) mlab("Pooled" "Dummy")

*	Fixed effects
xtset firm year, y
xtreg roa_true rd_true, fe
est sto fe_true

estout pooled_true_bias dummy fe_true, cells(b(star fmt(%9.3f)) se(par)) drop(*firm) ///
	stats(r2_a N N_g, fmt(%9.3f %9.0g) labels(R-squared Obs Groups)) ///
	label varlabels(_cons Constant) ///
	numbers collabels(none) mlab("Pooled" "Dummy" "FE True")

	
***=============================================================================
*	Pooled OLS Regression
***======================
forvalues v = 1/3 {
	qui reg roa_false_`v' rd_false_`v'
	est sto pooled_false_`v'
}

estout pooled_true_bias pooled_false_1 pooled_false_2 pooled_false_3 , cells(b(star fmt(%9.3f)) se(par))                ///
        stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared))      ///
        legend label varlabels(_cons Constant) ///
		mlabel("True FE, OVB" "False FE 1" "False FE 2" "False FE 3") ///
		collabels(none)


	
	
	
***=============================================================================
*	Simulation
***======================	
/*
	1)	Set data generating process with true fixed effect (within-firm std dev = 0)
	2)	Generate variables
	3)	Regress
	3)	Repeat 1, 2, & 3 100 times
	4)	For each of the 100 regressions, store
		-	beta estimates 
		-	standard deviation of the fixed effect
	5)	Reset data generating process with slightly higher fixed effect deviation
	6)	Repeat 2 - 4
	7)	Combine all results into single dataset with variables
		-	estimate of beta
		-	fixed effect within-firm standard deviation
*/
	
capt erase data_full.dta	
forvalues v = 0(1)14 {
	forvalues r = 1(1)10 {
		display "`v'" "`r'"
		clear all
		set obs 100
		gen firm = _n
		capt noisily gen mu = runiformint(0,10) if `v'==0
		expand 7
		gen std = `v'
		gen run = `r'
		bysort firm: gen year = _n + 2000
		replace mu = runiformint(5-`v',5+`v') if `v'!=0
		gen rd = rnormal(11,8) + mu
		gen e = 3*rnormal() + 3*mu
		gen roa = rd + e
		qui xtset firm year, y
		qui xtsum mu
		gen sd_w = `r(sd_w)'
		qui xtreg roa rd, fe
		gen b = _b[rd]
		capt noisily append using data_full.dta
		save data_full.dta, replace
	}
}

bysort std: egen mean_sd_w=mean(sd_w)
replace mean_sd_w = round(mean_sd_w,.1)

graph box b, over(mean_sd) scheme(plotplain) ylab(0(.5)4) yline(1)

scatter b sd_w, scheme(plotplain) msize(tiny) ylab(0(.5)4) xlab(0(2)14) ///		/// CREATE THIS GRAPH FOR MULTIPLE STRENGTHS OF CORRELATION BETWEEN MU AND ERROR
	yline(1)		
	
		
		

/*
***	Increasing FE standard deviation
bysort firm: gen fixed_true2=floor(rnormal(50, 5)) if _n==1
replace fixed_true2=fixed_true2[_n-1] if fixed_true2==.

bysort firm: gen fixed_true3=floor(rnormal(50, 10)) if _n==1
replace fixed_true3=fixed_true3[_n-1] if fixed_true3==.
		
bysort firm: gen fixed_true4=floor(rnormal(50, 20)) if _n==1
replace fixed_true4=fixed_true4[_n-1] if fixed_true4==.

forvalues v = 2/4 {
	gen rd`v' = rnormal() + fixed_true`v'
	gen e`v' = 3*rnormal() + fixed_true`v'
	gen roa`v' = rd`v' + e`v'
}

forvalues j = 2/4 {
	qui reg roa`j' rd`j', robust
	est sto reg_`j'
}

estout reg_2 reg_3 reg_4 , cells(b(star fmt(%9.3f)) se(par))                ///
        stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared))      ///
        legend label varlabels(_cons Constant) ///
		collabels(none)

		
*	Pooled regression clustered by firm
est clear 

qui reg roa_true rd_t, cluster(firm)
est sto reg_t

forvalues v = 1/3 {
	qui reg roa_f`v' m`v'_rd, cluster(firm)
	est sto reg_m`v'
}

estout reg_t reg_m1 reg_m2 reg_m3 , cells(b(star fmt(%9.3f)) se(par))                ///
        stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared))      ///
        legend label varlabels(_cons Constant) ///
		mlabel("True FE, OVB" "False FE 1" "False FE 2" "False FE 3") ///
		collabels(none)



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



***=============================================================================
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

simulate _b _se, reps(20): fe_t_ols

sum _b_rd, d

histogram _b_rd, scheme(plotplain) xline(1, lw(thick)) xlab(.75(.25)1.75) percent

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

capt log close
