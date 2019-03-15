capt log using fe_log.txt, text
***=========================================
*	What Fixed in Fixed Effects?
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
gen mu_t = runiformint(40,60)
label var mu_t "True Firm Fixed Effect"

expand 5


*** False fixed effects
gen mu_f1 = runiformint(49,51)
label var mu_f1 "False Firm Fixed Effect (49-51)"

gen mu_f2 = runiformint(40,60)
label var mu_f2 "False Firm Fixed Effect (40-60)"

gen mu_f3 = runiformint(20,80)
label var mu_f3 "False Firm Fixed Effect (20-90)"


***	True fixed effect
sort firm mu_t mu_f1 mu_f2 mu_f3
gen rd_t = rnormal(11,8) + mu_t
label var rd_t "R&D with mu_t"

gen e_t = 3*rnormal() + 3*mu_t
label var e_t "Error with mu_t"


***	Year
by firm: gen year = _n + 1999
label var year "Year"

order firm year

***	Independent variables
forvalues v = 1/3 {
	gen rd_f`v' = rnormal(11,8) + mu_f`v'
	label var rd_f`v' "R&D with mu_f`v'"

	gen e_f`v' = 3*rnormal() + 3*mu_f`v'
	label var e_f`v' "Error with mu_f`v'"
}

***	Dependent variable
gen roa_t = 1*rd_t + e_t
label var roa_t "ROA with fe_t"

forvalues v = 1/3 {
	gen roa_f`v' = rd_f`v' + e_f`v'
	label var roa_f`v' "ROA with rd_f`v'"
}

order *, alpha
order year, after(firm)


***=============================================================================///	CONVERT THESE TO PROGRAMS AND SIMULATIONS
*	Fixed effects works															///		TO DEMONSTRATE SAMPLING DISTRIBUTION
***====================
*	Biased specification
reg roa_t rd_t


*	Dummy-variable approach
reg roa_t rd_t i.firm
est sto dum1

estout dum1, cells(b(star fmt(%9.3f)) se(par)) drop(*firm) ///
	stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) ///
	label varlabels(_cons Constant) ///
	numbers collabels(none) mlab("Dummy")

*	Fixed effects (mean differencing)
xtset firm year, y
xtreg roa_t rd_t, fe
est sto md1

estout dum1 md1, cells(b(star fmt(%9.3f)) se(par)) drop(*firm) ///
	stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) ///
	label varlabels(_cons Constant) ///
	numbers collabels(none) mlab("Dummy" "Mean Dif")

*	Random effects
xtreg roa_t rd_t, re
est sto re1

estout dum1 md1 re1, cells(b(star fmt(%9.3f)) se(par)) drop(*firm) ///
	stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) ///
	label varlabels(_cons Constant) ///
	numbers collabels(none) mlab("Dummy" "Mean Dif" "RE")

hausman re1	md1


	
***=============================================================================
*	Pooled OLS Regression
***======================
est clear 

reg roa_t rd_t
est sto reg_t

forvalues v = 1/3 {
	qui reg roa_f`v' rd_f`v'
	est sto reg_f`v'
}

estout reg_t reg_f1 reg_f2 reg_f3 , cells(b(star fmt(%9.3f)) se(par))                ///
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
bysort firm: gen mu_t2=floor(rnormal(50, 5)) if _n==1
replace mu_t2=mu_t2[_n-1] if mu_t2==.

bysort firm: gen mu_t3=floor(rnormal(50, 10)) if _n==1
replace mu_t3=mu_t3[_n-1] if mu_t3==.
		
bysort firm: gen mu_t4=floor(rnormal(50, 20)) if _n==1
replace mu_t4=mu_t4[_n-1] if mu_t4==.

forvalues v = 2/4 {
	gen rd`v' = rnormal() + mu_t`v'
	gen e`v' = 3*rnormal() + mu_t`v'
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

qui reg roa_t rd_t, cluster(firm)
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
