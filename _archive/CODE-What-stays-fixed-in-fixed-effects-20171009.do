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
gen t_mu = runiformint(0,10)
label var t_mu "True Firm Fixed Effect"

expand 7


*** False fixed effects
gen m1_mu = runiformint(49,51)
label var m1_mu "False Firm Fixed Effect (49-51)"

gen m2_mu = runiformint(50,60)
label var m2_mu "False Firm Fixed Effect (50-60)"

gen m3_mu = runiformint(20,90)
label var m3_mu "False Firm Fixed Effect (50-60)"


***	7 year panel variables
sort firm t_mu m1_mu m2_mu m3_mu
by firm: gen year = _n + 2000
label var year "Year"


***==================
*	True fixed effect
***==================
gen rd = rnormal(11,8) + t_mu
label var rd "R&D with t_mu"

gen e = 3*rnormal() + 3*t_mu
label var e "Error with t_mu"


***================================================================
*	False fixed effect
*
*	The higher the variance in the fixed effect, the worse the bias
*
*	TO DO: 	Adopt a variable naming convention that enables looping
*			through regressions later in the code.
***================================================================
forvalues v = 1/3 {
	gen m`v'_rd = rnormal(11,8) + m`v'_mu
	label var m`v'_rd "R&D with m`v'_mu"

	gen m`v'_e = 3*rnormal() + 3*m`v'_mu
	label var m`v'_e "Error with m`v'_mu"
}

***============
*	Generate DV
***============
gen roa = rd + e
label var roa "ROA with t_fe"

forvalues v = 1/3 {
	gen roa_m`v' = m`v'_rd + m`v'_e
	label var roa_m`v' "ROA with m`v'_rd"
}

rename (roa rd e) (t_roa t_rd t_e)
order *, alpha
order year, after(firm)

***======================
*	Pooled OLS Regression
***======================

*	Robust errors
est clear 

qui reg t_roa t_rd, robust
est sto reg_t

forvalues v = 1/3 {
	qui reg roa_m`v' m`v'_rd, robust
	est sto reg_m`v'
}

estout reg_t reg_m1 reg_m2 reg_m3 , cells(b(star fmt(%9.3f)) se(par))                ///
        stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared))      ///
        legend label varlabels(_cons Constant) ///
		mlabel("True FE, OVB" "False FE 1" "False FE 2" "False FE 3") ///
		collabels(none)
		

***	Increasing FE standard deviation
bysort firm: gen t_mu2=floor(rnormal(50, 5)) if _n==1
replace t_mu2=t_mu2[_n-1] if t_mu2==.

bysort firm: gen t_mu3=floor(rnormal(50, 10)) if _n==1
replace t_mu3=t_mu3[_n-1] if t_mu3==.
		
bysort firm: gen t_mu4=floor(rnormal(50, 20)) if _n==1
replace t_mu4=t_mu4[_n-1] if t_mu4==.

forvalues v = 2/4 {
	gen rd`v' = rnormal() + t_mu`v'
	gen e`v' = 3*rnormal() + t_mu`v'
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

qui reg t_roa t_rd, cluster(firm)
est sto reg_t

forvalues v = 1/3 {
	qui reg roa_m`v' m`v'_rd, cluster(firm)
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
