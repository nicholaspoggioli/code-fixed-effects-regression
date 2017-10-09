***=========================================
*	What Fixed in Fixed Effects?
*	By: Nicholas Poggioli (poggi005@umn.edu)
*	
*	Stata version 13.1
***=========================================


***============
*	Environment
***============
clear all
set more off
set seed 61047


***=============================
*	True fixed effect assumption
***=============================
*	200 firms
set obs 200

gen mu1 = floor(rnormal(20,3))
label var mu1 "Unobserved firm fixed effect"

gen firm = _n
label var firm "Firm-specific ID"

*	Create 7 observations for each firm
expand 7

bysort firm: gen year = _n + 2000
label var year "Year of observation"

gen rd = rnormal(11,8) + mu1
label var rd "R&D expenditure (random + mu1)"

gen e = 3*rnormal() + 3*mu1
label var e "Error term (correlated with mu1)"

gen roa = rd + e

tempfile data_true
save `data_true'

*	Regression
reg roa rd
est sto reg_true

xtset firm year
xtreg roa rd, fe
estimates store fe_true


***==============================
*	False fixed effect assumption
***==============================
clear

*	200 firms
set obs 200

gen firm = _n
label var firm "Firm-specific ID"

*	Create 7 observations for each firm
expand 7

gen mu1 = floor(rnormal(20,3))
label var mu1 "Unobserved firm fixed effect"

bysort firm: gen year = _n + 2000
label var year "Year of observation"

gen rd = rnormal(11,8) + mu1
label var rd "R&D expenditure (random + mu1)"

gen e = 3*rnormal() + 3*mu1
label var e "Error term (3*random + 3*mu1)"

gen roa = rd + e
label var roa "Return on assets (rd + e)"

*	Regression
reg roa rd
est sto reg_false

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
