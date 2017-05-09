***==============================================
*	Simulating issues in fixed effects regression
*	Author: Nicholas Poggioli (poggi005@umn.edu),
*			in consultation with J. Myles Shaver
*	Plan
*	
*	1)	Simulate a balanced panel dataset of 20 firms over 12 years
*	2)	Specify a fixed effects data-generating process
*	3)	Examine the accuracy of fixed effects models under various conditions
***==============================================
clear all
set more off
*set seed 61047

***==========================================
*	1)		Simulate of a fixed effects model
***==========================================
set obs 20
gen firm=_n

expand 12
bysort firm: gen t=_n

xtset firm t, y


***========================================
*	2)		Specify data generating process
***========================================
/*	Plan
	1)	Generate random predictor variable x
	2)	Generate random predictor variable v that is constant for each firm
	3)	Generate i.i.d. error term e
	4)	Generate constant a
	4)	Generate dependent variable by specifying data generating process
*/
///	Generate x
gen x=rnormal()

///	Generate v
bysort firm: gen v = rnormal()
by firm: replace v = v[_n-1] if _n!=1

///	Generate e
gen e = rnormal()

///	Generate a
gen a = 1

//	Generate dependent variable by specifying data generating process
gen y = a + 1.3*x + v + e


***=================================================
*	3)	Examine accuracy of several empirical models
***=================================================
///	OLS regression

*	Pooled
reg y x

/*
*	Year dummies
reg y x i.t
reg y x i.t, robust
*/

*	Fixed effects
xtreg y x, fe
xtreg y x, robust





***=========================
*	Programs and simulations
***=========================
///	OLS misspecified
capt program drop dgp_ols_naive
program define dgp_ols_naive, eclass
	version 13.1
	drop _all
	set obs 20
	gen firm=_n
	expand 12
	bysort firm: gen t = _n
	xtset firm t, y
	bysort firm:gen v = rnormal(15,2)
	by firm:replace v = v[_n-1] if _n!=1
	local corr=0.5	/*	see http://www.stata.com/statalist/archive/2007-08/msg00530.html	*/
	qui sum v
	local sz=r(sd)
	local mz=r(mean)
	local s2=`sz'^2*(1/`corr'^2-1)
	gen x = v + sqrt(`s2')*invnorm(uniform())
	gen e=rnormal()
	gen y= (1.25*x) + v + e
	reg y x
end

simulate _b _se, reps(1000): dgp_ols_naive
*sum _b_x, d

*qui sum _b_x, d
*histogram _b_x,  xline(1.3) normal kden xlab(1.2(.5)1.7) freq 

gen model="1: OLS Naive"
compress
save "D:\Dropbox\Projects\Papers-Working\fixed-effects-regression\code-fixed-effects-regression\ols_naive.dta", replace


///	OLS correct specification
capt program drop dgp_ols
program define dgp_ols, eclass
	version 13.1
	drop _all
	set obs 20
	gen firm=_n
	expand 12
	bysort firm: gen t = _n
	xtset firm t, y
	bysort firm:gen v = rnormal(15,2)
	by firm:replace v = v[_n-1] if _n!=1
	local corr=0.5	/*	see http://www.stata.com/statalist/archive/2007-08/msg00530.html	*/
	qui sum v
	local sz=r(sd)
	local mz=r(mean)
	local s2=`sz'^2*(1/`corr'^2-1)
	gen x = v + sqrt(`s2')*invnorm(uniform())
	gen e=rnormal()
	gen y= (1.25*x) + v + e
	reg y x v
end

simulate _b _se, reps(1000): dgp_ols
*sum _b_x, d

*qui sum _b_x, d
*histogram _b_x,  xline(1.3) normal kden xlab(1.2(.5)1.7) freq 

gen model="3: OLS Correct"
compress
save "D:\Dropbox\Projects\Papers-Working\fixed-effects-regression\code-fixed-effects-regression\ols_correct.dta", replace


///	Fixed effects
capt program drop dgp_fe
program define dgp_fe, eclass
	version 13.1
	drop _all
	set obs 200
	gen firm=_n
	expand 12
	bysort firm: gen t = _n
	xtset firm t, y
	bysort firm:gen v = rnormal(15,2)
	by firm:replace v = v[_n-1] if _n!=1
	local corr=0.5	/*	see http://www.stata.com/statalist/archive/2007-08/msg00530.html	*/
	qui sum v
	local sz=r(sd)
	local mz=r(mean)
	local s2=`sz'^2*(1/`corr'^2-1)
	gen x = v + sqrt(`s2')*invnorm(uniform())
	gen e=rnormal()
	gen y= (1.25*x) + v + e
	xtreg y x, fe
end

simulate _b _se, reps(1000): dgp_ols
*sum _b_x, d

*qui sum _b_x, d
*histogram _b_x,  xline(1.3) normal kden xlab(1.2(.5)1.7) freq 

gen model="2: OLS-FE"
compress
save "D:\Dropbox\Projects\Papers-Working\fixed-effects-regression\code-fixed-effects-regression\ols_fe.dta", replace



///	Random effects
capt program drop dgp_re
program define dgp_re, eclass
	version 13.1
	drop _all
	set obs 200
	gen firm=_n
	expand 12
	bysort firm: gen t = _n
	xtset firm t, y
	bysort firm:gen v = rnormal(15,2)
	by firm:replace v = v[_n-1] if _n!=1
	local corr=0.5	/*	see http://www.stata.com/statalist/archive/2007-08/msg00530.html	*/
	qui sum v
	local sz=r(sd)
	local mz=r(mean)
	local s2=`sz'^2*(1/`corr'^2-1)
	gen x = v + sqrt(`s2')*invnorm(uniform())
	gen e=rnormal()
	gen y= (1.25*x) + v + e
	xtreg y x, re
end

simulate _b _se, reps(1000): dgp_ols
*sum _b_x, d

*qui sum _b_x, d
*histogram _b_x,  xline(1.3) normal kden xlab(1.2(.5)1.7) freq 

gen model="4: OLS-RE"
compress
save "D:\Dropbox\Projects\Papers-Working\fixed-effects-regression\code-fixed-effects-regression\ols_re.dta", replace



///	Combined data
use "D:\Dropbox\Projects\Papers-Working\fixed-effects-regression\code-fixed-effects-regression\ols_naive.dta", clear
append using "D:\Dropbox\Projects\Papers-Working\fixed-effects-regression\code-fixed-effects-regression\ols_fe.dta"
append using "D:\Dropbox\Projects\Papers-Working\fixed-effects-regression\code-fixed-effects-regression\ols_correct.dta"
append using "D:\Dropbox\Projects\Papers-Working\fixed-effects-regression\code-fixed-effects-regression\ols_re.dta"


///	Histogram by model
*Point estimates
histogram _b_x, bin(100) xline(1.25) xlab(1.0(.25)1.75) xmtick(1.0(.25)1.75) by(model, col(1))

*Standard errors
graph hbox _se_x, over(model)












*END
