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
	gen y= (1.3*x) + v + e
	reg y x
end

simulate _b _se, reps(600): dgp_ols
sum _b_x, d

qui sum _b_x, d
histogram _b_x,  xline(`r(mean)') normal




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
	gen y= (1.3*x) + v + e
	xtreg y x, fe
end

simulate _b _se , reps(2000): dgp_fe
sum _b_x, d

qui sum _b_x, d
histogram _b_x,  xline(`r(mean)') normal




capt program drop dgp_re
program define dgp_re, eclass
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
	gen y= (1.3*x) + v + e
	xtreg y x, re
end

simulate _b _se, reps(200): dgp_re
sum _b_x, d

qui sum _b_x, d
histogram _b_x,  xline(`r(mean)') normal

