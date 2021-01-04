clear
/*******************
1)	How do I simulate a between effect versus a within effect?
	
	Probably could use the hybrid model to generate the data. Include the 
	group means to generaet the within coefficient and specify a coefficient
	for each dummy variable.
	
	
	Or use a fully specified random coefficients model to generate the DV, then
	run fixed effects models on those.

*/

***	ENVIRONMENT
set seed 61047
set obs 200

***	GENERATE PANEL DATA


*	Reshape to long
reshape long rd perf, i(firm) j(year)
replace year = year + 2007

xtset firm year, y

***	REGRESSION

*	Between effects
xtreg perf rd, be cluster(firm)

*	Fixed effects
xtreg perf rd, fe cluster(firm)
