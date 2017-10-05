*https://blog.stata.com/2014/07/18/how-to-simulate-multilevellongitudinal-data/
*https://www.statalist.org/forums/forum/general-stata-discussion/mata/1399455-how-to-do-simulation-in-stata-with-a-variable-generated-in-mata

clear all
set obs 2000

***	Generate variables
*	Firm
gen firm = _n

*	Year (5-year panel)
expand 5
bysort firm: gen year = _n + 1999

*	Correlation structure
*	https://blog.stata.com/2016/05/23/understanding-omitted-confounders-endogeneity-omitted-variable-bias-and-related-concepts/
matrix C = (1, .2, 0\ .2, 1, .2\ 0, .2, 1)

drawnorm rd loc e, corr(C) means(6, 20, 0) sd(1, 3, 1)

bysort firm: gen loc2=floor(loc) if _n==1
by firm: replace loc2=loc2[_n-1] if loc2==.

*	Dependent variable: roa
gen roa = .5*rd + loc + e
gen roa2 = .5*rd + loc2 + e

xtset firm year

xtreg roa rd, fe	/*	Demonstrates large bias if a fixed effect is not fixed	*/

xtreg roa2 rd, fe	/*	Demonstrates no bias if a fixed effect is fixed			*/

***	Alternative approach
bysort firm: gen loc = ceil(rnormal(30,5)) if _n==1
by firm: replace loc=loc[_n-1] if loc==.

gen rd = rnormal(10,3) + .2*loc

gen e = rnormal()

gen roa = .2*rd + .3*loc + e


gen loc2 = loc
bysort firm: replace loc2=loc+2 if _n>2

gen roa2 = .2*rd + .3*loc2 + e

xtreg roa2 rd, fe

reg roa2 rd
