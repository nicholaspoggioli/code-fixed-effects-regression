*https://blog.stata.com/2014/07/18/how-to-simulate-multilevellongitudinal-data/
*https://www.statalist.org/forums/forum/general-stata-discussion/mata/1399455-how-to-do-simulation-in-stata-with-a-variable-generated-in-mata

clear all

set seed 61047

set obs 200

***	Generate variables
*	Firm
gen firm = _n

*	Year (8-year panel)
expand 8
bysort firm: gen year = _n + 2000

*	Random error
gen e = rnormal()

*	Regressor of interest x's
gen x1 = rnormal()
gen x2 = rnormal()

*	Observed, time-invariant z's
by firm: gen z1 = floor(rnormal(0,10)) if _n==1
by firm: replace z1=z1[_n-1] if z1==.

*	Unobserved, time-invariant mu's
by firm: gen mu1= floor(rnormal(0,10)) if _n==1
by firm: replace mu1=mu1[_n-1] if mu1==.

*	Dependent variable y
gen y = .5*x1 + 1.5*x2 + 2*z1 + 2.5*mu1 + e


***======================
*	Regress
*
*	Correct coefficients:
*		x1:			0.5
*		x2:			1.5
*		z1:			2.0
*		mu1:		2.5
***======================

***	Pooled OLS: Correct specification
reg y x1 x2 z1 mu1
/*
------------------------------------------------------------------------------
           y |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
          x1 |   .5095489   .0769669     6.62   0.000     .3577545    .6613433
          x2 |   1.413562   .0715814    19.75   0.000     1.272389    1.554735
          z1 |   2.001036   .0055838   358.36   0.000     1.990023    2.012048
      mu1 |    2.50203   .0062727   398.88   0.000     2.489659    2.514401
       _cons |  -.0514409   .0732437    -0.70   0.483    -.1958923    .0930106
------------------------------------------------------------------------------

--	This works because all of the observations are independent, in which case
	pooled OLS works.
*/


***	Pooled OLS: Misspecification due to unobserved alpha
reg y x1 x2 z1
/*
------------------------------------------------------------------------------
           y |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
          x1 |  -2.521765   2.183506    -1.15   0.250    -6.827948    1.784418
          x2 |  -.4879978   2.036165    -0.24   0.811    -4.503602    3.527606
          z1 |   1.369035   .1526449     8.97   0.000     1.067998    1.670072
       _cons |   1.024761   2.086667     0.49   0.624    -3.090441    5.139963
------------------------------------------------------------------------------

--	Omitted variable bias is very bad!
*/


***	Random effects: Assume the missing mu1 uncorrelated with regressors. This
*					assumption invalid because z1 and mu1 are correlated, and
*					RE model estimation of z1 is consequently biased.
xtset firm year
xtreg y x1 x2 z1, re
est store re
/*
------------------------------------------------------------------------------
           y |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
          x1 |   .5178614   .0779799     6.64   0.000     .3650236    .6706992
          x2 |   1.436218   .0724146    19.83   0.000     1.294288    1.578148
          z1 |   1.364556   .4949072     2.76   0.006     .3945556    2.334556
       _cons |   1.174201   6.739651     0.17   0.862    -12.03527    14.38367
-------------+----------------------------------------------------------------
     sigma_u |  30.054479
     sigma_e |  .99201154
         rho |  .99891172   (fraction of variance due to u_i)
------------------------------------------------------------------------------

--	z1 is biased because _______________
*/
corr y x1 x2 z1 mu1
/*(obs=1600)
             |        y       x1       x2       z1      mu1
-------------+---------------------------------------------
           y |   1.0000
          x1 |  -0.0351   1.0000
          x2 |   0.0313  -0.0022   1.0000
          z1 |   0.6126  -0.0303  -0.0106   1.0000
         mu1 |   0.7593  -0.0394  -0.0136  -0.0457   1.0000

*/


***	Fixed effects
xtreg y x1 x2 z1, fe
est store fe
/*
------------------------------------------------------------------------------
           y |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
          x1 |   .5182289   .0777875     6.66   0.000     .3647246    .6717331
          x2 |   1.436448   .0722359    19.89   0.000     1.293899    1.578997
          z1 |          0  (omitted)
       _cons |   1.651813   .0704931    23.43   0.000     1.512703    1.790923
-------------+----------------------------------------------------------------
     sigma_u |  35.571813
     sigma_e |  .99201154
         rho |  .99922289   (fraction of variance due to u_i)
------------------------------------------------------------------------------
*/
hausman fe re
