***	Code to simulate panel data
*	From Bou, J. C., & Satorra, A. (2018). Univariate Versus Multivariate Modeling of Panel Data: Model Specification and Goodness-of-Fit Testing. Organizational Research Methods, 21(1), 150–196. http://doi.org/10.1177/1094428117715509

set more off
clear all

***	GENERATE DATA
set seed 2016
matrix M = 0, 0, 0, 0, 0, 0 
matrix V = (1, .4, .3, .2, .1, .4 \.4, 1, .4, .3, .5, .8 \ .3, .4, 1, .4, .2, .2 \.2, .3, .4, 1, .8, 0 \.1, .5, .2, .8, 1, 0 \ .4, .8, .2, 0, 0, 1)
matrix list M 
matrix list V 
drawnorm x1 x2 x3 x4 eta z, n(3000) cov(V) means(M)
gen y1 = 1 + 2*x1 + 3*z + eta + rnormal() 
gen y2 =1 + 0*x2 + 3*z + eta + rnormal() 
gen y3 =1 + 2*x3 + 3*z + eta + rnormal() 
gen y4 =1 + 4*x4 + 3*z + eta + rnormal() 
gen i =_n

*	Reshape long
reshape long x y, i(i) j(t) 
xtset i t

***	REGRESSIONS

*	Fixed effects
xtreg y x z, mle
mixed y x z || i:, variance

*	RE (ML Estimator) Model (Using the xtreg and mixed Options; Table 4)
xtreg y x z, mle 
mixed y x z || i:, variance

*	Hausmann Test
quietly xtreg y x, fe 
estimates store fixed 
quietly xtreg y x z, re 
estimates store random 
hausman fixed random

*	Reshape wide
reshape wide

*	Multivariate Approach without the TI Assumption (Table 3)
sem (u@1 x1 z -> y1) (u@1 x2 z -> y2) (u@1 x3 z -> y3) /// 
	(u@1 x4 z -> y4), latent(u) cov(_lexogenous*_oexogenous) /// 
	cov(z*u@0)
	
Multivariate FE (Table 4)
sem(_cons@alphau@1x1@bz@g->y1)(_cons@alphau@1x2@bz@g->y2)/// (_cons@alphau@1 x3@bz@g->y3) (_cons@ alphau@1x4@bz@g->y4),/// latent(u) var(e._OEn@e)cov(_lex*_oex)cov(z*u@0)
Multivariate RE (Table 4)
sem(_cons@alphau@1x1@bz@g->y1)(_cons@alphau@1x2@bz@g->y2)/// (_cons@alphau@1 x3@bz@g->y3) (_cons@ alphau@1x4@bz@g->y4),/// latent(u) var(e._OEn@e)cov(_lex*_oex@0)

*	Reshape long
reshape long
