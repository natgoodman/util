#################################################################################
##
## Author:  Nat Goodman
## Created: 20-10-26
##          from covid/R/util.R created 20-01-14
##          from misig/util.R created 19-01-01
##          from repwr/R/util.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## Wrappers for R's set functions and operators
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Set functions and operators ----

## set predicates.
##   subset from stackoverflow.com/questions/26831041. thx!
##   psubset (proper subset) adapted from sets package
##   equal included ro stylistic consistency
is_subset=function(x,y) all(x %in% y)
is_superset=function(x,y) all(y %in% x)
is_psubset=function(x,y) is_subset(x,y)&(length(x)!=length(y))
is_psuperset=function(x,y) is_superset(x,y)&(length(x)!=length(y))
is_equalset=function(x,y) setequal(x,y)
## operator versions of above.
## note that some of these conflict with tidyverse. sorry but I don't use tidyverse
"%<=%"=function(x,y) is_subset(x,y)
"%>=%"=function(x,y) is_superset(x,y)
"%<%"=function(x,y) is_psubset(x,y)
"%>%"=function(x,y) is_psuperset(x,y)
"%==%"=function(x,y) is_equalset(x,y)
## set operators
## intersect, difference, union
"%&%"=function(x,y) intersect(x,y)
"%-%"=function(x,y) setdiff(x,y)
"%+%"=function(x,y) union(x,y)
## symmetric difference. other implementations in stackoverflow.com/questions/19797954
symdiff=function(x,y) union(setdiff(x,y),setdiff(y,x))
"%--%"=function(x,y) symdiff(x,y)
