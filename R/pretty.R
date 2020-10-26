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
## Pretty print typical values of i, n, d, sd & m
## Used in early stats blog posts
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Pretty print ----
pretty=function(name,value) {
  fun=switch(name,i=i_pretty,n=n_pretty,d=d_pretty,sd=sd_pretty,m=m_pretty,as.character);
  fun(value);
}
i_pretty=function(i) sprintf("%03i",i)
n_pretty=function(n) as.character(n);
d_pretty=function(d) sprintf('%3.2f',d);
sd_pretty=function(sd) sprintf('%3.2f',sd);
m_pretty=function(m) {
  exponent=floor(log10(m));
  significand=m/10^exponent;
  paste0(significand,'e',exponent);
}
