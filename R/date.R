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
## Date functions I use in covid project
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Date functions ----

## test if x is Date
is_date=function(x) is_class(x,'Date')
is_POSIXdate=function(x) is_class(x,'POSIXt')
## convert date strings we encounter in covid to Dates
## NG 21-01-20: generalized for 2021 and beyond. never imagined COVID would last this long...
as_date=function(x) {
  ## R seems to convert dates to numeric when passed through sapply. sigh...
  dates=sapply(x,function(x) {
    if (is_date(x)||is.numeric(x)||is.na(x)) x
    else if (is_POSIXdate(x)) as.Date(x)
    else {
      if (is.character(x)) {
        ## if (endsWith(x,' UTC')) x=sub(' UTC$','',x); # for DOH >= 20-05-24
        if (grepl('^2\\d-',x)) format='%y-%m-%d'
        else if (grepl('^20\\d\\d-',x)) format='%Y-%m-%d'
        else if (grepl('20\\d\\d',x)) format='%Y%m%d'
        else if (grepl('/2\\d$',x)) format='%m/%d/%y'
        else stop('Unexpected date format: ',x);
        as.Date(x,format=format);
      }
      ## else BREAKPOINT('Unable to create date from ',x);
      else stop('Unable to create date from ',x);
    }});
  ## R's as.Date bombs on empty lists. sigh...
  if (length(dates)==0) dates=integer(0);
  as.Date(dates,origin='1970-01-01');
}
## convert date to version strings we use, eg, 20-05-13
as_version=function(date) strftime(date,format='%y-%m-%d')
## day-of-week manipulation we use
Weekdays=cq(Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday);
dayofweek=Vectorize(function(day) which(Weekdays==day))
match_day=function(day) match.arg(ucfirst(day),Weekdays)
inc_day=function(day,i=0) Weekdays[((dayofweek(day)+i-1)%%7)+1]               
## get date of Sunday for week containing date
sunday_week=function(date) date-dayofweek(weekdays(date))+1
