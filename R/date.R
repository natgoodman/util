#################################################################################
##
## Author:  Nat Goodman
## Created: 20-10-26
##          from covid/R/util.R created 20-01-14
##          from misig/util.R created 19-01-01
##          from repwr/R/util.R created 18-05-03
##
## Copyright (C) 2020-2021 Nat Goodman.
## 
## Date functions I use in covid project
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Date functions ----

## constants we use
## names of days and months. number of days in months
init_date=function() {
  WEEKDAYS<<-cq(Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday);
  WDAYS<<-substr(WEEKDAYS,1,3)
  MONTHS<<-cq(January,February,March,April,May,June,
              July,August,September,October,November,December);
  MONS<<-substr(MONTHS,1,3);
  MONTHDAYS<<-setNames(c(31,28,31,30,31,30,31,31,30,31,30,31),MONTHS);
  MONDAYS<<-setNames(MONTHDAYS,MONS);
  INIT.DATE<<-TRUE;
}
if (!exists('INIT.DATE')||!INIT.DATE) init_date();

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
## day is date or day string (eg, Mon or Monday)
dayofweek=Vectorize(function(day) {
  if (is_date(day)) day=weekdays(day) else day=ucfirst(day);
  if (nchar(day)>3) which(WEEKDAYS==day) else which(WDAYS==day);
})
match_day=Vectorize(function(day) {
  if (is_date(day)) day=weekdays(day) else day=ucfirst(day);
  if (nchar(day)>3) match.arg(day,WEEKDAYS) else match.arg(day,WDAYS);
})
## TODO: figure out how to vectorize: Vectorize turns dates into ints!
inc_day=function(day,i=1) {
  if (is_date(day)) return(day+i);
  nday=((dayofweek(day)-1+i)%%7)+1;
  if (nchar(day)>3) WEEKDAYS[nday] else WDAYS[nday];
}
## month-of-year manipulation we use
## month is date or month string (eg, Jan or January)
monthofyear=monofyear=Vectorize(function(month) {
  if (is_date(month)) months=months(month);
  if (nchar(month)>3) which(MONTHS==month) else which(MONS==month);
})
match_month=Vectorize(function(month) {
  if (is_date(month)) month=MONTHS(month) else month=ucfirst(month);
  if (nchar(month)>3) match.arg(month,MONTHS) else match.arg(month,MONS);
})
## TODO: figure out how to vectorize: Vectorize turns dates into ints!
inc_month=function(month,i=1) {
  if (is_date(month)) return(seq(month,len=2,by=paste(i,'month'))[2]);
  nmonth=((monthofyear(month)-1+i)%%12)+1;
  if (nchar(month)>3) MONTHS[nmonth] else MONS[nmonth];
}
## get date of Sunday for week containing date
sunday_week=function(date) date-dayofweek(weekdays(date))+1
## adapted from www.r-bloggers.com/dates-in-r-and-the-first-day-of-the-month. Thx!
## NG 21-09-19: code moved here from covid/R/label.R
##              extended to get last day of month
mon_day=function(date,day) {
  date=as_date(date);                   # in case comes to us numeric
  repv(date,day);                       # extend args to same length
  ## as.Date(paste(sep='-',unique(format(date,format="%Y-%m")),day))
  if (any(day=='last')) {
    month=months(date);
    year=format(date,format="%Y");
    leap=is_leap(year);
  }
  day=ifelse(day!='last',day,ifelse(month!='February'|!leap,MONTHDAYS[month],29));
  as_date(paste(sep='-',unique(format(date,format="%Y-%m")),day))
}
## adapted from lubridate. Thx!
is_leap=function(year) {
  year=as.numeric(year);
  (year%%4==0)&((year%%100!=0)|(year%%400==0));
}
## extract year from dates - analogous to R's 'months'
years=function(date,abbreviate=FALSE) {
  if (!is_date(date)) date=as_date(date);
  fmt=if(abbreviate) '%y' else '%Y';
  format(date,fmt);
}
