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
## Debugging and software dev functions
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Debugging functions ----
## TODO: BREAKPOINT is sooo feeble :(
## BREAKPOINT=browser;
BREAKPOINT=function(...,text="",condition=NULL,expr=TRUE,skipCalls=0L) {
  if (!expr) return();
  if (length(list(...))>0) print(paste(...));
  parent.env=parent.frame(n=1);
  with(parent.env,browser(skipCalls=5)); # skipCalls=5 empirically determined
  ## browser(text=text,condition=condition,skipCalls=skipCalls+1);
}
## traceback with args I like
tback=function(max.lines=2) traceback(max.lines=max.lines)

## ---- Dev functions ----
## close all graphics devices
devs.close=function() for (dev in dev.list()) dev.off(dev)
## display color palette
pal=function(col,border="light gray",...) {
 n=length(col)
 plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),axes=FALSE,xlab="",ylab="",...)
 rect(0:(n-1)/n,0,1:n/n,1,col=col,border=border)
}
## helpers to cope with flaky internet during covid crisis
plon=function(base='plot',file.png=filename('figure',base,suffix='png')) {
  if (exists('dev.png')) ploff();
  png(filename=file.png,height=8,width=8,units='in',res=200,pointsize=12);
  dev.png<<-dev.cur();
  file.png<<-file.png;
}
ploff=function(dev=dev.png,file=file.png) {
  dev.off(dev);
  system(paste('pjto',file));
}
## save R workspace and history
sv=function() {save.image(); savehistory(); }
