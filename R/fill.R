#################################################################################
##
## Author:  Nat Goodman
## Created: 20-11-26
##          from util/fillrep.R created 20-10-26
##          from covid/R/util.R created 20-01-14
##          from misig/util.R created 19-01-01
##          from repwr/R/util.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## Functions for filling various data types
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Fill ----
## fill matrix to desired number of rows or columns
## allow lower and uppr case param names for compatibility with my code and R
fillr=function(x,length.out,fill=NA,LENGTH.OUT=length.out,FILL=fill) {
  if (is.null(dim(x))) x=matrix(x,nrow=1);
  if (nrow(x)<LENGTH.OUT) {
    FILL=matrix(FILL,nrow=LENGTH.OUT-nrow(x),ncol=ncol(x));
    colnames(FILL)=colnames(x);
    x=rbind(x,FILL);
  }
  x;
}
fillc=function(x,length.out,fill=NA,LENGTH.OUT=length.out,FILL=fill) {
  if (is.null(dim(x))) x=matrix(x,ncol=1);
  if (ncol(x)<LENGTH.OUT) {
    FILL=matrix(FILL,nrow=nrow(x),ncol=LENGTH.OUT-ncol(x));
    x=cbind(x,FILL);
  }
  x;
}
fill=function(val,LENGTH.OUT,FILL=NA) {
  if (is.data.frame(val)) {
    if (nrow(val)<LENGTH.OUT) val[(nrow(val)+1):LENGTH.OUT,]=FILL
    else val=head(val,LENGTH.OUT);
  }
  else {
    if (length(val)<LENGTH.OUT) val[(length(val)+1):LENGTH.OUT]=FILL
    else val=head(val,LENGTH.OUT);
  }
  val;
};

## ---- Strip ----
## opposite of fill
strip=function(val,LENGTH.OUT=NA,FILL=NA) {
  ## get index of last real data
  if (is.na(FILL))
    i=if(is.data.frame(val)) apply(val,1,function(val) !all(is.na(val))) else !is.na(val)
  else
    i=if(is.data.frame(val)) apply(val,1,function(val) any(ifelse(is.na(val),TRUE,val!=FILL)))
      else ifelse(is.na(val),TRUE,val!=FILL);
  if (any(i)) {
    last=max(which(i));
    if (!is.na(LENGTH.OUT)) last=max(last,LENGTH.OUT);
    val=head(val,last);
  } else val=NULL;
  val;
};

