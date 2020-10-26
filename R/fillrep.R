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
## Functions for filling and repeating various data types
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

## ---- Repeat ----

## repeat rows or columns of 2-dimensional matrix-like object. like rep
## or repeat vector into matrix
## like rep, ... can be times, length.out, or each
## based on StackOverflow https://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
repr=function(x,...) {
  if (is.null(dim(x))) x=matrix(x,nrow=1);
  i=rep(seq_len(nrow(x)),...);
  x=x[i,,drop=F];
  rownames(x)=NULL;
  x;
}
repc=function(x,...) {
  if (is.null(dim(x))) x=matrix(x,ncol=1);
  j=rep(seq_len(ncol(x)),...);
  x=x[,j,drop=F];
 ##  colnames(x)=NULL;
  x;
}
## repeat variables in parent
##   typically used to extend function arguments or object elements to same length
## LENGTH.OUT, TIMES, or EACH analogous to R's rep (but uppercase to avoid conflict with ...)
##   LENGTH.OUT can be 'max', 'min', or number. default: 'max'
## FILL used to fill args that are too small
## as with rep, EACH done first, then if both LENGTH.OUT & TIMES specified, LENGTH.OUT wins
## TODO: if called with no args, extends all of parents args. cf parent_dots,... 
repv=function(...,LENGTH.OUT=NA,EACH=1,TIMES=1,ENV,NAMES=character(),FILL=NULL) {
  dots=match.call(expand.dots=FALSE)$...;
  names=c(as.character(dots),NAMES);
  if (missing(ENV)) ENV=parent.frame(n=1);
  vals=mget(names,envir=ENV);
  ## do in pieces starting with EACH
  if (EACH>1)
    vals=lapply(vals,function(val)
      if(is.data.frame(val)) repr(val,each=EACH) else rep(val,each=EACH));
  ## next, LENGTH or TIMES
  if (is.na(LENGTH.OUT)&&TIMES>1) 
    vals=lapply(vals,function(val)
      if(is.data.frame(val)) repr(val,times=TIMES) else rep(val,times=TIMES))
  else {
    ## calculate desired length
    if (is.na(LENGTH.OUT)) LENGTH.OUT='max';
    if (!is.numeric(LENGTH.OUT)) {
      if (LENGTH.OUT %notin% cq(max,min)) 
        stop(paste("Invalid LENGTH.OUT: must be numeric, 'max', or 'min', not",LENGTH.OUT));
      lengths=sapply(vals,function(val) if (is.data.frame(val)) nrow(val) else length(val));
      LENGTH.OUT=if (LENGTH.OUT=='max') max(lengths) else min(lengths);
    }
    ## do it!
    vals=lapply(vals,function(val)
      if (is.null(FILL)) {
        ## usual case. repeat to desired length
        if (is.data.frame(val)) repr(val,length.out=LENGTH.OUT)
        else rep(val,length.out=LENGTH.OUT);
      }
      else fill(val,LENGTH.OUT,FILL));
  }
  ## assign to same-named variables in parent
  lapply(seq_along(vals),function(i) {
    val=vals[[i]];
    name=names[i];
    assign(name,val,envir=ENV);
  })
  LENGTH.OUT;
}
