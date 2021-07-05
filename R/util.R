#################################################################################
##
## Author:  Nat Goodman
## Created: 20-01-14
##          from misig/util.R created 19-01-01
##          from repwr/R/util.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## Miscellaneous utility functions
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Miscellaneous functions ----

## NG 18-10-24: wrap function - propogate locals and ... then call function
##   funfun are additional functions called by fun with ... args
## TODO: handle partial matching of ... params
## adapted from stackoverflow.com/questions/4124900
wrapfun=function(fun,funfun=NULL,...) {
  env=parent.frame(n=1);
  x=ls(envir=env);
  fx=do.call(c,lapply(c(fun,funfun),function(fun) names(formals(fun))));
  args=sapply(x[x%in%fx],function(x) get(x,envir=env),simplify=F);
  dots=list(...);
  args=c(args,dots[names(dots)%in%fx]);
  do.call(fun,args);
}
## get function arguments - to remove args that will fail from ... and related
funargs=function(...) do.call(c,lapply(list(...),function(fun) names(formals(fun)) %-% '...' ))

## tack id onto filebase if not NULL or NA
paste_id=function(base,id=NULL,sep='.') {
  ## test id this way to avoid running is.na when id=NULL 
  if (is.null(id)) return(base);
  if (is.na(id)) return(base);
  paste(sep=sep,base,id);
}  

## like match.arg but uses general matching and, if several.ok, returns 'em all
## pmatch_choice=
##   function(arg,choices,several.ok=T,none.ok=F,start=T,ignore.case=T,perl=F,fixed=F,invert=F) {
##     ## m=startsWith(choices,arg);
##     pat=if(start) paste0('^',arg) else arg;
##     m=grep(pat,choices,ignore.case=ignore.case,perl=perl,value=T,fixed=fixed,invert=invert);
##     if (length(m)==0&&!none.ok)
##       stop(paste(sep=' ',"'arg' matched none of",paste(collapse=', ',choices),
##            "but 'none.ok' is FALSE"));
##     if (length(m)>1&&!several.ok)
##       stop(paste(sep=' ',"'arg' matched several of",paste(collapse=', ',choices),
##                  "but 'several.ok' is FALSE"));
##     if (length(m)==0) NULL else m;
##   }
pmatch_choice=
  function(arg,choices,
           allseveral.ok=TRUE,allnone.ok=FALSE,several.ok=allseveral.ok,none.ok=allnone.ok,
           start=TRUE,null.ok=TRUE,simplify=TRUE,
           ignore.case=TRUE,perl=FALSE,fixed=FALSE,invert=FALSE) {
    if (is.null(arg)&!null.ok) stop("arg is NULL, but null.ok is FALSE");
    if (length(arg)<=1) {
      if (missing(allseveral.ok)) allseveral.ok=several.ok;
      if (missing(allnone.ok)) allnone.ok=none.ok;
    }
    ## m=startsWith(choices,arg);
    pat=if(start) paste0('^',arg) else arg;
    ml=sapply(pat,function(pat)
      grep(pat,choices,ignore.case=ignore.case,perl=perl,value=T,fixed=fixed,invert=invert),
      simplify=FALSE,USE.NAMES=FALSE);
    names(ml)=arg;
    if (!(none.ok&several.ok)) {
      ## check whether each arg matched uniquely
      len=sapply(ml,function(m) length(m));
      bad=(len==0)&!none.ok;
      if (any(bad))
        stop(paste(sep=' ',"arg(s)",paste(collapse=', ',arg[bad]),
                   "matched none of",paste(collapse=', ',choices),
                   "but 'none.ok' is FALSE"));
      bad=(len>1)&!several.ok;
      if (any(bad))
        stop(paste(sep=' ',"arg(s)",paste(collapse=', ',arg[bad]),
                   "matched several of",paste(collapse=', ',choices),
                   "but 'several.ok' is FALSE"));
    }
    m=unique(unlist(ml));
    ## check overall match
    if (length(m)==0&&!allnone.ok)
      stop(paste(sep=' ',"arg(s)",paste(collapse=', ',arg),
                 "matched none of",paste(collapse=', ',choices),
                 "but 'allnone.ok' is FALSE"));
    if (length(m)>1&&!allseveral.ok)
       stop(paste(sep=' ',"arg(s)",paste(collapse=', ',arg),
                  "matched several of",paste(collapse=', ',choices),
                  "but 'allseveral.ok' is FALSE"));
    if (length(m)==0) NULL
    else if (simplify) m else ml;
  }
## generate sequence from min(x) to max(x). similar to R's 'seq'
## x can be vector, matrix, data.frame, list of vectors
## when relative=FALSE
##   by, length.out are step-size and length as in R's seq
##     except that if both are set, length wins insted of being an error
## when relative=TRUE
##   by is fraction of range (max(x)-min(x))
##   length.out is number of elements between each elemnt of x
## CAUTION: when x is matrix-like, 'by' may cause varying column lengths! 'length' is safer
seqx=function(x,by=NULL,length.out=NULL,relative=FALSE,na.rm=TRUE) {
  if (all(sapply(c(by,length.out),is.null))) stop("One of 'by' or 'length.out' must be specified");
  ## is.list catches data.frames as well as 'real' lists
  if (is_2d(x)) apply(x,2,function(x) seqx_(x,by,length.out,relative,na.rm))
  else seqx_(x,by,length.out,relative,na.rm);
}
seqx_=function(x,by,length.out,relative,na.rm) {
  lo=min(x,na.rm=na.rm);
  hi=max(x,na.rm=na.rm);
  if (!relative) {
    if (!is.null(length.out)) seq(lo,hi,length.out=length.out) else seq(lo,hi,by=by);
  } else {
    if (!is.null(length.out)) {
      ## length.out is elements per interval
      ## code adapted from stackoverflow.com/questions/54449370. Thx!
      c(do.call(c,lapply(1:(length(x)-1),function(i) {
        lo=x[i]; hi=x[i+1]; delta=hi-lo; step=delta/length.out; seq(lo,hi-step,by=step)})),
        hi);
    } else {
      ## by is fraction of total range
      by=by*(hi-lo);
      seq(lo,hi,by=by);
    }}
}
## test if x is "real" list, not data frame
is_list=function(x) is.list(x)&&!is.data.frame(x)
## test if x is a vector - workaround for R's is.veector not working on Dates....
is_vector=function(x) is_date(x)||is.vector(x)
## test if x is named list -- all elements have names -- analogous to Perl hash
is_nlist=is_hash=function(x)
  is_list(x)&&!is.null(names(x))&&all(sapply(names(x),function(name) nchar(name)>0))
## test if x is unnamed list -- no elements have names
is_ulist=function(x)
  is_list(x)&&(is.null(names(x))||all(sapply(names(x),function(name) nchar(name)==0)))
## test if x is 2D matrix-like object
is_2d=function(x) (!is.null(dim(x)))&&(length(dim(x))==2)
## test blank field
is_blank=function(x) (x=='')|is.null(x)|is.na(x)
## test 'simple' value - eg, name or string
is_simple=function(x) is.symbol(x)||(is.character(x)&&length(x)<=1)
## test if x is "real" logical, not NA
is_logical=function(x) is.logical(x)&&!is.na(x)
## test if x is an object of given class
is_class=function(x,class) class %in% class(x)

## not in - based on example in RefMan - more intutive than !%in%
"%notin%"=function(x,table) match(x,table,nomatch=0)==0
## upper/lower case fist character. like the Perl functions
ucfirst=function(x) {
  substr(x,1,1)=toupper(substr(x,1,1));
  x;
}
lcfirst=function(x) {
  substr(x,1,1)=tolower(substr(x,1,1));
  x;
}
## between, near - to subset sim results. closed on bottom, open on top
## between=function(x,lo,hi,tol=0) x>=lo-tol&x<hi+tol;
## NG 21-01-20: to handle non-numeric args, don't do arithmetic if tol==0
between=function(x,lo,hi,tol=0) if (tol!=0) x>=lo-tol&x<hi+tol else x>=lo&x<hi
near=function(x,target,tol=.01) between(x,target-tol,target+tol)
## general between: open, closed on either end
btwn=function(x,lo,hi,tol=0,open=c(FALSE,TRUE),closed=!open) {
  if (all(closed)) btwn_cc(x,lo,hi,tol)
  else if (!any(closed)) btwn_oo(x,lo,hi,tol)
  else if (closed[1]) btwn_co(x,lo,hi,tol)
  else btwn_oc(x,lo,hi,tol)
}
btwn_cc=function(x,lo,hi,tol=0) if (tol!=0) x>=lo-tol&x<=hi+tol else x>=lo&x<=hi
btwn_co=function(x,lo,hi,tol=0) if (tol!=0) x>=lo-tol&x<hi+tol else x>=lo&x<hi
btwn_oc=function(x,lo,hi,tol=0) if (tol!=0) x>lo-tol&x<=hi+tol else x>lo&x<=hi
btwn_oo=function(x,lo,hi,tol=0) if (tol!=0) x>lo-tol&x<hi+tol else x>lo&x<hi

## round up or down to nearest multiple of u. from https://grokbase.com/t/r/r-help/125c2v4e14/
round_up=function(x,u) ceiling(x/u)*u;
round_dn=function(x,u) floor(x/u)*u;
## x can be range or single number (lower bound)
round_rng=function(x,y,u) 
  if (missing(y)) c(round_dn(x[1],u),round_up(x[2],u)) else c(round_dn(x,u),round_up(y,u))

## pick n items from x approx evenly spaced
pick=function(x,n.want,n.min=1,rep.ok=FALSE,exclude=NULL) {
  x=x[x%notin%exclude];
  if (length(x)<n.min) stop('too few elements in x');
  if (length(x)<n.want&!rep.ok) x
  else {
    step=1/(n.want+1);
    probs=seq(step,by=step,len=n.want)
    unname(quantile(x,probs=probs,type=1))
  };
}

## expand.grid for data frames. args can be data frames or vectors
## NULL args are skipped 
expand_df=function(...) {
  dots=list(...);
  dots=dots[sapply(dots,function(x) !is.null(x))];
  dots=lapply(seq_along(dots),function(i) {
    df=dots[[i]]
    if (!is.data.frame(df)) {
      df=as.data.frame(df,stringsAsFactors=FALSE);
      nm=names(dots)[i];
      colnames(df)=if(!is.null(nm)&&nchar(nm)>0) nm else paste0('X',i);
    }
    df;
  })
  ## line below from stackoverflow.com/questions/13640157. Thanks!
  Reduce(function(...) merge(..., by=NULL), dots)
}
