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
## Generate name=value strings for use in error messages and the like
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Generate name=value strings ----

## Generalize nvq in light of what I learned from edit
## subsumes paste_nv, nvq, nvq_file
## IGNORE can be logical or value to use for non-existant names and other errors
## PRETTY is function mapping name,value pairs to pretty values
## NV is escape hatch using standard evaluation
##   list of names or name,value pairs
## if NV and ... both present, NV entries come after dots entries
nv=function(...,SEP=' ',EQUAL='=',PRETTY=FALSE,IGNORE=FALSE,NV=list()) {
    dots=match.call(expand.dots=FALSE)$...;
    if (is.null(dots)&&length(NV)==0) return('');
    err.ok=if(is_logical(IGNORE)) IGNORE else TRUE;
    err.skip=is_logical(IGNORE);
    err.val=if(is_logical(IGNORE)) NA else IGNORE;
    names=names(dots);
    if (is.null(names)) names=rep('',length(dots));
    ## un-named dots are simple name=value 
    unnamed=(nchar(names)==0);
    if (!all(sapply(dots[unnamed],function(x) is_simple(x))))
      stop("unnamed elements of ... must be names or character strings");
    dots[unnamed]=sapply(dots[unnamed],as.name);
    ## use each dot as it's own name, eg, 'x' becomes 'x=x'
    names[unnamed]=sapply(dots[unnamed],as.character)
    parent.env=parent.frame(n=1);
    bad=NULL;
    values=sapply(seq_along(dots),function(i)
      tryCatch(eval(dots[[i]],parent.env), error=function(c) 
        if (err.ok) {bad<<-c(bad,i); err.val}
        else {
          c$message=paste0("Unable to evaluate '",deparse(dots[[i]]),"': ",c$message);
          c$call='nv';
          stop(c);
        }
        ));
    ## tack on names. values from NV
    ## names=c(names,sapply(NV,function(nv) if (length(nv)==1) '' else nv[1]));
    names=c(names,sapply(NV,function(nv) nv[1]));
    values=c(values,sapply(seq_along(NV),function(i) {
      nv=NV[[i]];
      if (length(nv)!=1) nv[-1]
      else {
        name=nv;
        tryCatch(get(name,parent.env),error=function(c) 
          if (err.ok) {bad<<-c(bad,i+length(dots)); err.val}
          else {
            c$message=paste0("Unable to evaluate '",name,"': ",c$message);
            c$call='nv';
            stop(c);
          })
      }}));
    if (!is.null(bad)&err.skip) {
      names=names[-bad];
      values=values[-bad];
    }
    values=sapply(seq_along(values),function(i) {
      name=names[i];
      value=unlist(values[i]);
      if (is.function(PRETTY)) value=sapply(value,function(value) pretty(name,value));
      if (length(value)>1) paste0('c(',paste(collapse=',',value),')') else value;
    });
    ## if (is.function(PRETTY)) values=PRETTY(names,values)
    paste(collapse=SEP,paste(sep=EQUAL,names,values));
}


