#################################################################################
##
## Author:  Nat Goodman
## Created: 20-07-13
##          from code in util_file.R created 20-05-03
##          from dat.R created 20-05-02
##          from frecl/R/dat.R created 20-01-17
##          from misg/R/datman.R created 19-01-01
##          from repwr/R/datman.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## Save and load utilities
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## ---- Save and Load ----
## save data in RData and optionally txt formats
save_=
  function(data,base=NULL,save=NA,save.RData=save,save.txt=FALSE,file=NULL,obj.ok=TRUE,
           pjto=FALSE) {
    if (is.null(base)&&is.null(file))
      stop("No place to save data: 'base' and 'file' are both NULL");
    if (is.null(file)) {
      ## save RData if args allow
      file=filename(base=base,suffix='RData');
      if (ok_to_save(save.RData,file)) save_RData_(data,file=file,pjto=pjto);
      ## save txt if args allow
      file=filename(base=base,suffix='txt');
      if (ok_to_save(save.txt,file)) save_txt_(data,file=file,obj.ok=obj.ok,pjto=pjto);
    } else {
      ## save RData or txt depending on suffix or flags
      sfx=suffix(file);
      if (sfx=='RData'&&ok_to_save(save.RData,file)) save_RData_(data,file=file,pjto=pjto)
      else if (sfx=='txt'&&ok_to_save(save.txt,file))
        save_txt_(data,file=file,obj.ok=obj.ok,pjto=pjto)
      else {
        ## suffix not informative. use 'save' flags to decide format
        if (!is.na(save.RData)&save.RData) save_RData_(data,file=file,pjto=pjto)
        else if (!is.na(save.txt)&save.txt) save_txt_(data,file=file,obj.ok=obj.ok,pjto=pjto)
        else stop("Unable to determine output format from file name or 'save' flags: ",
                  nv(file,save,save.txt));
      }
    }
  }
ok_to_save=function(save,file) (is.na(save)&!file.exists(file))|(!is.na(save)&save)
                  
save_RData_=function(data,file,pjto=FALSE) {
  save(data,file=file);
  if (pjto) system(paste('pjto',file)); # copy to Mac if desired
  file;
}
save_txt_=function(data,file,obj.ok=TRUE,pjto=FALSE) {
  if (is_2d(data)) write.table(data,file=file,sep='\t',quote=FALSE,row.names=FALSE)
  else {
    if (!obj.ok) stop("Trying to save generic object but obj.ok=FALSE")
    if (is.list(data)) {
      sink(file);
      print(data);
      sink();
    } else if (is.vector(data)) {
          names=names(data);
          if (!is.null(names)) {
            data=data.frame(name=names,value=as.character(data));
            write.table(data,file=file,sep='\t',quote=FALSE,row.names=FALSE);
          } else writeLines(as.character(data),file);
    }
    else stop(paste('Unable to save text for class',class(data),'. Sorry'));
  }
  if (pjto) system(paste('pjto',file)); # copy to Mac if desired
  file;
}
## load data from RData file
load_=function(base=NULL,file=NULL) {
  if (is.null(base)&&is.null(file)) stop("No place to get data: 'base' and 'file' are both NULL");
  if (is.null(file)) file=resuffix(base,suffix='RData');
  what=load(file=file);                 # what is name of saved data
  get(what);                            # return it
}
## read data from txt file - mostly for testing - not guaranteed to work
read_=
  function(base=NULL,file=NULL,stringsAsFactors=FALSE,check.names=FALSE,...) {
    if (is.null(base)&&is.null(file))
      stop("No place to get data: 'base' and 'file' are both NULL");
    if (is.null(file)) file=resuffix(base,suffix='txt');
    read.delim(file,stringsAsFactors=stringsAsFactors,check.names=check.names,...);
}
