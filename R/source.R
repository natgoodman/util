#################################################################################
##
## Author:  Nat Goodman
## Created: 20-10-25
##          from covid/R/source.R created 20-05-03
##          from frecl/R/source.R created20-01-14
##          from bayez/source.R created 19-05-22
##          from run.R created 19-02-18
##          from ovrfx.R created 19-02-03 
##          from siglo.R created 19-01-01
##          from repwr/R/repwr.R created 17-10-05 
##           and repwr/R/sim.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## Source files for util repo
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################

## source 'cqcl' first to get 'cq' - needed by application source.R
## HERE code from stackoverflow.com/questions/13645106. Thx!
HERE=(function() getSrcFilename(sys.call(sys.nframe()),full.names=TRUE))()
UTILDIR=dirname(HERE);                  # must do dirname in separate statement for some reason
source(file.path(UTILDIR,'cqcl.R'))

## ---- source the files ----
## source default files. assume README doc until init runs
source_files=function(files=SOURCE) {
  sapply(files,source);
  invisible();
}
## ## source doc-specific files
## source_doc=function(doc=param(doc)) {
##   docr=paste(sep='.',doc,'R');
##   source_ifexists(paste(sep='_','R/dat',docr));
##   source_ifexists(paste(sep='_','R/doc',docr));
##   source_ifexists(paste(sep='_','R/docfun',docr));
##   source_ifexists(paste(sep='_','R/plot',docr));
##   source_ifexists(paste(sep='_','R/stats',docr));
## }
## source optional files
source_ifexists=function(file) if (file.exists(file)) source(file);
                         
## source all files
## NG 19-09-10: can't call param(doc) in empty workspace - param.env doens't exist
## source_all=function(files=SOURCE,doc=param(doc)) {
source_all=function(SOURCE=SOURCE,UTIL=UTIL) {
  files=c(sapply(SOURCE,function(src) file.path('R',paste0(src,'.R'))),
          sapply(UTIL,function(util) file.path(UTILDIR,paste0(util,'.R'))));
  source_files(files);
  ## source dat_XXX, doc_XXX files so top level functions defined
  ## NOTE: these top level functions call init which re-sources doc-specific files
  ## source_files(list.files('R',pattern='^(doc_|dat_).*.R',full.names=T));
  ## if (exists('param.env')) source_doc();
}
## application has to run source_all()
## source_all();
