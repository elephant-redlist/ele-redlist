# Makefile for generating the red R package
#
PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)
R_FILES := $(wildcard R/*.R)
STAN_FILES := $(wildcard stan/*.stan)
PKG_FILES := DESCRIPTION NAMESPACE $(R_FILES) $(STAN_FILES)

ifeq ($(OS),Windows_NT) 
	RM = rm -rf
	CP = cp -f
	CD = cd
else
	RM = rm -rf
	CP = cp -f
	CD = cd
endif

#all: document install clean

#./inst/doc/*.html: ./vignettes/*.Rmd
#	$(CD) vignettes; Rcmd Sweave forest.Rmd; Rcmd Sweave savannah.Rmd
#	$(CP) ./vignettes/*.html ./inst/doc/
    
install: $(PKG_FILES)
	$(RM) $(PKG_NAME)_*.tar.gz
	Rcmd build --no-build-vignettes .
	Rcmd INSTALL $(PKG_NAME)_*.tar.gz
    
#document:
#    Rscript -e "roxygen2::roxygenise()"
	
DESCRIPTION NAMESPACE: $(R_FILES)
	Rscript -e "roxygen2::roxygenise()"
	Rscript version_update.R

clean:
	$(RM) $(PKG_NAME)_*.zip
	$(RM) $(PKG_NAME)_*.tar.gz
	$(RM) man/
	$(RM) vignettes/*.html
	$(RM) vignettes/*.R
