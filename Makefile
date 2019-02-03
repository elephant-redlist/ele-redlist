# Makefile for generating the stanUtils R package
#
PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)
R_FILES := $(wildcard R/*.R)
STAN_FILES := $(wildcard stan/*.stan)
PKG_FILES := DESCRIPTION NAMESPACE $(R_FILES)

ifeq ($(OS),Windows_NT) 
	RM = rm -rf
	CP = cp -f
	CD = cd
else
	RM = rm -rf
	CP = cp -f
	CD = cd
endif

all: install clean

./inst/doc/*.html: ./vignettes/*.Rmd
	Rcmd INSTALL --build .
	Rscript -e "library(devtools); build_vignettes()"
	
./data/*.RData: $(STAN_FILES)
	Rscript -e "reg01 <- readLines('./stan/regression_region.stan');      save(reg01, file = './data/reg01.RData')"
	Rscript -e "reg02 <- readLines('./stan/regression_region_pike.stan'); save(reg02, file = './data/reg02.RData')"
	Rscript -e "reg03 <- readLines('./stan/regression_global.stan');      save(reg03, file = './data/reg03.RData')"
	Rscript -e "reg04 <- readLines('./stan/regression_region_interaction.stan');      save(reg04, file = './data/reg04.RData')"
	Rscript -e "reg05 <- readLines('./stan/regression_region_interaction_pike.stan');      save(reg05, file = './data/reg05.RData')"

install: $(PKG_FILES) ./inst/doc/*.html ./data/*.RData
	Rcmd build --no-build-vignettes .
	Rcmd INSTALL --html .
	
DESCRIPTION NAMESPACE: $(R_FILES)
	Rscript version_update.R
	Rscript -e "library(devtools); document()"	

clean:
	$(RM) $(PKG_NAME)_*.zip
	$(RM) $(PKG_NAME)_*.tar.gz
	$(RM) man/
