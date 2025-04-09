# Makefile for generating the red R package

PKGNAME = `sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGVERS = `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`

.PHONY: all

all: install

build: install_deps
	R CMD build .

install_deps: install_man
	Rscript \
	-e 'if (!requireNamespace("remotes")) install.packages("remotes")' \
	-e 'remotes::install_deps(dependencies = TRUE)'
    
install_man:
	Rscript \
	-e 'if (!requireNamespace("roxygen2")) install.packages("roxygen2")' \
	-e 'roxygen2::roxygenize()'

install: build
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

