
data("dat_global",   package = "redData", lib.loc = Sys.getenv("R_LIBS_USER"))
data("dat_forest",   package = "redData", lib.loc = Sys.getenv("R_LIBS_USER"))
data("dat_savannah", package = "redData", lib.loc = Sys.getenv("R_LIBS_USER"))

test_that("check 'global' surveys", {
    
    # number of surveys
    expect_equal(nrow(dat_global), 1325)
})

test_that("check 'global' sites", {
    
    # number of sites
    expect_equal(length(unique(dat_global$zone)), 475)
})

test_that("check 'global' countries", {
    
    # number of countries 
    # (4 countries are duplicated because they contain both species)
    expect_equal(length(unique(dat_global$country)), 37 + 4)
})

test_that("check 'global' regions", {
    
    # number of regions
    expect_equal(length(unique(dat_global$subregion)), 4)
})

test_that("check 'forest' surveys", {
    
    # number of surveys
    expect_equal(nrow(dat_forest), 350)
})

test_that("check 'forest' sites", {
    
    # number of sites
    expect_equal(length(unique(dat_forest$zone)), 150)
})

test_that("check 'forest' countries", {

    # number of countries
    expect_equal(length(unique(dat_forest$country)), 18)
})

test_that("check 'forest' regions", {

    # number of regions
    expect_equal(length(unique(dat_forest$subregion)), 1)
})

test_that("check 'savannah' surveys", {
    
    # number of surveys
    expect_equal(nrow(dat_savannah), 975)
})

test_that("check 'savannah' sites", {
    
    # number of sites
    expect_equal(length(unique(dat_savannah$zone)), 325)
})

test_that("check 'savannah' countries", {
    
    # number of countries
    expect_equal(length(unique(dat_savannah$country)), 23)
})

test_that("check 'savannah' regions", {
    
    # number of countries
    expect_equal(length(unique(dat_savannah$subregion)), 3)
})

