test_that("check 'AED'", {
    
    load("data-v0.0.4/AED.rda")
    dat_v0.0.4 <- AED; rm(AED)
    
    data("AED", package = "redData", lib.loc = Sys.getenv("R_LIBS_USER"))
    dat_v1.1.5 <- AED %>% filter(year < 2016) %>% filter(!is.na(density))
    
    dfr1 <- dat_v0.0.4 %>% group_by(zone) %>% summarise(n_0.0.4 = n())
    dfr2 <- dat_v1.1.5 %>% group_by(zone) %>% summarise(n_1.1.5 = n()) 
    dfr <- full_join(dfr1, dfr2, by = "zone")
    dfr$match <- dfr$n_0.0.4 == dfr$n_1.1.5
    expect_false(nrow(filter(dfr, !match)) > 0)
})

test_that("check 'forest'", {
    
    load("data-v0.0.4/forest.rda")
    dat_v0.0.4 <- dat; rm(dat)
    
    data("forest", package = "redData", lib.loc = Sys.getenv("R_LIBS_USER"))
    dat_v1.1.5 <- dat
    
    dfr1 <- dat_v0.0.4 %>% group_by(zone) %>% summarise(n_0.0.4 = n())
    dfr2 <- dat_v1.1.5 %>% group_by(zone) %>% summarise(n_1.1.5 = n()) 
    dfr <- full_join(dfr1, dfr2, by = "zone")
    dfr$match <- dfr$n_0.0.4 == dfr$n_1.1.5
    expect_false(nrow(filter(dfr, !match)) > 0)
})

test_that("check 'savannah'", {
    
    load("data-v0.0.4/savannah.rda")
    dat_v0.0.4 <- dat; rm(dat)
    
    data("savannah", package = "redData", lib.loc = Sys.getenv("R_LIBS_USER"))
    dat_v1.1.5 <- dat
    
    dfr1 <- dat_v0.0.4 %>% group_by(zone) %>% summarise(n_0.0.4 = n())
    dfr2 <- dat_v1.1.5 %>% group_by(zone) %>% summarise(n_1.1.5 = n()) 
    dfr <- full_join(dfr1, dfr2, by = "zone")
    dfr$match <- dfr$n_0.0.4 == dfr$n_1.1.5
    expect_false(nrow(filter(dfr, !match)) > 0)
})



