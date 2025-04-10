test_that("check package verion", {
    
    expect_true(utils::packageVersion("redData") == "1.1.5")
})

test_that("check 'AED'", {
    
    load("data-v0.0.4/AED.rda")
    dat_v0.0.4 <- AED; rm(AED)
    
    data("AED", package = "redData", lib.loc = Sys.getenv("R_LIBS_USER"), envir = environment())
    dat_v1.1.5 <- AED %>% filter(year < 2016) %>% filter(!is.na(density)); rm(AED)
    
    dfr1 <- dat_v0.0.4 %>% group_by(zone) %>% summarise(n_0.0.4 = n())
    dfr2 <- dat_v1.1.5 %>% group_by(zone) %>% summarise(n_1.1.5 = n()) 
    dfr <- full_join(dfr1, dfr2, by = "zone")
    dfr$match <- dfr$n_0.0.4 == dfr$n_1.1.5
    expect_false(nrow(filter(dfr, !match | is.na(match))) > 0)
})

test_that("check number of surveys per site (forest)", {
    
    load("data-v0.0.4/forest.rda")
    
    # accent not removed from "Mont Tingui and Kinkené" 
    # leading to duplication of zone
    dat$zone[grep("Mont Tingui", dat$zone)] <- "Mont Tingui and Kinkene"
    
    dat_v0.0.4 <- dat; rm(dat, forest)
    
    data("forest", package = "redData", lib.loc = Sys.getenv("R_LIBS_USER"), envir = environment())
    dat_v1.1.5 <- dat; rm(dat, forest)
    
    dfr1 <- dat_v0.0.4 %>% group_by(zone) %>% summarise(n_0.0.4 = n())
    dfr2 <- dat_v1.1.5 %>% group_by(zone) %>% summarise(n_1.1.5 = n()) 
    dfr <- full_join(dfr1, dfr2, by = "zone")
    dfr$match <- dfr$n_0.0.4 == dfr$n_1.1.5
    expect_false(nrow(filter(dfr, !match | is.na(match))) > 0)
})

test_that("check survey data (forest)", {
    
    load("data-v0.0.4/forest.rda")
    dat_v0.0.4 <- data.frame(zone = rownames(forest$survey_area)[forest$XS], y = forest$y); rm(dat, forest)
    
    # accent not removed from "Mont Tingui and Kinkené" 
    # leading to duplication of zone
    dat_v0.0.4$zone[grep("Mont Tingui", dat_v0.0.4$zone)] <- "Mont Tingui and Kinkene"
    
    data("forest", package = "redData", lib.loc = Sys.getenv("R_LIBS_USER"), envir = environment())
    dat_v1.1.5 <- data.frame(zone = rownames(forest$survey_area)[forest$XS], y = forest$y); rm(dat, forest)
    
    dfr1 <- dat_v0.0.4 %>% group_by(zone) %>% summarise(y = sum(y)) %>% rename(y_0.0.4 = y)
    dfr2 <- dat_v1.1.5 %>% group_by(zone) %>% summarise(y = sum(y)) %>% rename(y_1.1.5 = y) 
    
    dfr <- full_join(dfr1, dfr2, by = "zone")
    dfr$match <- dfr$y_0.0.4 == dfr$y_1.1.5
    expect_false(nrow(filter(dfr, !match | is.na(match))) > 0)
})

test_that("check number of surveys per site (savannah)", {
    
    load("data-v0.0.4/savannah.rda")
    dat_v0.0.4 <- dat; rm(dat, savannah)
    
    data("savannah", package = "redData", lib.loc = Sys.getenv("R_LIBS_USER"), envir = environment())
    dat_v1.1.5 <- dat; rm(dat, savannah)
    
    dfr1 <- dat_v0.0.4 %>% group_by(zone) %>% summarise(n_0.0.4 = n())
    dfr2 <- dat_v1.1.5 %>% group_by(zone) %>% summarise(n_1.1.5 = n()) 
    dfr <- full_join(dfr1, dfr2, by = "zone")
    dfr$match <- dfr$n_0.0.4 == dfr$n_1.1.5
    expect_false(nrow(filter(dfr, !match | is.na(match))) > 0)
})

test_that("check survey data (savannah)", {
    
    load("data-v0.0.4/savannah.rda")
    dat_v0.0.4 <- data.frame(zone = rownames(savannah$survey_area)[savannah$XS], y = savannah$y); rm(dat, savannah)
    
    # accent not removed from "Mont Tingui and Kinkené" 
    # leading to duplication of zone
    dat_v0.0.4$zone[grep("Mont Tingui", dat_v0.0.4$zone)] <- "Mont Tingui and Kinkene"
    
    data("savannah", package = "redData", lib.loc = Sys.getenv("R_LIBS_USER"), envir = environment())
    dat_v1.1.5 <- data.frame(zone = rownames(savannah$survey_area)[savannah$XS], y = savannah$y); rm(dat, savannah)
    
    dfr1 <- dat_v0.0.4 %>% group_by(zone) %>% summarise(y = sum(y)) %>% rename(y_0.0.4 = y)
    dfr2 <- dat_v1.1.5 %>% group_by(zone) %>% summarise(y = sum(y)) %>% rename(y_1.1.5 = y) 
    
    dfr <- full_join(dfr1, dfr2, by = "zone")
    dfr$match <- dfr$y_0.0.4 == dfr$y_1.1.5
    expect_false(nrow(filter(dfr, !match | is.na(match))) > 0)
})

