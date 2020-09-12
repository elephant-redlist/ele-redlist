
require(plyr)
require(reshape2)

label <- function(x, upper = FALSE) {

	region <- c("east", "forest", "north", "south", "savannah", "global")
	
	label <- region[match(tolower(x), region)]

	ifelse(upper, toupper(label), label)
}

region_label <- function(x) {
  switch(x,
         "N" = "savannah",
         "S" = "savannah",
         "E" = "savannah",
         "forest" = "forest",
         as.character(NA))
         
}

################

region       <- "GLOBAL"
data_stamp   <- "080519"

message("Preparing data for ", label(region, TRUE), " region")

# prepare census data

dat <- read.csv(paste0(label(region, TRUE), "_AED_1979_2016_FINAL_", data_stamp,".csv"))[, c('flags', 'species', 'new_region', 'country', 'input_zone2', 'type', 'reliability', 'year', 'estimate', 'survey_area')]

# check region
message("Found regions: ", paste(unique(dat$new_region), collapse = ", "))
dat$region <- sapply(dat$new_region, region_label)
dat$new_region <- NULL
message("... converted to: ", paste(unique(dat$region), collapse = ", "))

# if global then country may have >1 species
if (region == "GLOBAL") {
	dat$country <- paste0(dat$country, " (", dat$species, ")")
}
dat$species <- NULL

# if any "max" then all surveys flagged as survey_area_shrinkage = TRUE
tmp <- ddply(dat, .(input_zone2), summarize, survey_area_shrinkage = any(flags == "max"))
dat <- merge(dat, tmp, by = "input_zone2")
dat$flags <- NULL

# only use data up to 2015
dat <- subset(dat, year < 2016)

# coerce data
dat$year        <- as.integer(as.character(dat$year))
dat$estimate    <- as.numeric(as.character(dat$estimate))
dat$survey_area <- as.numeric(as.character(dat$survey_area))
dat$country     <- trimws(as.character(dat$country))
dat$zone        <- trimws(as.character(dat$input_zone2))
dat$reliability <- trimws(as.character(dat$reliability))

dat$input_zone2 <- NULL

dat$density <- dat$estimate / dat$survey_area

save(dat, file = paste0(label(region, TRUE), "_AED_1979_2016_raw.rda"))

# write csv
write.csv(dat, file = paste0(label(region, TRUE), "_AED_1979_2016_raw.csv"), row.names = FALSE)

# clean data and create summary
rmarkdown::render("clean_data.Rmd", output_file = "data_summary.html", output_dir = file.path("../inst/doc"))


