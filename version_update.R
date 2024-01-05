# Get Version and date
VERSION <- scan('DESCRIPTION',what = character(),skip = 3,nlines = 1)[2]
VERSION <- nzbd::versionUpdate(VERSION, "0.0.1")
DATE    <- Sys.Date()
TIME    <- Sys.time()

# update DESCRIPTION
DESCRIPTION    <- readLines('DESCRIPTION')
DESCRIPTION[4] <- paste('Version:', VERSION)
DESCRIPTION[5] <- paste('Date:', DATE)
writeLines(DESCRIPTION, 'DESCRIPTION')
rm(DESCRIPTION)

# Write .onAttach
filename <- "R/zzz.R"
cat(".onAttach <- function(libname, pkgname)\n", file = filename)
cat("{\n", file = filename, append = TRUE)
cat(paste("    packageStartupMessage(\"red version ", VERSION, " (", DATE, ")\")\n", sep = ""), file = filename, append = TRUE)
cat("}\n", file = filename, append = TRUE)
rm(filename)

# run roxygen
roxygen2::roxygenise()
