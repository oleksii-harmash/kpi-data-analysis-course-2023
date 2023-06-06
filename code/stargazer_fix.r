# Delete the stargazer package if it's installed
if ("stargazer" %in% installed.packages()) {
  remove.packages("stargazer")
}

# Download the source
download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")

# Unpack
untar("stargazer_5.2.3.tar.gz")

# Read the source file with .inside.bracket fun
stargazer_src <- readLines("stargazer/R/stargazer-internal.R")

# Move the length check 5 lines up so it precedes is.na(.)
stargazer_src[1990] <- stargazer_src[1995]
stargazer_src[1995] <- ""

# Save back
writeLines(stargazer_src, con = "stargazer/R/stargazer-internal.R")

# Compile and install the patched package
install.packages("stargazer", repos = NULL, type = "source")