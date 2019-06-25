library(dplyr)
library(usethis)

all_sex <- read.delim("../all_sex.tab", stringsAsFactors = FALSE)
rep_het <- calc_reported_het(all_sex)
use_data(rep_het, overwrite = TRUE)
