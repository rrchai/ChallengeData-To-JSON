if (!suppressWarnings(require("pacman", character.only = TRUE))) {
  install.packages("pacman", repos = "https://cran.r-project.org/")
}

pkg_list <- c("tidyverse", "readr", "jsonlite", "googlesheets4", "reticulate")
pacman::p_load(pkg_list, character.only = TRUE)

use_condaenv("rocc-service", required = TRUE)
source_python("mongoIdMaker.py")

cleanProperty <- function(property, type = "other") {
  if (type == "name") {
    clean_x <-
      lapply(property, function(x) {
        trimws(x, "both") %>%
          gsub("[[:punct:]]", "", .)
      })
  } else {
    clean_x <-
      str_split(property, ",") %>%
      lapply(., function(x) {
        trimws(gsub("[^[:alnum:]]+", "-", x), whitespace = "-") %>%
          tolower()
      })
  }

  return(clean_x)
}
