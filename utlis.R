if(!suppressWarnings(require("pacman", character.only = TRUE))) {
  install.packages("pacman", repos = "https://cran.r-project.org/")
}

pkg_list <- c("tidyverse", "readr", "jsonlite", "readr", "googlesheets4", "reticulate", "reshape2")
pacman::p_load(pkg_list, character.only = TRUE)

use_condaenv("rocc-service", required = TRUE)
source_python("mongoIdMaker.py")

cleanProperty <- function(property, type = "other") {
  if (type == "name") {
    clean_x <-
      lapply(property, function(x) {
        split_name <- trimws(x, "both") %>%
          gsub("[[:punct:]]", "", .) %>%
          tolower() %>%
          str_split(" ") %>%
          unlist()
        n <- length(split_name)  
        first_name <- str_c(sapply(split_name[-n], function(i) substring(i, 1, 1)), collapse = "")
        last_name <- split_name[n]
        new_name <- str_c(first_name, last_name, collapse = "")
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
