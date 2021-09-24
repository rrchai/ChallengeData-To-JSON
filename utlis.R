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
