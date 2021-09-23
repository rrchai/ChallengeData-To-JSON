if(!suppressWarnings(require("pacman", character.only = TRUE))) {
  install.packages("pacman", repos = "https://cran.r-project.org/")
}

pkg_list <- c("tidyverse", "readr", "jsonlite", "googlesheets4", "reticulate")
pacman::p_load(pkg_list, character.only = TRUE)


cleanProperty <- function(property, collapse=FALSE) {
  
  clean_x <- 
    str_split(property, ",") %>% 
    lapply(., function(x) {
      trimws(gsub("[^[:alnum:]]+", "-", x), whitespace="-") %>%
        tolower()
    })
  
  if (collapse) {
    clean_x <- sapply(clean_x, FUN=paste0, collapse = ",")
  }
  
  return(clean_x)
}


maxLength <- function(x, sep = ",") {
  x %>% str_split(sep) %>% lengths(.) %>% max
}


makeQuiet <- function(expr) { 
  invisible(capture.output(suppressMessages(suppressWarnings(expr)))) 
} 

idMaker <- function(n = 12)
{
  char <- sample(c(letters, 0:9), n, replace=T)
  ids <- paste0(char, collapse = "") 
  return(ids)
}


