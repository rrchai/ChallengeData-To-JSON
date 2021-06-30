if(!suppressWarnings(require("pacman", character.only = TRUE))) {
  install.packages("pacman", repos = "https://cran.r-project.org/")
}

pkg_list <- c("tidyverse", "readr", "jsonlite", "readr", "googlesheets4", "reticulate")
pacman::p_load(pkg_list, character.only = TRUE)


cleanProperty <- function(property, collapse=FALSE) {
  
  clean_x <- 
    str_split(property, ",") %>% 
    lapply(., function(x) {
      trimws(x, "both") %>% 
        tolower() %>%
        gsub(" ", "-", .) %>%
        gsub("\\.|\\'|\\&", "", .) %>%
        gsub("--", "-", .)
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


## Dummy codes that used to extract persons information from mock data

# mock_df <- as.data.frame(fromJSON("mock-challenges.json", simplifyDataFrame = T))
# mock_df <- prettify(toJSON(mock_df), indent = 2);mock_df
# if (overwrite) write(mock_df, "mock-persons.json")
# 
# # add couple challenges + match challenge names
# person_meta_raw <- as.data.frame(fromJSON("mock-persons-added.json", simplifyDataFrame = T))[, 1:2] 
# 
# # clean up the organizations and names
# # 1. convert to challenge-name-org meta data frame
# person_meta_df <- 
#   sapply(1:nrow(person_meta_raw), function(i){
#     dream <- person_meta_raw$name[i]
#     org <- person_meta_raw$organizers[[i]]
#     if(nrow(org) == 0) {
#       new.org <- NULL
#     } else {
#       # remove I() class
#       org[["organizations"]] <- sapply(org[["organizations"]], function(j) {
#         j %>% unlist() %>% paste0(., collapse = ",")
#       })
#       suppressWarnings(
#         # split organizations for the same names
#         new.org <- org %>% separate(organizations, paste0("org", 1:maxLength(.$organizations)), sep = ",") %>%
#           # reshape org to multiple rows in df
#           reshape2::melt(id.vars = c("first_name", "last_name"), value.name = "organizations", na.rm = TRUE) %>%
#           select(-variable)
#       )
#     }
#     new.org$challenge <- dream
#     return(new.org)
#   }) %>% bind_rows()
# 
# # c2. lean up the organization to match the organizations.json
# # TODO: add function that has warning if there is mismatched names detected b
# g1 <- c("NIH/NCI",
#         "CTD2",
#         "Mount Sinai School of Medicine",
#         "Evidation",
#         "Merck",
#         "University of Rochester Medical Center",
#         "University of North Carolina",
#         "University of North Carolina at Chapel Hill at Chapel Hill",
#         "Ohio State University Comprehensive Cancer Center",
#         "\\&")
# g2 <- c("National Institutes of Health",
#         "Cancer Target Discovery And Development",
#         "Icahn School of Medicine at Mount Sinai",
#         "Evidation Health",
#         "Merck Co.",
#         "University of Rochester",
#         "University of North Carolina at Chapel Hill",
#         "University of North Carolina at Chapel Hill",
#         "Ohio State University",
#         "and")
# for (i in seq_along(g1)) {
#   person_meta_df$organizations <- gsub(g1[i], g2[i], person_meta_df$organizations)
# }
# any(!na.omit(unique(person_meta_df$organizations)) %in% orgs$name)  # FALSE
# # add orgID
# person_meta_df$organizationId <- orgs$organizationId[match(person_meta_df$organizations, orgs$name)]
# # 3. clean up names
# # TODO: replace with a function with regrex to clean up name
# person_meta_df$first_name <- gsub("S. Louis Bridges", "Louis", person_meta_df$first_name)
# person_meta_df$last_name <- gsub("Jr.", "Bridges", person_meta_df$last_name)
# 
# for (name in c("first_name", "last_name")) {
#   person_meta_df[[name]] <-
#     person_meta_df[[name]] %>%
#     strsplit(., " ") %>% 
#     sapply(., "[", 1)
# }
# 
# # 4. collapse organizationId for same person and same challenge
# # correct column names to match person schema
# colnames(person_meta_df)[2:3] <- c("firstName", "lastName")
# # save df
# if (overwrite) write_csv(person_meta_df, "persons.csv", na = "")
# 
# person_meta_df <-
#   person_meta_df %>% 
#   group_by(challenge, firstName, lastName) %>% 
#   summarise(organizationIds = I(list(organizationId))) %>%
#   ungroup

# save persons to json for seeding persons independently
# reorder to match challenge data's order
# any(!new_meta$challenge %in% person_meta_df$challenge) # FALSE
# persons.df <- merge(data.frame(challenge=new_meta$challengeName), person_meta_df, 
#                     by="challenge", all=F, sort = F)
# persons.json <- prettify(toJSON(list(persons=persons.df)), indent = 2);persons.json
# 
# # do not change orders
# if (overwrite) write(persons.json, "persons.json")    

# 5. collapse persons info for each challenge
# Note: Not assume they are the same person for whom has same names but different challenges
#   })