### This script is to convert challenge data to json format following the ROCC schema
### The following json files may use mock up property for development purpose:
### 1. organizations.json

source("utlis.R")
source("config.R")

options(gargle_oauth_email = your_email_address) # for googlesheet
overwrite <- TRUE

dir.create("seedData", showWarnings = FALSE)

## Load Challenge Landscape sheet
meta <- googlesheets4::read_sheet(lanscape_url, sheet = "challenges") %>%
  mutate(across(everything(), as.character)) %>% 
  janitor::remove_empty(which = "rows")

# read fix csv
if (file.exists("validation.csv")) {
  fix <- readr::read_csv("validation.csv", col_types = cols(.default = "c")) %>% janitor::remove_empty(which = "rows")
} else {
  fix <- data.frame(matrix(ncol = 3, nrow = 0)) %>% `colnames<-`(c("invalid", "valid", "type"))
}

#### Topics ####
topics <- cleanProperty(meta$challengeKeywords)

#### Organizations ####
orgs <- googlesheets4::read_sheet(lanscape_url, sheet = "organizations", col_types = "ccc")
# create mongoIds
org_ids <- replicate(nrow(orgs), mongoIdMaker())

## validate orgs length
# fix if there are existing corrections
if (nrow(fix) > 0) {
  items <- fix %>% filter(type == "org_name")
  makeQuiet(
    lapply(1:nrow(items), function(i) {
      item <- items[i, ]
      orgs$challengeOrganization <<- gsub(item$invalid, item$valid, orgs$challengeOrganization, fixed = TRUE)
    })
  )
}

org_invalid <- orgs$challengeOrganization[which(nchar(orgs$challengeOrganization) > 60)]; org_invalid

if (length(org_invalid) > 0) {
  fix <- rbind(fix, data.frame(invalid=org_invalid, valid=c(""), type="org_name"))
  write_csv(fix, "validation.csv")
  stop(sQuote(org_invalid[1]), " > schema max length (60) :\n")
}

orgs_df <- data.frame(id=org_ids,
                      login=cleanProperty(orgs$challengeOrganization) %>% unlist,
                      name=orgs$challengeOrganization,
                      description=c("This is an awesome organization"),
                      email=c("contact@example.org"),
                      websiteUrl=orgs$url,
                      avatarUrl= paste0("https://github.com/Sage-Bionetworks/rocc-app/raw/main/images/logo/", 
                                        org_logins,
                                        ".png")
                      ) %>% arrange(name)

orgs_json <- toJSON(list(organizations=orgs_df), pretty = TRUE)
if (overwrite) write(orgs_json, "seedData/organizations.json")    

#### dataProviders ####
dataProviders_data <- cleanProperty(meta$dataContributors)
# if empty, convert to empty array
makeQuiet(
  lapply(seq_along(dataProviders_data), function(i) {
    if (is.na(dataProviders_data[[i]])) {
      dataProviders_data[[i]] <<- data.frame()
    }
  })
)

if (length(clean.orgId) > 0) {
  makeQuiet(
    lapply(seq_along(dirty.orgId), function(i) {
      lapply(seq_along(dataProviders_data), function(j) {
        lapply(seq_along(dataProviders_data[[j]]), function(k) {
          dataProviders_data[[j]][[k]] <<- gsub(dirty.orgId[i], clean.orgId[i], dataProviders_data[[j]][[k]])
        })
      })
    })
  )
}

#### Persons ####
# data was based on the mock json data and tidy up to a data frame
persons_data <- googlesheets4::read_sheet(lanscape_url, sheet = "persons", col_types = "cccccc")[,-1]

# remove challenge not in meta and reorder by meta's challenge name 
setdiff(persons_data$challengeName, meta$challengeName) 
# match meta orders 
persons_data <- persons_data %>% arrange(challengeName = factor(challengeName, levels = meta$challengeName))
# add validation for orgID if needed

# make fake personIDS for challenges
use_condaenv("rocc-service", required = TRUE)
source_python("mongoIdMaker.py")
set.seed(1111)

persons_data$id <- sapply(persons_data$firstName, function(i)
  ifelse(is.na(i), NA, idMaker() %>% mongoIdMaker())
  )
any(duplicated(na.omit(persons_data$id))) # check if duplicated by any chance

# prepare for challenge data - organizerIds
persons <- lapply(meta$challengeName, function(x) {
  persons_data$id[persons_data$challengeName == x] 
})
makeQuiet(
  lapply(seq_along(persons), function(i) {
    if (length(persons[[i]]) == 0) {
      persons[[i]] <<- data.frame()
    }
  })
)

# create person object json 
persons_df <- persons_data %>% 
  filter(!is.na(firstName)) %>% 
  mutate(firstName = trimws(firstName, "both"),
         lastName = trimws(lastName, "both")) %>%
  select(6, 2, 3, 5) 
persons_df$organizationIds <- I(cleanProperty(persons_df$organizationIds))
makeQuiet(
  lapply(1:nrow(persons_df), function(i) {
    if (is.na(persons_df$organizationIds[i])) {
      persons_df$organizationIds[i] <<- I(list(data.frame()))
    }
  })
)
persons.json <- toJSON(list(persons=persons_df), pretty = TRUE)
if (overwrite) write(persons.json, "seedData/persons.json")

# old  ----
# persons <- persons_data %>% select(1:3, 5) %>% nest_legacy(!challengeName, .key = "organizers")
# # if empty, change to empty data.frame, so toJSON later can convert it to an empty array
# makeQuiet(
#   lapply(1:nrow(persons), function(i) {
#     if (is.null(persons$organizers[[i]]) || is.na(persons$organizers[[i]]$firstName)) {
#       print(i)
#       persons$organizers[[i]] <<- I(data.frame())
#     }
#   })
# )
# old  ----

#### grants ####
# Only collected three for example
grants_data <- googlesheets4::read_sheet(lanscape_url, sheet = "grants", col_types = "ccccc")
# remove new line symbol
grants_data$description <- gsub("\n", " ", grants_data$description, fixed = TRUE) 
grants_data$grantId <- sapply(1:3, function(i) idMaker() %>% mongoIdMaker())
# reorder and only use id, name, description
grants_data <- grants_data[, c("grantId", "name", "description")]
colnames(grants_data)[1] <- "id"
grants.json <- toJSON(list(grants=grants_data), pretty = TRUE)
if (overwrite) write(grants.json, "seedData/grants.json")
# TODO: below code only work when challenge only has one grant, fix when we have more info
grants <- sapply(meta$challengeGrants, function(g) {
  name <- intersect(g, grants_data$name)
  if (length(name) > 0) return(grants_data$grantId[grants_data$name == name]) else list(data.frame())
  # })ifelse(meta$challengeGrants %in% grants_data$name, grants_data$grantId, list(data.frame()))
})
#### challenges ####
# trim summary to short descriptions for now
meta$challengeSummary <- gsub("\n", " ", meta$challengeSummary, fixed = TRUE) 
short_summary <- 
  ifelse(nchar(meta$challengeSummary) > 280, 
         paste0(substr(meta$challengeSummary, 1, 276), " ..."),
         meta$challengeSummary)

challenges.df <-
  data.frame(name = meta$challengeName,
             description = short_summary,
             summary = meta$challengeSummary,
             # if no dates aka NA, it will be excluded
             startDate = as.Date(meta$challengeStart, "%Y-%m-%d"), 
             endDate = as.Date(meta$challengeEnd, "%Y-%m-%d"),
             url = paste0("https://www.synapse.org/#!Synapse:", meta$challengeSite),
             status = meta$challengeStatus,
             tagIds = I(cleanProperty(meta$challengeKeywords)),
             organizerIds = I(persons),
             dataProviderIds = I(dataProviders_data),
             # empty for now
             grantIds = I(grants) 
  )
challenges.json <- prettify(toJSON(list(challenges=challenges.df), pretty = T), indent = 2)

if (overwrite) write(challenges.json, "seedData/challenges.json")

# cp file to rocc-app
if (overwrite) {
  system(paste0('cp seedData/tags.json ', path_to_save_json))
  system(paste0('cp seedData/organizations.json ', path_to_save_json))
  system(paste0('cp seedData/grants.json ', path_to_save_json))
  system(paste0('cp seedData/persons.json ', path_to_save_json))
  system(paste0('cp seedData/challenges.json ', path_to_save_json))
}
