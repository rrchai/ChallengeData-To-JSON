
source("utlis.R")

your_email_address <- "<your-email-addrerss>"
lanscape_url <- "<sheet url>"
path_to_save_json <- "/rocc-app/src/app/seeds/dream/"
options(gargle_oauth_email = your_email_address) # for googlesheet

overwrite <- FALSE
dir.create("seedData", showWarnings = FALSE)

## Load Challenge Landscape sheet
meta <- googlesheets4::read_sheet(lanscape_url, sheet = "challenges") %>%
  mutate(across(everything(), as.character))

#### tags ####
tags <- cleanProperty(meta$challengeKeywords) %>% unlist %>% unique
tags.json <- toJSON(list(tags=data.frame(id=tags, description="")), pretty = TRUE)

if (overwrite) write(tags.json, "seedData/tags.json")

#### Org ####
#create org id using tags method
orgs_data <- googlesheets4::read_sheet(lanscape_url, sheet = "organizations", col_types = "ccc")
orgs_data$shortName <- ifelse(orgs_data$shortName == "", NA, orgs_data$shortName)
orgs <- data.frame(id = cleanProperty(orgs_data$challengeOrganization) %>% unlist,
                   name=orgs_data$challengeOrganization,
                   url=orgs_data$url,
                   shortName=orgs_data$shortName
                   ) %>% arrange(name)

dirty.orgId <- orgs$id[which(nchar(orgs$id) > 60)]; dirty.orgId
# [1] "applied-proteogenomics-organizational-learning-and-outcomes-network"            
# [2] "eunice-kennedy-shriver-national-institute-of-child-health-and-human-development"
clean.orgId <- c("apollo-network", "eunice-kennedy-shriver-national-institute")
message("orgId length > schema max length (60) :\n", 
    paste0("change '", dirty.orgId, "' to '", clean.orgId, "'", collapse = "\n")
)

makeQuiet(
  lapply(seq_along(dirty.orgId), function(i) {
    orgs$id <<- gsub(dirty.orgId[i], clean.orgId[i], orgs$id, fixed = TRUE)
  })
)

orgs.json <- toJSON(list(organizations=orgs), pretty = TRUE)
if (overwrite) write(orgs.json, "seedData/organizations.json")    

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

dirty.orgId2 <- setdiff(unique(unlist(dataProviders_data)), orgs$id) %>% sort; dirty.orgId2
# [1] "eunice-kennedy-shriver-national-institute-of-child-health-and-human-development"
# [2] "applied-proteogenomics-organizational-learning-and-outcomes-network"
clean.orgId2 <- c("apollo-network", "eunice-kennedy-shriver-national-institute")
if (length(dirty.orgId2) > 0) {
  message("orgId in data dataProvider not match orgId in organizations:\n", 
          paste0("change '", dirty.orgId2, "' to '", clean.orgId2, "'", collapse = "\n")
  )
  makeQuiet(
    lapply(seq_along(dirty.orgId2), function(i) {
      lapply(seq_along(dataProviders_data), function(j) {
        lapply(seq_along(dataProviders_data[[j]]), function(k) {
          dataProviders_data[[j]][[k]] <<- gsub(dirty.orgId2[i], clean.orgId2[i], dataProviders_data[[j]][[k]])
        })
      })
    })
  )
}

#### Persons ####
# data was based on Verena's mock json data and tidy up to a data frame
persons_data <- googlesheets4::read_sheet(lanscape_url, sheet = "persons", col_types = "ccccc")
# remove challenge not in meta and reorder by meta's challenge name 
diff <- setdiff(persons_data$challengeName, meta$challengeName)
persons_data <- persons_data %>% 
  filter(challengeName != diff) %>%
  arrange(challengeName = factor(challengeName, levels = meta$challengeName))

# dirty.orgId3 <- setdiff(unique(persons_data$organizationIds), orgs$id) %>% sort; dirty.orgId3

# make fake personIDS for challenges
use_condaenv("rocc-service", required = TRUE)
source_python('mangoIdMaker.py')
persons_data$organizerId <- sapply(persons_data$organizationIds, function(i)
  ifelse(is.na(i), NA, idMaker() %>% mongoIdMaker())
  )
any(duplicated(na.omit(persons_data$organizerId))) # check if duplicated by any chance

# prepare for challenge data - organizerIds
persons <- lapply(meta$challengeName, function(x) {
  persons_data$organizerId[persons_data$challengeName == x] 
})
makeQuiet(
  lapply(seq_along(persons), function(i) {
    if (is.na(persons[[i]])) {
      persons[[i]] <<- data.frame()
    }
  })
)

# save as json in order of meta's challenge name
persons_df <- persons_data %>% 
  filter(!is.na(firstName)) %>% 
  select(6, 2, 3, 5) 

persons.json <- toJSON(list(persons=persons_df), pretty = TRUE)
if (overwrite) write(persons.json, "seedData/persons-fakeIds.json")

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
grants.json <- toJSON(list(grants=grants_data), pretty = TRUE)
if (overwrite) write(grants.json, "seedData/grants-fakeIds.json")
# TODO: below code only work when challenge only has one grant, fix when we have more info
grants <- ifelse(meta$challengeGrants %in% grants_data$name, grants_data$grantId, list(data.frame()))

#### challenges ####
# trim summary to short descriptions for now
meta$challengeSummary <- gsub("\n|/", " ", meta$challengeSummary, fixed = TRUE) 
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

if (overwrite) write(challenges.json, "seedData/challenges-fakeIds.json")

# cp file to rocc-app
if (overwrite) {
  system(paste0('cp seedData/tags.json ', path_to_save_json))
  system(paste0('cp seedData/organizations-fix.json ', path_to_save_json))
  system(paste0('cp seedData/grants-fakeIds.json ', path_to_save_json))
  system(paste0('cp seedData/persons-fakeIds.json ', path_to_save_json))
  system(paste0('cp seedData/challenges-fakeIds.json ', path_to_save_json))
}
