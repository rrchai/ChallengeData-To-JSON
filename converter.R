
source("utlis.R")

your_email_address <- "<your-email-addrerss>"
lanscape_url <- "<annotation-sheet-url>"
path_to_save_json <- "rocc/rocc-app/src/app/seeds/dream/"
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

dirty.orgId <- orgs$organizationId[which(nchar(orgs$organizationId) > 60)]
# [1] "applied-proteogenomics-organizational-learning-and-outcomes-network"            
# [2] "eunice-kennedy-shriver-national-institute-of-child-health-and-human-development"
clean.orgId <- c("apollo-network", "eunice-kennedy-shriver-national-institute")
message("orgId length > schema max length (60) :\n", 
    paste0("change '", dirty.orgId, "' to '", clean.orgId, "'", collapse = "\n")
)

makeQuiet(
  lapply(seq_along(dirty.orgId), function(i) {
    orgs$organizationId <<- gsub(dirty.orgId[i], clean.orgId[i], orgs$organizationId, fixed = TRUE)
  })
)

orgs.json <- toJSON(list(organizations=orgs), pretty = TRUE)
if (overwrite) write(orgs.json, "seedData/organizations.json")    

#### dataProviders ####
dataProviders_data <- cleanProperty(meta$dataContributors)
# if empty, convert to empty array
makeQuiet(
  lapply(seq_along(dataProvider_data), function(i) {
    if (is.na(dataProviders_data[[i]])) {
      dataProviders_data[[i]] <<- data.frame()
    }
  })
)

#### Persons ####
# data was based on Verena's mock json data and tidy up to a data frame
persons_data <- googlesheets4::read_sheet(lanscape_url, sheet = "persons", col_types = "ccccc")

persons <- persons_data %>% select(1:4) %>% nest_legacy(!challengeName, .key = "organizers")
persons <- merge(persons, meta, by = "challengeName", all = TRUE) %>% select(1:2)
# if empty, change to empty data.frame, so toJSON later can convert it to an empty array
makeQuiet(
  lapply(1:nrow(persons), function(i) {
    if (is.null(persons$organizers[[i]]) || is.na(persons$organizers[[i]]$firstName)) {
      print(i)
      persons$organizers[[i]] <<- I(data.frame())
    }
  })
)

#### grants ####
# Only collected three for example
grants_data <- googlesheets4::read_sheet(lanscape_url, sheet = "grants", col_types = "ccccc")
grants.json <- toJSON(list(grants=grants_data), pretty = TRUE)
if (overwrite) write(grants.json, "seedData/grants.json")

#### challenges ####
# trim summary to short descriptions for now
short_summary <- 
  ifelse(nchar(meta$challengeSummary) > 280, 
         paste0(substr(meta$challengeSummary, 1, 276), " ..."),
         meta$challengeSummary)

challenges.df <- 
  data.frame(name = meta$challengeName,
             description = short_summary,
             summary = meta$challengeSummary,
             # if no dates aka NA, it will be excluded
             startDate = as.character(as.Date(meta$challengeStart, "%Y-%m-%d")), 
             endDate = as.character(as.Date(meta$challengeEnd, "%Y-%m-%d")),
             url = paste0("https://www.synapse.org/#!Synapse:", meta$challengeSite),
             status = meta$challengeStatus,
             tagsIds = I(cleanProperty(meta$challengeKeywords)),
             organizerIds = I(persons$organizers[match(meta$challengeName, persons$challengeName)]),
             dataProvider = I(dataProviders_data),
             # empty for now
             grantIds = I(ifelse(meta$challengeGrants %in% grants_data$name, meta$challengeGrants, list(data.frame()))) 
  )
challenges.json <- prettify(toJSON(list(challenges=challenges.df)), indent = 2);challenges.json

if (overwrite) write(challenges.json, "seedData/challenges.json")

# cp file to rocc-app
if (overwrite) {
  system(paste0('cp seedData/tags.json ', path_to_save_json))
  system(paste0('cp seedData/organizations.json ', path_to_save_json))
  system(paste0('cp seedData/grants.json ', path_to_save_json))
  system(paste0('cp seedData/challenges.json ', path_to_save_json))
}