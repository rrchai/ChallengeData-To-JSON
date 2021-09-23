### This script is to convert challenge data to json format following the ROCC schema
### The some json files may use mock up property for development purpose:

# to assign empty array <- I(data.frame())
source("utlis.R")
source("config.R")

options(gargle_oauth_email = your_email_address) # for googlesheet
overwrite <- TRUE

dir.create("seedData", showWarnings = FALSE)

## Load Challenge Landscape sheet
meta <- googlesheets4::read_sheet(lanscape_url, sheet = "challenges") %>%
  mutate(across(everything(), as.character)) %>% 
  janitor::remove_empty(which = "rows")

#### Topics ####
topics <- cleanProperty(meta$challengeKeywords)

#### Organizations ####
orgs <- googlesheets4::read_sheet(lanscape_url, sheet = "organizations", col_types = "ccc")

## validation
org_invalid <- orgs$challengeOrganization[which(nchar(orgs$challengeOrganization) > 60)]; org_invalid
if (length(org_invalid) > 0) {
  stop(sQuote(org_invalid[1]), " > schema max length (60) :\n")
}

## create orgs login
org_logins <- cleanProperty(orgs$challengeOrganization) %>% unlist
## create orgs json
orgs_df <- data.frame(id=replicate(nrow(orgs), mongoIdMaker()),
                      login=org_logins,
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

#### Persons ####
persons <- googlesheets4::read_sheet(lanscape_url, sheet = "persons", col_types = "ccc") %>% 
  drop_na(challengeName, fullName)

## validation
person_challenge_invalid <- setdiff(persons$challengeName, meta$challengeName)
if (length(person_challenge_invalid) > 0) {
  stop(paste0(sQuote(person_challenge_invalid), collapse = ", "), " not match the name in challenges")
}

person_org_invalid <-
  setdiff(cleanProperty(persons$organizations, ",") %>% unlist() %>% unique() %>% na.omit(),
          orgs_df$login)
if (length(person_org_invalid) > 0) {
  stop(paste0(sQuote(person_org_invalid), collapse = ", "), " not match the name in organizations")
}

## condense person with the same name for now, for users.json
persons <- persons %>%
  group_by(fullName) %>%
  summarise(organizations = paste0(organizations, collapse = ","))

## clean up person names
person_names <- cleanProperty(persons$fullName, type = "name") %>% unlist()
## create orgs json
persons_df <- data.frame(
                id=replicate(nrow(persons), mongoIdMaker()),
                login=cleanProperty(person_names) %>% unlist(),
                name=persons$fullName,
                bio=c("A great bio"),
                email=c("contact@example.org"),
                avatarUrl= c("")
) %>% arrange(name)

## save as users.json
persons.json <- toJSON(list(users=persons_df), pretty = TRUE)
if (overwrite) write(persons.json, "seedData/users.json")

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
