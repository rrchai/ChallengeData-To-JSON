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
orgs <- googlesheets4::read_sheet(lanscape_url, sheet = "organizations", col_types = "ccc") %>%
  janitor::remove_empty(which = "rows")

## validation
org_invalid <- orgs$challengeOrganization[which(nchar(orgs$challengeOrganization) > 60)]
org_invalid
if (length(org_invalid) > 0) {
  stop(sQuote(org_invalid[1]), " > schema max length (60) :\n")
}

## create orgs login
org_logins <- cleanProperty(orgs$challengeOrganization) %>% unlist()
## create orgs avatar
orgs_avatar <- ifelse(file.exists(file.path(path_to_rocc_app, "images/logo/", paste0(org_logins, ".png"))),
  paste0("https://github.com/Sage-Bionetworks/rocc-app/raw/main/images/logo/", org_logins, ".png"),
  ""
)
## create orgs json
orgs_df <- data.frame(
  id = replicate(nrow(orgs), mongoIdMaker()),
  login = org_logins,
  name = orgs$challengeOrganization,
  description = c("This is an awesome organization"),
  email = c("contact@example.org"),
  websiteUrl = orgs$url,
  avatarUrl = orgs_avatar
) %>% arrange(name)
orgs_json <- toJSON(list(organizations = orgs_df), pretty = TRUE)
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
  setdiff(
    cleanProperty(persons$organizations, ",") %>% unlist() %>% unique() %>% na.omit(),
    orgs_df$login
  )
if (length(person_org_invalid) > 0) {
  stop(paste0(sQuote(person_org_invalid), collapse = ", "), " not match the name in organizations")
}

## only take unique persons for now, for users.json
users <- unique(persons$fullName) %>% trimws("both")

## clean up person names
user_logins <- cleanProperty(users, type = "name") %>%
  unlist() %>%
  cleanProperty(.) %>%
  unlist()
## create persons json
users_df <- data.frame(
  id = replicate(length(users), mongoIdMaker()),
  login = user_logins,
  name = users,
  bio = c("A great bio"),
  email = c("contact@example.org"),
  avatarUrl = c("")
) %>% arrange(name)

users_json <- toJSON(list(users = users_df), pretty = TRUE)
if (overwrite) write(users_json, "seedData/users.json")

#### Org Memberships ####
org_members <- persons %>%
  separate_rows(organizations, sep = ",") %>%
  mutate(organizations = trimws(organizations)) %>%
  select(fullName, organizations) %>%
  distinct(across(everything())) %>%
  drop_na()

## validation
org_member_org_invalid <- setdiff(org_members$organizations %>% na.omit(), orgs_df$name)
if (length(org_member_org_invalid) > 0) {
  stop(paste0(sQuote(org_member_org_invalid), collapse = ", "), " not match the name in organizations")
}

## replace name with Ids
org_members <- org_members %>%
  mutate(
    userIds = persons_df$id[match(fullName, persons_df$name)],
    orgIds = orgs_df$id[match(organizations, orgs_df$name)]
  )
## create org-membership json
org_members_df <- data.frame(
  id = replicate(nrow(org_members), mongoIdMaker()),
  state = c("active"),
  role = c("admin"),
  organizationId = org_members$orgIds,
  userId = org_members$userIds
)
org_members_json <- toJSON(list(orgMemberships = org_members_df), pretty = TRUE)
if (overwrite) write(org_members_json, "seedData/org-memberships.json")

#### Challenge Platforms ####
platforms <- googlesheets4::read_sheet(lanscape_url, sheet = "platforms", col_types = "cc") %>%
  janitor::remove_empty(which = "rows")
## create platform login
platform_logins <- cleanProperty(platforms$platformName) %>% unlist()
## create platform avatar
platform_avatar <- ifelse(file.exists(file.path(path_to_rocc_app, "images/logo/", paste0(platform_logins, ".png"))),
  paste0("https://github.com/Sage-Bionetworks/rocc-app/raw/main/images/logo/", platform_logins, ".png"),
  ""
)
## create platform json
platforms_df <- data.frame(
  id = replicate(nrow(platforms), mongoIdMaker()),
  name = platform_logins,
  displayName = platforms$platformName,
  websiteUrl = platforms$url,
  avatarUrl = platform_avatar
) %>% arrange(name)
platforms_json <- toJSON(list(challengePlatforms = platforms_df), pretty = TRUE)
if (overwrite) write(platforms_json, "seedData/challenge-platforms.json")

#### challenges ####
## validation
challenge_platform_invalid <- setdiff(meta$challengePlatform %>% na.omit(), platforms_df$displayName)
if (length(challenge_platform_invalid) > 0) {
  stop(paste0(sQuote(challenge_platform_invalid), collapse = ", "), " not match the name in platforms")
}
challenge_host_invalid <- setdiff(meta$challengeHost %>% na.omit(), orgs_df$name)
if (length(challenge_host_invalid) > 0) {
  stop(paste0(sQuote(challenge_host_invalid), collapse = ", "), " not match the name in organizations")
}
challenges_df <- data.frame(
  id = replicate(nrow(meta), mongoIdMaker()),
  name = cleanProperty(meta$challengeName) %>% unlist(),
  displayName = meta$challengeName,
  description = c("This challenge is an awesome challenge."),
  startDate = meta$challengeStart,
  endDate = meta$challengeEnd,
  websiteUrl = meta$challengeSite,
  status = meta$challengeStatus,
  platformId = platforms_df$id[match(meta$challengePlatform, platforms_df$displayName)],
  ownerId = orgs_df$id[match(meta$challengeHost, orgs_df$name)],
  topics = I(cleanProperty(meta$challengeKeywords)),
  fullName = c(""),
  createdAt = c(""),
  updatedAt = c("")
)
challenges_json <- prettify(toJSON(list(challenges = challenges_df), pretty = T), indent = 2)
if (overwrite) write(challenges_json, "seedData/challenges.json")

#### Challenge READMEs ####
readmes_df <- data.frame(
  challengeId = challenges_df$id,
  text = trimws(meta$challengeSummary, "both")
)
readmes_json <- prettify(toJSON(list(challengeReadmes = readmes_df), pretty = T), indent = 2)
if (overwrite) write(readmes_json, "seedData/challenge-readmes.json")

# cp file to rocc-app
if (overwrite) {
  system(paste0("cp seedData/*.json ", path_to_rocc_app, "src/app/seeds/production/"))
}
