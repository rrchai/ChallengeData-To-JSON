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
  NA
)
## create orgs json
orgs_df <- data.frame(
  `_id` = replicate(nrow(orgs), mongoIdMaker()),
  login = org_logins,
  name = orgs$challengeOrganization,
  description = c("This is an awesome organization"),
  email = c("contact@example.org"),
  websiteUrl = orgs$url,
  avatarUrl = orgs_avatar,
  check.names = FALSE
) %>% arrange(name)

# only create neccessary org accounts
orgs_json <- toJSON(list(organizations = orgs_df %>% filter(name %in% unique(meta$challengeHost))), 
                    pretty = TRUE)
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

## clean up organizers names
persons$login <- persons$fullName %>% 
  cleanProperty(type = "name") %>% 
  unlist() 
persons$`_id` <- replicate(nrow(persons), mongoIdMaker())

#### Users ####
user_names <- c("Thomas Schaffter", "Rong Chai",
                "Verena Chung", "Jiaxin Zhang",
                "Jake Albrecht", "Michael Mason")
## create persons json
users_df <- data.frame(
  `_id` = replicate(length(user_names), mongoIdMaker()),
  login = unlist(cleanProperty(user_names, type = "name")),
  name = user_names,
  bio = c("A great bio"),
  email = c("contact@example.org"),
  avatarUrl = c("https://avatars.githubusercontent.com/u/3056480", 
                "https://avatars.githubusercontent.com/u/73901500",
                "https://avatars.githubusercontent.com/u/9377970",
                "https://avatars.githubusercontent.com/u/5205872",
                "https://avatars.githubusercontent.com/u/6445835",
                "https://avatars.githubusercontent.com/u/16109262"),
  check.names = FALSE
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
    userIds = persons$`_id`[match(fullName, persons$fullName)],
    orgIds = orgs_df$`_id`[match(organizations, orgs_df$name)]
  )
## create org-membership json
org_members_df <- data.frame(
  `_id` = replicate(nrow(org_members), mongoIdMaker()),
  state = c("active"),
  role = c("admin"),
  organizationId = org_members$orgIds,
  userId = org_members$userIds,
  check.names = FALSE
)
org_members_json <- toJSON(list(orgMemberships = org_members_df), pretty = TRUE)
if (overwrite) write(org_members_json, "seedData/orgMemberships.json")

#### Challenge Platforms ####
platforms <- googlesheets4::read_sheet(lanscape_url, sheet = "platforms", col_types = "cc") %>%
  janitor::remove_empty(which = "rows")
## create platform login
platform_logins <- cleanProperty(platforms$platformName) %>% unlist()
## create platform avatar
platform_avatar <- ifelse(file.exists(file.path(path_to_rocc_app, "images/logo/", paste0(platform_logins, ".png"))),
  paste0("https://github.com/Sage-Bionetworks/rocc-app/raw/main/images/logo/", platform_logins, ".png"),
  "https://via.placeholder.com/200x200"
)
## create platform json
platforms_df <- data.frame(
  `_id` = replicate(nrow(platforms), mongoIdMaker()),
  name = platform_logins,
  displayName = platforms$platformName,
  websiteUrl = platforms$url,
  avatarUrl = platform_avatar,
  check.names = FALSE
) %>% arrange(name)
platforms_json <- toJSON(list(challengePlatforms = platforms_df), pretty = TRUE)
if (overwrite) write(platforms_json, "seedData/challengePlatforms.json")

#### Challenge READMEs ####
readmes_df <- data.frame(
  `_id` = replicate(nrow(meta), mongoIdMaker()),
  text = trimws(meta$challengeSummary, "both"),
  check.names = FALSE
)
readmes_json <- prettify(toJSON(list(challengeReadmes = readmes_df), pretty = T), indent = 2)
if (overwrite) write(readmes_json, "seedData/challengeReadmes.json")

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
challenge_topic_invalide <- unlist(topics)[nchar((unlist(topics))) > 30 | nchar(unlist(topics)) < 3]
if (length(challenge_topic_invalide) > 0) {
  stop(paste0(sQuote(challenge_topic_invalide), collapse = ", "), " not fulfill topic length limits")
}
## create challenge url
challenges_df <- data.frame(
  `_id` = replicate(nrow(meta), mongoIdMaker()),
  name = cleanProperty(meta$challengeName) %>% unlist(),
  displayName = meta$challengeName,
  description = c("This challenge is an awesome challenge."),
  startDate = meta$challengeStart,
  endDate = meta$challengeEnd,
  websiteUrl = meta$challengeSite,
  status = meta$challengeStatus,
  readmeId = readmes_df$`_id`,
  platformId = platforms_df$`_id`[match(meta$challengePlatform, platforms_df$displayName)],
  ownerId = orgs_df$`_id`[match(meta$challengeHost, orgs_df$name)],
  topics = I(cleanProperty(meta$challengeKeywords)),
  fullName = paste0(orgs_df$login[match(meta$challengeHost, orgs_df$name)], 
                    "/", 
                    cleanProperty(meta$challengeName) %>% unlist()),
  createdAt = c(""),
  updatedAt = c(""),
  check.names = FALSE
)
challenges_json <- prettify(toJSON(list(challenges = challenges_df), pretty = T), indent = 2)
if (overwrite) write(challenges_json, "seedData/challenges.json")

#### Challenge Organizers ####
## assume persons with same name in different challenge are not the same organizers
## create organizers json
organizers_df <- data.frame(
  `_id` = replicate(nrow(persons), mongoIdMaker()),
  name = persons$fullName,
  roles = I(replicate(nrow(persons), data.frame())),
  challengeId = challenges_df$`_id`[match(persons$challengeName, challenges_df$displayName)],
  check.names = FALSE
) %>% arrange(name)

organizers_json <- toJSON(list(challengeOrganizers = organizers_df), pretty = TRUE)
if (overwrite) write(organizers_json, "seedData/challengeOrganizers.json")

#### Challenge Supporting ####
## collect the data for supporting organizations
sponsors <- meta[, c("challengeName", "challengeSponsors", "dataContributors")] %>% 
  separate_rows(challengeSponsors, sep = ",") %>%
  separate_rows(dataContributors, sep = ",") %>%
  mutate(challengeSponsors=trimws(challengeSponsors, "both"),
         dataContributors=trimws(dataContributors, "both")) %>%
  reshape2::melt(id.vars = "challengeName", variable.name = "roles", value.name = "organizations", na.rm = TRUE)

## use proper roles
sponsors$roles <- ifelse(sponsors$roles == "dataContributors", "DataProvider", NA)

## validation
sponsors_invalid <- setdiff(sponsors$organizations %>% na.omit(), orgs_df$name)
if (length(sponsors_invalid) > 0) {
  stop(paste0(sQuote(sponsors_invalid), collapse = ", "), " not match the name in orgs")
}

## collapse by challenge
sponsors <- sponsors %>% 
  mutate(login=orgs_df$login[match(sponsors$organizations, orgs_df$name)]) %>% 
  group_by(across(c(-roles))) %>%
  summarise(roles = str_c(roles %>% na.omit() %>% unique(), collapse = ","), .groups = 'drop')

## create organizers json
sponsors_df <- data.frame(
  `_id` = replicate(nrow(sponsors), mongoIdMaker()),
  name = sponsors$organizations,
  roles = I(replicate(nrow(sponsors), data.frame())),
  challengeId = challenges_df$`_id`[match(sponsors$challengeName, challenges_df$displayName)],
  check.names = FALSE
)
inx <- which(sponsors$roles == "DataProvider")
sponsors_df$roles[inx] <- I(data.frame("DataProvider")) 

sponsors_json <- toJSON(list(challengeSponsors = sponsors_df), pretty = TRUE)
if (overwrite) write(sponsors_json, "seedData/challengeSponsors.json")

# cp file to rocc-app
if (overwrite) {
  file_list <- list.files("seedData", ".json$")
  file.copy(from = paste0("seedData/", file_list), 
            to = "~/Clone/rocc/rocc-db-client/data/seeds/production/", 
            overwrite = TRUE)
}
