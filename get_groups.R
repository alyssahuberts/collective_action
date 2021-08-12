#### Script to get neighborhood groups from twitter
# Created 2/16/2021
# last edited 2/16/2021
######
library(rtweet)
library(tidyverse)
## store api keys 
app_name <- "Tandeo"
api_key <- "J62vGxJYJkiYjscXMMF7VBxuS"
api_secret_key <- "w5gSl08F3Ycg13wyxebh32VxJIMWxGh08cNWUTcgjVJrrDYPQB"
consumer_key = api_key
consumer_secret = api_secret_key
access_token <- "2307234206-UaTbWu1ikS2TTldqi0QeMw71aHdnKfwFmPxtC3N"
access_token_secret <- "Qhv3zesJu1f1vDiE9rrjUuZ0aupbZ0hOdcrtRDGk0kIXA"
token <- create_token(app = app_name, consumer_key, consumer_secret, access_token = access_token, access_secret = access_token_secret) #note- this does this in browser; slightly annoying but fine
## save token to home directory
path_to_token <- file.path(path.expand("~"), ".twitter_token.rds")
saveRDS(token, path_to_token)
## create env variable TWITTER_PAT (with path to saved token)
env_var <- paste0("TWITTER_PAT=", path_to_token)
## save as .Renviron file (or append if the file already exists)
cat(env_var, file = file.path(path.expand("~"), ".Renviron"), 
    fill = TRUE, append = TRUE)
readRenviron("~/.Renviron")



# read in neighborhood names (simplified, at least to start)
colonias_simplified <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/3_Twitter/2_Data/2_Use/coloniascdmx_grouped_simplified.csv")


simplify_text <- function(text){
text <-tolower(text)
text <- str_replace(text, "á", "a")
text <- str_replace(text, "à", "i")
text <- str_replace(text, "é", "e")
text <- str_replace(text, "è", "e")
text <- str_replace(text, "í", "i")
text <- str_replace(text, "í", "ì")
text <- str_replace(text, "ó", "o")
text <- str_replace(text, "ò", "o")
text <- str_replace(text, "ú", "u")
text <- str_replace(text, "ù", "u")
text <- str_replace(text, "ñ", "n")
return(text)
}

#  search for all accounts that tweet the colonia's (simplified name) AND colonos|vecinos|copaco|colonia
# It seems like we can't do the or's and the ands int he same call (like with parentheses) so instead I'll loop through the different combinations 
# start with colonia 
colonia_accounts <- c()
for(i in 1:length(colonias_simplified$search_terms)){
  search_phrase <- paste("colonia ", colonias_simplified$search_terms[i], sep = "")
  x <- search_users(search_phrase)
if(length(x)>0){
  users_keep <- x %>% filter(lang == "es")
  if(length(users_keep$status_id)>0){
  users_keep$cve_col <- colonias_simplified$cve_col[i]
  colonia_accounts <- bind_rows(colonia_accounts, users_keep)}
}
}
save(colonia_accounts, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/groups/accounts_colonia.Rdata")

colonos_accounts <- c()
for(i in 1:length(colonias_simplified$search_terms)){
  search_phrase <- paste("colonos ", colonias_simplified$search_terms[i], sep = "")
  x <- search_users(search_phrase)
  if(length(x)>0){
    users_keep <- x %>% filter(lang == "es")
    if(length(users_keep$status_id)>0){
      users_keep$cve_col <- colonias_simplified$cve_col[i]
      colonos_accounts <- bind_rows(colonos_accounts, users_keep)}
  }
}
save(colonos_accounts, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/groups/accounts_colonos.Rdata")

vecinos_accounts <- c()
for(i in 1:length(colonias_simplified$search_terms)){
  search_phrase <- paste("vecinos ", colonias_simplified$search_terms[i], sep = "")
  x <- search_users(search_phrase)
  if(length(x)>0){
    users_keep <- x %>% filter(lang == "es")
    if(length(users_keep$status_id)>0){
      users_keep$cve_col <- colonias_simplified$cve_col[i]
      vecinos_accounts <- bind_rows(vecinos_accounts, users_keep)}
  }
}
save(vecinos_accounts, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/groups/accounts_vecinos.Rdata")

copaco_accounts <- c()
for(i in 1:length(colonias_simplified$search_terms)){
  search_phrase <- paste("copaco ", colonias_simplified$search_terms[i], sep = "")
  x <- search_users(search_phrase)
  if(length(x)>0){
    users_keep <- x %>% filter(lang == "es")
    if(length(users_keep$status_id)>0){
      users_keep$cve_col <- colonias_simplified$cve_col[i]
      copaco_accounts <- bind_rows(copaco_accounts, users_keep)}
  }
}
save(copaco_accounts, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/groups/accounts_copaco.Rdata")

load("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/groups/accounts_colonia.Rdata")
load("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/groups/accounts_colonos.Rdata")
load("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/groups/accounts_vecinos.Rdata")
load("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/groups/accounts_copaco.Rdata")

accounts <- bind_rows(colonia_accounts, colonos_accounts, copaco_accounts, vecinos_accounts)
accounts <- accounts %>% group_by(screen_name) %>% slice(1)
colonia_accounts <- accounts %>% group_by(cve_col) %>% tally()
save(colonia_accounts, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/groups/accounts_all.Rdata")

ggplot(accounts)+ geom_bar(aes(x =as.Date(account_created_at)), stat = "count") 
colonia_accounts_2019_2020 <- accounts %>%
  filter(as.Date(account_created_at) < as.Date("2020-01-01")&
           as.Date(account_created_at) > as.Date("2018-01-01")) %>% 
  group_by(cve_col) %>% tally()

filter(as.Date(account_created_at) < as.Date("2020-01-01")) %>%  