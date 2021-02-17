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

x <- search_users("coloniaORcolonosORvecinosORcopaco florida", 10) %>%  filter(lang =="es")


x <- search_users("colonos", 100)
y <- search_users("vecinos", 50)
z <- search_users("copaco", 50)
a <- search_users("colonia", 50)

# function to check whether something can be associated with mexico city 
#cdmx_terms <- c("cdmx|ciudad mexico|distrito federal|df|d.f|alvaro obregon|benito juarez|coyacan|
#cuajimalpa|cuauhtemoc|gustavo a madero|gam|gustavo a. madero|iztacalco|iztapalapa|magdalena contreras|
#milpa alta|tlahuac|tlalpan|venustiano carranza|vc|xochimilco")
#dat <- x
#check_cdmx <- function(id= "1361650503734673408"){
 # description <- simplify_text(dat[dat$status_id==id, "description"] %>% slice(1))
#  location <- simplify_text(dat[dat$status_id == id,"location"] %>% slice(1))
#  place <- ifelse(!is.na(dat[dat$status_id == id,"place_full_name"]), simplify_text(dat[dat$status_id == id,"place_full_name"]), NA)
 # cdmx <- ifelse((str_detect(description, cdmx_terms)==TRUE|
  #                str_detect(location, cdmx_terms)==TRUE|
   #               (is.na(place) ==FALSE & str_detect(place, cdmx_terms)==TRUE)), 1,0)
  #return(cdmx)
#}
#x$cdmx <- lapply(FUN =check_cdmx, X=x$status_id)


