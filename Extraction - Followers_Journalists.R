################# Loading Libraries & any functions

pkgs <- c("purrr", "data.table", "dplyr", "rtweet", "ggplot2", "igraph", "intergraph", "ggnetwork", "extrafont", "ggrepel", "ggiraph",
		"ggthemes", "viridis", "scales", "grid", "gtable")
sapply(pkgs, require, character.only = TRUE)

# Specify locations/paths
basePath <- "C:\\Users\\kazami\\Desktop\\Aimia\\Campaign Analytics + BI + Self-learning\\Personal Projects\\Twitter N-League"
dataPath <- "Data"




################# Obtain an authentication token

tw_token <- create_token(app = "experimentation101", # whatever you named app
  			 consumer_key = "etjOWrV0DhWRdjnWpK4LDnZhF",
  			 consumer_secret = "")





################# Obtain friends (those that are followed by) N-League leadership

jl_l <- c("Shahidmasooddr", "mubasherlucman", "HamidMirGEO", "arsched", "Kashifabbasiary", "WaseemBadami", "AajKamranKhan", "asmashirazi",
		"UmarCheema1", "Ahmad_Noorani", "MurtazaGeoNews", "AnsarAAbbasi", "_Mansoor_Ali", "usmanmanzoor", "javeednusrat",
		"NasimZehra", "MalickViews", "SSEHBAI1", "Fahdhusain", "Matiullahjan919", "meherbokhari", "GFarooqi", "jawabdeyh",
		"asmachaudhry24", "nadia_a_mirza", "QuatrinaHosain", "Fereeha", "jasmeenmanzoor", "Maria_Memon", "muneebfaruq",
		"fawadchaudhry", "SaleemKhanSafi", "ImtiazAlamSAFMA", "WusatUllahKhan", "ZarrarKhuhro", "Razarumi", "murtazasolangi",
		"mazdaki", "titojourno", "IftikharFirdous", "umairjav", "cyalm", "abbasnasir59", "AzharAbbas3", "FarrukhKPitafi", 
		"ajmaljami", "alisalmanalvi", "usmanmanzoor", "Xadeejournalist", "AsadKharal", "NadeemfParacha", "TheHaroonRashid", "paktoday", 
		"SaleemFarrukh", "TalatHussain12", "AmirMateen2", "hidhussain", "wajih_sani", "wajih_sani", "BeenishSaleem", "mujibshami1",
		"Babar_Sattar", "sanabucha", "MoeedNj", "najamsethi", "nadeemmalik", "KlasraRauf", "shazbkhanzdaGEO", "DrDanish5", "HassanNisar")
jl_l <- unique(jl_l)



# A function to stop querying Twitter API and sleep until the quota are replenished
quota_replenish <- function(twitter_token, query_type){
stopifnot(length(query_type) == 1)
tw_limit<- rate_limit(token = twitter_token) %>% setDT
limit_query <- tw_limit[grep(query_type, query),]
print(paste("More queries allowed by Twitter API prior to quota exhaustion:", limit_query$remaining))
if(limit_query$remaining < 2){
	print(paste("Sleeping for", round(limit_query$reset), "minutes to prevent exceeding query limit"))
}
if(limit_query$remaining < 2){
	Sys.sleep(as.numeric(limit_query$reset) * 60)
}
}

# A function to extract followers for each of our recorded tweeter
extract_friends <- function(user_name, twitter_token){
f_df <- get_friends(user = user_name, token = twitter_token) %>% setDT
f_df[, tweeter := user_name]
quota_replenish(twitter_token = twitter_token, query_type = "friends/ids")
return(f_df)
}

# A function to extract a user's info from twitter
extract_userInfo <- function(user_name, twitter_token){
print(paste("Extracting info for", user_name))
info_df <- lookup_users(users = user_name, token = twitter_token)
quota_replenish(twitter_token = twitter_token, query_type = "users/lookup")
return(info_df)
}



# Run our function for extracting friends/those people a person follows
all_friends <- jl_l %>%
		map(function(x) extract_friends(user_name = x, twitter_token = tw_token)) %>%
		rbindlist

setcolorder(all_friends, c("tweeter", "ids"))


# Run our function for extracting a user's info
user_info <- jl_l %>%
		map(function(x) extract_userInfo(user_name = x, twitter_token = tw_token)) %>%
		rbindlist 


# Save datasets
saveRDS(object = all_friends, file = paste(basePath, dataPath, "journalists_friends.rds", sep = "/"))
saveRDS(object = user_info, file = paste(basePath, dataPath, "journalists_info.rds", sep = "/"))
