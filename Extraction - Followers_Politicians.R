################# Loading Libraries

pkgs <- c("purrr", "data.table", "dplyr", "rtweet", "ggplot2")
sapply(pkgs, require, character.only = TRUE)





################# Obtain an authentication token

tw_token <- create_token(app = "experimentation101", # whatever you named app
  			 consumer_key = "etjOWrV0DhWRdjnWpK4LDnZhF",
  			 consumer_secret = "")





################# Obtain friends (those that are followed by) N-League leadership

nl_l <- c("MaryamNSharif", "KhawajaMAsif", "KhSaad_Rafique", "DaniyalNA116", "AbidSherAli", "ranasanaulah", "CMShehbaz", "Tallal_MNA", "HamzaSsharif", 
		"mushtaqminhas", "betterpakistan", "ChNisarOfficial", "marvi_memon", "PMLNMEDIA", "kdastgirkhan", "pmln_org", "RealM_Zubair", 
		"MShamsZ", "SaadSheikhPmln", "Atifrauf79", "ahsanzubair_107")
nl_l <- unique(nl_l)

# Some Notes: 
#	Hamza Shahbaz has been inactive since 2013
#	betterpakistan is Ahsan Iqbal


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
all_friends <- nl_l %>%
		map(function(x) extract_friends(user_name = x, twitter_token = tw_token)) %>%
		rbindlist


# Run our function for extracting a user's info
user_info <- nl_l %>%
		map(function(x) extract_userInfo(user_name = x, twitter_token = tw_token)) %>%
		rbindlist 



# Specify locations/paths
basePath <- "C:\\Users\\kazami\\Desktop\\Aimia\\Campaign Analytics + BI + Self-learning\\Personal Projects\\Twitter N-League"
dataPath <- "Data"

saveRDS(object = all_friends, file = paste(basePath, dataPath, "nleague_friends.rds", sep = "/"))
saveRDS(object = user_info, file = paste(basePath, dataPath, "nleague_info.rds", sep = "/"))






############# Now collecting tweeter data for PTI Leadership

pti_l <- c("ImranKhanPTI", "SMQureshiPTI", "ImranIsmailPTI", "AndleebAbbas", "Asad_Umar", "ShireenMazari1", "naeemul_haque", "AliHZaidiPTI",
		"SaifullahNyazee", "JahangirKTareen", "Shafqat_Mahmood", "ArifAlvi", "PTIofficial", "Ali_MuhammadPTI", 
		"ChMSarwar", "shiblifaraz","FaisalJavedKhan", "nadirlegharipti", "NazBalochPTI", "aleemkhan_pti", 
		"MuradSaeedPTI", "OmarCheemaPTI", "Pervaiz_Khattak", "AzamSwatiPTI", "Fayazchohanpti", "SaifullahNyazee")
pti_l <- unique(pti_l)

# Run our function for extracting friends/those people a person follows
all_friends <- pti_l %>%
		map(function(x) extract_friends(user_name = x, twitter_token = tw_token)) %>%
		rbindlist

setcolorder(all_friends, c("tweeter", "ids"))


# Run our function for extracting a user's info
user_info <- pti_l %>%
		map(function(x) extract_userInfo(user_name = x, twitter_token = tw_token)) %>%
		rbindlist 


# Save datasets
saveRDS(object = all_friends, file = paste(basePath, dataPath, "pti_friends.rds", sep = "/"))
saveRDS(object = user_info, file = paste(basePath, dataPath, "pti_info.rds", sep = "/"))







############# Now collecting tweeter data for PPP Leadership

ppp_l <- c("BBhuttoZardari", "YR_Gillani", "SaleemMandvi", "ShaziaAttaMarri", "S_KhursheedShah", "NisarKhuhro_SME", "HinaRKhar", "sherryrehman",
		"nadyaaGabol", "sharjeelinam", "SyedaShehlaRaza", "JamKhanShoroPPP", "NadeemAfzalChan", "fkkundi", "BilawalHouseKhi", "MManzoorWattoo",
		"Qamarzkaira", "ShahNafisa", "Owais_Muzaffar", "FaryalTalpurPk", "AAliZardari", "JehangirBader", "SaeedGhani1", "drabdulqayoom", "sh_basra",
		"SassuiPalijo", "Sayedmuradshah", "SenRehmanMalik")
ppp_l <- unique(ppp_l)


# Run our function for extracting friends/those people a person follows
all_friends <- ppp_l %>%
		map(function(x) extract_friends(user_name = x, twitter_token = tw_token)) %>%
		rbindlist

setcolorder(all_friends, c("tweeter", "ids"))


# Run our function for extracting a user's info
user_info <- ppp_l %>%
		map(function(x) extract_userInfo(user_name = x, twitter_token = tw_token)) %>%
		rbindlist 


# Save datasets
saveRDS(object = all_friends, file = paste(basePath, dataPath, "ppp_friends.rds", sep = "/"))
saveRDS(object = user_info, file = paste(basePath, dataPath, "ppp_info.rds", sep = "/"))


