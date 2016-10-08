################# Loading Libraries

pkgs <- c("purrr", "data.table", "dplyr", "rtweet", "twitteR", "ggplot2", "lubridate")
sapply(pkgs, require, character.only = TRUE)





################# Obtain an authentication token

consumer_key = "etjOWrV0DhWRdjnWpK4LDnZhF"
consumer_secret = ""

setup_twitter_oauth(consumer_key, consumer_secret)






################# Obtain friends (those that are followed by) N-League leadership

all_tw <- c("MaryamNSharif", "KhawajaMAsif", "KhSaad_Rafique", "DaniyalNA116", "AbidSherAli", "ranasanaulah", "CMShehbaz", "Tallal_MNA", "HamzaSsharif", 
		"mushtaqminhas", "betterpakistan", "ChNisarOfficial", "marvi_memon", "PMLNMEDIA", "kdastgirkhan", "pmln_org", "RealM_Zubair", 
		"MShamsZ", "SaadSheikhPmln", "Atifrauf79", "ahsanzubair_107",
	"ImranKhanPTI", "SMQureshiPTI", "ImranIsmailPTI", "AndleebAbbas", "Asad_Umar", "ShireenMazari1", "naeemul_haque", "AliHZaidiPTI",
		"SaifullahNyazee", "JahangirKTareen", "Shafqat_Mahmood", "ArifAlvi", "PTIofficial", "Ali_MuhammadPTI", 
		"ChMSarwar", "shiblifaraz","FaisalJavedKhan", "nadirlegharipti", "NazBalochPTI", "aleemkhan_pti", 
		"MuradSaeedPTI", "OmarCheemaPTI", "Pervaiz_Khattak", "AzamSwatiPTI", "Fayazchohanpti", "SaifullahNyazee",
	"BBhuttoZardari", "YR_Gillani", "SaleemMandvi", "ShaziaAttaMarri", "S_KhursheedShah", "NisarKhuhro_SME", "HinaRKhar", "sherryrehman",
		"nadyaaGabol", "sharjeelinam", "SyedaShehlaRaza", "JamKhanShoroPPP", "NadeemAfzalChan", "fkkundi", "BilawalHouseKhi", "MManzoorWattoo",
		"Qamarzkaira", "ShahNafisa", "Owais_Muzaffar", "FaryalTalpurPk", "AAliZardari", "JehangirBader", "SaeedGhani1", "drabdulqayoom", "sh_basra",
		"SassuiPalijo", "Sayedmuradshah", "SenRehmanMalik",
	"Shahidmasooddr", "mubasherlucman", "HamidMirGEO", "arsched", "Kashifabbasiary", "WaseemBadami", "AajKamranKhan", "asmashirazi",
		"UmarCheema1", "Ahmad_Noorani", "MurtazaGeoNews", "AnsarAAbbasi", "_Mansoor_Ali", "usmanmanzoor", "javeednusrat",
		"NasimZehra", "MalickViews", "SSEHBAI1", "Fahdhusain", "Matiullahjan919", "meherbokhari", "GFarooqi", "jawabdeyh",
		"asmachaudhry24", "nadia_a_mirza", "QuatrinaHosain", "Fereeha", "jasmeenmanzoor", "Maria_Memon", "muneebfaruq",
		"fawadchaudhry", "SaleemKhanSafi", "ImtiazAlamSAFMA", "WusatUllahKhan", "ZarrarKhuhro", "Razarumi", "murtazasolangi",
		"mazdaki", "titojourno", "IftikharFirdous", "umairjav", "cyalm", "abbasnasir59", "AzharAbbas3", "FarrukhKPitafi", 
		"ajmaljami", "alisalmanalvi", "usmanmanzoor", "Xadeejournalist", "AsadKharal", "NadeemfParacha", "TheHaroonRashid", "paktoday", 
		"SaleemFarrukh", "TalatHussain12", "AmirMateen2", "hidhussain", "wajih_sani", "wajih_sani", "BeenishSaleem", "mujibshami1",
		"Babar_Sattar", "sanabucha", "MoeedNj", "najamsethi", "nadeemmalik", "KlasraRauf", "shazbkhanzdaGEO", "DrDanish5", "HassanNisar")
all_tw_c <- unique(all_tw)



# A function to stop querying Twitter API and sleep until the quota are replenished
quota_replenish <- function(twitter_token, query_type){
stopifnot(length(query_type) == 1)
tw_limit<- getCurRateLimitInfo() %>% setDT
limit_query <- tw_limit[grep(query_type, resource),]
wait_time <- ifelse(Sys.time() - limit_query$reset < 0, -1 * (Sys.time() - limit_query$reset), Sys.time() - limit_query$reset)
print(paste("More queries allowed by Twitter API prior to quota exhaustion:", limit_query$remaining))
if(as.numeric(limit_query$remaining) < 2){
	print(paste("Sleeping for", round(wait_time), "minutes to prevent exceeding query limit"))
}
if(as.numeric(limit_query$remaining) < 2){
	Sys.sleep(wait_time * 60)
}
}

# A function to extract followers for each of our recorded tweeter
extract_friends <- function(user_name, no_of_favs){
f_df <- favorites(user = user_name, n = no_of_favs) 
if(length(f_df) != 0){
	f_df <- f_df %>% twListToDF %>% setDT
	f_df[, tweeter := user_name]
}
quota_replenish(query_type = "/favorites/list")
return(f_df)
}


# Run our function for extracting friends/those people a person follows
all_favs <- all_tw_c %>%
		map(function(x) extract_friends(user_name = x, no_of_favs = 1000)) %>%
		discard(is.na(.)) %>% 
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


