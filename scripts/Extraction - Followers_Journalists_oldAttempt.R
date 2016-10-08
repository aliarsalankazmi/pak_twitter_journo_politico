################# Loading Libraries

pkgs <- c("purrr", "data.table", "dplyr", "rtweet", "ggplot2", "igraph", "intergraph", "ggnetwork", "extrafont", "ggrepel", "ggiraph",
		"ggthemes", "viridis", "scales")
sapply(pkgs, require, character.only = TRUE)





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
		"ajmaljami", "alisalmanalvi", "usmanmanzoor", "Xadeejournalist", "AsadKharal", "NadeemfParacha", "TheHaroonRashid",
		"SaleemFarrukh", "TalatHussain12", "AmirMateen2", "hidhussain", "wajih_sani", "wajih_sani", "BeenishSaleem", 
		"Babar_Sattar", "sanabucha", "MoeedNj", "najamsethi", "nadeemmalik", "KlasraRauf", "shazbkhanzdaGEO")
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



# Specify locations/paths
basePath <- "C:\\Users\\kazami\\Desktop\\Aimia\\Campaign Analytics + BI + Self-learning\\Personal Projects\\Twitter N-League"
dataPath <- "Data"
#saveRDS(object = all_friends, file = paste(basePath, dataPath, "journalists_friends.rds", sep = "/"))
#saveRDS(object = user_info, file = paste(basePath, dataPath, "journalists_info.rds", sep = "/"))

if("all_friends" %in% ls() == FALSE){all_friends <- readRDS(file = paste(basePath, dataPath, "journalists_friends.rds", sep = "/"))}
if("user_info" %in% ls() == FALSE){user_info <- readRDS(file = paste(basePath, dataPath, "journalists_info.rds", sep = "/"))}

# Set order of columns & name
setcolorder(all_friends, c("tweeter", "ids"))
setnames(all_friends, c("tweeter", "ids"), c("tweeter", "friend_id"))

# A bit of tidying
.simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}
user_info[, name := .simpleCap(tolower(name)), by = name]


# Consolidating the info
user_info_friends_temp <- merge(x = all_friends, 
		      	   y = user_info[, .(user_id, friend_name = name, friend_screen_name = screen_name)], 
		      	   by.x = "friend_id", by.y = "user_id", all.x = TRUE)

user_info_friends <- merge(x = user_info_friends_temp, 
			   y = user_info[, .(user_id, name, screen_name, location, followers_count, friends_count, favourites_count, verified, statuses_count)], 
		           by.x = "tweeter", by.y = "screen_name", all.x = TRUE)

rm(user_info_friends_temp); gc(T,T)





####### Basic reporting

br1 <- user_info[, .(totalFollowers = sum(followers_count),
	      totalFriends = sum(friends_count),
	      totalStatuses = sum(statuses_count), 
	      friendsFollowers = sum(followers_count, friends_count)), by = name] 

br1 %>%
	ggplot(data = br1, aes(x = totalFollowers, y = totalFriends, colour = friendsFollowers)) +
	geom_point(size = 3) +
	#geom_text_repel(data = br1[totalFollowers > quantile(totalFollowers, .9) | totalFriends > quantile(totalFriends, .9)],
	#		aes(label = name, x = totalFollowers, y = totalFriends), colour = "black", size = rel(4)) +
	scale_color_viridis() + 
	scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) +
	ylab("Number of People Followed by each Journalist") + xlab("Number of Followers for each Journalist") +
	ggtitle("Pakistani Journalists on Twitter", subtitle = "How the following and friendship of Pakistani Journalists on Twitter stacks up against each other") +  
	guides(colour = FALSE) + 
	theme_classic() +
	theme(title = element_text(size = rel(1.2), face = "bold", colour = "grey80"),
	      plot.title = element_text(size = rel(1.5)),
	      text = element_text(family = "Cambria"))


br2 <- user_info_friends[!is.na(friend_id), 
			    .(total_friends = length(friend_name),
			     journ_friends = sum(!is.na(friend_name)),
			     nonjourn_friends = sum(is.na(friend_name))),
			     by = name
			][, prcnt_journ_friends := journ_friends/total_friends]

setorder(br2, -prcnt_journ_friends)
br2[, name := factor(name, levels = name, ordered = TRUE)]

br2 %>% 
	ggplot(data = ., aes(x = prcnt_journ_friends, y = name)) +
	geom_segment(aes(yend=name), xend=0, color='grey50') +
	geom_point(size=3, colour = "IndianRed") +
	scale_x_continuous(labels = percent, breaks = pretty_breaks(n = 6)) +
	xlab("Percent of Pakistani Journalist Friends") +
	ggtitle("What type of people do Pakistani Journalists follow on Twitter?", 
		 subtitle = "Here, type of people have been divided into Pakistani Journalists and others") + 
	#caption("Here, Pakistani Journalists comprise of only the same set of people shown here") +
	theme_classic() +
	theme(title = element_text(size = rel(1.2), face = "bold"),
	      plot.title = element_text(size = rel(1.5)),
	      text = element_text(family = "Cambria"))




ge1 <- graph.edgelist(as.matrix(all_friends[!is.na(ids)]))






