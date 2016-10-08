################# Loading Libraries & any functions

pkgs <- c("purrr", "data.table", "dplyr", "tidyr", "lubridate", "rtweet", "ggplot2", "igraph", "intergraph", "ggnetwork", "extrafont", "ggrepel", "ggiraph",
		"ggthemes", "viridis", "scales", "grid", "gtable", "FactoMineR", "tidytext", "topicmodels", "textcat", "wordcloud")
sapply(pkgs, require, character.only = TRUE)

# Specify locations/paths
basePath <- "C:\\Users\\kazami\\Desktop\\Aimia\\Campaign Analytics + BI + Self-learning\\Personal Projects\\Twitter N-League"
dataPath <- "Data"

# Function courtesy of Bob Rudis: rud.is/b
ggplot_with_subtitle <- function(gg, 
                                 label="", 
                                 fontfamily=NULL,
                                 fontsize=10,
                                 hjust=0, vjust=0, 
                                 bottom_margin=5.5,
                                 newpage=is.null(vp),
                                 vp=NULL,
                                 ...) {
 
  if (is.null(fontfamily)) {
    gpr <- gpar(fontsize=fontsize, ...)
  } else {
    gpr <- gpar(fontfamily=fontfamily, fontsize=fontsize, ...)
  }
 
  subtitle <- textGrob(label, x=unit(hjust, "npc"), y=unit(hjust, "npc"), 
                       hjust=hjust, vjust=vjust,
                       gp=gpr)
 
  data <- ggplot_build(gg)
 
  gt <- ggplot_gtable(data)
  gt <- gtable_add_rows(gt, grobHeight(subtitle), 2)
  gt <- gtable_add_grob(gt, subtitle, 3, 4, 3, 4, 8, "off", "subtitle")
  gt <- gtable_add_rows(gt, grid::unit(bottom_margin, "pt"), 3)
 
  if (newpage) grid.newpage()
 
  if (is.null(vp)) {
    grid.draw(gt)
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(gt)
    upViewport()
  }
 
  invisible(data)
 
}


.simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}





################# Load datasets

j_all_friends <- readRDS(file = paste(basePath, dataPath, "journalists_friends.rds", sep = "/"))
j_user_info <- readRDS(file = paste(basePath, dataPath, "journalists_info.rds", sep = "/"))
nl_all_friends <- readRDS(file = paste(basePath, dataPath, "nleague_friends.rds", sep = "/"))
nl_user_info <- readRDS(file = paste(basePath, dataPath, "nleague_info.rds", sep = "/"))
pti_all_friends <- readRDS(file = paste(basePath, dataPath, "pti_friends.rds", sep = "/"))
pti_user_info <- readRDS(file = paste(basePath, dataPath, "pti_info.rds", sep = "/"))
ppp_all_friends <- readRDS(file = paste(basePath, dataPath, "ppp_friends.rds", sep = "/"))
ppp_user_info <- readRDS(file = paste(basePath, dataPath, "ppp_info.rds", sep = "/"))
all_fav_tweets <- readRDS(file = paste(basePath, dataPath, "all_fav_tweets.rds", sep = "/"))


# Set order of columns & name
setcolorder(j_all_friends, c("tweeter", "ids"))						#### For journalists
setnames(j_all_friends, c("tweeter", "ids"), c("tweeter", "friend_id"))
setcolorder(nl_all_friends, c("tweeter", "ids"))					#### For N League
setnames(nl_all_friends, c("tweeter", "ids"), c("tweeter", "friend_id"))
setcolorder(pti_all_friends, c("tweeter", "ids"))					#### For PTI
setnames(pti_all_friends, c("tweeter", "ids"), c("tweeter", "friend_id"))
setcolorder(ppp_all_friends, c("tweeter", "ids"))					#### For PPP
setnames(ppp_all_friends, c("tweeter", "ids"), c("tweeter", "friend_id"))



# A bit of tidying
j_user_info[, name := .simpleCap(tolower(name)), by = name]
j_user_info[, category := "Journalist"]
nl_user_info[, name := .simpleCap(tolower(name)), by = name]
nl_user_info[, category := "NL"]
pti_user_info[, name := .simpleCap(tolower(name)), by = name]
pti_user_info[, category := "PTI"]
ppp_user_info[, name := .simpleCap(tolower(name)), by = name]
ppp_user_info[, category := "PPP"]


# Consolidating the info
all_user_info <- ls(pattern = ".*user_info$") %>%
			map(get) %>%
			rbindlist
all_friends <- ls(pattern = ".*all_friends$") %>%
			map(get) %>%
			rbindlist

user_info_friends_temp <- merge(x = all_friends, 
		      	   y = all_user_info[, .(user_id, friend_name = name, friend_screen_name = screen_name, friend_category = category)], 
		      	   by.x = "friend_id", by.y = "user_id", all.x = TRUE)

user_info_friends <- merge(x = user_info_friends_temp, 
			   y = all_user_info[, .(user_id, name, screen_name, user_category = category, location, followers_count, friends_count, 
						 favourites_count, verified, statuses_count)], 
		           by.x = "tweeter", by.y = "screen_name", all.x = TRUE)

rm(user_info_friends_temp, j_user_info, nl_user_info, pti_user_info, ppp_user_info, j_all_friends, nl_all_friends, pti_all_friends, ppp_all_friends)
gc(T,T)





################# Basic reporting

br1 <- all_user_info[category == "Journalist", .(totalFollowers = sum(followers_count),
	      totalFriends = sum(friends_count),
	      totalStatuses = sum(statuses_count), 
	      friendsFollowers = sum(followers_count, friends_count)), by = name] 

g1 <- br1 %>%
	ggplot(data = ., aes(x = totalFollowers, y = totalFriends)) +
	geom_point(size = 3, colour = "#9c1819") +
	geom_text_repel(data = br1[totalFollowers > quantile(totalFollowers, .9) | totalFriends > quantile(totalFriends, .9)],
			aes(label = name, x = totalFollowers, y = totalFriends), colour = "black", size = rel(4)) +
	scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) +
	ylab("Number of People Followed by each Journalist") + xlab("Number of Followers for each Journalist") +
	ggtitle("Pakistani Journalists on Twitter") +  
	guides(colour = FALSE) + 
	theme_bw() +
	theme(title = element_text(size = rel(1.2), face = "bold", colour = "#9c1819"),
	      plot.title = element_text(size = rel(1.5), hjust = 0),
	      text = element_text(family = "Cambria"))
	

ggplot_with_subtitle(g1, 
		     label = "How the following and friendship of Pakistani Journalists on Twitter stacks up against each other",
		     fontfamily = "Calibri",
		     cex = 1.5)
ggsave(paste(basePath, "Graphs", "j_graph1.png", sep = "/"), height = 10, width = 12, units='in', dpi=600)






br2 <- user_info_friends[!is.na(friend_id) & user_category == "Journalist", 
			    .(total_friends = length(friend_name),
			     journ_friends = sum(grepl("Journalist", friend_category)),
			     nonjourn_friends = sum(friend_category != "Journalist")),
			     by = name
			][, prcnt_journ_friends := journ_friends/total_friends]

setorder(br2, -prcnt_journ_friends)
br2[, name := factor(name, levels = name, ordered = TRUE)]

g2 <- br2 %>% 
	ggplot(data = ., aes(x = prcnt_journ_friends, y = name)) +
	geom_segment(aes(yend=name), xend=0, color='grey50') +
	geom_point(size=3, colour = "#9c1819") +
	scale_x_continuous(labels = percent, breaks = pretty_breaks(n = 6)) +
	xlab("Percent of Pakistani Journalist Friends") + ylab("Journalist") +
	ggtitle("What type of people do Pakistani Journalists follow on Twitter?") +
	theme_bw() +
	theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
	      title = element_text(size = rel(1.2), face = "bold", colour = "#9c1819"),
	      plot.title = element_text(size = rel(1.5), hjust = 0),
	      text = element_text(family = "Cambria"))

ggplot_with_subtitle(g2, 
		     label = paste("The type of people have been divided into Pakistani Journalists and others.",
			 "Here, Pakistani Journalist Friends are the same set of people already shown in the graph.", sep = "\n"),
		     fontfamily = "Calibri",
		     cex = 1.5)
ggsave(paste(basePath, "Graphs", "j_graph2.png", sep = "/"), height = 10, width = 15, units='in', dpi=600)






br3 <- user_info_friends[!is.na(friend_name) & user_category == "Journalist" & friend_category == "Journalist", 
			 .(followedBy = length(unique(tweeter))), 
			 by = friend_name]

setorder(br3, -followedBy)
br3[, friend_name := factor(friend_name, levels = friend_name, ordered = TRUE)]

g3 <- br3 %>%
	ggplot(data = ., aes(x = followedBy, y = friend_name)) +
	geom_segment(aes(yend = friend_name), xend = 0, color = 'grey50') +
	geom_point(size = 3, colour = "#9c1819") +
	scale_x_continuous(labels = comma, breaks = pretty_breaks(n = 6)) +
	xlab("Number of Followers who are Pakistani Journalists") + ylab("Journalist") +
	ggtitle("From the same group, how many Journalists follow a particular Journalist?") +
	theme_bw() +
	theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
	      title = element_text(size = rel(1.2), face = "bold", colour = "#9c1819"),
	      plot.title = element_text(size = rel(1.5), hjust = 0),
	      text = element_text(family = "Cambria"))

ggplot_with_subtitle(g3, 
		     label = "This is an intra-group comparison to show which are the most followed Pakistani Journalists",
		     fontfamily = "Calibri",
		     cex = 1.2)
ggsave(paste(basePath, "Graphs", "j_graph3.png", sep = "/"), height = 10, width = 15, units='in', dpi=600)






br4 <- all_user_info[, tweetsPerDay := round(statuses_count/as.numeric(Sys.Date() - as.Date(created_at), unit = "days"))]
setorder(br4, -tweetsPerDay)
br4[, name := factor(name, levels = name, ordered = TRUE)]

g4 <- br4[category == "Journalist"] %>%
	ggplot(data = ., aes(x = tweetsPerDay, y = name)) +
	geom_segment(aes(yend = name), xend = 0, color = 'grey50') +
	geom_point(size = 3, colour = "#9c1819") +
	scale_x_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
	ggtitle("How much does a Journalist Typically Tweet per Day?") +
	theme_bw() +
	theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
	      title = element_text(size = rel(1.2), face = "bold", colour = "#9c1819"),
	      plot.title = element_text(size = rel(1.5), hjust = 0),
	      axis.title = element_blank(),
	      text = element_text(family = "Cambria"))

ggplot_with_subtitle(g4, 
		     label = "This graph shows the Mean number of tweets per day by each Journalist",
		     fontfamily = "Calibri",
		     cex = 1.2)
ggsave(paste(basePath, "Graphs", "j_graph4.png", sep = "/"), height = 10, width = 15, units = 'in', dpi = 500)









################# Social Network Analysis


user_info_friends[is.na(friend_name), friend_name := ""]
ge1 <- graph_from_data_frame(d = user_info_friends[friend_name != "" & user_category == "Journalist" & friend_category == "Journalist", .(name, friend_name)],
			     vertices = all_user_info[category == "Journalist", .(name, followers_count, friends_count, statuses_count, favourites_count)],
			     directed = TRUE)

V(ge1)$centralisation_degree <- centr_degree(ge1)$res
V(ge1)$centralisation_between <- centr_betw(ge1)$res
V(ge1)$centralisation_eigen <- centr_eigen(ge1)$vector
ge1 <- simplify(ge1, edge.attr.comb="sum")

ge_df <- ggnetwork(ge1, layout="fruchtermanreingold", arrow.gap=0, cell.jitter=0) %>% setDT
#ge_df_lbl <- ge_df[centralisation_eigen > quantile(centralisation_eigen, .9) | centralisation_eigen < quantile(centralisation_eigen, .3),
#			       .(x, y, vertex.names)] %>%
#	     unique %>%
#	     copy
ge_df_lbl <- ge_df[, .(x, y, vertex.names)] %>%
		unique %>%
		copy


sng1 <- ggplot() +
  		geom_edges(data = ge_df, 
        		aes(x = x, y = y, xend = xend, yend = yend),
        		color = "grey70", curvature = 0.2, size = 0.05, alpha = 1/2, arrow = arrow(length = unit(0.5, "lines"), type = "closed")) +
  		geom_nodes(data = ge_df,
             		aes(x = x, y = y, xend = xend, yend = yend, size = sqrt(centralisation_degree)),
             		colour = "#701112", alpha = .8) +
  		geom_text_repel(data = ge_df_lbl, aes(x = x, y = y, label = vertex.names), color = "#701112", family = "Cambria") +
		theme_blank() +
		theme(text = element_text(family = "Cambria"), legend.position="none")
ggsave(filename = paste(basePath, "Graphs", "j_graph5.png", sep = "/"), plot = sng1, height = 10, width = 20, units='in', dpi=200)





ge1 <- graph_from_data_frame(d = user_info_friends[friend_name != "", .(name, friend_name)],
			     vertices = all_user_info[, .(name, category, followers_count, friends_count, statuses_count, favourites_count)],
			     directed = TRUE)

V(ge1)$centralisation_degree <- centr_degree(ge1)$res
V(ge1)$centralisation_between <- centr_betw(ge1)$res
V(ge1)$centralisation_eigen <- centr_eigen(ge1)$vector
ge1 <- simplify(ge1, edge.attr.comb="sum")

ge_df <- ggnetwork(ge1, layout="fruchtermanreingold", arrow.gap=0, cell.jitter=0) %>% setDT
ge_df_lbl <- ge_df[, .(x, y, vertex.names, category)] %>%
		unique %>%
		copy


sng1 <- ggplot() +
  		geom_edges(data = ge_df, 
        		aes(x = x, y = y, xend = xend, yend = yend),
        		color = "grey70", curvature = 0.2, size = 0.05, alpha = 1/2, arrow = arrow(length = unit(0.5, "lines"), type = "closed")) +
  		geom_nodes(data = ge_df,
             		aes(x = x, y = y, xend = xend, yend = yend, size = sqrt(centralisation_degree), colour = category), alpha = .8) +
  		geom_text_repel(data = ge_df_lbl, aes(x = x, y = y, label = vertex.names, colour = category), family = "Cambria") +
		theme_blank() +
		theme(text = element_text(family = "Cambria"), legend.position="none")
ggsave(filename = paste(basePath, "Graphs", "j_graph5.png", sep = "/"), plot = sng1, height = 10, width = 20, units='in', dpi=200)









################# Correspondence Analysis


sn_df1 <- user_info_friends[user_category == "Journalist", 
				.(counts = .N), 
				by = .(name, friend_category)
		][, friend_category := ifelse(is.na(friend_category), "Others", friend_category)] %>%
			spread(friend_category, counts) 

for(j in colnames(sn_df1)){
	set(sn_df1, which(is.na(sn_df1[[j]])), j, 0)
}
df1 <- data.frame(sn_df1)
rownames(df1) <- df1$name

ca1 <- CA(df1[, -1], graph = TRUE)
ca_df1 <- ca1$row$coord[, c(1,2)] %>% 
		data.frame(stringsAsFactors = FALSE) %>%
		mutate(name = rownames(.)) %>%
		setDT
ca_df2 <- ca1$col$coord[, c(1,2)] %>% 
		data.frame(stringsAsFactors = FALSE) %>%
		mutate(name = rownames(.)) %>%
		setDT

g6 <- ggplot(data = ca_df1, aes(x = Dim.1, y = Dim.2)) +
	geom_point(size = 3, colour = "#9c1819") + 
	geom_text_repel(data = ca_df1[Dim.1 > 0.25 | Dim.1 < -0.25 | Dim.2 > 0.1 | Dim.2 < -0.1],
			aes(label = name, x = Dim.1, y = Dim.2), colour = "#9c1819", size = rel(4)) +
	geom_text_repel(data = ca_df2, aes(x = Dim.1, y = Dim.2, label = name), colour = "#189c9b", size = 5, family = "Cambria") +
	geom_hline(yintercept = 0, linetype = 2) +
	geom_vline(xintercept = 0, linetype = 2) +
	theme_void() + 
	theme(title = element_text(size = rel(1.2), face = "bold", colour = "#9c1819"),
	      plot.title = element_text(size = rel(1.5), hjust = 0),
	      #axis.title = element_blank(),
	      text = element_text(family = "Cambria"))

ggsave(paste(basePath, "Graphs", "j_graph6.png", sep = "/"), plot = g6, height = 10, width = 15, units='in', dpi=200)



sn_df1 <- user_info_friends[user_category == "Journalist" & friend_category != "Others", 
				.(counts = .N), 
				by = .(name, friend_category)
		][, friend_category := ifelse(is.na(friend_category), "Others", friend_category)] %>%
			spread(friend_category, counts) 

for(j in colnames(sn_df1)){
	set(sn_df1, which(is.na(sn_df1[[j]])), j, 0)
}
df1 <- data.frame(sn_df1)
rownames(df1) <- df1$name

ca1 <- CA(df1[, -1], graph = TRUE)
ca_df1 <- ca1$row$coord[, c(1,2)] %>% 
		data.frame(stringsAsFactors = FALSE) %>%
		mutate(name = rownames(.)) %>%
		setDT
ca_df2 <- ca1$col$coord[, c(1,2)] %>% 
		data.frame(stringsAsFactors = FALSE) %>%
		mutate(name = rownames(.)) %>%
		setDT

g7 <- ggplot(data = ca_df1, aes(x = Dim.1, y = Dim.2)) +
	geom_point(size = 3, colour = "#9c1819") + 
	geom_text_repel(data = ca_df1[Dim.1 > 0.2 | Dim.1 < -0.3 | Dim.2 > 0.15 | Dim.2 < -0.2],
			aes(label = name, x = Dim.1, y = Dim.2), colour = "#9c1819", size = rel(4)) +
	geom_text_repel(data = ca_df2, aes(x = Dim.1, y = Dim.2, label = name), colour = "#189c9b", size = 5, family = "Cambria") +
	geom_hline(yintercept = 0, linetype = 2) +
	geom_vline(xintercept = 0, linetype = 2) +
	theme_void() + 
	theme(title = element_text(size = rel(1.2), face = "bold", colour = "#9c1819"),
	      plot.title = element_text(size = rel(1.5), hjust = 0),
	      axis.title = element_blank(),
	      text = element_text(family = "Cambria"))

ggsave(paste(basePath, "Graphs", "j_graph7.png", sep = "/"), plot = g7, height = 10, width = 15, units='in', dpi=200)








################# Multi-Dimensional Scaling

c_df1 <- user_info_friends[!is.na(friend_name), .(counts = .N), by = .(name, user_category, friend_name)] %>%
		spread(friend_name, counts)

for(j in colnames(c_df1)){
	set(c_df1, which(is.na(c_df1[[j]])), j, 0)
}
df1 <- data.frame(c_df1)
rownames(df1) <- df1$name

cmd1 <- cmdscale(dist(df1[,-c(1, 2)]))

cmd_df1 <- cmd1 %>%
		data.frame(stringsAsFactors = FALSE) %>%
		mutate(name = rownames(.), user_category = df1$user_category) %>%
		setDT

g8a <- ggplot(data = cmd_df1, aes(x = X1, y = X2)) + 
	geom_point(aes(colour = user_category), size = 3) +
	scale_colour_brewer(palette = "Set1", name = "Legend") +
	theme_void() +
	theme(title = element_text(size = rel(1.2), face = "bold", colour = "#9c1819"),
	      plot.title = element_text(size = rel(1.5), hjust = 0),
	      axis.title = element_blank(),
	      text = element_text(family = "Cambria"),
	legend.position = "top", legend.direction = "horizontal", 
	legend.text = element_text(size = rel(1.1), colour = "#9c1819"), legend.background = element_rect(colour = "grey80")) +
	ggtitle("Multi-Dimensional Scaling for Pakistani Journalists & Politicians")

ggsave(paste(basePath, "Graphs", "j_graph8a.png", sep = "/"), plot = g8a, height = 10, width = 15, units='in', dpi=200)



g8b <- ggplot(data = cmd_df1, aes(x = X1, y = X2)) + 
	geom_point(aes(colour = user_category), size = 3) +
	geom_density2d(colour = "grey30") +
	scale_colour_brewer(palette = "Set1", name = "Legend") +
	theme_void() +
	theme(title = element_text(size = rel(1.2), face = "bold", colour = "#9c1819"),
	      plot.title = element_text(size = rel(1.5), hjust = 0),
	      axis.title = element_blank(),
	      text = element_text(family = "Cambria"),
	legend.position = "top", legend.direction = "horizontal", 
	legend.text = element_text(size = rel(1.1), colour = "#9c1819"), legend.background = element_rect(colour = "grey80")) +
	ggtitle("Multi-Dimensional Scaling for Pakistani Journalists & Politicians")

ggplot_with_subtitle(g8b, 
		     label = "Adding Density Curves to see areas with a Higher Concentration of Points",
		     fontfamily = "Calibri",
		     cex = 1.5)

ggsave(paste(basePath, "Graphs", "j_graph8b.png", sep = "/"), plot = g8b, height = 10, width = 15, units='in', dpi=200)


g8c <- ggplot(data = cmd_df1, aes(x = X1, y = X2)) + 
	geom_point(aes(colour = user_category), size = 3) +
	geom_text_repel(data = cmd_df1[sample(x = 1:nrow(cmd_df1), size = round(nrow(cmd_df1) * 95/100)),], 
			aes(label = name, colour = user_category), show.legend = FALSE) +
	scale_colour_brewer(palette = "Set1", name = "Legend") +
	theme_void() +
	theme(title = element_text(size = rel(1.2), face = "bold", colour = "#9c1819"),
	      plot.title = element_text(size = rel(1.5), hjust = 0),
	      axis.title = element_blank(),
	      text = element_text(family = "Cambria"),
	legend.position = "top", legend.direction = "horizontal", 
	legend.text = element_text(size = rel(1.1), colour = "#9c1819"), legend.background = element_rect(colour = "grey80")) +
	ggtitle("Multi-Dimensional Scaling for Pakistani Journalists & Politicians")

ggsave(paste(basePath, "Graphs", "j_graph8c.png", sep = "/"), plot = g8c, height = 10, width = 15, units='in', dpi=200)











################# Text Mining

# Clean source of the action (i.e. liking a tweet)
all_fav_tweets[, status_source := gsub("<a.+?>(.+?)</a>$", replacement = "\\1", x = statusSource, ignore.case = TRUE)]
all_fav_tweets[, status_source := gsub(".*iPhone.*", replacement = "iPhone", x = status_source, ignore.case = TRUE)]
all_fav_tweets[, status_source := gsub(".*Android.*", replacement = "Android", x = status_source, ignore.case = TRUE)]
all_fav_tweets[, status_source := gsub(".*Blackberry.*", replacement = "Blakberry", x = status_source, ignore.case = TRUE)]
all_fav_tweets[, status_source := gsub(".*Windows\\s*Phone.*", replacement = "Windows Phone", x = status_source, ignore.case = TRUE)]
all_fav_tweets[, status_source := gsub(".*iPad.*", replacement = "iPad", x = status_source, ignore.case = TRUE)]
all_fav_tweets[, status_source := gsub(".*Nokia.*", replacement = "Nokia", x = status_source, ignore.case = TRUE)]
all_fav_tweets[, status_source := gsub("Twitter for Mac", replacement = "Mac", x = status_source, ignore.case = TRUE)]
all_fav_tweets[, status_source := gsub(".*ios.*", replacement = "iOS", x = status_source, ignore.case = TRUE)]
all_fav_tweets[, status_source := gsub(".*GeoNews.*", replacement = "GeoNews", x = status_source, ignore.case = TRUE)]
all_fav_tweets[, status_source := gsub(".*Dunya.*", replacement = "Dunya", x = status_source, ignore.case = TRUE)]


# Clean statuses
all_fav_tweets[, clean_text := gsub("(<.+?>)+", "", text)]
all_fav_tweets[, clean_text := gsub("[[:digit:]]+", "", clean_text)]
#all_fav_tweets[, clean_text := tolower(clean_text)] #this does not work in presence of non-utf8 encodable texts
all_fav_tweets[, clean_text := gsub("@([[:alpha:]]+)\\b", "", clean_text)]
all_fav_tweets[, clean_text := gsub("#[[:alpha:]]+\\b", "", clean_text)]
all_fav_tweets[, clean_text := gsub("http\\S+\\s*", "", clean_text, ignore.case = TRUE)]  #copied from: http://stackoverflow.com/questions/25352448/remove-urls-from-string-in-r
all_fav_tweets[, clean_text := gsub("[[:punct:]]+", "", clean_text, ignore.case = TRUE)]
all_fav_tweets[, clean_text := gsub("amp", " ", clean_text)]
all_fav_tweets[, clean_text := gsub("pakistanis*", "pakistan", clean_text, ignore.case = TRUE)]
all_fav_tweets[, clean_text := gsub("indians*", "india", clean_text, ignore.case = TRUE)]
all_fav_tweets[, clean_text := gsub("[^0-9A-Za-z///' ]", "", clean_text)] #copied from: http://stackoverflow.com/questions/11970891/r-remove-special-characters-from-data-frame
all_fav_tweets[, clean_text := gsub("\\s+", " ", clean_text)]
all_fav_tweets[, clean_text := gsub("^\\s+|\\s+$", "", clean_text)]

# (Try to) Detect language
all_fav_tweets[, lang := textcat(text)]

# Re-clean statuses
all_fav_tweets[lang == "english", clean_text := tolower(clean_text)]

# Merge required data into a data frame
fav_tweets_info <- merge(all_fav_tweets[, .(tweeter, screenName, status_source, clean_text, lang)],
			 all_user_info[, .(name, screen_name, category)],
			 by.x = "tweeter", by.y = "screen_name", all = FALSE)





# Sentiment scoring
edic <- sentiments %>% copy %>% setDT


ssg1 <- 
fav_tweets_info[category == "Journalist" & lang == "english", .(name, clean_text)] %>%
		group_by(name) %>%
		unnest_tokens(output = word, input = clean_text) %>%
		anti_join(stop_words) %>%
		inner_join(edic[lexicon == "bing", .(word, sentiment)]) %>%
		group_by(name, sentiment) %>%
		summarise(totals = n()) %>%
		ungroup() %>%
		group_by(name) %>%
		mutate(totalWords = sum(totals)) %>%
		spread(sentiment, totals, fill = 0) %>%
		mutate(overallScore = positive - negative) %>%
		mutate(sentimentFlag = ifelse(overallScore > 0, "Pos", "Neg")) %>%
		ggplot(data = ., aes(y = overallScore, x = reorder(name, overallScore))) +
		geom_bar(stat = "identity", aes(fill = sentimentFlag, alpha = totalWords)) +
		coord_flip() +
		scale_fill_manual(values = c("#9c1819","#189c59")) +
		guides(fill = FALSE, alpha = FALSE) +
		ylab("Sentiment Score") + xlab("") +
		scale_y_continuous(breaks = pretty_breaks(n = 7)) +
		theme_minimal() +
		theme(title = element_text(size = rel(1.2), face = "bold", colour = "#9c1819"),
	      		plot.title = element_text(size = rel(1.5), hjust = 0), text = element_text(family = "Cambria")) +
		ggtitle("Scoring Sentiments of Pakistani Journalists' Favourite Tweets")


ggplot_with_subtitle(ssg1, 
		     label = paste("Sentiment score is the simple sum of the count of Positive & Negative words. Opacity of graphs is a function of the Total number of words",
				   "scored for Sentiment. Hence, a transparent graph indicates Low Total number of words."
				    , sep = "\n"),
		     fontfamily = "Calibri",
		     cex = 1.2)

ggsave(paste(basePath, "Graphs", "j_graph9.png", sep = "/"), height = 8, width = 12, units = 'in', dpi = 500)






# Topic Modelling, Clustering
totalTopics <- 7

tm1 <- 	fav_tweets_info[category == "Journalist" & lang == "english", .(name, clean_text)] %>%
		group_by(name) %>%
		mutate(fav_index = row_number(.)) %>%  #add index for each person's fav tweets, otherwise all tweets fav'd by a person are counted as 1 document
		ungroup() %>%
		unnest_tokens(output = word, input = clean_text) %>%
		#mutate(word = wordStem(word)) %>%
		anti_join(x = ., y = stop_words) %>%
		group_by(name, fav_index, word) %>%
		summarise(totalCounts = n()) %>%
		ungroup %>% 
		split(x = ., f = .$name) %>%
		map(.f = function(x) cast_dtm(data = x, document = fav_index, term = word, value = totalCounts)) %>%
		map(.f = function(doc) LDA(x = doc, k = totalTopics, control = list(seed = 007)))
 


tm2 <- 	tm1 %>%
		map(.f = function(x) tidy(x)) %>%
		map2(.x = ., .y = as.list(names(tm1)), .f = cbind) %>%
		rbindlist %>%
		rename(name = `.y[[i]]`) %>%
		group_by(name, topic) %>%
		top_n(n = 10, wt = beta) %>%
		ungroup()
				
		
# Visualising topics for journalists with the extreme sentiment scores

# Negative sentiment scores
nj <- c("Murtaza Ali Shah", "Raza Ahmad Rumi", "Moeed Pirzada", "Zarrar Khuhro", "Nasim Zehra", "Shaheen Sehbai")
# Positive sentiment scores
pj <- c("Waseem Badami", "Fahd Husain", "Meher Bokhari", "Hassan Nisar", "Mubasher Lucman")


	tm2 %>% 
		filter(name %in% nj) %>%
		spread(topic, beta, fill = 0) %>%
		setnames(old = names(.)[grepl("[[:digit:]]+", names(.))], 
			 new = paste("Topic", 1:totalTopics)) %>%
		data.frame(stringsAsFactors = FALSE) %>%
		split(f = .$name, drop = TRUE) %>%
		map(function(df){dfTemp <- df; row.names(dfTemp) <- dfTemp$term; return(dfTemp)}) %>%
		map(function(df){tdm <- as.matrix(df[, !names(df) %in% c("name", "term")])
				 fDir <- paste(basePath, "Graphs", sep = "/")
				 fName <- paste0(unique(df$name), "_wc.png")
				 png(filename = paste(fDir, fName, sep = "/"), width=12, height=8, units="in", res=300)
				 comparison.cloud(tdm, max.words = 100)
				 dev.off()})


	tm2 %>% 
		filter(name %in% pj) %>%
		spread(topic, beta, fill = 0) %>%
		setnames(old = names(.)[grepl("[[:digit:]]+", names(.))], 
			 new = paste("Topic", 1:totalTopics)) %>%
		data.frame(stringsAsFactors = FALSE) %>%
		split(f = .$name, drop = TRUE) %>%
		map(function(df){dfTemp <- df; row.names(dfTemp) <- dfTemp$term; return(dfTemp)}) %>%
		map(function(df){tdm <- as.matrix(df[, !names(df) %in% c("name", "term")])
				 fDir <- paste(basePath, "Graphs", sep = "/")
				 fName <- paste0(unique(df$name), "_wc.png")
				 png(filename = paste(fDir, fName, sep = "/"), width=12, height=8, units="in", res=300)
				 comparison.cloud(tdm, max.words = 100)
				 dev.off()})




		map2(.x = ., .y = names(.), .f = 

		tbl_df %>%
		head
	
		ggplot(data = ., aes(x = reorder(term, beta), y = beta)) +
		geom_bar(stat = "identity") +
		facet_wrap(~ topic + name, scales = "free") + 
		theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1))

































################# Clustering


kc <- 2:20

kc %>% 
	map(.f = function(c) kmeans(x = cmd_df1[, c("X1", "X2"), with = FALSE], centers = c, nstart = 100)) %>%
	map_dbl("betweenss") %>%
	data.frame(centers = kc, betweenss = .) %>%
	ggplot(data = ., aes(x = centers, y = betweenss)) +
	geom_point() + geom_line()

kc %>% 
	map(.f = function(c) kmeans(x = cmd_df1[, c("X1", "X2"), with = FALSE], centers = c, nstart = 100)) %>%
	map_dbl("tot.withinss") %>%
	data.frame(centers = kc, withinss = .) %>%
	ggplot(data = ., aes(x = centers, y = withinss)) +
	geom_point() + geom_line()

cmd_df1[, clusterN := kmeans(x = cmd_df1[, c("X1", "X2"), with = FALSE], centers = 4, nstart = 100)$cluster]

g8d <- ggplot(data = cmd_df1, aes(x = X1, y = X2)) + 
	geom_point(aes(colour = user_category), size = 3) +
	geom_density2d(aes(colour = factor(clusterN))) +
	scale_colour_brewer(palette = "Set1", name = "Legend") +
	theme_void() +
	theme(title = element_text(size = rel(1.2), face = "bold", colour = "#9c1819"),
	      plot.title = element_text(size = rel(1.5), hjust = 0),
	      axis.title = element_blank(),
	      text = element_text(family = "Cambria"),
	legend.position = "top", legend.direction = "horizontal", 
	legend.text = element_text(size = rel(1.1), colour = "#9c1819"), legend.background = element_rect(colour = "grey80")) +
	ggtitle("Multi-Dimensional Scaling for Pakistani Journalists & Politicians")

ggplot_with_subtitle(g8d, 
		     label = "Adding Density Curves to see areas with a Higher Concentration of Points",
		     fontfamily = "Calibri",
		     cex = 1.5)

ggsave(paste(basePath, "Graphs", "j_graph8d.png", sep = "/"), plot = g8d, height = 10, width = 15, units='in', dpi=200)





km1 <- kmeans(x = df1[, -1]
	

ggplot_with_subtitle(g1, 
		     label = "How the following and friendship of Pakistani Journalists on Twitter stacks up against each other",
		     fontfamily = "Calibri",
		     cex = 1.5)
ggsave(paste(basePath, "Graphs", "j_graph1.png", sep = "/"), height = 10, width = 12, units='in', dpi=600)




df2 <- df1[, -1]

df3 <- dist(df2)



sn_df1 <- user_info_friends[, .(counts = .N), by = .(name, user_category, friend_category)
		][, friend_category := ifelse(is.na(friend_category), "Others", friend_category)] %>%
			spread(friend_category, counts)






# More ideas: cluster and use hive plot, find journalists followed by most other journalists, see of the political parties, whom does each journalist follow the most



ge2 <- cluster_edge_betweenness(ge1)
ge3 <- simplify(contract(ge1, membership(ge2))) 
plot(ge3)

ge2 <- cluster_infomap(ge1)
ge3 <- simplify(contract(ge1, membership(ge2))) 
plot(ge3)


ge1 <- graph.edgelist(as.matrix(user_info_friends[friend_name != "", .(name, friend_name)]))
#ge1 <- graph.edgelist(as.matrix(user_info_friends[, .(user_id, friend_id)]))
ge2 <- cluster_louvain(ge1)


#ge1 <- simplify(ge1, edge.attr.comb="sum")
#ge2 <- cluster_walktrap(ge1)
ge2 <- cluster_spinglass(ge1)
ge3 <- simplify(contract(ge1, membership(ge2))) 







