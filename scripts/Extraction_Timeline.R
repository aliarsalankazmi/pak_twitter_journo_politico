################# Loading Libraries

pkgs <- c("purrr", "data.table", "dplyr", "rtweet", "ggplot2", "twitteR")
sapply(pkgs, require, character.only = TRUE)





################# Obtain an authentication token

apiKey <- "etjOWrV0DhWRdjnWpK4LDnZhF"
apiSecret <- ""
setup_twitter_oauth(apiKey, apiSecret)



################# Obtain followers for Noon League

mnTL <- userTimeline(user = "MaryamNSharif", n = 3200)


mnF <- favorites(user = "MaryamNSharif", n = 3000)




