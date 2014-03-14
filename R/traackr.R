GetInfluencerConnections <- function(username,api_key){

library(rjson)


#username <- "Jack"


#Get Influencer
#Get Twitter User uid
#Test
url <- paste("http://api.traackr.com/1.0/influencers/lookup/twitter/",username,"?api_key=",api_key,sep = "")
answer <- fromJSON(file=url, method='C')


data = answer$influencer 
uid <- data[[1]]$uid
uid <- uid[1]

message <- paste("UID of Twitter-User",username,":",uid)
print(message)

#######################################

#Get Influencer Connections:

#UIDs

url <- paste("http://api.traackr.com/1.0/influencers/connections/",uid,"?api_key=",api_key,sep = "")

answer <- fromJSON(file=url, method='C')


connections <- answer[[1]]

length <- length(connections[[1]]$connections_from)

df = data.frame(no = 1:length)

for (i in 1:nrow(df)){
 
df$uid[i] = connections[[1]]$connections_from[[i]]$native_id
 

 
}


#Names

for (i in 1:nrow(df)){


user_uid = df$uid[i]	

url <- paste("http://api.traackr.com/1.0/influencers/show/",user_uid,"?with_channels=false&api_key=",api_key,sep = "")
name_all <- fromJSON(file=url, method='C')


name <- name_all$influencer
name <- name[[1]]$name


df$name[i] = name


title <- name_all$influencer
title <- title[[1]]$title

if(nchar(title)<1)
{title <- "NA"}


df$title[i] = title

message <- paste(i,"of",length)
print(message)


}

print(df)




return(df)


}