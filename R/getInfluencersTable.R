GetInfluencerConnectionsTable <- function(username,api_key){

library(rjson)

 

#username <- "Jack"



#Get Influencer
#Get Twitter User uid

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

uid = connections[[1]]$connections_from[[i]]$native_id

type = connections[[1]]$connections_from[[i]]$type
#if influencer is just Twitter User, not Traackr User
if(identical("TRAACKR",type)==FALSE)	{


	df$uid[i] = "INFLUENCER NOT IN SYSTEM"
}
else{


 
df$uid[i] = uid
 
 }

 
}

#####################################
#
#Get influencer info

#create influencer list:


influencer_list <- df$uid
#length <- length(influencer_list)
influencer_string <- paste(as.list(influencer_list),collapse=",")

url <- paste("http://api.traackr.com/1.0/influencers/show/",influencer_string,"?with_channels=false&api_key=",api_key,sep = "")
name_all <- fromJSON(file=url, method='C')	


length <- length(name_all[[1]])

# the influencers
influencers <- name_all[[1]]
######################

for (i in 1:length){

#the current influencer
curr_influencer <- influencers[[i]]

#influencer UID
user_uid = curr_influencer$uid	

if(identical("INFLUENCER NOT IN SYSTEM",user_uid)==FALSE){


#influencer name
name <- curr_influencer$name


df$name[i] = name

# influencer title
title <- curr_influencer$title


if(nchar(title)<1)
{title <- "NA"}


df$title[i] = title


#Linfluencer location

location <- curr_influencer$location

if(nchar(location)<1)
{location <- "NA"}


df$location[i] = location

}
# if the User is not a Twitter user, but a "Traackr" user
else{
	df$name[i] = "/"
	df$title[i] = "/"

}

}
library(gridExtra)
grid.table(df)


}