GetInfluencerConnections <- function(username,api_key){

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

for (i in 1:(nrow(df))){


user_uid = df$uid[i]	

if(identical("INFLUENCER NOT IN SYSTEM",user_uid)==FALSE){


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
}

else{
	df$name[i] = "/"
	df$title[i] = "/"

}



message <- paste(i,"of",length)
print(message)


}

#print(df)




return(df)


}