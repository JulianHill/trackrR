\name{GetInfluencerConnections}
\alias{GetInfluencerConnections}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Influencers and Connections
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetInfluencerConnections(username, api_key)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{username}{
%%     ~~Describe \code{username} here~~
}
  \item{api_key}{
%%     ~~Describe \code{api_key} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (username, api_key) 
{
    library(rjson)
    url <- paste("http://api.traackr.com/1.0/influencers/lookup/twitter/", 
        username, "?api_key=", api_key, sep = "")
    answer <- fromJSON(file = url, method = "C")
    data = answer$influencer
    uid <- data[[1]]$uid
    uid <- uid[1]
    message <- paste("UID of Twitter-User", username, ":", uid)
    print(message)
    url <- paste("http://api.traackr.com/1.0/influencers/connections/", 
        uid, "?api_key=", api_key, sep = "")
    answer <- fromJSON(file = url, method = "C")
    connections <- answer[[1]]
    length <- length(connections[[1]]$connections_from)
    df = data.frame(no = 1:length)
    for (i in 1:nrow(df)) {
        df$uid[i] = connections[[1]]$connections_from[[i]]$native_id
    }
    for (i in 1:nrow(df)) {
        user_uid = df$uid[i]
        url <- paste("http://api.traackr.com/1.0/influencers/show/", 
            user_uid, "?with_channels=false&api_key=", api_key, 
            sep = "")
        name_all <- fromJSON(file = url, method = "C")
        name <- name_all$influencer
        name <- name[[1]]$name
        df$name[i] = name
        title <- name_all$influencer
        title <- title[[1]]$title
        if (nchar(title) < 1) {
            title <- "NA"
        }
        df$title[i] = title
        message <- paste(i, "of", length)
        print(message)
    }
    print(df)
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
