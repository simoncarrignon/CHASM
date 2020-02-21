library(tweetbotornot)
#  this function take a file from the IRA dataset and covnert it in rtweet format (then readable by botornot)
ira2rtweets <- function(tl){
    ##so far this is the only modif I did
    tl$created_at=as.POSIXct(tl$created_at)
    return(tl)
}


#you have to use token registrations before being able to do what follows: 

## get online timelines
LeCuisinotron <- get_timelines("LeCuisinotron", n = 3000)
damianjruck <- get_timelines("damianjruck", n = 3000)
heuredelasieste <- get_timelines("heuredelasieste", n = 3000)
temptoetiam <- get_timelines("temptoetiam", n = 3000)

#you can directly check multiple useres
users <- c("simoncarrignon","holyhologram","LeCuisinotron","damianjruck")

botornot(users)


##offline bot detect using ira data:
alltestL=sapply(list.files("ira_data",full.names=T),function(i)
       {
           singleTL=read.csv(i,row.names=1)
           #tryCatch({as.data.frame(botornot(singleTL,fast=T))},error = function(e){data.frame(screen_name=unique(singleTL$screen_name),user_id=unique(singleTL$user_id),prob_bot=NA)})
           try(botornot(singleTL,fast=F))
  })


# get all tls fro ira folders
allIraTl  <- lapply(list.files("ira_data",full.names=T),read.csv,row.names=1)
allIraTl  <- lapply(allIraTl,ira2rtweets)

#try one :
botornot(allIraTl[[1]])


# botscore for all:
allBotScore  <- sapply(allIraTl,botornot,fast=F)

