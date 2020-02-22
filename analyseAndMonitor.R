library(rtweet)

token <- create_token(
  app = "",
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = "")


##Initialise the list with some first " small" analayse 
files=list.files(pattern=paste0("bernie[1234].*.json"))
active_users=lapply(files,function(f)
                    {
                        tryCatch(
                                 {
                                     print(paste("parsing file",f))
                                     parsed=parse_stream(f)
                                     parsed$screen_name
                                 },error = function(w) {
                                     return(NULL)
                                 })
                    })
counts=getNewCounts(unlist(active_users))
tmpcounts=updateCounts(counts,c())
watchlist=updateWatchList(counts,c(),10)
watchtimlines=lapply(watchlist,get_timeline,n=3000)
    oldfiles=c(list.files(pattern="bernie[012345].*.json"))

##Monitore
wait=0
while(TRUE){
    files=c(list.files(pattern="bernie.*.json"),oldfiles)
    newfiles = files[ !(files %in% oldfiles)]
    oldfiles=unique(files)
    if(length(newfiles)>0){
        active_users=lapply(newfiles,function(f)
                            {
                                print(paste("parsing file",f))
                                tryCatch(
                                         {
                                             parsed=parse_stream(f)
                                             parsed$screen_name
                                         },error = function(w) {
                                             return(NULL)
                                         })
                            })
        print(paste("parsing",length(active_users)," active users"))
        newcounts=getNewCounts(unlist(active_users))
        rm(active_users)
        tmpcounts=updateCounts(counts,newcounts)
        print(paste(nrow(tmpcounts)-nrow(counts),"new active users added"))
        if(nrow(tmpcounts)<nrow(counts)){print("problem");print(nrow(tmpcounts));print(nrow(counts))}
        if(nrow(tmpcounts)>nrow(counts)){
            counts=tmpcounts
            rm(tmpcounts)
            print("update users on the watchlist")
            newwatchlist=updateWatchList(counts,watchlist,50)
            if(length(newwatchlist)>0){
                print(paste(length(newwatchlist)," new users added on the watchlist"))
                watchlist=c(watchlist,newwatchlist)
                print(paste("updating watchlist timeline"))
                watchtimlines=appendWatchList(watchlist,watchtimlines)
                print(paste("done updating watchlist timeline"))
                writeAll(watchtimlines,"%Y%m%d-%H")
            }
        }
        save(file="supsects.bin",watchtimlines)

    }
    else {
        print("waiting")
        wait=wait+1
        if(wait%%10 == 0){
                print("force update")
                watchtimlines=appendWatchList(watchlist,watchtimlines)
                writeAll(watchtimlines,"%Y%m%d-%H")
        }
        Sys.sleep(60) 
    }
}


###### the functions below should be loaded before


#' @param nameslist a vector of character that list twitter username
#' @param timeline_list a list of timelines 
#' @return return a new list of timelines with the tweets of each names  in namelist.
#  if one of the elements of nameslist was not already in timeline_list we add id and collect hist last 3000 tweets
#  if not we check if the account tweeted since the last time and add the new one to users dataframe
appendWatchList <- function(nameslist,timeline_list){
    for(name in nameslist){
        prev_timeline=timeline_list[[name]]
        last_tweet=prev_timeline[1,]
        if(is.null(last_tweet))
            new_tweets=get_timeline(name,3000)
        else
            new_tweets=get_timeline(name, since_id=last_tweet$status_id)
        if(nrow(new_tweets)>0){
            timeline_list[[name]] = rbind(new_tweets,prev_timeline)
            print(paste(name,"tweeted",nrow(new_tweets),"more time"))
        }
        Sys.sleep(1)
    }
    return(timeline_list)
}

#from a list of apparition of useres return a dataframe with count of tweets for each name
    getNewCounts <- function(listusers){
        tmpcounts=table(listusers)
        counts=data.frame(screen_name=as.character(names(tmpcounts)),counts=as.numeric(tmpcounts))
        return(counts)
    }

#this function compare a list of name that are watched with a dataframe with count of tweets and names and return the names of accoutns form this dataframe that are not in the watched list
    updateWatchList <- function(newcount,initial_namelist,n){
        newtop=newcount$screen_name[order(newcount$counts,decreasing=T)][1:n]
        #newtop=newcount$screen_name[newcount$counts>50]
        res=newtop[!(newtop %in% initial_namelist)]
        res=as.character(res)
        names(res)=res
        return(unique(res))
    }

#this function takes two dataframe with count of tweet for user name and merge them by summing the number of tweets for those that match in the two list or adding the one that are absent in one df or the other
    updateWatchList <- function(newcount,initial_namelist,n){
    updateCounts  <- function(oldcounts,newcount){
        oldcounts$counts[which(oldcounts$screen_name %in% newcounts$screen_name)] =
        newcounts$counts[which(newcounts$screen_name %in% oldcounts$screen_name)] +
        oldcounts$counts[which(oldcounts$screen_name %in% newcounts$screen_name)]
        newres=rbind(
              oldcounts,
              newcounts[which(!(newcounts$screen_name %in% oldcounts$screen_name)),]
              )
        return(newres)
    }

#Take a list of timeline and write a csv with the count of tweets per hours (or something else defined by `timeformat`
    writeAll <- function(listoftimelines,timeformat="%Y%m%d-%H"){
        alldates=sort(unique(unlist(sapply(watchtimlines,function(i)unique(lapply(i$created_at,format,format=timeformat ))))))
        formatted=sapply(watchtimlines,function(i)sapply(i$created_at,format,format=timeformat ))
        counts=sapply(formatted,function(i)table(factor(i,levels=alldates)))
        id=0
        outname=paste0("activity_watched",id,".csv")
        while(file.exists(outname)){
            outname=paste0("activity_watched",id,".csv")
            id=id+1
        }
        write.csv(file=outname,t(counts))
        write.csv(file=file.path("~/public_html/bd4ss/data/",outname),t(counts))
    }
