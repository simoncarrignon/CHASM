
library(devtools) #we will load the local rtweets version to be able to modify it and get my version of it
load_all("rtweet/") #install and load local version

# Be carefull that rtweets should be on the branch "counts" ie `git checkout counts`


#allCharles <- search_fullarchive(q="from:CharlesMBlow",fromDate="201501010000", toDate="201701010000",env_name="awesome",count=TRUE,parse=F)

#Before anything you'll have to get Api authorisation


token <- create_token(
  app = "yourappname",
  consumer_key = "yourconsumerkey",
  consumer_secret = "yourconsumersecret",
  access_token = "youraccesstoken",
  access_secret = "youraccesssecret")

environment = "awesome" #your developer environment name

candidates=c("CharlesMBlow","aliceantheaume")
names(candidates)=candidates

allacountsTL = list()
for( c in candidates){
    all=list()

    #a proximate handling of the request. This 1/ could/should be avoid using the next_token send back by Twitter and not month by month 2/ should groups users 
    for(YY in 2015:2017){
        for (MM in 1:12){
            dat=paste0(YY,sprintf("%02d",MM),"010000")
            if(MM==02)dat2=paste0(YY,sprintf("%02d",MM),"280000")
            else if(MM%in%c(1,3,5,7,8,10,12))dat2=paste0(YY,sprintf("%02d",MM),"310000")
            else dat2=paste0(YY,sprintf("%02d",MM),"300000")
            all[[as.character(dat)]]=search_fullarchive(q=paste0("from:",c),fromDate=dat, toDate=dat2,env_name=environment,count=TRUE,parse=F)
            Sys.sleep(1) #traiting twitter api with care 
    }}


    allcounts=do.call("rbind",lapply(all,function(i)i[[1]][[1]]))

    allcounts$weeks=1:nrow(allcounts)%/% 7

    byweekcount=tapply(allcounts$count,allcounts$weeks,sum)

    allacountsTL[[c]]=byweekcount
}


