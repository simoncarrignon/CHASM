library(rtweet)

token <- create_token(
  app = "",
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = "")



ind=0
#this loops simply listen to twitter stream during 5 minute then write the results in a file and start again
while(TRUE){
    test=stream_tweets("bernie",timeout=5*60,parse=FALSE,file_name=paste0("sanders_",ind, ".json"))
    ind=ind+1
}
