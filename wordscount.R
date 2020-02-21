library(tidyverse)
library(tidytext)
library(rtweet)


#Take a timeline and return all unique words from this timeline
getAllWords <- function(timeline,replace_reg=NULL,unnest_reg=NULL){
    if(is.null(replace_reg))replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
    if(is.null(unnest_reg))unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
    alltext = timeline %>% filter(!str_detect(text, "^RT")) %>% 
    mutate(text = str_replace_all(text, replace_reg, ""))
    text=alltext[,c("text","created_at")]
    allwords=text %>% unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
    filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))
    return(allwords[,"word"])
}


#count number of words
getWordsCounts <- function(listwords,wordspace){
    counts=NULL
    if(is.null(dim(listwords)))
       counts=table(factor(listwords[,"word"],levels=wordspace)) #ira dataset
   else
       counts=table(factor(listwords[["word"]],levels=wordspace)) #rtweet output
    return(counts/sum(counts))
}


allfiles=list.files("ira_data/",full.names=T) #get all filenames
alldf=lapply(allfiles,read.csv)#import all files as dataframe (may take times)

allTL=lapply(alldf,getAllWords) # extract all words for all datagrames and stor in a list

names(allTL)=basename(allfiles) #give to each element of our list the names of the files
names(allTL)=sapply(names(allTL),substring,first=1,last=10) #shorten the name

allwords=unlist(allTL) #generate a unique list of all words used by all twitter accounts in our databse
vocab=unique(allwords)
countedword=lapply(allTL,getWordsCounts,wordspace=vocab) #now we count for each twitter account which words they use in their own vocabulary and at wich frequency, and which words they don't use

names(countedword)=paste(names(allTL),lapply(countedword,function(n)paste0(names(sort(n,decreasing=T)[1:5]),collapse=" "))) #here we add the top five words used by each account to visualise them on a graph

wordscounts=table(allwords) #count how many each of these word have been used
mostused=sort(wordscounts,decreasing=T) #rank the words by their usage
mostused=mostused[mostused>20] #take the words that have been used more than 20 times
limitedmatrix=sapply(countedword,"[",names(mostused)) #we restrain our analyse to those words
plot(hclust(dist(t(limitedmatrix))),cex=.8)

#redo with known people 



users <- c("simoncarrignon","holyhologram","LeCuisinotron","damianjruck","ralexbentley","mcotsar","duransamson","xilrian","heuredelasieste","damiengrapton","svalver","ricard_sole","brigan_raman","acerbialberto")
alltl=lapply(users,get_timelines,n=3000)
alltl_words=lapply(alltl,getAllWords)
names(alltl_words)=users
allwords=unlist(alltl_words) 
vocab=unique(allwords)
countedword=sapply(alltl_words,getWordsCounts,wordspace=vocab)
plot(hclust(dist(t(countedword))),cex=.8)
