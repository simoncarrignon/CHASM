library(tidyverse)
library(tidytext)

getAllWords <- function(timeline,replace_reg=NULL,unnest_reg=NULL,N=10){
    if(is.null(replace_reg))replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
    if(is.null(unnest_reg))unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
    alltext = timeline %>% filter(!str_detect(text, "^RT")) %>% 
    mutate(text = str_replace_all(text, replace_reg, ""))
    text=alltext[,c("text","created_at")]
    allwords=text %>% unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
    filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))
    return(allwords)
}

getWordsCounts <- function(listwords,wordspace){
    counts=table(factor(listwords[,"word"],levels=wordspace))
    return(counts/sum(counts))
    }

allfiles=list.files("ira_data/")
allTL=lapply(allfiles,function(i)getAllWords(read.csv(file.path("ira_data/",i))))
names(allTL)=allfiles
names(allTL)=sapply(allfiles,substring,first=1,last=10)

words=unique(unlist(lapply(allTL,"[", "word")))

wordscounts=table(unlist(lapply(allTL,"[", "word")))


countedword=lapply(allTL,getWordsCounts,wordspace=words)

mostused=sort(wordscounts,decreasing=T)
mostused=mostused[mostused>1]

names(countedword)=paste(names(allTL),lapply(countedword,function(n)paste0(names(sort(n,decreasing=T)[1:5]),collapse=" ")))

limitedmatrix=sapply(countedword,"[",names(mostused))
plot(hclust(dist(t(limitedmatrix))),cex=.8)





