#install.packages("iterators")
#install.packages("XML")
#install.packages("httr")
#install.packages("itertools")
#install.packages("RColorBrewer")
#install.packages("PubMedWordcloud")

library(wordcloud)
library(tm)
library(iterators)
library(itertools)
library(RColorBrewer)
#library(PubMedWordcloud)




##make sure it is the publisHTML URL IE pubhtml#
url <- "https://docs.google.com/a/toutbay.com/spreadsheets/d/18FUQzBiSr-NFy6aSGR06uXYQMlX2WEkLnC3FqFi3RN0/pubhtml#"
googleSpreadSheet <- readGoogleSheet(url)
googleSpreadSheet <- cleanGoogleTable(googleSpreadSheet, table=1)

#HardCoded by ordinal position of columns in Google SpreadSheet. Additional columns get 1
masterWeightsVector <- c(12,8,8,8,6,6,4,4,2,2)
#a color list based on frequency or by word order if ordered.colors is specified. **based off Column
#masterColorVector <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#Triad System
masterColorVector <- c("#E67A17", "#E6B317", "#9A833D", "#7B5E04", "#03434C", "#080756", "#26575E", "#35346B","#107D8D","#2825A0","#F6A962")
#masterColorVector <- brewer.pal(8,"Dark2")
#masterColorVector <- c(masterColorVector,c("#66CCFF","#FF0000","#000000"))

masterColorVector <- brewer.pal(11,"Set3")
WordsColorVector <- character()
WordsVector <- character()
WordsFreqVector <- numeric();

##Iterate over Data Frame

itrGoogleSpreadSheet <- ihasNext(googleSpreadSheet)
itrVectorWeights <- ihasNext(masterWeightsVector)
itrVectorColor <- ihasNext(masterColorVector)

while (hasNext(itrGoogleSpreadSheet)) {
  words = nextElem(itrGoogleSpreadSheet)
  if (hasNext(itrVectorWeights)==FALSE){wordFreq = 1} else{wordFreq = nextElem(itrVectorWeights)}
  if (hasNext(itrVectorColor)==FALSE){} else{wordColor = nextElem(itrVectorColor)}
  if (hasNext(itrGoogleSpreadSheet)==FALSE){ WordsVector = c(WordsVector,toupper(words[!is.na(words)]))} 
else{WordsVector = c(WordsVector,words[!is.na(words)])}
#WordsVector <- c(WordsVector,words[!is.na(words)])
WordsFreqVector <-c(WordsFreqVector,rep(wordFreq,length(words[!is.na(words)])))
WordsColorVector <-c(WordsColorVector,rep(wordColor,length(words[!is.na(words)])))
}


titleName <- "WordCloud1.png"
png(titleName, width=12, height=8, units="in", res=500)
##FONT vfont=c("sans serif","plain"))
wordcloud(WordsVector, WordsFreqVector, rot.per = .15,scale=c(3,.25), min.freq = 1,random.order=F,colors=WordsColorVector, ordered.colors=TRUE)
#wordcloud(WordsVector, WordsFreqVector, max.words = 200, rot.per = .15,scale=c(3,.25), min.freq = 1,random.order=FALSE,colors=WordsColorVector,use.r.layout=F, ordered.colors=TRUE)
dev.off()

titleName <- "WordCloud2.png"
png(titleName, width=12, height=8, units="in", res=500)

plotWordcloud(WordsVector, WordsFreqVector, random.order=F, max.words = 300,colors=WordsColorVector,min.freq = 1, algorithm = "circle", rot.per = .15)
dev.off()


display.brewer.all()

