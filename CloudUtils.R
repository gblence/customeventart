# CleanTweets() - Takes the junk out of a vector of
CleanTweets <- function(some_txt)
{
  some_txt <- iconv(some_txt, to='ASCII', sub='')
  some_txt <- sub('â€”', '', some_txt)
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt <- sub('\n', '', some_txt)
  
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}

ucfirst <- function(txt) {
  uc_text <- paste(
    u_to_upper_case(substring(txt, 1, 1)), 
    substring(txt, 2),
    sep="", collapse=""
  )
  
  return(uc_text)
} 


####CODE From github
##https://github.com/dirkchen/twitter-hashtag-analytics
ConstructCorpus <- function(textVec) {
  
  # Construct text corpus
  text   <- c(textVec)
  
  more.stopwords <- c("via", "rt", "mt", "amp")
  
  # create a object
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, TrimToASCII)
  corpus <- tm_map(corpus, TrimUrls)
  
  
  #corpus <- tm_map(corpus, stripWhitespace) 
  
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, TrimHtml)
  
  
  corpus <- tm_map(corpus, TrimHashtags)
  
  corpus <- tm_map(corpus, TrimUsers)
  
  
  
  corpus <- tm_map(corpus, removePunctuation) 
  
  
  corpus <- tm_map(corpus, removeNumbers)
  
  
  #corpus <- tm_map(corpus, removeWords, stopwords("english")
  corpus <- tm_map(corpus, function(x) 
    removeWords(x, append(stopwords("english"), more.stopwords)))
  
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, TrimWhiteSpace)
  return(corpus)
}
##!!!!check out this for future https://r-forge.r-project.org/scm/viewvc.php/pkg/wordcloud/R/cloud.R?view=markup&root=cens&pathrev=110
MakeWordCloud <- function(words,freqs,titleName) { 
  # create document term matrix applying some transformations
#   ap.tdm <- TermDocumentMatrix(corpus)
#   ap.m <- as.matrix(ap.tdm)
#   ap.v <- sort(rowSums(ap.m), decreasing=TRUE)
#   ap.d <- data.frame(word = names(ap.v), freq=ap.v)
#   table(ap.d$freq)
  pal2 <- brewer.pal(8, "Dark2")
  
  
  #fileName <- deparse(substitute(corpus))
  # save the image in png format
  png(titleName, width=12, height=8, units="in", res=500)
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, titleName)
  wordcloud(ap.d$word, ap.d$freq, 
            scale=c(8, .8), min.freq = 3, 
            max.words = Inf, random.order = FALSE, 
            rot.per = .15, colors = pal2,main="Title")
  dev.off()
}
MakeComparisonWordCloud <- function(corpus,titleName) {
  ap.tdm <- TermDocumentMatrix(Users.All.Corpus)
  ap.m <- as.matrix(ap.tdm)
  colnames(ap.m) = c("Cigar City", "Tampa Bay Brewing", "Boston Beer Co","New Belgium Brewing Co")
  ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
  ap.d <- data.frame(word = names(ap.v),freq=ap.v)
  table(ap.d$freq)
  pal2 <- brewer.pal(8,"Dark2")
  png("titleName", width=12,height=8, units='in', res=300)
  ###
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, "titleName")
  ###
  comparison.cloud(ap.m, scale=c(4,.2),min.freq=3,
                   max.words=Inf, random.order=FALSE, rot.per=.15,title.size=1.5, colors=pal2,main="Title")
  dev.off()
}

MakeCommonWordCloud <- function(matrix,titleName) {
  ap.tdm <- TermDocumentMatrix(Users.All.Corpus)
  ap.m <- as.matrix(ap.tdm)
  
  #fileName <- deparse(substitute(corpus))
  # save the image in png format
  png(titleName, width=12, height=8, units="in", res=500)
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, titleName)
  #wordcloud(ap.d$word, ap.d$freq, 
  #         scale=c(8, .4), min.freq = 5, 
  #        max.words = 500, random.order = FALSE, 
  #       rot.per = .15,title.size=1.5, colors = pal2,main="Title")
  
  commonality.cloud(ap.m, random.order=FALSE, 
                    colors = brewer.pal(8, "Dark2"))
  
  #commonality.cloud(ap.d$word, ap.d$freq, 
  #    scale=c(8, .4), min.freq = 5, 
  #   max.words = 500, random.order = FALSE, 
  #    rot.per = .15, colors = pal2,main="Title")
  # commonality.cloud(matrix, random.order=FALSE, 
  #                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC"),
  #                max.words=500,main="Title")
  dev.off()
}

########Trim Strings
TrimAt <- function(x) {
  # remove @ from text
  
  sub('@', '', x)
}
TrimUTF <- function(x) {
  # remove @ from text
  
  iconv(x, "UTF-8", "UTF-8")
}
TrimHtml <- function(x) {
  # remove @ from text
  
  sub('â€”', '', x)
}

TrimToASCII <- function(x) {
  # remove @ from text
  
  iconv(x, to='ASCII', sub='')
}


TrimHead <- function(x) {
  # remove starting @, .@, RT @, MT @, etc.
  
  sub('^(.*)?@', '', x)
}

TrimUsers <- function(x) {
  # remove users, i.e. "@user", in a tweet
  
  str_replace_all(x, '(@[[:alnum:]_]*)', '')
}

TrimHashtags <- function(x) {
  # remove hashtags, i.e. "#tag", in a tweet
  
  str_replace_all(x, '(#[[:alnum:]_]*)', '')
}

TrimUrls <- function(x) {
  # remove urls in a tweet
  
  str_replace_all(x, 'http[^[:blank:]]+', '')
}

TrimOddChar <- function(x) {
  # remove odd charactors
  iconv(x, to = 'UTF-8')
}
TrimWhiteSpace <- function(x) {
  # remove odd charactors
  # returns string w/o leading whitespace
  x <- sub("^\\s+", "", x)
  # returns string w/o trailing whitespace
  sub("\\s+$", "", x)
}
bytecode.convert <- function(x) {iconv(enc2utf8(x), sub = "byte")}