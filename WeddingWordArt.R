#############################################################################
#####Craft Breweries and thier Beers 
#####http://www.ratebeer.com/ for rattings
#####Both Companies's selected beers have had multiple recent reviews

###Cigar City Brewing #http://www.cigarcitybrewing.com/ CS
#Jai Alai India Pale Ale(IPL):s=3.82 #ratings=1087   #Reg # highest score
#Maduro Brown Ale: s=3.56 #ratings=822 #Reg # lowest score
#----------
#Hunahpu's Imperial Stout: s=4.28 #ratings=699 # Special Beer
#----------
#Cafe Americana Double Stout  s=4.1 #ratings=52 #highest Score with Lower Count
#Evander Ale   s=3.17 #ratings=50 #lowest Score with Lower Count
#Wakatu   s=3.13 #ratings=26 #lowest Score with Lower Count

###Cold Storage Craft Brewery #http://floridaavenuebrewing.com/ CSC
#Florida Avenue IPA  :s=2.79 #ratings=58  
#Florida Avenue Blueberry Ale   : s=3.66 #ratings=38 

###Tampa Bay Brewing Co. #http://www.tampabaybrewingcompany.com/ TBB
#Wild Warthog Weizen  :s=3.23 #ratings=91  
#True Blonde Ale    : s=2.93 #ratings=78 
#############################################################################


##Products DataFrames
#CS.JaiAlai                    <- twListToDF(searchTwitter('Jai Alai Ale', n=1000,since="2013-11-25", cainfo="cacert.pem"))
CS.MaduroBrownAle             <- twListToDF(searchTwitter('Maduro Brown Ale', n=1000, cainfo="cacert.pem"))  
CS.MaduroBrownAle <- CS.MaduroBrownAle[CS.MaduroBrownAle$isRetweet ==FALSE,]
#CS.HunahpuImperialStout       <- twListToDF(searchTwitter("Hunahpu's Imperial Stout", n=1000, cainfo="cacert.pem"))  
#CS.CafeAmericanaDoubl         <- twListToDF(searchTwitter('Cafe Americana Double Stout', n=1000, cainfo="cacert.pem"))  
#CS.EvanderAle                 <- twListToDF(searchTwitter('Evander Ale', n=1000, cainfo="cacert.pem"))  
#CS.Wakatu                     <- twListToDF(searchTwitter('Wakatu', n=1000, cainfo="cacert.pem"))   
#TBB.WildWarthogWeizen         <- twListToDF(searchTwitter('Wild Warthog Weizen', n=1000, cainfo="cacert.pem"))   
TBB.TrueBlondeAle             <- twListToDF(searchTwitter('True Blonde Ale', n=1000, cainfo="cacert.pem"))  
TBB.TrueBlondeAle <- TBB.TrueBlondeAle[TBB.TrueBlondeAle$isRetweet ==FALSE,]
##Companies
#search by company user
#Can't use Since Paramater so remove tweets by date using subset lubridate ymd
CS.User <-  twListToDF(userTimeline("CigarCityBeer", n=1000,cainfo="cacert.pem"))
CS.User <- subset(CS.User, created > ymd("2013-11-25"))
TBB.User <- twListToDF(userTimeline("TBBCo", n=1000,cainfo="cacert.pem")) 
TBB.User <- subset(TBB.User, created > ymd("2013-11-25"))
##Test with major Craft Products Product
#Boston Beer Co -> Samuel Adams
BBC.SamuelAdams  <- twListToDF(searchTwitter('"Samuel Adams" OR "Sam Adams"', n=1000, cainfo="cacert.pem"))  
BBC.SamuelAdams <- BBC.SamuelAdams[BBC.SamuelAdams$isRetweet ==FALSE,]
BBC.User <- twListToDF(userTimeline("SamuelAdamsBeer", n=1000,cainfo="cacert.pem"))
BBC.User <- subset(BBC.User, created > ymd("2013-11-25"))
#Lagunitas Brewing Co -> Lagunitas India Pale Ale
LB.LagunitasIPA  <- twListToDF(searchTwitter('"Lagunitas India Pale Ale" OR "Lagunitas IPA"', n=1000, cainfo="cacert.pem"))  
LB.LagunitasIPA <- LB.LagunitasIPA[LB.LagunitasIPA$isRetweet ==FALSE,]
LB.User <- twListToDF(userTimeline("lagunitasbruhws", n=1000,cainfo="cacert.pem"))
LB.User <- subset(LB.User, created > ymd("2013-11-25"))
#New Belgium Brewing Company - Fat Tire
NBB.FatTire <- twListToDF(searchTwitter('"Fat Tire"', n=1000, cainfo="cacert.pem"))  
NBB.FatTire <- NBB.FatTire[NBB.FatTire$isRetweet ==FALSE,]
NBB.User <- twListToDF(userTimeline("newbelgium", n=1000,cainfo="cacert.pem"))
NBB.User <- subset(NBB.User, created > ymd("2013-11-25"))

##Additional Twitter User Stats as of 2013-12-06
#Following
CS.User.Following <- 80
TBB.User.Following <- 1228
BBC.User.Following <-  432
LB.User.Following <- 15232
NBB.User.Following <- 14811
#Following
CS.User.Followers <- 23924
TBB.User.Followers <- 3628
BBC.User.Followers <- 32637
LB.User.Followers <- 35281
NBB.User.Followers <-184645


######################################################
############Word clouds
#Get Text, clean them up,create Corpus and wordcloud
#Products Tweets
CS.MaduroBrownAle.Corpus <- ConstructCorpus(CS.MaduroBrownAle$text)
MakeWordCloud(CS.MaduroBrownAle.Corpus,"Cigar City Maduro Brown Ale.png")

TBB.TrueBlondeAle.Corpus <- ConstructCorpus(TBB.TrueBlondeAle$text)
MakeWordCloud(TBB.TrueBlondeAle.Corpus,"Tampa Bay Brewing - True Blonde Ale.png")

BBC.SamuelAdams.Corpus <- ConstructCorpus(BBC.SamuelAdams$text)
MakeWordCloud(BBC.SamuelAdams.Corpus,"Boston Beer Co - Samuel Adams.png")

LB.LagunitasIPA.Corpus <- ConstructCorpus(LB.LagunitasIPA$text)
MakeWordCloud(LB.LagunitasIPA.Corpus,"Lagunitas Brewing Co - Lagunitas India Pale Ale.png")

NBB.FatTire.Corpus <- ConstructCorpus(NBB.FatTire$text)
MakeWordCloud(NBB.FatTire.Corpus,"New Belgium Brewing Company - Fat Tire.png")
dev.off()
#User Tweets
CS.User.Corpus <- ConstructCorpus(CS.User$text)
MakeWordCloud(CS.User.Corpus,"Cigar City.png")

TBB.User.Corpus <- ConstructCorpus(TBB.User$text)
MakeWordCloud(TBB.User.Corpus,"Tampa Bay Brewing.png")

BBC.User.Corpus <- ConstructCorpus(BBC.User$text)
MakeWordCloud(BBC.User.Corpus,"Boston Beer Co.png")

LB.User.Corpus <- ConstructCorpus(LB.User$text)
MakeWordCloud(LB.User.Corpus,"Lagunitas Brewing Co.png")

NBB.User.Corpus <- ConstructCorpus(NBB.User$text)
MakeWordCloud(NBB.User.Corpus,"New Belgium Brewing Company.png")

######################################################
############Create Comparison & Commonality Word Cloud
####Products
CS.MaduroBrownAle.clean = CleanTweets(c(CS.MaduroBrownAle$text))
TBB.TrueBlondeAle.clean = CleanTweets(c(TBB.TrueBlondeAle$text))
BBC.SamuelAdams.clean = CleanTweets(c(BBC.SamuelAdams$text))
LB.LagunitasIPA.Clean = CleanTweets(c(LB.LagunitasIPA$text))
NBB.FatTire.clean = CleanTweets(c(NBB.FatTire$text))

CS.MaduroBrownAle.clean.V = paste(CS.MaduroBrownAle.clean, collapse=" ")
TBB.TrueBlondeAle.clean.V = paste(TBB.TrueBlondeAle.clean, collapse=" ")
BBC.SamuelAdams.clean.V = paste(BBC.SamuelAdams.clean, collapse=" ")
LB.LagunitasIPA.Clean.V = paste(LB.LagunitasIPA.Clean, collapse=" ")
NBB.FatTire.clean.V = paste(NBB.FatTire.clean, collapse=" ")


# put everything in a single vector
Products.All = c(CS.MaduroBrownAle.clean.V, TBB.TrueBlondeAle.clean.V, BBC.SamuelAdams.clean.V, LB.LagunitasIPA.Clean.V,NBB.FatTire.clean.V)

# remove stop-words
Products.All = removeWords(Products.All,c(stopwords("english"),"adams","samuel","adam","sam", "true", "blonde", "maduro", "brown", "lagunitas","fat", "tire", "ale","india","ipa","pale"))
# create corpus
Products.All.Corpus = Corpus(VectorSource(Products.All))
# create term-document matrix
Products.All.TDM = TermDocumentMatrix(Products.All.Corpus)

# convert as matrix
Products.All.TDM = as.matrix(Products.All.TDM)

# add column names
colnames(Products.All.TDM) = c("Cigar City - Maduro Brown Ale", "Tampa Bay Brewing - True Blonde Ale", "Boston Beer Co - Samuel Adams", "Lagunitas Brewing Co - Lagunitas India Pale Ale","New Belgium Brewing Company - Fat Tire")
MakeComparisonWordCloud(Products.All.TDM,"Comparison Product Cloud.png")
MakeCommonWordCloud(Products.All.TDM,"Commonality Product Cloud.png")

####Users
CS.User.clean = CleanTweets(c(CS.User$text))
TBB.User.clean = CleanTweets(c(TBB.User$text))
BBC.User.clean = CleanTweets(c(BBC.User$text))
#LB.User.clean = CleanTweets(c(LB.User$text))
NBB.User.clean = CleanTweets(c(NBB.User$text))

CS.User.clean.V = paste(CS.User.clean, collapse=" ")
TBB.User.clean.V = paste(TBB.User.clean, collapse=" ")
BBC.User.clean.V = paste(BBC.User.clean, collapse=" ")
#LB.User.clean.V = paste(LB.User.clean, collapse=" ")
NBB.User.clean.V = paste(NBB.User.clean, collapse=" ")


# put everything in a single vector
Users.All = c(CS.User.clean.V, TBB.User.clean.V, BBC.User.clean.V,NBB.User.clean.V)

# remove stop-words
Users.All = removeWords(Users.All,c(stopwords("english"),"adams","samuel","adam","sam", "true", "blonde", "maduro", "brown", "lagunitas","fat", "tire", "ale","india","ipa","pale"))
# create corpus
Users.All.Corpus = Corpus(VectorSource(Users.All))
# create term-document matrix
Users.All.TDM = TermDocumentMatrix(Users.All.Corpus)

# convert as matrix
Users.All.TDM = as.matrix(Users.All.TDM)

# add column names
colnames(Users.All.TDM) = c("Cigar City", "Tampa Bay Brewing", "Boston Beer Co","New Belgium Brewing Co")

# comparison cloud
# save the image in png format

MakeComparisonWordCloud(Users.All.TDM,"Comparison Company Cloud.png")
MakeCommonWordCloud(Users.All.TDM,"Commonality Company Cloud.png")

#dev.off()
######################################################
############Continued in Twitter2.R
#remove instances
#rm(list = c('CS2','BM','CSC2'))
#Write to CSV
#write.csv(Rangers.df, file='C:/temp/RangersTweets.csv', row.names=F)