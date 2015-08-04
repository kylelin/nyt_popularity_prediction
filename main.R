####################################################
# MAIN SCRIPT
# AUTHOR Yanpeng Lin
####################################################

# library load
libs = c("ggplot2", 
         "tm", 
         "randomForest", 
         "caret",
         "psych", 
         "reshape", 
         "ggdendro")

lapply(libs, library, character.only=TRUE)
options(warn = 2, error = recover)

# utility function load
source('util.R')

# data load
trainData = read.csv("./data/NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
testData  = read.csv("./data/NYTimesBlogTest.csv", stringsAsFactors=FALSE)
testData$Popular = NA

## combine training set and testing until finishing data transformation
newsData = rbind(trainData, testData)

# bag-of-words on Headline
# TODO

# feature format
newsData$Summary = ifelse(nchar(cleanupText(newsData$Snippet)) > nchar(cleanupText(newsData$Abstract)),
                      cleanupText(newsData$Snippet),
                      cleanupText(newsData$Abstract))

## replacement of some proper nouns to single word
## 'word of the day'
## 'time square'
## 'pictures of the day', 'photos of the day'
## 'daily clip report'
originalText    = c("new york times", "new york city", "new york", "silicon valley", 
                    "times insider", "fashion week", "white house", 
                    "international herald tribune archive", 
                    "president obama", "hong kong", "big data", "golden globe", 
                    "word of the day", "time square", "pictures of the day",
                    "photos of the day", "daily clip report")

replacementText = c("NYT", "NYC", "NewYork", "SiliconValley", "TimesInsider",
                    "FashionWeek", "WhiteHouse", "IHT", "Obama", "HongKong",
                    "BigData", "GoldenGlobe", "WordofDay", "TimeSquare", "PicOfDay",
                    "PicOfDay", "DailyClipReport")

newsData$Headline = phaseSub(newsData$Headline, originalText, replacementText, ignore.case=TRUE)
newsData$Summary  = phaseSub(newsData$Summary,  originalText, replacementText, ignore.case=TRUE)

rm(originalText)
rm(replacementText)

## combine Headline and Summary
newsData$Text = paste(newsData$Headline, newsData$Summary)

## missing categories
## misCat = subset(news, news$NewsDesk=="" | news$SectionName=="" | news$SubsectionName=="")
## dim(misCat)
## 6721 11 
## misCat = subset(news, news$NewsDesk=="" & news$SectionName=="" & news$SubsectionName=="")
## dim(misCat)
## 1626 11
## there are 1626 articles have no categories at all!

categoryMap = as.data.frame(table(news$NewsDesk, news$SectionName, news$SubsectionName))
names(categoryMap) = c("NewsDesk", "SectionName", "SubsectionName", "Freq")
categoryMap = subset(categoryMap, Freq > 0)
#categoryMap[order(categoryMap$SectionName),]
## fill NewsDesk by most common SectionName
news$NewsDesk = ifelse(news$NewsDesk=="" & news$SectionName=="Arts", "Culture", news$NewsDesk)
news$NewsDesk = ifelse(news$NewsDesk=="" & news$SectionName=="Business Day", "Business", news$NewsDesk)
news$NewsDesk = ifelse(news$NewsDesk=="" & news$SectionName=="Health", "Science", news$NewsDesk)
news$NewsDesk = ifelse(news$NewsDesk=="" & news$SectionName=="Multimedia", "", news$NewsDesk)
news$NewsDesk = ifelse(news$NewsDesk=="" & news$SectionName=="N.Y. / Region", "Metro", news$NewsDesk)
news$NewsDesk = ifelse(news$NewsDesk=="" & news$SectionName=="Open", "Technology", news$NewsDesk)
news$NewsDesk = ifelse(news$NewsDesk=="" & news$SectionName=="Opinion", "OpEd", news$NewsDesk)
news$NewsDesk = ifelse(news$NewsDesk=="" & news$SectionName=="Technology", "Business", news$NewsDesk)
news$NewsDesk = ifelse(news$NewsDesk=="" & news$SectionName=="Travel", "Travel", news$NewsDesk)
news$NewsDesk = ifelse(news$NewsDesk=="" & news$SectionName=="U.S.", "National", news$NewsDesk)
news$NewsDesk = ifelse(news$NewsDesk=="" & news$SectionName=="World", "Foreign", news$NewsDesk)

## fix Crosswords
idx = which(news$SectionName=="Crosswords/Games")
news$NewsDesk[idx]       = "Styles"
news$SectionName[idx]    = "Puzzles"
news$SubsectionName[idx] = ""
## fix U.S.
idx = which(news$NewsDesk=="Styles" & news$SectionName=="U.S.")
news$NewsDesk[idx]       = "Styles"
news$SectionName[idx]    = "Style"
news$SubsectionName[idx] = ""

#categoryMap[order(categoryMap$NewsDesk),]
## fill SectionName by most common NewsDesk
news$SectionName = ifelse(news$SectionName=="" & news$NewsDesk=="Culture", "Arts", news$SectionName)
news$SectionName = ifelse(news$SectionName=="" & news$NewsDesk=="Foreign", "World", news$SectionName)
news$SectionName = ifelse(news$SectionName=="" & news$NewsDesk=="National", "U.S.", news$SectionName)
news$SectionName = ifelse(news$SectionName=="" & news$NewsDesk=="OpEd", "Opinion", news$SectionName)
news$SectionName = ifelse(news$SectionName=="" & news$NewsDesk=="Science", "Science", news$SectionName)
news$SectionName = ifelse(news$SectionName=="" & news$NewsDesk=="Sports", "Sports", news$SectionName)
news$SectionName = ifelse(news$SectionName=="" & news$NewsDesk=="Styles", "Style", news$SectionName)
news$SectionName = ifelse(news$SectionName=="" & news$NewsDesk=="TStyle", "Magazine", news$SectionName)

## fill all empty ( NewsDesk, SectionName, SubsectionName ) cases
idx = which(news$NewsDesk == "" & news$SectionName == "" & news$SubsectionName == "" &
              grepl("^(first draft|lunchtime laughs|politics helpline|today in politics|verbatim)",
                    news$Headline, ignore.case=TRUE))
news$NewsDesk[idx]       = "National"
news$SectionName[idx]    = "U.S."
news$SubsectionName[idx] = "Politics"

## fill empty SectionName based on political terms
idx = which(news$SectionName=="" &
              grepl(paste0("white house|democrat|republican|tea party|",
                           "obama|biden|boehner|kerry|capitol|senat|",
                           "sen\\.|congress|president|washington|politic|",
                           "rubio|palin|clinton|bush|limbaugh|rand paul|",
                           "christie|mccain|election|poll|cruz|constitution|",
                           "amendment|federal|partisan|yellen|govern|",
                           "gov\\.|legislat|supreme court|campaign|",
                           "primary|primaries|justice|jury"),
                    news$Text, ignore.case=TRUE))
news$NewsDesk[idx]       = "National"
news$SectionName[idx]    = "U.S."
news$SubsectionName[idx] = "Politics"

news$NewsDesk[which(news$NewsDesk=="")]             = "Missing"
news$SectionName[which(news$SectionName=="")]       = "Missing"
news$SubsectionName[which(news$SubsectionName=="")] = "Missing"

rm(idx)
## date feature
news$PubDate = strptime(news$PubDate, "%Y-%m-%d %H:%M:%S")
news$PubDay  = as.Date(news$PubDate)
## it is expected that different behaviours at different times of the day.publication
news$Weekday = news$PubDate$wday
news$Hour    = news$PubDate$hour


news$HeadlineCharCount = nchar(news$Headline)
news$SummaryCharCount  = nchar(news$Summary)
# Use the regular expression symbol \\W to match non-word characters, using + to indicate one or more in a row, along with gregexpr to find all matches in a string. Words are the number of word separators plus 1.
news$HeadlineWordCount = sapply(gregexpr("\\W+", gsub("[[:punct:]]", "", news$Headline)), length) + 1
news$SummaryWordCount  = sapply(gregexpr("\\W+", gsub("[[:punct:]]", "", news$Summary)),  length) + 1

news$NewsDesk       = as.factor(news$NewsDesk)
news$SectionName    = as.factor(news$SectionName)
news$SubsectionName = as.factor(news$SubsectionName)
news$Popular        = as.factor(news$Popular)
news$LogWordCount   = log(1 + news$WordCount)


### Distribution of LogWordCount
newsTrain = head(news, nrow(news_train))
newsTest  = tail(news, nrow(news_test))

p = ggplot(newsTrain, aes(x=WordCount, fill=Popular)) + 
  geom_density(aes(y=..scaled..), alpha=0.4) +
  ggtitle("Distribution of WordCount") +
  xlab("WordCount") +
  theme(axis.title.y = element_blank())
ggsave(filename="doc/imgs/wc.png", plot=p, type="cairo-png", dpi=300, width=8, height=8)


p = ggplot(newsTrain, aes(x=LogWordCount, fill=Popular)) + 
  geom_density(aes(y=..scaled..), alpha=0.4) +
  ggtitle("Distribution of LogWordCount") +
  xlab("log(1 + WordCount)") +
  theme(axis.title.y = element_blank())
ggsave(filename="doc/imgs/wc_log.png", plot=p, type="cairo-png", dpi=300, width=8, height=8)

## Popular article tend to have fewer words of headline around 41 ~ 48 chars.
p = ggplot(newsTrain, aes(x=HeadlineCharCount, fill=Popular)) + 
  geom_density(aes(y=..scaled..), alpha=0.4) +
  ggtitle("Distribution of HeadlineCharCount") +
  xlab("# Characters in Headline") +
  theme(axis.title.y = element_blank())
ggsave(filename="doc/imgs/cc_headline.png", plot=p, type="cairo-png", dpi=300, width=8, height=8)

p = ggplot(newsTrain, aes(x=HeadlineWordCount, fill=Popular)) + 
  geom_density(aes(y=..scaled..), alpha=0.4) +
  ggtitle("Distribution of HeadlineWordCount") 
  xlab("# Words in Headline") +
  theme(axis.title.y = element_blank())
ggsave(filename="doc/imgs/wc_headline.png", plot=p, type="cairo-png", dpi=300, width=8, height=8)

#### 
PopularNewsTrain   = subset(newsTrain, newsTrain$Popular==1)
UnpopularNewsTrain = subset(newsTrain, newsTrain$Popular==0)
 
t.test(PopularNewsTrain$LogWordCount, UnpopularNewsTrain$LogWordCount)
var.test(PopularNewsTrain$LogWordCount, UnpopularNewsTrain$LogWordCount)


news$DayOfWeek = as.factor(weekdays(news$PubDate))
news$DayOfWeek = factor(news$DayOfWeek, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

### Distribution of LogWordCount for each weekday
newsTrain = head(news, nrow(news_train))
newsTest  = tail(news, nrow(news_test))

p = ggplot(newsTrain, aes(x=LogWordCount, fill=Popular)) +
  geom_density(aes(y=..scaled..), alpha=0.4) +
  ggtitle("Distribution of LogWordCount") +
  xlab("Log(1 + WordCount)") +
  theme(axis.title.y = element_blank()) +
  facet_wrap( ~ DayOfWeek, ncol=2) +
  theme(plot.title = element_text(size=16, face="bold")) +
  theme(text=element_text(family="AvantGarde", size=14))
ggsave(filename="doc/imgs/wc_log_wday.png", plot=p, type="cairo-png", dpi=300, width=8, height=8)

## hour vs popular
hourMatrix = as.matrix(table(news$Hour, news$Popular))
hourMatrix = data.frame("Unpopular" = hourMatrix[, 1], 
                        "Popular" = hourMatrix[, 2], 
                        "PopularDensity" = hourMatrix[, 2]/(hourMatrix[, 1] + hourMatrix[, 2]))
hourMatrix[order(hourMatrix$PopularDensity),]
# around 22:00, largest popular density
"
   Unpopular Popular PopularDensity
4        169       2     0.01169591
5        240      11     0.04382470
3         57       3     0.05000000
7        369      25     0.06345178
1         25       3     0.10714286
8        299      39     0.11538462
2         21       3     0.12500000
13       390      57     0.12751678
17       348      52     0.13000000
6        190      30     0.13636364
9        267      49     0.15506329
16       382      74     0.16228070
14       375      77     0.17035398
0        103      22     0.17600000
10       315      69     0.17968750
12       428      94     0.18007663
18       269      60     0.18237082
11       414      93     0.18343195
15       345      93     0.21232877
19       140      53     0.27461140
20       114      44     0.27848101
21        76      34     0.30909091
23        46      21     0.31343284
22        57      85     0.59859155
"

## PubDay vs popular
DailyArticles        = as.data.frame(table(news$PubDay))
names(DailyArticles) = c("PubDay", "NumDailyArticles")
DailyArticles$PubDay = as.Date(as.character(DailyArticles$PubDay), format="%Y-%m-%d")
news                 = merge(news, DailyArticles, by = "PubDay", all.x=TRUE)

news$Pop = as.numeric(as.character(news$Popular))
news$Pop[which(is.na(news$Popular))] = "N/A"
news$Pop = as.factor(news$Popular)

### daily article distribution
newsTrain = head(news, nrow(news_train))
newsTest  = tail(news, nrow(news_test))

p = ggplot(newsTrain, aes(x=NumDailyArticles, fill=Pop)) + 
  geom_density(aes(y=..scaled..), alpha=0.4) +
  ggtitle("Distribution of NumDailyArticles") +
  xlab("# Daily Articles Published") +
  scale_fill_discrete(name="Popular") +
  theme(axis.title.y = element_blank())
ggsave(filename="doc/imgs/daily_article_pub.png", plot=p, type="cairo-png", dpi=300, width=8, height=8)

## PubDay per section vs popular
DailySectionArticles        = as.data.frame(table(news$PubDay, news$SectionName))
names(DailySectionArticles) = c("PubDay", "SectionName", "NumDailySectionArticles")
DailySectionArticles$PubDay = as.Date(as.character(DailySectionArticles$PubDay), format="%Y-%m-%d")
news                        = merge(news, DailySectionArticles, all.x=TRUE)

### daily per section article distribution
newsTrain = head(news, nrow(news_train))
newsTest  = tail(news, nrow(news_test))

p = ggplot(newsTrain, aes(x=NumDailySectionArticles, fill=Pop)) +
  geom_density(aes(y=..scaled..), alpha=0.4) +
  ggtitle("Distribution of NumDailySectionArticles") +
  xlab("# Daily Articles Published") +
  scale_fill_discrete(name="Popular") +
  theme(axis.title.y = element_blank()) + 
  facet_wrap( ~ SectionName, ncol=3)
ggsave(filename="doc/imgs/daily_article_sec_pub.png", plot=p, type="cairo-png", dpi=300, width=8, height=8)

## hourly published vs popular
HourlyArticles        = as.data.frame(table(news$PubDay, news$Hour))
names(HourlyArticles) = c("PubDay", "Hour", "NumHourlyArticles")
HourlyArticles$PubDay = as.Date(as.character(HourlyArticles$PubDay), format="%Y-%m-%d")
news                  = merge(news, HourlyArticles, all.x=TRUE)

### hourly published article distribution
newsTrain = head(news, nrow(news_train))
newsTest  = tail(news, nrow(news_test))

p = ggplot(newsTrain, aes(x=NumHourlyArticles, fill=Pop)) +
  geom_density(aes(y=..scaled..), alpha=0.4) +
  ggtitle("Distribution of NumHourlyArticles") +
  xlab("# Hourly Articles Published") +
  scale_fill_discrete(name="Popular") +
  theme(axis.title.y = element_blank()) + 
  facet_wrap( ~ Hour, ncol=8)
ggsave(filename="doc/imgs/hourly_article_pub.png", plot=p, type="cairo-png", dpi=300, width=12, height=4)

# feature creating
## domain specified feature
### search
news$SEO = as.factor(ifelse(news$HeadlineCharCount <= 48, 1, 0))

## the following features are created according to article contents
news$Question    = as.factor(ifelse(grepl("?", news$Headline), 1, 0))
news$Exclamation = as.factor(ifelse(grepl("!", news$Headline), 1, 0))
news$HowTo       = as.factor(ifelse(grepl("^how to", news$Headline, ignore.case=TRUE), 1, 0))

## extract features from negative examples
sort(UnpopularNewsTrain$Headline)
## recurrent items wil never get popular
news$NoComment = as.factor(ifelse(grepl(
  paste0("6 q's about the news|daily|fashion week|first draft|in performance|",
         "international arts events happening in the week ahead|",
         "inside the times|lunchtime laughs|pictures of the day|playlist|",
         "podcast|q. and a.|reading the times|test yourself|",
         "throwback thursday|today in|the upshot|tune in to the times|",
         "tune into the times|under cover|verbatim|walkabout|weekend reading|",
         "weekly news quiz|weekly wrap|what we're (reading|watching)|",
         "what's going on in this picture|word of the day"),
  news$Headline, ignore.case=TRUE), 1, 0))

news$Recurrent = as.factor(ifelse(grepl(
  "^(ask well|facts & figures|think like a doctor|readers respond|no comment necessary)",
  news$Headline, ignore.case=TRUE), 1, 0))

news$Obama      = as.factor(ifelse(grepl("obama|president", news$Headline, ignore.case=TRUE), 1, 0))
news$Republican = as.factor(ifelse(grepl("republican", news$Headline, ignore.case=TRUE), 1, 0))

## especially on holidays, people may have much more time to read and comment on blog articles
Holidays = c(as.POSIXlt("2014-09-01 00:00", format="%Y-%m-%d %H:%M"),
             as.POSIXlt("2014-10-13 00:00", format="%Y-%m-%d %H:%M"),
             as.POSIXlt("2014-10-31 00:00", format="%Y-%m-%d %H:%M"),
             as.POSIXlt("2014-11-11 00:00", format="%Y-%m-%d %H:%M"),
             as.POSIXlt("2014-11-27 00:00", format="%Y-%m-%d %H:%M"),
             as.POSIXlt("2014-12-24 00:00", format="%Y-%m-%d %H:%M"),
             as.POSIXlt("2014-12-25 00:00", format="%Y-%m-%d %H:%M"),
             as.POSIXlt("2014-12-31 00:00", format="%Y-%m-%d %H:%M"))
 
news$Holiday = as.factor(ifelse(news$PubDate$yday %in% Holidays$yday, 1, 0))

"
   Unpopular Popular PopularDensity
4        169       2     0.01169591
5        240      11     0.04382470
3         57       3     0.05000000
7        369      25     0.06345178
1         25       3     0.10714286
8        299      39     0.11538462
2         21       3     0.12500000
13       390      57     0.12751678
17       348      52     0.13000000
6        190      30     0.13636364
9        267      49     0.15506329
16       382      74     0.16228070
14       375      77     0.17035398
0        103      22     0.17600000
10       315      69     0.17968750
12       428      94     0.18007663
18       269      60     0.18237082
11       414      93     0.18343195
15       345      93     0.21232877
19       140      53     0.27461140
20       114      44     0.27848101
21        76      34     0.30909091
23        46      21     0.31343284
22        57      85     0.59859155
"
### right after around 19:00, the popular density increases almost 30%
### it would be caused by most people leave work and have time to catch up on blog posts.
news$BeforeHoliday = as.factor(ifelse(news$PubDate$yday %in% (Holidays$yday-1) & 
                                          news$PubDate$hour>=17, 1, 0))

### some topics
news$Current = as.factor(ifelse(grepl(
  "ebola|ferguson|michael brown|cuba|embargo|castro|havana",
  news$Text, ignore.case=TRUE), 1, 0))
 
news$Cuba = as.factor(ifelse(grepl("cuba|embargo|castro|havana",
                                     news$Text, ignore.case=TRUE), 1, 0))
 
news$Recap = as.factor(ifelse(grepl("recap",
                                       news$Headline, ignore.case=TRUE), 1, 0))
 
news$UN = as.factor(ifelse(grepl("u.n.|united nations|ban ki-moon|climate",
                                      news$Text, ignore.case=TRUE), 1, 0))
 
news$Health = as.factor(ifelse(grepl(
  paste0("mental health|depress(a|e|i)|anxiety|schizo|",
         "personality|psych(i|o)|therap(i|y)|brain|autis(m|t)|",
         "carb|diet|cardio|obes|cancer|homeless"),
  news$Headline), 1, 0))
 
news$Family = as.factor(ifelse(grepl(
  "education|school|kids|child|college|teenager|mother|father|parent|famil(y|ies)",
  news$Headline, ignore.case=TRUE), 1, 0))
 
news$Tech = as.factor(ifelse(grepl(
  paste0("twitter|facebook|google|apple|microsoft|amazon|",
         "uber|phone|ipad|tablet|kindle|smartwatch|",
         "apple watch|match.com|okcupid|social (network|media)|",
         "tweet|mobile| app "),
  news$Headline, ignore.case=TRUE), 1, 0))
 
news$Security = as.factor(ifelse(grepl("cybersecurity|breach|hack|password",
                                         news$Headline, ignore.case=TRUE), 1, 0))
 
news$Biz = as.factor(ifelse(grepl(
  paste0("merger|acqui(s|r)|takeover|bid|i.p.o.|billion|",
         "bank|invest|wall st|financ|fund|share(s|holder)|market|",
         "stock|cash|money|capital|settlement|econo"),
  news$Headline, ignore.case=TRUE), 1, 0))
 
news$War = as.factor(ifelse(grepl(
  paste0("israel|palestin|netanyahu|gaza|hamas|iran|",
         "tehran|assad|syria|leban(o|e)|afghan|iraq|",
         "pakistan|kabul|falluja|baghdad|islamabad|",
         "sharif|isis|islamic state"),
  news$Text, ignore.case=TRUE), 1, 0))
 
 
news$Holidays = as.factor(ifelse(grepl("thanksgiving|hanukkah|christmas|santa",
                                         news$Text, ignore.case=TRUE), 1, 0))
 
news$Boring = as.factor(ifelse(grepl(
  paste0("friday night music|variety|[[:digit:]]{4}|photo|today|",
         "from the week in style|oscar|academy|golden globe|diary|",
         "hollywood|red carpet|stars|movie|film|celeb|sneak peek|",
         "by the book|video|music|album|spotify|itunes|taylor swift|",
         "veteran|palin|kerry|mccain|rubio|rand paul|yellen|partisan|",
         "capitol|bush|clinton|senator|congressman|governor|chin(a|e)|",
         "taiwan|tibet|beijing|hongkong|russia|putin"),
  news$Text, ignore.case=TRUE), 1, 0))

newsTrain = head(news, nrow(news_train))
newsTest  = tail(news, nrow(news_test))

# modeling
## random forest
rfModel = randomForest(Popular ~ PubDay + Hour + SectionName + NewsDesk + SubsectionName + 
                                 WordCount + Weekday + HeadlineCharCount + SummaryCharCount + 
                                 HeadlineWordCount + SummaryWordCount + LogWordCount + 
                                 NumDailyArticles + NumDailySectionArticles + NumHourlyArticles + SEO + 
                                 Exclamation + HowTo + NoComment + Recurrent + Obama + Republican + 
                                 Holiday + BeforeHoliday + Current + Cuba + Recap + UN + Health + 
                                 Family + Tech + Security + Biz + War + Holidays + Boring, 
                      data=newsTrain, nodesize=5, ntree=1000, importance=TRUE)

trainPartition = createDataPartition(y=newsTrain$Popular, p=0.5, list=FALSE)
tuneTrain      = newsTrain[trainPartition, ]
rfModel.tuned  = train(Popular ~ PubDay + Hour + SectionName + NewsDesk + SubsectionName + 
                                 WordCount + Weekday + HeadlineCharCount + SummaryCharCount + 
                                 HeadlineWordCount + SummaryWordCount + LogWordCount + 
                                 NumDailyArticles + NumDailySectionArticles + NumHourlyArticles + SEO + 
                                 Exclamation + HowTo + NoComment + Recurrent + Obama + Republican + 
                                 Holiday + BeforeHoliday + Current + Cuba + Recap + UN + Health + 
                                 Family + Tech + Security + Biz + War + Holidays + Boring, 
                   data=tuneTrain, 
                   method="rf", 
                   trControl=trainControl(method="cv", number=5))

### importance of features
imp = melt(importance(rfModel, type=1))
imp$sign = ifelse(imp$value>=0, "positive", "negative")
 
p = ggplot(imp, aes(x=reorder(X1, value, max), y=value, group=X2, fill=sign)) +
  geom_bar(stat="identity") +
  coord_flip() +
  ggtitle("Relative Importance of Predictors") +
  theme(axis.title.y = element_blank()) +
  ylab("Mean Decrease Accuracy") +
  theme(legend.position = "none") 
ggsave(filename="doc/imgs/rf_feature_importance.png", plot=p, type="cairo-png", dpi=300, width=8, height=8)

## AUC of basic random forest
calcAUC(rfModel, newsTrain$Popular)


"
PubDay + Hour + SectionName + NewsDesk + SubsectionName + 
WordCount + Weekday + HeadlineCharCount + SummaryCharCount + 
HeadlineWordCount + SummaryWordCount + LogWordCount + 
NumDailyArticles + NumDailySectionArticles + NumHourlyArticles + SEO + 
Exclamation + HowTo + NoComment + Recurrent + Obama + Republican + 
Holiday + BeforeHoliday + Current + Cuba + Recap + UN + Health + 
Family + Tech + Security + Biz + War + Holidays + Boring, 
"
## until now, we haven't use any text feature like `Summary`, `Headline`
## and we only used supervised learning algorithms like random forest
## by utilizing unsupervised algorithms, it is expected to extract some 
## common structure between training and testing data

## random forest + k-means
### tf-idf
dropWords = c(stopwords("SMART"))
CorpusHeadline = Corpus(VectorSource(news$Headline))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, dropWords)
CorpusHeadline = tm_map(CorpusHeadline, stemDocument, language="english")
dtmHeadline = DocumentTermMatrix(CorpusHeadline)
tdmHeadline = TermDocumentMatrix(CorpusHeadline)

sparseHeadline = removeSparseTerms(dtmHeadline, 0.995)  
sparseHeadline = as.data.frame(as.matrix(sparseHeadline))
colnames(sparseHeadline) = make.names(paste0("H",colnames(sparseHeadline)))

tsparseHeadline = removeSparseTerms(tdmHeadline, 0.995)  
tsparseHeadline = as.data.frame(as.matrix(tsparseHeadline))
colnames(tsparseHeadline) = make.names(paste0("H",colnames(tsparseHeadline)))
 
CorpusSummary = Corpus(VectorSource(news$Summary))
CorpusSummary = tm_map(CorpusSummary, tolower)
CorpusSummary = tm_map(CorpusSummary, PlainTextDocument)
CorpusSummary = tm_map(CorpusSummary, removePunctuation)
CorpusSummary = tm_map(CorpusSummary, removeWords, dropWords)
CorpusSummary = tm_map(CorpusSummary, stemDocument, language="english")
dtmSummary = DocumentTermMatrix(CorpusSummary)
tdmSummary = TermDocumentMatrix(CorpusSummary)

sparseSummary = removeSparseTerms(dtmSummary, 0.99)  
sparseSummary = as.data.frame(as.matrix(sparseSummary))
colnames(sparseSummary) = make.names(paste0("S",colnames(sparseSummary)))
 
tsparseSummary = removeSparseTerms(tdmSummary, 0.99)  
tsparseSummary = as.data.frame(as.matrix(tsparseSummary))
colnames(tsparseSummary) = make.names(paste0("S",colnames(tsparseSummary)))
 
CorpusText = Corpus(VectorSource(news$Text))
CorpusText = tm_map(CorpusText, tolower)
CorpusText = tm_map(CorpusText, PlainTextDocument)
CorpusText = tm_map(CorpusText, removePunctuation)
CorpusText = tm_map(CorpusText, removeWords, dropWords)
CorpusText = tm_map(CorpusText, stemDocument, language="english")
tdmText = TermDocumentMatrix(CorpusText)
sparseText = removeSparseTerms(tdmText, 0.99)  
sparseText = as.data.frame(as.matrix(sparseText))
colnames(tsparseSummary) = make.names(colnames(sparseText))

freqTerms = findFreqTerms(dtmHeadline, lowfreq=100)
termFreq  = colSums(as.matrix(dtmHeadline))
termFreq  = subset(termFreq, termFreq>=100)
df        = data.frame(term=names(termFreq), freq=termFreq)

p = ggplot(df, aes(x=reorder(term, freq, max), y=freq)) +
  geom_bar(stat="identity") +
  ggtitle("Most Common Terms in the Headline") +
  xlab("Terms") +
  ylab("Frequency") +
  coord_flip()
ggsave(filename="doc/imgs/dtf_headline.png", plot=p, type="cairo-png", dpi=300, width=8, height=8)

freqTerms = findFreqTerms(dtmSummary, lowfreq=100)
termFreq  = colSums(as.matrix(dtmSummary))
termFreq  = subset(termFreq, termFreq>=100)
df        = data.frame(term=names(termFreq), freq=termFreq)

p = ggplot(df, aes(x=reorder(term, freq, max), y=freq)) +
  geom_bar(stat="identity") +
  ggtitle("Most Common Terms in the Summary") +
  xlab("Terms") +
  ylab("Frequency") +
  coord_flip()
ggsave(filename="doc/imgs/dtf_summary.png", plot=p, type="cairo-png", dpi=300, width=8, height=24)


matrixSparseSummary  = as.matrix(tsparseSummary)
matrixSparseSummary.distMatrix = dist(scale(matrixSparseSummary))
matrixSparseSummary.clusters   = hclust(matrixSparseSummary.distMatrix, method="ward.D2")

dSummary = as.dendrogram(matrixSparseSummary.clusters)
dSummaryData <- dendro_data(dSummary, type = "rectangle")

p = ggplot(segment(dSummaryData)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip() 
ggsave(filename="doc/imgs/hclust_summary.png", plot=p, type="cairo-png", dpi=300, width=8, height=8)

kSummary   = 25
mSummary   = t(tsparseSummary)
KMCSummary = kmeans(mSummary, kSummary)
 
for (i in 1:kSummary) {
  cat(paste("cluster ", i, ": ", sep=","))
  s = sort(KMCSummary$centers[i, ], decreasing=TRUE)
  cat(names(s)[1:15], sep=", ", "\n")
}

matrixSparseHeadline  = as.matrix(tsparseHeadline)
matrixSparseHeadline.distMatrix = dist(scale(matrixSparseHeadline))
matrixSparseHeadline.clusters   = hclust(matrixSparseHeadline.distMatrix, method="ward.D2")

dHeadline = as.dendrogram(matrixSparseHeadline.clusters)
dHeadlineData <- dendro_data(dHeadline, type = "rectangle")

p = ggplot(segment(dHeadlineData)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip() 
ggsave(filename="doc/imgs/hclust_headline.png", plot=p, type="cairo-png", dpi=300, width=8, height=8)

kHeadline   = 25
mHeadline   = t(tsparseHeadline)
KMCHeadline = kmeans(mHeadline, kHeadline)
 
for (i in 1:kHeadline) {
  cat(paste("cluster ", i, ": ", sep=","))
  s = sort(KMCHeadline$centers[i, ], decreasing=TRUE)
  cat(names(s)[1:15], sep=", ", "\n")
}

matrixSparseText  = as.matrix(sparseText)
matrixSparseText.distMatrix = dist(scale(matrixSparseText))
matrixSparseText.clusters   = hclust(matrixSparseText.distMatrix, method="ward.D2")

dText = as.dendrogram(matrixSparseText.clusters)
dTextData <- dendro_data(dText, type = "rectangle")

p = ggplot(segment(dTextData)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip() 
ggsave(filename="doc/imgs/hclust_text.png", plot=p, type="cairo-png", dpi=300, width=8, height=8)

kText   = 25
mText   = t(tsparseHeadline)
KMCText = kmeans(mText, kText)
 
for (i in 1:kText) {
  cat(paste("cluster ", i, ": ", sep=","))
  s = sort(KMCText$centers[i, ], decreasing=TRUE)
  cat(names(s)[1:15], sep=", ", "\n")
}

news$HeadlineCluster = as.factor(KMCHeadline$cluster)
news$SummaryCluster  = as.factor(KMCSummary$cluster)
news$TextCluster     = as.factor(KMCText$cluster)

newsTrain = head(news, nrow(news_train))
newsTest  = tail(news, nrow(news_test))

# modeling
## random forest
rfModelKMC = randomForest(Popular ~ PubDay + Hour + SectionName + NewsDesk + SubsectionName +
                                 WordCount + Weekday + HeadlineCharCount + SummaryCharCount +
                                 HeadlineWordCount + SummaryWordCount + LogWordCount +
                                 NumDailyArticles + NumDailySectionArticles + NumHourlyArticles + SEO +
                                 Exclamation + HowTo + NoComment + Recurrent + Obama + Republican +
                                 Holiday + BeforeHoliday + Current + Cuba + Recap + UN + Health +
                                 Family + Tech + Security + Biz + War + Holidays + Boring +
                                 TextCluster, 
                      data=newsTrain, nodesize=5, ntree=1000, importance=TRUE)

trainPartition = createDataPartition(y=newsTrain$Popular, p=0.5, list=FALSE)
tuneTrain      = newsTrain[trainPartition, ]
rfModelKMC.tuned  = train(Popular ~ PubDay + Hour + SectionName + NewsDesk + SubsectionName +
                                 WordCount + Weekday + HeadlineCharCount + SummaryCharCount +
                                 HeadlineWordCount + SummaryWordCount + LogWordCount +
                                 NumDailyArticles + NumDailySectionArticles + NumHourlyArticles + SEO +
                                 Exclamation + HowTo + NoComment + Recurrent + Obama + Republican +
                                 Holiday + BeforeHoliday + Current + Cuba + Recap + UN + Health +
                                 Family + Tech + Security + Biz + War + Holidays + Boring +
                                 TextCluster,
                   data=tuneTrain,
                   method="rf",
                   trControl=trainControl(method="cv", number=5))

## AUC of basic random forest
calcAUC(rfModelKMC, newsTrain$Popular)
