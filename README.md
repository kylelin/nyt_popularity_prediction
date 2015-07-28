### Which New York Times blog articles will be the most popular?
#### Introduction
Newspapers and online news aggregators like Google News need to understand which news articles will be the most popular, so that they can prioritize the order in which stories appear. 
In this repository, I tried to predict the popularity of a set of New York Times blog articles from the time period September 2014-December 2014.

The following screenshot shows an example of the New York Times technology blog "Bits" homepage:
![NYT_blogs][NYT_bits]

Many blog articles are published each day, and the New York Times has to decide which articles should be featured. I tried to answer that what features of a blog post
make it popular.

#### Data Files

| File Name        | Description   | Avaliable Formats  |
| -----------------| --------------| -------------------|
| NYTimesBlogTest  | 6532 articles | [NYTimesBlogTest]  |
| NYTimesBlogTrain | 1870 articles | [NYTimesBlogTrain] |

#### Variable
The dependent variable in this problem is the variable Popular, which labels if an article had 25 or more comments in its online comment 
section (equal to 1 if it did, and 0 if it did not). The dependent variable is provided in the training data set, but not the testing dataset.

The independent variables consist of 8 pieces of article data available at the time of publication, and a unique identifier:

- `NewsDesk`, the New York Times desk that produced the story (Business, Culture, Foreign, etc.)
- `SectionName`, the section the article appeared in (Opinion, Arts, Technology, etc.)
- `SubsectionName`, the subsection the article appeared in (Education, Small Business, Room for Debate, etc.)
- `Headline`, the title of the article
- `Snippet`, a small portion of the article text
- `Abstract`, a summary of the blog article, written by the New York Times
- `WordCount`, the number of words in the article
- `PubDate`, the publication date, in the format "Year-Month-Day Hour:Minute:Second"
- `UniqueID`, a unique identifier for each article


#### Features
Before start modeling, take time to understand the problem well. Here's the [New York Times'blog directory].
Generally, headline is most important in attracting people's attention.

##### Snippet \subset Abstract
When we look at the data by `news[idx, 5:6]`, it is clear that `Snippet` almost same as `Abstract`.

```
> news[231,5:6]
                                                                   Snippet
231 A federal judge on Thursday said Ohio could not cut the voting period.
                                                                  Abstract
231 A federal judge on Thursday said Ohio could not cut the voting period.

> which(news$Snippet!=news$Abstract)
  [1]   22   25  122  286  472  829 1003 1041 1134 1145 1207 1307 1395 1462 1489
 [16] 1493 1565 1657 1658 1687 1779 1796 2028 2354 2360 2475 2514 2608 2633 2678
 [31] 2773 2840 2871 3015 3117 3139 3240 3431 3509 3530 3534 3578 3871 3958 3998
 [46] 4097 4113 4204 4368 4416 4488 4514 4627 4671 4774 4805 4824 4841 4901 4931
 [61] 4962 5079 5124 5185 5262 5299 5441 5546 5554 5561 5604 5665 5669 5720 5758
 [76] 5774 5814 5889 5961 5970 6052 6088 6155 6185 6329 6368 6404 6506 6616 6629
 [91] 6763 6785 6824 6884 6906 6924 6938 7000 7045 7054 7144 7146 7165 7192 7206
[106] 7244 7247 7270 7309 7310 7315 7329 7380 7462 7488 7551 7561 7631 7643 7655
[121] 7687 7814 7821 7957 8084 8088 8145 8266 8328

> news[22,5:6]
                                                                                                                                                                                                                                                  Snippet
22 In an open letter, Su Yutong, a Chinese journalist who was fired from a German public broadcaster last month after a debate over the Tiananmen Square massacre, called on the broadcasters director general to speak out for press freedom while in...
                                                                                                                                                                                                                                                     Abstract
22 In an open letter, Su Yutong, a Chinese journalist who was fired from a German public broadcaster last month after a debate over the Tiananmen Square massacre, called on the broadcasters director general to speak out for press freedom while in China.
```

Most cases where `Snippet` != `Abstract` is because a ellipses at the end of `Abstract`.
The left cases which are not caused by ellipses are blank abstract, abstract with HTML code inside and some snippet that read "[Photo: View on Instagram.]". Here, we can create a new feature called `Summary` taking from more informative of `Snippet` and `Abstract`.





[NYT_bits]:doc/imgs/NYT_bits.png
[NYTimesBlogTest]:data/NYTimesBlogTest.csv
[NYTimesBlogTrain]:data/NYTimesBlogTrain.csv
[New York Times'blog directory]:http://www.nytimes.com/interactive/blogs/directory.html
