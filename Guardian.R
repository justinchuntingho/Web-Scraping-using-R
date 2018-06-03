
# First, you need to go to https://open-platform.theguardian.com/ to register for an api key

install.packages("GuardianR")
library(GuardianR)

articles <- get_guardian(keywords = "Brexit",
                         section = NULL,
                         from.date = "2018-01-01",
                         to.date = "2018-01-31",
                         api.key = "xxxxxxxxxxxxxxxxxxxx") # paste your api key here

str(articles)

articles$body <- iconv(articles$body, "", "ASCII", "byte")
articles$body <- gsub("<.*?>", "", articles$body)

library(quanteda)
dfm <- dfm(corpus(articles, text_field = "body"), remove = stopwords("english"), remove_punct = TRUE)
topfeatures(dfm, 100)
textplot_wordcloud(dfm)