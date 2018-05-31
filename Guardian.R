install.packages("GuardianR")

library(GuardianR)

articles <- get_guardian(keywords = "Brexit",
                         section = NULL,
                         from.date = "2018-01-01",
                         to.date = "2018-01-31",
                         api.key = "cd428c88-1153-4281-9806-4cc5e674fe6d")

str(articles)

articles$body <- iconv(articles$body, "", "ASCII", "byte")
articles$body <- gsub("<.*?>", "", articles$body)

library(quanteda)
dfm <- dfm(corpus(articles, text_field = "body"), remove = stopwords("english"), remove_punct = TRUE)
topfeatures(dfm, 100)
textplot_wordcloud(dfm)