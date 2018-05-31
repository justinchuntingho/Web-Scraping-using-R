library(rvest)
library(magrittr)
library(stringr)


# SPS Website -------------------------------------------------------------

# For every webscraping task, you need do three things:
# 1. Download the html
# 2. Select the node that contains the information you want
# 3. Parse the html and get either: A. the text; or B. more links

# Consider Scenerio A: you have a link that contains all the information you need
# For example, if you would like to scrape the email addresse of all SPS staff from the website: http://www.sps.ed.ac.uk/staff

# Step 1: Download the html
html <- read_html("http://www.sps.ed.ac.uk/staff")

# Step 2: Identify the node, I recommend SelectorGadget
# There are many ways to do it, you could identify the cell by CSS ".person_contact"

# Step 3: Parse the codes
# Selecting the cells
html %>% html_nodes(".person_contact") %>% html_text() 

# Second approch, selecting the links by adding "a" after a space
html %>% html_nodes(".person_contact a") %>% html_text()  # To get links, use html_attr("href") instead

# Scenerio B: what if you are interested in what they are saying in their staff profile?
# You need to get the links

staff.links <- html %>% html_nodes(".person_name a") %>% html_attr("href")

# Now we have the link, but it would take forever to visit one by one
# We need to write a loop to do step 1 to 3 for EVERY link

for (i in 1:length(staff.links[1:10])){
  profile <- read_html(staff.links[i]) # download the html from the link
  profile %>% html_nodes("article") %>% # select the node
    html_text() %>% # parse the text
    cat(file=paste("profile_", i, ".txt", sep = ""), sep="", append=FALSE) # Save it to a file
}







# DailyMail ---------------------------------------------------------------

# Moving on to scraping Daily Mail
# Tips for web scraping: always try to find 1. text version 2. page that contains a list of links
# Eg site archive

# Take April as an example
# 1. Scrape the links to the days
html <- read_html("http://www.dailymail.co.uk/home/sitemaparchive/month_201804.html")
day.links <- html %>% html_nodes(".ccox a") %>% html_attr("href")

# 2. Scrape the links of each day
article.links <- c()
for (i in 1:length(day.links)){
  temp <- read_html(paste("http://www.dailymail.co.uk",day.links[i], sep="")) # download the html from the link
  link <- temp %>% html_nodes(".debate.link-box a") %>% # select the node
    html_attr("href") # get the links
  print(day.links[i]) # this is not necessary, but it is a good practice to use print() to see the progress
  print(head(link)) # print() have a quick view of the links so as to ensure it is doing the right thing
  article.links <- c(article.links, link)
}

for (i in 1:length(article.links[1:10])){ # I am just download the first 10th, just for demonstration
  temp <- read_html(paste("http://www.dailymail.co.uk",article.links[i], sep="")) # download the html from the link
  temp %>% html_nodes("#js-article-text") %>% # select the node
    html_text() %>% # parse the text
    cat(file=paste("dailymail_", i, ".txt", sep = ""), sep="", append=FALSE) # Save it to a file
}

# What if I only want news articles, but not other articles?
# You can identity the *position* of the links containing the path "/news/"
grep("/news/", article.links)

# And you can use that *position* to select only the links of news articles
# Once you have the new links, just pug it into the above loop
news.link <- article.links[grep("/news/", article.links)]


# Reconstructing the url --------------------------------------------------

# What if I want to scrape the article for Brexit?
# There is a search function...
# Tips: you can try to maniplulate the url
# eg "http://www.dailymail.co.uk/home/search.html?offset=0&size=50&sel=site&searchPhrase=brexit&sort=recent&channel=comment&type=article&days=all"

offset <- c(0, 1:10 * 50)
paste0("http://www.dailymail.co.uk/home/search.html?offset=",offset[1],"&size=50&sel=site&searchPhrase=brexit&sort=recent&channel=comment&type=article&days=all")

brexit.links <- c()
for (i in 1:length(offset)){
  temp <- read_html(paste0("http://www.dailymail.co.uk/home/search.html?offset=",offset[i],"&size=50&sel=site&searchPhrase=brexit&sort=recent&channel=comment&type=article&days=all")) # download the html from the link
  link <- temp %>% html_nodes(".sch-res-title a") %>% # select the node
    html_attr("href") # get the links
  print(head(link)) # print() have a quick view of the links so as to ensure it is doing the right thing
  brexit.links <- c(brexit.links, link)
}

for (i in 1:length(brexit.links[1:10])){ # I am just download the first 10th, just for demonstration
  temp <- read_html(paste("http://www.dailymail.co.uk",article.links[i], sep="")) # download the html from the link
  text <- temp %>% html_nodes(".mol-para-with-font") %>% # select the node
    html_text() %>% 
    cat(file=paste("brexit_", i, ".txt", sep = ""), sep="", append=FALSE)
}

# One more trick, try the text based site!
# See the difference of the link, we have to reconstruct the link

brexit.textbased <- paste0("/textbased", brexit.links)
brexit.textbased <- gsub("/article-", "/text-", brexit.textbased)

for (i in 1:length(brexit.textbased[1:10])){ # I am just download the first 10th, just for demonstration
  temp <- read_html(paste("http://www.dailymail.co.uk",brexit.textbased[i], sep="")) # download the html from the link
  text <- temp %>% html_nodes(".article") %>% # select the node
    html_text() %>% 
    cat(file=paste("brexit_", i, ".txt", sep = ""), sep="", append=FALSE)
}

# You can also save the result as a vector object
brexit.post <- c()
for (i in 1:length(brexit.textbased)){ # I am just download the first 10th, just for demonstration
  temp <- read_html(paste("http://www.dailymail.co.uk",brexit.textbased[i], sep="")) # download the html from the link
  text <- temp %>% html_nodes(".article") %>% # select the node
    html_text() %>% 
    paste(collapse = "\n")
  brexit.post <- c(brexit.post, text)
  print(i)
}
saveRDS(brexit.post, "brexit.post.rds")

# A very simple exploration
library(quanteda)
dfm <- dfm(corpus(brexit.post), remove = c(stopwords('english'), "per", "cent"), remove_numbers = TRUE, remove_punct = TRUE) 

topfeatures(dfm, 100)

png("brexit.png", width = 1200, height = 1200)
textplot_wordcloud(dfm, min.freq = 1, colors = c("#1B9E77","#D95F02","#7570B3","#E7298A"))
dev.off()

# Tips: sometimes you might want to lay low
# Sys.sleep(sample(seq(1, 3, by=0.001), 1))

