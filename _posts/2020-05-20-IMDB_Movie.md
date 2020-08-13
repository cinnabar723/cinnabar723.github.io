---
title: "IBDM Movie Data Analysis"
date: 2020-05-20
tags: [Data Wrangling, Machine Learning, R]
header:
  image: "/images/movie.jpg"
excerpt: "Data Wrangling, Data Science, Messy Data"
mathjax: "true"
---

## Project Introduction
This group project is the analysis of IMDB movie data from 1916 to 2016 that is commissioned to examine how filmmakers can consistently produce high IMDB-scored lms. It would facilitate the maintenance of a good reputation and thus secure the long-term success of the company. In this project, we applied different data analysis techniques including clustering, random forest, text mining and GBM. We analyzed the data, predicted the scores and explored the correlation between different variables and scores. Our team determined that clustering is not very applicable and helpful to our analysis and decided to use only random forest, text mining and GBM. We found that social media exposure will have a positive correlation with the movie score. Content rating, budget, duration also have positive infuence on the IMDB score. Moreover, the final model that has the best predicting accuracy is the gradient boosting model with text mining.

## Project Code
```r
library(dplyr) # data manipulation
library(stringr)
library(ggplot2) # visualization
library(gridExtra)
library(grid)
library(lattice)
library(ggrepel)
library(ggthemes) 
library(scales) 
library(formattable)
install.packages('DaMiRseq')

library(tm)


### Load data into R
setwd("/Users/559/Desktop")
data = read.csv('movie_metadata.csv')
# Look at the data
str(data)
# 5043 observations

## Look at the duplicate rows
sum(duplicated(data))
# Delete duplicate rows
data <- data[!duplicated(data), ]
str(data)
# 4998 observations


## Missing values
sapply(data, function(x) sum(is.na(x)))
# Delete rows with null values for gross and budget because imputation will not do a good job here.
data <- data[!is.na(data$gross), ]
data <- data[!is.na(data$budget), ]
# Delete row with null values in other column
data <- data[!is.na(data$num_critic_for_reviews), ]
data <- data[!is.na(data$duration), ]
data <- data[!is.na(data$facenumber_in_poster), ]
data <- data[!is.na(data$aspect_ratio), ]
data <- data[!is.na(data$actor_1_facebook_likes), ]
data <- data[!is.na(data$actor_2_facebook_likes), ]
data <- data[!is.na(data$actor_3_facebook_likes), ]
str(data)
# 3768 observations
# Look at the missing values
sapply(data, function(x) sum(is.na(x)))
# After removing row with null value, we still have 75% of data.

table(data$aspect_ratio)


## Remove columns.
table(data$color)
data <- subset(data, select = -c(color))
# Becuae 96.6% movies are colored, let's delete color variable.
table(data$language)
data <- subset(data, select = -c(language))
# Becuae 95.4% movies are same language, let's delete language variable.
data <- subset(data, select = -c(movie_c_link))
# Because movie c link does not provide useful information to score, let's delete the link. 


## Clean Movie Title
# Movie titles have a special character (Â) at the end and some have whitespaces.
data$movie_title <- gsub("Â", "", as.character(factor(data$movie_title)))
str_trim(data$movie_title, side = "right")

## Combine data
# Content rating
table(data$content_rating)
# There are 29 blanks in the rating cell. Blank is a different form of missing values, delete rows with missing values.
data <- data[!(data$content_rating %in% ""),]
# Base on the history content ratings naming,we find M = GP = PG, X = NC-17
# We also replace "Approved", "Not Rated", "Passed", "Unrated" with the most common rating "R".
data$content_rating[data$content_rating == 'M']   <- 'PG' 
data$content_rating[data$content_rating == 'GP']  <- 'PG' 
data$content_rating[data$content_rating == 'X']   <- 'NC-17'
data$content_rating[data$content_rating == 'Approved']  <- 'R' 
data$content_rating[data$content_rating == 'Not Rated'] <- 'R' 
data$content_rating[data$content_rating == 'Passed']    <- 'R' 
data$content_rating[data$content_rating == 'Unrated']   <- 'R' 
data$content_rating <- factor(data$content_rating)
table(data$content_rating)
# 5 factors for content rating
# country
table(data$country)
levels(data$country) <- c(levels(data$country), "Others")
data$country[(data$country != 'USA')&(data$country != 'UK')] <- 'Others' 
data$country <- factor(data$country)
table(data$country)
# 3 factors for country

## Add Column profit and return on investment
data <- data %>% 
  mutate(profit = gross - budget,
         return_on_investment_perc = (profit/budget)*100)
str(data)
# 3739 data

# Exploratory Analysis
ggplot(data, aes(title_year)) +
  geom_bar() +
  labs(x = "Year movie was released", y = "Movie Count", title = "Histogram of Movie released") +
  theme(plot.title = element_text(hjust = 0.5))
data <- data[data$title_year >= 1975,]
# Since most movie are produced after 1975, we removed movies that are produced before 1975

data %>%
  group_by(director_name) %>%
  summarise(avg_c_score  = mean(c_score)) %>%
  arrange(desc(avg_c_score )) %>%
  top_n(20, avg_c_score ) %>%
  formattable(list(avg_c_score  = color_bar("pink")), align = 'l')

bp1 <- ggplot(data, aes(country, c_score)) + 
  geom_boxplot(aes(fill = country)) +
  theme_minimal() +
  theme(legend.position = "top")

bp2 <- ggplot(data, aes(content_rating, c_score)) + 
  geom_boxplot(aes(fill = content_rating)) +
  theme_minimal() +
  theme(legend.position = "top")

bp1
bp2
# grid.arrange(bp1,bp2, nrow = 1)


# If a movie getting more likes on Facebook, it tends to have a better c rating.
gp1 <- ggplot(data,aes(x=movie_facebook_likes,y=c_score))+
  geom_point()+
  geom_smooth(method='lm')

# If a movie getting more reviews, it tends to have a better c rating.
gp2 <- ggplot(data,aes(x=num_user_for_reviews,y=c_score))+
  geom_point()+
  geom_smooth(method='lm')

# If the director of the movie have more facebook likes, it tends to have a better c rating.
gp3 <- ggplot(data,aes(x=director_facebook_likes,y=c_score))+
  geom_point()+
  geom_smooth(method='lm')

# If the primary actor of the movie have more facebook likes, it tends to have a better c rating.
gp4 <- ggplot(data,aes(x=actor_1_facebook_likes,y=c_score))+
  geom_point()+
  geom_smooth(method='lm')

# If the secondary actor of the movie have more facebook likes, it tends to have a better c rating
gp5 <- ggplot(data,aes(x=actor_2_facebook_likes,y=c_score))+
  geom_point()+
  geom_smooth(method='lm')

# If other actors of the movie have more Facebook likes, it tends to have a better c rating. 
# However, other actors are less influential compare to primary and secondary actors.
gp6 <- ggplot(data,aes(x=actor_3_facebook_likes,y=c_score))+
  geom_point()+
  geom_smooth(method='lm')

grid.arrange(gp1, gp2, gp3,gp4,gp5,gp6, nrow = 2)

str(data)
# 3673 rows
write.csv(data,'movie_cleaned.csv')



########Distribution of c score
table(data$c_score)
mean(data$c_score)
median(data$c_score)
# The mean score of all movies is 6.444541 and median is 6.5. 
ggplot(data, aes(x=c_score,color=country))+
  geom_histogram(color="darkblue", fill="lightblue")

p <- ggplot(data, aes(x=c_score, color=country)) +
  geom_histogram(fill="white", position="dodge")+
  theme(legend.position="top")

p + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) 


install.packages('randomForest')
library(randomForest)
###### models before text mining 
data2 <- subset(data, select = -c(movie_title,plot_keywords,genres,director_name, actor_1_name,actor_2_name,actor_3_name))
head(data2)

set.seed(1031)
split = sample(1:nrow(data2),size = 0.8*nrow(data2))
train1 = data2 [split,]
test1 = data2 [-split,]
head(train1)
str(train1)
# 20 variables

###  Perdicting 

## Random Forest
forest_train1 = randomForest(c_score~.,data=train1,ntree=100)
y_pred = predict(forest_train1, newdata = test1)
pred1= predict(forest_train1, newdata = test1)
rmse1 = sqrt(mean((pred1-test1$c_score)^2))
rmse1
# 0.7078958 


###### Text Mining 
head(data$genres)
head(data$plot_keywords)

library(stringr)

### Most Common genres

data$genres <- as.character(data$genres)

data%>%
  unnest_tokens(input = genres, output = word)%>%
  select(word)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(20)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()

data%>%
  unnest_tokens(input = genres, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)



### Most common plot keywords
data$plot_keywords <- as.character(data$plot_keywords)


data%>%
  unnest_tokens(input = plot_keywords, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)

data%>%
  unnest_tokens(input = plot_keywords, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(20)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()

data%>%
  unnest_tokens(input = plot_keywords, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)


######
data$id <- seq.int(nrow(data))
afinn = get_sentiments('afinn')
YEafinn = read.table('https://raw.githubusercontent.com/pseudorational/data/master/AFINN-111.txt',
                     header = F,
                     quote="",
                     sep = '\t',
                     col.names = c('word','value'), 
                     encoding='UTF-8',
                     stringsAsFactors = F)


data %>%
  select(id,genres)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=genres)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  summarize(min=min(reviewSentiment),max=max(reviewSentiment),median=median(reviewSentiment),mean=mean(reviewSentiment))

nrc = read.table(file = 'https://raw.githubusercontent.com/pseudorational/data/master/nrc_lexicon.txt',
                 header = F,col.names = c('word','sentiment','num'),sep = '\t'); 
nrc = nrc[nrc$num!=0,]; nrc$num = NULL

nrc%>%
  group_by(sentiment)%>%
  count()

data %>%
  group_by(id)%>%
  unnest_tokens(output = word, input = genres)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()

table(nrc$sentiment)  

data %>%
  group_by(id)%>%
  unnest_tokens(output = word, input = plot_keywords)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()

data %>%
  group_by(id)%>%
  unnest_tokens(output = word, input = plot_keywords)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()+theme_wsj()


#### Ratings of each Review based on Emotions Expressed
data %>%
  group_by(id)%>%
  unnest_tokens(output = word, input = plot_keywords)%>%
  inner_join(nrc)%>%
  group_by(id,sentiment,c_score)%>%
  count()

data %>%
  group_by(id)%>%
  unnest_tokens(output = word, input = plot_keywords)%>%
  inner_join(nrc)%>%
  group_by(id,sentiment,c_score)%>%
  count()%>%
  group_by(sentiment, c_score)%>%
  summarize(n = mean(n))%>%
  data.frame()

data %>%
  group_by(id)%>%
  unnest_tokens(output = word, input = plot_keywords)%>%
  inner_join(nrc)%>%
  group_by(id,sentiment,c_score)%>%
  count()%>%
  group_by(sentiment, c_score)%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=c_score,y=n,fill=c_score))+
  geom_col()+
  facet_wrap(~sentiment)+
  guides(fill=F)+coord_flip()

#### Correlation between emotion expressed and plot keywords
data %>%
  group_by(id)%>%
  unnest_tokens(output = word, input = plot_keywords)%>%
  inner_join(nrc)%>%
  group_by(id,sentiment,c_score)%>%
  count()%>%
  ungroup()%>%
  group_by(sentiment)%>%
  summarize(correlation = cor(n,c_score))


##### Predictive Analysis with Text

#### Replacing | with " "
head(data$genres)
data$genres <- gsub("|", " ", data$genres, fixed = TRUE)
head(data$genres)

head(data$plot_keywords)
data$plot_keywords <- gsub("|", " ", data$plot_keywords, fixed = TRUE)
head(data$plot_keywords)

## Create a corpus for genres
corpus = Corpus(VectorSource(data$genres))
corpus[[100]][1]
dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(data$genres))),
                     lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))
dtm = DocumentTermMatrix(corpus)
dtm
inspect(dtm[1,'action'])
xdtm = removeSparseTerms(dtm,sparse = 0.9999)
xdtm
xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),
                                dictionary = dict_corpus,
                                type='prevalent')
colnames(xdtm) = make.names(colnames(xdtm))
sort(colSums(xdtm),decreasing = T)


#### Create a corpus for plot keywords
corpus2 = Corpus(VectorSource(data$plot_keywords))
corpus2[[100]][1]
dict2 = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(data$plot_keywords))),
                      lowfreq = 0)
dict_corpus2 = Corpus(VectorSource(dict2))
dtm2 = DocumentTermMatrix(corpus2)
dtm2
xdtm2 = removeSparseTerms(dtm2,sparse = 0.99)
xdtm2
xdtm2 = as.data.frame(as.matrix(xdtm2))
colnames(xdtm2) = stemCompletion(x = colnames(xdtm2),
                                 dictionary = dict_corpus2,
                                 type='prevalent')
colnames(xdtm2) = make.names(colnames(xdtm2))
sort(colSums(xdtm2),decreasing = T)


#### Create a corpus for director_name
data$director_name <- gsub(" ", "", data$director_name, fixed = TRUE)
## Only the pair of first and last name make sense
corpus3 = Corpus(VectorSource(data$director_name))
corpus3 = tm_map(corpus3,FUN = removePunctuation)
corpus3[[100]][1]
dict3 = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(data$director_name))),
                      lowfreq = 0)
dict_corpus3 = Corpus(VectorSource(dict3))
dtm3 = DocumentTermMatrix(corpus3)
dtm3
xdtm3 = removeSparseTerms(dtm3,sparse = 0.998)
xdtm3
xdtm3 = as.data.frame(as.matrix(xdtm3))
colnames(xdtm3) = stemCompletion(x = colnames(xdtm3),
                                 dictionary = dict_corpus3,
                                 type='prevalent')
colnames(xdtm3) = make.names(colnames(xdtm3))
sort(colSums(xdtm3),decreasing = T)



#### Create a corpus for actor 1
data$actor_1_name <- gsub(" ", "", data$actor_1_name, fixed = TRUE)
## Only the pair of first and last name make sense
corpus4 = Corpus(VectorSource(data$actor_1_name))
corpus4 = tm_map(corpus4,FUN = removePunctuation)
corpus4[[100]][1]
dict4 = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(data$actor_1_name))),
                      lowfreq = 0)
dict_corpus4 = Corpus(VectorSource(dict4))
dtm4 = DocumentTermMatrix(corpus4)
dtm4
xdtm4 = removeSparseTerms(dtm4,sparse = 0.998)
xdtm4
xdtm4 = as.data.frame(as.matrix(xdtm4))
colnames(xdtm4) = stemCompletion(x = colnames(xdtm4),
                                 dictionary = dict_corpus4,
                                 type='prevalent')
colnames(xdtm4) = make.names(colnames(xdtm4))
sort(colSums(xdtm4),decreasing = T)



##Combine data
head(data)
data_subset <- subset(data, select = -c(movie_title,plot_keywords,genres,director_name, actor_1_name,actor_2_name,actor_3_name))
head(data_subset)

xdtm2 <- subset(xdtm2, select = -c(war,crime,two))
xdtm4 <- subset(xdtm4, select = -c(cchpounder))

final_data = cbind(data_subset,xdtm)
final_data = cbind(final_data, xdtm2)
final_data = cbind(final_data, xdtm3)
final_data = cbind(final_data, xdtm4)

final_data <- subset(final_data, select = -c(music))


set.seed(1031)
split = sample(1:nrow(final_data),size = 0.8*nrow(final_data))
train = final_data [split,]
test = final_data [-split,]
head(train)
str(train)
# 786 variables
# 286 variable

###  Perdicting 

## Random Forest
forest_train = randomForest(c_score~.,data=train,ntree=100)
y_pred = predict(forest_train, newdata = test)
pred= predict(forest_train, newdata = test)
rmse = sqrt(mean((pred-test$c_score)^2))
rmse
### 0.66


# GBM
library(xgboost)
library(gbm)
set.seed(100)
gbm.fit <- gbm(
  formula = c_score~.,
  distribution = "gaussian",
  data = train,
  n.trees = 1000,
  interaction.depth = 15,
  shrinkage =  0.01,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
) 
print(gbm.fit)

# assess the model performance
pred= predict(gbm.fit, newdata = test,n.trees = 1000)
rmse = sqrt(mean((pred-test$c_score)^2))
rmse
#  0.6200085


par(mar = c(5, 13, 1, 1))
summary(
  gbm.fit, 
  cBars = 35,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)
```
