library(caret)
library(dplyr)
library(tidyverse)

## Load the data

list.files()
dat <- read.csv("winemag-data-130k-v2.csv", stringsAsFactors = FALSE)
summary(dat)

dat_no_na <- dat[complete.cases(dat),]
nrow(dat) - nrow(dat_no_na)

boxplot(dat_no_na$price, main = "Box Plot of Price")
# we have some outliers in price
#look at outliers
dat_no_na[dat_no_na$price > 1500,]


# correlation between price and points
plot(dat_no_na$points,dat_no_na$price)
cor(dat_no_na$price,dat_no_na$points)
# correlation = 0.416

boxplot(scale(dat_no_na[dat_no_na$price,c(6)]))

# categorical variables
boxplot(dat_no_na$variety,dat_no_na$points)

# Add Word count
dat_no_na <- dat_no_na %>%
  mutate(wordcount = sapply(strsplit(description, " "), length))
plot(dat_no_na$points,dat_no_na$wordcount)
cor(dat_no_na$points,dat_no_na$wordcount)
# correlation = 0.53

# Add Year
dat_no_na <- dat_no_na %>%
  extract(title, c("year"), regex = "([0-9]{4})", remove = F)
dat_no_na$year <- as.integer(dat_no_na$year)

plot(dat_no_na$points,dat_no_na$year)
dat_no_na[dat_no_na$year > 2019,c(12,13)]
dat_no_na[dat_no_na$year < 1900,c(12,13)]
# filter older than current year and younger than 1900 because obvious date extract issues
dat_no_na <- dat_no_na %>%
  filter(year>1900 & year <2019)
dat_no_na <- dat_no_na[complete.cases(dat_no_na),]
cor(dat_no_na$year,dat_no_na$points)
# correlation = 0.06

# Look at top 100 varieties by number of reviews
# let's look only at the top 100 varieties by number of reviews
dat_no_na <- dat_no_na %>%
  group_by(variety) %>%
  filter(n() > 100) %>%
  ungroup(variety)

table(dat_no_na$variety)
table(dat_no_na$country)

# Look at countries with more than 1000 records
dat_country <- dat_no_na %>%
  group_by(country) %>%
  filter(n()>=1000) %>%
  ungroup(country)

table(dat_country$country)
dat_country <- dat_country[complete.cases(dat_country),]

#now we can classify variety by color R/W and country by hemisphere N/S

americas <- c("Argentina","Brazil","Canada","Chile","Mexico","Peru",
              "Uruguay","US")
africas <- c("Morocco","South Africa")
australias <- c("Australia","New Zealand")
europes <- c("Armenia","Austria","Bulgaria","Croatio","Cyprus","Czech Republic",
            "England","France","Georgia","Germany","Greece","Hungary","Italy",
            "Luxembourg","Macedonia","Moldova","Portugal","Romania",
            "Serbia","Slovakia","Spain","Switzerland","Turkey","Ukraine")
asias <- c("India","Israel","Lebanon")

dat_country <- dat_country %>%
  mutate(continent = ifelse(country %in% americas,"Ame",
                            ifelse(country %in% africas,"Afr",
                                   ifelse(country %in% australias, "Aus",
                                          ifelse(country %in% europes, "Eur",
                                                 ifelse(country %in% asias, "Asi","Unk"))))))
# do we have any unclassified countries?
dat_country[dat_country$continent=="Unk",]

dat_country$continent <- as.factor(dat_country$continent)
dat_country %>%
  ggplot(aes(continent,points,color = continent)) +
  geom_point()


table(dat_country$variety)

reds <- c("Aglianico","Barbera","Blaufränkisch","Bonarda","Bordeaux-style Red Blend",
          "Cabernet Franc","Cabernet Sauvignon","Cabernet Sauvignon-Merlot",
          "Cabernet Sauvignon-Syrah","Carmenère","Champagne Blend",
          "Corvina, Rondinella, Molinara","Dolcetto","G-S-M","Gamay",
          "Garnacha","Grenache","Malbec","Mencía","Meritage","Merlot",
          "Monastrell","Montepulciano","Mourvèdre","Nebbiolo","Nerello Mascalese",
          "Nero d'Avola","Petit Verdot","Petite Sirah","Pinot Nero","Pinot Noir",
          "Pinotage","Port","Portuguese Red","Primitivo","Red Blend",
          "Rhône-style Red Blend","Sangiovese","Sangiovese Grosso","Shiraz",
          "Syrah","Tannat","Tempranillo","Tempranillo Blend","Tinta de Toro",
          "Touriga Nacional","Zinfandel","Zweigelt")
whites <- c("Albariño","Alvarinho","Bordeaux-style White Blend","Chardonnay",
            "Chenin Blanc","Fiano","Friulano","Garganega","Gewürztraminer",
            "Glera","Greco","Grenache Blanc","Grillo","Grüner Veltliner",
            "Melon","Moscato","Muscat","Pinot Bianco","Pinot Blanc",
            "Pinot Grigio","Pinot Gris","Portuguese White",
            "Rhône-style White Blend","Riesling","Rosé","Roussanne",
            "Sauvignon","Sauvignon Blanc","Sémillon","Sparkling Blend",
            "Torrontés","Turbiana","Verdejo","Verdicchio","Vermentino",
            "Vernaccia","Viognier","Viura","White Blend")

dat_country <- dat_country %>%
  mutate(color = ifelse(variety %in% reds,"Red",
                        ifelse(variety %in% whites, "White","Unk")))

# did we miss any?
dat_country[dat_country$color=="Unk",14]

dat_country$color <- as.factor(dat_country$color)

# look at sommelier words and frequency
# testing sommelier words (295 words)
# https://myvocabulary.com/word-list/sommelier-vocabulary/


sommelier_words <-c("Acceptance","Access","Accolades","Accompany","Acidity","Adhere","Advanced Sommelier Exam","Aficionado","Age-branding","Aging","Alcohol","Allure","Amount","Amplification","Appellation",
                    "Appropriate","Aroma","Attributes","Availability",
                    "Balance","Bar","Barrels","Base wine","Beverage","Blend","Blind tasting","Bordeaux","Bottling","Bouquet","Bracing","Brand","Brisk","Bubbles",
                    "Cabernet","Case","Cash flow","Cask","Cave","Certification","Chardonnay","Chateau","Chemistry","Chilled","Choices","Client","Collection","Color","Commercial","Competition","Complex",
                    "Component","Confidence","Connoisseur","Consistency","Consume","Consumer","Consumption","Content","Convert","Cork","Corkage","Courses",
                    "Decade","Decant","Delicious","Designation","Designation","Detect","Development","Diploma","Discerning","Disgorge","Distill","District","Dry",
                    "Education","Element","Elixir","Enhance","Enology","Enophile","Enthusiast","Error","Evaluate","Evaporation","Exalt","Exceptional","Excess","Expansive","Expectation","Expensive","Expert","Expertise",
                    "Fermentation","Filter","Flavor","Foil","Fortify","Fraction","Fruity","Full-bodied",
                    "Generic","Gifted","Glasses","Global","Gourmet","Grape vines","Grapes","Grower",
                    "Harvest","Healthy","Hearty","Hillside","History",
                    "Identification","Importance","Influence","Inhale","Interpret","Interpretation",
                    "Joy","Juice",
                    "Keen","Knack","Knowledge",
                    "Label","Lavish","Limited edition","Liquid","Liter",
                    "Majority","Mark-up","Market","Marketing","Master Sommelier","Mellow","Memorable","Mentor","Merit","Merlot","Moderation",
                    "Name","Native","Nature","Negotiant","Nuance",
                    "Oak","Oenology","Oenophile","Optional","Osmosis",
                    "Packaging","Palate","Patrons","Peak","Percent","Pinot noir","Planting","Plentiful","Popularity","Pour","Praise","Premium","Preparation","Press","Prestigious","Primary","Private-label",
                    "Producer","Production","Professional pickers","Prominent","Property","Proprietary","Protection","Prowess","Prune","Purchase","Purist","Pursue",
                    "Quaff","Quality","Quantity","Query","Quest","Questions",
                    "Rack","Reasoning","Recommendation","Regions","Reliability","Renown","Requirement","Research","Reserve","Residual","Restaurant","Restriction","Reveal","Ripe","Ripen","Ritual","Room temperature",
                    "Rotate","Rustic",
                    "Sample","Sauterne","Sauvignon blanc/gris","Savory","Second-tier","Sediment","Selection","Sensitivity","Sensory","Shears","Shiraz","Showcase","Sip","Skin","Smooth","Snob","Soil","Solution",
                    "Sommelier","Source","Sparkling","Spicy","Spoilage","Spritzy","Stage","Stomp","Store","Study","Subject","Sublime","Subtle","Succulent","Sugar","Suggest","Superior","Surplus","Syrah",
                    "Tannin","Tart","Taste","Taster","Tasting","Technique","Temperature","Texture","Theme","Thirst","Tier","Tourism","Tweak",
                    "Ubiquitous","Unchilled","Uncork","Under-appreciated","Uniform","Unique","Universal","Unsubtle",
                    "Value","Valve","Varietal","Varieties","Verdant","Version","Vibrant","Vines","Vineyard","Vineyard","Viniculture","Vinify","Vinous","Vintage","Vitis labrusca","Vitis vinifera",
                    "Wages","Weather","Weight","Wine cellar","Winemaker","Winery","Winner","Withstand",
                    "Yeast","Yield",
                    "Zest","Zinfandel","Zing","Zip","Zone")

#small_dec <- head(dat$description)

# https://winefolly.com/review/identifying-flavors-in-wine/
sommelier_words <- c("blackberry","black currant","marionberry","black plum","blueberry",
                     "black cherry","black raspberry","acai","jam","prune","fig","black raisins",
                     "cranberry","pomegranate","red currant","bing cherry","strawberry","cherry",
                     "raspberry","red plum","goji berry","dragon fruit","candied cherries","candied berries",
                     "apricot","peach","white peach","nectarine","apple","pear","red grapefruit",
                     "orange","pink grapefruit","passion fruit","lemon","lime","pineapple")


wcount <- sapply(sommelier_words, function(sw){
  str_count(dat_country$description,sw)
})
#rowSums(wcount)

dat_country <- dat_country %>%
  mutate(winewords = rowSums(wcount))

dat_country %>%
  ggplot(aes(points,winewords,color = color)) +
  geom_point()

table(dat_country$winewords)
cor(dat_country$points,dat_country$winewords)

dim(dat_country)
dat_country %>%
  summarise(n_user = n_distinct(taster_name),
            n_wines = n_distinct(title))

table(dat_country$designation)
table(dat_country$province)

wines <- dat_country %>%
  select(-X,-country,-description,-designation,-province,-region_1,-region_2,
         -taster_name,-taster_twitter_handle,-title,-variety,-winery)

rm(dat,dat_country,dat_no_na,africas,americas,asias,
   australias,europes,reds,whites, sommelier_words,wcount)

#reorder columns
wines <- wines[c("price","year","wordcount","winewords","continent","color","points")]

#dimensions of dataset
dim(wines)
# type for each attribute
sapply(wines,class)
#wines$points <- as.numeric(wines$points)
#wines$year <- as.numeric(wines$year)
#wines$wordcount <- as.numeric(wines$wordcount)

#look at data
head(wines)
summary(wines)

#levels
levels(wines$continent)
levels(wines$color)

# class distribution
percentage <- prop.table(table(wines$color))*100
cbind(freq=table(wines$color), percentage=percentage)

percentage <- prop.table(table(wines$continent))*100
cbind(freq=table(wines$continent), percentage=percentage)

# statistical summary
summary(wines)

# visualize data
# univariate plots
x_num<-wines[,1:4]
x_fac<-wines[,5:6]
y<-wines[,7]

#numerical input ditribution

par(mfrow=c(1,4))
for (i in 1:4) {
  boxplot(x_num[,i], main=names(wines)[i])
}

#factor input distribution
par(mfrow=c(1,2))
for (i in 1:2) {
  plot(x_fac[,i], main=names(wines)[i+4])
}

# output distribution
boxplot(y,main=names(wines)[7])

# multivariate plots
#scatterplot
# price vs points
wines %>%
  ggplot(aes(x=price,y=points)) +
  geom_point()

# log scale
wines %>%
  ggplot(aes(x=price,y=points)) +
  geom_point() +
  scale_x_log10()

cor(wines$price,wines$points)
cor(log(wines$price),wines$points)

# year vs points
wines[wines$year>1990,] %>%
  ggplot(aes(x=year,y=points)) +
  geom_point()

cor(wines$year,wines$points)
# cor = 0.065
#no correlation between year and points


# wordcount vs points
wines %>%
  ggplot(aes(x=wordcount,y=points)) +
  geom_point()

wines %>%
  ggplot(aes(x=wordcount,y=points)) +
  geom_point() +
  scale_x_log10()

cor(wines$wordcount,wines$points)
cor(log(wines$wordcount),wines$points)
# cor = 0.54

# winewords vs points
wines %>%
  ggplot(aes(x=winewords,y=points)) +
  geom_point()

cor(wines$winewords,wines$points)
# cor = 0.088


# continent vs points
wines %>%
  ggplot(aes(x=continent,y=points,fill=continent)) +
  geom_boxplot()

# continent vs points for each color
wines %>%
  ggplot(aes(x=continent,y=points,fill = color)) +
  geom_boxplot()


# color vs points

wines %>%
  ggplot(aes(x=color,y=points,fill=color)) +
  geom_boxplot()

# color vs wordcount for fun
wines %>%
  ggplot(aes(x=color,y=wordcount, col=color)) +
  geom_jitter()

# correlations
cor(log(wines$price),wines$points)
cor(wines$year,wines$points)
cor(wines$wordcount,wines$points)
cor(wines$winewords,wines$points)


# wordcount vs points and color
wines %>%
  ggplot(aes(wordcount,points, color = color)) +
  geom_point()

# wordcount vs points and continent
wines %>%
  ggplot(aes(wordcount,points, color = continent)) +
  geom_point()

# price vs points and color
wines[wines$price<500 & wines$color=="White" & wines$continent=="Afr",] %>%
  ggplot(aes(price,points, color = color)) +
  geom_point()


# small test to test models and get timing
wines_mini <- wines[1:1000,]

models <- c("lm","svmLinear",
            "gamLoess", 
            "knn", "kknn", "gam",
            "rf", "ranger", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")
#models <- c("gamLoess")

fits <- lapply(models, function(model){ 
  print(model)
  fit <- train(points ~ ., method = model, data = wines_mini)
  
}) 

names(fits) <- models

#fits$svmLinear$results["RMSE"]
#rmse_results <- sapply(names(fits),function(f){
#  print(f)
#  print(fits[[f]]$results["RMSE"])
#  data_frame(method=f, RMSE = fits[[f]]$results["RMSE"])
#})

rmse_results <-data_frame(method = names(fits[1]), RMSE = fits$lm$results$RMSE)
rmse_results<-bind_rows(rmse_results,
                       data_frame(method = names(fits[2]),
                                  RMSE = fits$svmLinear$results$RMSE))
rmse_results<-bind_rows(rmse_results,
                        data_frame(method = names(fits[3]),
                                   RMSE = fits$gamLoess$results$RMSE))
rmse_results<-bind_rows(rmse_results,
                        data_frame(method = names(fits[4]),
                                   RMSE = fits$knn$results$RMSE))
rmse_results<-bind_rows(rmse_results,
                        data_frame(method = names(fits[5]),
                                   RMSE = fits$kknn$results$RMSE))
rmse_results<-bind_rows(rmse_results,
                        data_frame(method = names(fits[6]),
                                   RMSE = fits$gam$results$RMSE))
rmse_results<-bind_rows(rmse_results,
                        data_frame(method = names(fits[7]),
                                   RMSE = fits$rf$results$RMSE))
rmse_results<-bind_rows(rmse_results,
                        data_frame(method = names(fits[8]),
                                   RMSE = fits$ranger$results$RMSE))
rmse_results<-bind_rows(rmse_results,
                        data_frame(method = names(fits[9]),
                                   RMSE = fits$Rborist$results$RMSE))
rmse_results<-bind_rows(rmse_results,
                        data_frame(method = names(fits[10]),
                                   RMSE = fits$avNNet$results$RMSE))
rmse_results<-bind_rows(rmse_results,
                        data_frame(method = names(fits[11]),
                                   RMSE = fits$mlp$results$RMSE))
rmse_results<-bind_rows(rmse_results,
                        data_frame(method = names(fits[12]),
                                   RMSE = fits$monmlp$results$RMSE))
rmse_results<-bind_rows(rmse_results,
                        data_frame(method = names(fits[13]),
                                   RMSE = fits$gbm$results$RMSE))
rmse_results<-bind_rows(rmse_results,
                        data_frame(method = names(fits[14]),
                                   RMSE = fits$svmRadial$results$RMSE))
rmse_results<-bind_rows(rmse_results,
                        data_frame(method = names(fits[15]),
                                   RMSE = fits$svmRadialCost$results$RMSE))
rmse_results<-bind_rows(rmse_results,
                        data_frame(method = names(fits[16]),
                                   RMSE = fits$svmRadialSigma$results$RMSE))

rmse_results %>% knitr::kable()

#model with lowest RMSE
rmse_results[which.min(rmse_results$RMSE),]


#set up test and train
set.seed(123)
test_index <- createDataPartition(y = wines$points, times = 1, p=0.2, list = FALSE)

wine_train <- wines[-test_index,]
temp <- wines[test_index,]

## Make sure variety and year in test set is in train set as well
wine_test <- temp %>%
  semi_join(wine_train, by = "continent") %>%
  semi_join(wine_train, by = "price") %>%
  semi_join(wine_train, by = "year") %>%
  semi_join(wine_train, by = "color")

# add rows removed from test set back to train set
removed <- anti_join(temp, wine_test)
wine_train <- rbind(wine_train, removed)

rm(temp, removed)

#summarize dataset
summary(wine_train)



#models
modelsA <- c("svmLinear",
            "gamLoess", 
            "knn", "kknn", "gam")
modelsB <- c("ranger", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")

fits <- lapply(modelsA, function(model){ 
  print(model)
  fit <- train(points ~ ., method = model, data = wine_train)
  #y_hat <- predict(fit,
  #                 wine_test,
  #                 type = "raw")
  #cm <- confusionMatrix(y_hat, wine_test$points)
  #cm$overall["Accuracy"]
}) 

names(fits) <- modelsA

rmse_results <- sapply(names(fits),function(f){
  print(f)
  print(fits[[f]]$results["RMSE"])
  data_frame(method=f, RMSE = fits[[f]]$results["RMSE"])
})

fits$svmLinear$results["RMSE"]
fits$gamLoess$results["RMSE"]
fits$knn$results["RMSE"]
fits$kknn$results["RMSE"]
fits$gam$results["RMSE"]
# Results are RMSE's > 2 - very bad

fit <- train(points ~ ., method = "ranger", data = wine_train)
fit            

myRMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

y_hat <- predict(fit,wine_test)
#round values to points
y_hat <- ifelse(y_hat >100,100,ifelse(y_hat<80,80,round(y_hat)))

confusionMatrix(data = as.factor(y_hat),reference = as.factor(wine_test$points))
confusionMatrix(data = as.factor(y_hat),reference = as.factor(wine_test$points))$overall["Accuracy"]
myRMSE(wine_test$points,y_hat)
#RMSE = 2.037

par(mfrow = c(2,2))
plot(fit)

#### Could be end of report ####

wine_test %>%
  ggplot(aes(y_hat, points, color = y_hat/points)) +
  geom_point()

fits$knn
fits$svmLinear
y_hat <- predict(fits$svmLinear,wine_test)

#stratify y_hat
max(y_hat)
min(y_hat)
y_hat <- ifelse(y_hat >100,100,ifelse(y_hat<80,80,round(y_hat)))

levels(as.factor(y_hat))
levels(as.factor(wine_test$points))

confusionMatrix(data = as.factor(y_hat), reference = as.factor(wine_test$points))

sum(y_hat-wine_test$points)/length(wine_test$points)
plot(wine_test$points,y_hat-wine_test$points)

plot(y_hat)
plot(wine_test$points)

myScore <- function(true_ratings, predicted_ratings){
  mean(ifelse(abs(true_ratings-predicted_ratings)<2,1,0))
}
result<-myScore(wine_test$points,y_hat)
result
