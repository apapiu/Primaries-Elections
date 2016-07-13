library(dplyr)
library(tidyr)
library(plotly)
counties <- read.csv("county_facts.csv")
primary <- read.csv("primary_results.csv")

primary %>% filter(party == "Democrat") -> primary

primary$votes <- NULL
primary %>% spread(candidate, fraction_votes) -> dem_votes

dem_votes$`Martin O'Malley` <- NULL
dem_votes$` No Preference` <- NULL
dem_votes$` Uncommitted` <- NULL

primary %>% group_by(fips) %>% summarize(sum(votes)) -> num_votes
#USE THIS to remove counties that have few votes.

dem_votes$winner <- "Hillary"
dem_votes$winner[dem_votes$`Bernie Sanders` > dem_votes$`Hillary Clinton`] <- "Bernie"

dem_votes <- inner_join(dem_votes, counties, by = "fips")

train <-dem_votes[,c(8,11:dim(dem_votes)[2])] #winner plus different demographics.
train$winner <- as.factor(train$winner)

#models:
library(ranger)
library(randomForest)
library(xgboost)
library(gbm)

ranger(winner ~., data = train)

model_rf <- randomForest(winner ~., data = train)
plot(model_rf, ylim = c(0,0.5))
varImpPlot(model_rf)
model_rf

#very nice separation:
ggplot(train, aes(x = EDU635213, y = log(1 + RHI225214))) +
    geom_point(aes(color = winner), alpha = 0.8) +
    scale_color_brewer(palette = "Set1") +
    xlim(c(60, 100))


ggplot(train, aes(x = log(1 + AGE775214), y = log(1 + RHI325214))) +
    geom_point(aes(color = winner), alpha = 0.8) +
    scale_color_brewer(palette = "Set1")

#let's do a 3D one:





#XGBoost:
X <- as.matrix(train[,2:52])
y = 1*(train$winner == "Hillary")

xgb.cv(data = X, label = y, nfold = 10, nrounds = 200,
       objective = "binary:logistic",
       metrics = c("error", "auc")) -> cv.error

plot(cv.error$test.error.mean)
plot(cv.error$test.auc.mean)

max(cv.error$test.auc.mean)

model_xgb <- xgboost(data = X, label = y,nrounds = 200,
                     objective = "binary:logistic")

xgb.importance(feature_names = colnames(X), model = model_xgb) -> xgb_importance

#Now let's do PCA on the most important 10 features
#Sort off slightly supervised unsupervised learning.
xgb_importance$Feature[1:10] ->important_features

X_imp <- X[,important_features]

X_imp <- scale(X_imp)
pca <- prcomp(X_imp)
proj <- predict(pca, X_imp)
proj_2d <- data.frame(proj[,1:2], winner = train$winner)
biplot(pca)

#this isn't very informative, you get a better view by percentage black and 
ggplot(proj_2d, aes (x = PC1, y = PC2)) +
    geom_point(aes(color = winner))

#some more EDA
hist(train$EDU635213)
train$hs_bin <- cut(train$EDU635213, breaks = 30)
train$black_bin <- cut(sqrt(train$RHI225214), breaks = 30) #take sqrt here to visualize better

pplot <- function(x) {
    x <- substitute(x)
    ggplot(data = train, aes_q(x = x, fill = substitute(winner))) + 
        geom_bar(stat = "count", position = "fill", width = 0.8) +
        scale_fill_brewer(palette = "Set1")
}

pplot(hs_bin) + coord_flip()  + geom_bar(position = "fill")+
    labs(title = "Winner by Percentage of People With High School Diploma", 
         xlab = "Percentage with HS Diploma")


pplot(black_bin) + geom_bar() + coord_flip()

plot(dem_votes$RHI225214, dem_votes$`Hillary Clinton`)


