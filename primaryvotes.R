library(tigris)
library(leaflet)
library(dplyr)
library(acs)
library(ggplot2)
library(plotly)
library(grid)
library(gridExtra)


#we'll focus mostly on iowa:

primary <- read.csv("PrimaryResults.csv", stringsAsFactors = FALSE)

iowa.counties <- counties(state = "IA") #spatial data using tigris

bernie.ia <- filter(primary, State == "Iowa", Party == "Democrat",
                    Candidate == "Bernie Sanders")

merge <- geo_join(iowa.counties, bernie.ia, "NAME", "County")

#map of percentage voting for bernie
pal <- colorNumeric(c("Blues"), domain = NULL)
popup = paste0(merge$County, " County", "<br>", 
"Percentage voting for Bernie: ", 100*merge$FractionVotes, "%")

leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = merge, fillOpacity = .6,
                color = "white", weight = 1,
                smoothFactor = .2,
                fillColor = ~pal(FractionVotes),
                popup = popup) %>% 
    addLegend(pal = pal,values = merge$FractionVotes*100,
              title = "Percentage Voting <br> for Bernie")


#more interesting stuff:
#general results
state = "Iowa"
party = "Republican"

Votes <- primary %>% 
    filter( State == state, Party == party) %>% 
    group_by(County) %>% 
    summarize(Candidate = Candidate[which.max(FractionVotes)],
              Vote = max(FractionVotes))
head(Votes)

Counties <- counties(state = state) #gives the geospatial files

Merge <- geo_join(Counties, Votes, "NAME", "County")

pal = colorFactor(c("dark red","dark green", "blue"), domain = Merge$Candidate)
popup = paste0(Merge$County, " County")

leaflet() %>% 
    addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(data = Merge, weight = 1, color = "white",
                smoothFactor = 0.2, popup = popup,
                fillColor = ~pal(Merge$Candidate), fillOpacity = .7) %>% 
    addLegend(pal = pal, value = Merge$Candidate,
              title = "Winner by County")

###data munging:

geo <- geo.make(state = state, county = "*")
income <- acs.fetch(table.name = "Per Capita Income", col.names = "pretty",
                    geography = geo)

#income@acs.colnames use this to see the colnames
income.df <- data_frame(County = income@geography$NAME,
                        Income = income@estimate[,c("Per Capita Income:  Per capita income in the past 12 months (in 2011 inflation-adjusted dollars) ")]
) #use the dplyr data_frame version to keep the strings

income.df$County <- sapply(income.df$County, function(x) 
{strsplit(x, " County", fixed = TRUE)[[1]][1]})

Votes <- inner_join(Votes, income.df, by = "County")


education <- read.csv("iowa_edu.csv", stringsAsFactors = FALSE)

College <- data_frame(County = education$GEO.display.label, 
                      College = education$HC01_EST_VC05)

College <- College[-1,]

College$County <- sapply(College$County, function(x) 
{strsplit(x, " County", fixed = TRUE)[[1]][1]})

Votes <- inner_join(Votes, College, by = "County")

#-----
temp <- read.csv("rural.iowa.csv", stringsAsFactors = FALSE)
temp <- temp[1:99, 1:3]

temp$Urban <- lapply(temp$Urban, function(x){gsub(",","", x, fixed = TRUE)})
temp$Total <- lapply(temp$Total, function(x){gsub(",","", x, fixed = TRUE)})
temp$Total <- as.numeric(temp$Total)
temp$Urban <- as.numeric(temp$Urban)
temp$Urban <- temp$Urban/temp$Total*100

Urban <- data_frame(County = temp$County, Urban = temp$Urban)

Votes <- inner_join(Votes, Urban, by = "County")
Votes$College <- as.numeric(Votes$College)

head(Votes)


Avg <- Votes %>% 
    group_by(Candidate) %>%
    summarize(mean(Income), mean(Urban),mean (College)) 

#Candidate: 

candidate = "Ted Cruz"

rubio <- primary %>%
            filter(Candidate == candidate, State == "Iowa")

rubio <- inner_join(Votes[,-c(2,3)], rubio, by = "County")

rubio$Income <- rubio$Income/1000
rubio$FractionVotes <- rubio$FractionVotes *100 #normalize a bit


g1 <- qplot(x = Income, y = FractionVotes, data = rubio) +
        geom_smooth(method='lm',formula=y~x)

g2 <- qplot(x = Urban, y = FractionVotes, data = rubio) +
        geom_smooth(method='lm',formula=y~x)

g3 <- qplot(x = College, y = FractionVotes, data = rubio) +
    geom_smooth(method='lm',formula=y~x)

grid.arrange(g1, g2, g3, nrow =1, ncol = 3)

summary(lm(FractionVotes ~ Urban + Income + College, data = rubio))
# so a 

predict(lm(FractionVotes ~ Urban + Income + College, data = rubio),
        newdata=list(hydro=1.0), interval = "prediction")

