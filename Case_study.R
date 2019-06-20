##Dependencies
## This are the libraries that we will use in this projec
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)


## We are loading the dataset from the beers and the dataset from breweries in the chunk code below.
##The beers dataset is bears, the breweries dataset is brew.

url<-"https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Beers.csv"
beers <- read.csv(url(url), sep = ",", header = TRUE)
head(beers) 

url<-"https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Breweries.csv"
brew <- read.csv(url(url), sep = ",", header = TRUE)
head(brew) 





## QUESTION 1: BREWERIES PER STATE

## In the following code we count how many breweries are pers state in the US territory.NumBreweries describe the 
## amount of breweries per state in the US territory.Then we ordered in descending order. Colorado and California
## are the states with the highest amount of breweries in their state.

brew$State<- as.character(brew$State)
brew$Brew_ID<- as.integer(brew$Brew_ID)
brew$Name<-  as.character(brew$Name)
brew %>% group_by(State) %>% summarize(numBreweries = n_distinct(Name)) -> numBreweries

numBreweries
numBreweries <- arrange(numBreweries, desc(numBreweries))
numBreweries


# creating chart for breweries
numBreweries %>% ggplot(aes(x = State, y = numBreweries, fill = State)) + geom_col() + labs(title = "Breweries Per State") + theme( axis.text = element_text(size = rel(0.4)) )





## QUESTION 2: MERGED DATA SET

## In the following code chunk we merge beer data with the breweries data. We print the first 6 observations and 
## the last six observations to check the merged file in order to explore the dataset. Combined is the combined dataset.

beers$Brew_ID<-beers$Brewery_id
beers$Brewery_id<-NULL
head(beers)
head(brew)
combined<- merge(brew, beers,by = "Brew_ID")

# renaming Name.x to Brewery_Name
colnames(combined)[2] <- "Brewery_Name"
# renaming Name.y to Beer_Name
colnames(combined)[5] <- "Beer_Name"

head(combined, 6)
tail(combined, 6)




## QUESTION 3: MISSING DATA

## In the following chunk code we report the number of NA's in each column of the combined dataset. NA_count is the amount 
## of NA in each column in the combined dataset.

na_count <-sapply(combined, function(y) sum(length(which(is.na(y)))))
na_count

   
   

## QUESTION 4: MEDIAN ALCOHOL CONTENT

## We compute the median alcohol content and international bitterness unit in a beer for each state from the combined dataset.
## in order to compute the alcohol content and international bitterness unit in a beer for each state from the combined dataset
## We grouped the combined dataset by State, then we sumarized by alcohol content and international bitterness unit per beer using
## the median function. We decided to get rid of the NA values, we used the pipe function from the dplyr library to design this code.
## The final product of the function is plot1. 
## We Plot a bar chart to compare the median alcohol content and international bitterness unit per beer for each state 
## from the combined dataset. We used the ggplot library to perform the plot.

combined %>% group_by(State) %>% summarize(Alcohol_content = median(ABV, na.rm = TRUE), Bitterness= median(IBU, na.rm = TRUE)) ->plot1
plot1

# summarize(group_by(combined, State), AlcoholContent = median(ABV, na.rm = T), Bitterness = median(IBU, na.rm = T) ) -> altPlot1

ggplot(data=plot1, aes(x= State, y=Alcohol_content, fill= Bitterness)) + geom_bar(stat="identity") + ggtitle("Bitterness per Alcohol Content") + labs(x= "States",  y= "Alcohol Content") + theme(axis.text.x = element_text(size =4 , angle =45, hjust = 1, vjust = 1)) 

                  
#p1= ggplot(data=plot1, aes(x= State, y=Alcohol_content, fill= Bitterness)) 
#p2= p1 + geom_bar(stat="identity")
#p3= p2 + ggtitle("Bitterness per Alcohol Content") + labs(x= "States",  y= "Alcohol Content")
#p4= p3 + theme(axis.text.x = element_text(size =4 , angle =45, hjust = 1, vjust = 1)) 
#p4 




## QUESTION 5: MAX IBU & ABV STATES

## The following code show the state with the maximum alcoholic (ABV) beer, and the state with 
## the most bitter (IBU) beer.The maximum alcoholic (ABV) beer, and bitter (IBU) beer is represented by maxal.



#### NOTE THIS JUST GIVES MAX VALUES FOR EACH COLUMN
maxal<- sapply(plot1, max, na.rm = TRUE)
maxal
 
# Highest Alcohol Content
head(beers[order(beers$ABV, decreasing = T, na.last = T),],1)

# Highest Bitterness
head(beers[order(beers$IBU, decreasing = T, na.last = T),],1)



## QUESTION 6: SUMMARY STATS FOR ABV

##Summary statistics for the ABV variable from the beers dataset.

summary(beers$ABV)
 



## QUESTION 7: ABV & IBU RELATIONSHIP
##### MUCH EASIER WAY TO DO THIS ####

# Scatterplot btween Alcohol & Bitterness
plot(combined$ABV, combined$IBU)

# tests R correlation between ABV & IBU ignoring NA entries
cor.test(combined$ABV, combined$IBU, na.action(na.omit("NA")))

#predicting bitterness based on alcohol content
beer.lm <- lm(combined$IBU ~ combined$ABV, na.action(na.omit("NA")))
summary(beer.lm)

                  
## In the following code using ggplot we draw a scatter plot between the bitterness and alcohol content.
## It look like there is a trend toward a llinear relatioship between the 2 mentioned variable, p3 is the plot.
p1= ggplot(dat= plot1, aes(x= Alcohol_content, y= Bitterness))
p2= p1 +geom_point(shape=1) + geom_smooth(method = "lm") 
p3= p2 + ggtitle("Bitterness per Alcohol Content")  
p3
## In the following code we found frequest beers style and name in the US territory.
beers %>% count(Style) %>% arrange(desc(n)) -> BeersStyle
BeersStyle
beers %>% count(Name) %>% arrange(desc(n)) -> BeersName
BeersName


##Just trying ( this code now does not work though)
## In the following code we find the most frequent beers style and name by state in the Us territory. NameState is the most 
## frequent beer name by state in the US territory.NameStyle is the most frequent beer name per state in the  US territory. 

NameStyle<- dcast(combined, State + Name.x ~Style, value.var= sum)

combined$Name.x<- as.vector(combined$Name)
combined$Style<- as.vector(combined$Style)
combined %>% group_by(State, Style) %>% dplyr::summarize(Stile = n_distinct(Style),
                                                         Name = n_distinct(Name.x)) %>% arrange(desc(Name)) -> NS
NS

# Starting with something like this?
# also because State is a factor not a string, maybe that is an issue?
# I know that count() and tally() need it to be a char
spread(combined, State, Style)
spread(combined, Style, State)
                  
