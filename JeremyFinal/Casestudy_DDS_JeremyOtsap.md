---
title: "Case Study"
author: "Jeremy Otsap"
date: "June 26, 2019"
output: 
  html_document:
    keep_md: true
---



## Data Sets

Here we are loading the beers and breweries data sets directly from their github repositories


```r
url<-"https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Beers.csv"
beers <- read.csv(url(url), sep = ",", header = TRUE)
head(beers) 
```

```
##                  Name Beer_ID   ABV IBU Brewery_id
## 1            Pub Beer    1436 0.050  NA        409
## 2         Devil's Cup    2265 0.066  NA        178
## 3 Rise of the Phoenix    2264 0.071  NA        178
## 4            Sinister    2263 0.090  NA        178
## 5       Sex and Candy    2262 0.075  NA        178
## 6        Black Exodus    2261 0.077  NA        178
##                            Style Ounces
## 1            American Pale Lager     12
## 2        American Pale Ale (APA)     12
## 3                   American IPA     12
## 4 American Double / Imperial IPA     12
## 5                   American IPA     12
## 6                  Oatmeal Stout     12
```

```r
url<-"https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Breweries.csv"
brew <- read.csv(url(url), sep = ",", header = TRUE)
head(brew) 
```

```
##   Brew_ID                      Name          City State
## 1       1        NorthGate Brewing    Minneapolis    MN
## 2       2 Against the Grain Brewery    Louisville    KY
## 3       3  Jack's Abby Craft Lagers    Framingham    MA
## 4       4 Mike Hess Brewing Company     San Diego    CA
## 5       5   Fort Point Beer Company San Francisco    CA
## 6       6     COAST Brewing Company    Charleston    SC
```


### Beers.csv:
Name: Name of the beer.
Beer_ID: Unique identifier of the beer.
ABV: Alcohol by volume of the beer.
IBU: International Bitterness Units of the beer.
Brewery_ID: Brewery id associated with the beer.
Style: Style of the beer.
Ounces: Ounces of beer.

### Breweries.csv:
Brew_ID: Unique identifier of the brewery.
Name: Name of the brewery.
City: City where the brewery is located.
State: U.S. State where the brewery is located.


## INTRODUCTION

We are looking at data for popular brands of beers, as well as a collection of breweries throughout the US. Given these datasets we are looking to gain a better understanding of drinking preferences as well as brewery demographics.


## QUESTION 1: BREWERIES PER STATE

In the following code we count the number of breweries per state. We assign this information into the numBreweries dataframe and then order the data in descending order by the nunber of breweries. Colorado and California have the highest number of breweries. DC, North Dakota, South Dakota, and West Virginia are the lowest with only one brewery for the entire state. 



```r
# changing from factor to char
brew$State <- as.character(brew$State)
# changing to int
brew$Brew_ID <- as.integer(brew$Brew_ID)
# changing to char
brew$Name <-  as.character(brew$Name)

# removing trailing whitespace with str_trim
str_trim(brew$State, side = "left") -> brew$State
brew %>% group_by(State) %>% summarize(numBreweries = n_distinct(Name)) -> numBreweries
numBreweries <- arrange(numBreweries, desc(numBreweries))
tail(numBreweries)
```

```
## # A tibble: 6 x 2
##   State numBreweries
##   <chr>        <int>
## 1 MS               2
## 2 NV               2
## 3 DC               1
## 4 ND               1
## 5 SD               1
## 6 WV               1
```


Number of breweries by state


```r
# creating chart for breweries
numBreweries %>% ggplot(aes(x = State, y = numBreweries, fill = State)) + geom_col() + labs(title = "Breweries Per State") + theme( axis.text = element_text(size = rel(0.4)) )
```

![](Casestudy_DDS_JeremyOtsap_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## QUESTION 2: MERGED DATA SET

In the following code we merge beer data with the breweries data using the Brewery's Unique ID. To validate we successfuly combined these two data sets, We print the first eix and the last six observations. This will now allow us to further explore the dataset and examine potential correlations between the specific qualities of the beer and the demographics of the breweries which created them.



```r
combined <- merge(brew, beers, by.x = "Brew_ID", by.y = "Brewery_id")

# renaming Name.x to Brewery_Name
colnames(combined)[2] <- "Brewery_Name"
# renaming Name.y to Beer_Name
colnames(combined)[5] <- "Beer_Name"

head(combined, 6)
```

```
##   Brew_ID       Brewery_Name        City State     Beer_Name Beer_ID   ABV
## 1       1 NorthGate Brewing  Minneapolis    MN       Pumpion    2689 0.060
## 2       1 NorthGate Brewing  Minneapolis    MN    Stronghold    2688 0.060
## 3       1 NorthGate Brewing  Minneapolis    MN   Parapet ESB    2687 0.056
## 4       1 NorthGate Brewing  Minneapolis    MN  Get Together    2692 0.045
## 5       1 NorthGate Brewing  Minneapolis    MN Maggie's Leap    2691 0.049
## 6       1 NorthGate Brewing  Minneapolis    MN    Wall's End    2690 0.048
##   IBU                               Style Ounces
## 1  38                         Pumpkin Ale     16
## 2  25                     American Porter     16
## 3  47 Extra Special / Strong Bitter (ESB)     16
## 4  50                        American IPA     16
## 5  26                  Milk / Sweet Stout     16
## 6  19                   English Brown Ale     16
```

```r
tail(combined, 6)
```

```
##      Brew_ID                  Brewery_Name          City State
## 2405     556         Ukiah Brewing Company         Ukiah    CA
## 2406     557       Butternuts Beer and Ale Garrattsville    NY
## 2407     557       Butternuts Beer and Ale Garrattsville    NY
## 2408     557       Butternuts Beer and Ale Garrattsville    NY
## 2409     557       Butternuts Beer and Ale Garrattsville    NY
## 2410     558 Sleeping Lady Brewing Company     Anchorage    AK
##                      Beer_Name Beer_ID   ABV IBU                   Style
## 2405             Pilsner Ukiah      98 0.055  NA         German Pilsener
## 2406         Porkslap Pale Ale      49 0.043  NA American Pale Ale (APA)
## 2407           Snapperhead IPA      51 0.068  NA            American IPA
## 2408         Moo Thunder Stout      50 0.049  NA      Milk / Sweet Stout
## 2409  Heinnieweisse Weissebier      52 0.049  NA              Hefeweizen
## 2410 Urban Wilderness Pale Ale      30 0.049  NA        English Pale Ale
##      Ounces
## 2405     12
## 2406     12
## 2407     12
## 2408     12
## 2409     12
## 2410     12
```


## QUESTION 3: MISSING DATA

In the following chunk code we report the number of missing values (listed as "NA") for each parameter. NA_count is the amount of NA in each column in the combined dataset. while only there are only 62 missing value in ABV (less than 3%) there were 1005 missing values in IBU-- over 40%. Therefore as we proceed we recommend caution on any graphs or analysis made involving the bitterness attribute


```r
na_count <-sapply(combined, function(y) sum(length(which(is.na(y)))))
na_count
```

```
##      Brew_ID Brewery_Name         City        State    Beer_Name 
##            0            0            0            0            0 
##      Beer_ID          ABV          IBU        Style       Ounces 
##            0           62         1005            0            0
```



## QUESTION 4: MEDIAN ALCOHOL CONTENT

We compute the median for both alcohol content [ABV] and bitterness [IBU] for each state. In order to do this we first grouped the combined dataset by State, then sumarized that grouped data calculating the median for ABV and IBU. NA values were ignored and the resulting data was put into a dataframe called "plot1." We then created the chart below to visualize this data where the height represents the alcohol content and the darkness of color represents the bitterness


```r
# data first grouped by state
# then piped to summarize function which aggregates on median() function
# note that NA values were ignored
combined %>% group_by(State) %>% summarize(Alcohol_content = median(ABV, na.rm = TRUE), Bitterness= median(IBU, na.rm = TRUE)) -> plot1
plot1
```

```
## # A tibble: 51 x 3
##    State Alcohol_content Bitterness
##    <chr>           <dbl>      <dbl>
##  1 AK             0.056        46  
##  2 AL             0.06         43  
##  3 AR             0.052        39  
##  4 AZ             0.055        20.5
##  5 CA             0.058        42  
##  6 CO             0.0605       40  
##  7 CT             0.06         29  
##  8 DC             0.0625       47.5
##  9 DE             0.055        52  
## 10 FL             0.057        55  
## # ... with 41 more rows
```

```r
ggplot(plot1, aes(x=reorder(State,-Alcohol_content),y=Alcohol_content, fill=Bitterness)) + geom_bar(stat="identity") + coord_flip(ylim=c(.04,.0650)) + theme(legend.position='none') + scale_fill_gradient2(midpoint=median(plot1$Alcohol_content), space='Lab') + theme_classic()+labs(x="State", y="ABV", legend="ABV")+ ggtitle("Median ABV by state") + theme(plot.title = element_text(hjust = 0.5),axis.text=element_text(size=5)) 
```

![](Casestudy_DDS_JeremyOtsap_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


Additionally, we also created tables to quickly show top 5 and bottom 5 for ABV and IBU


The top 5 states for median alcohol content


```r
### CORRECT DATA ###
head(plot1[order(plot1$Alcohol_content, decreasing = T, na.last = T),],5) 
```

```
## # A tibble: 5 x 3
##   State Alcohol_content Bitterness
##   <chr>           <dbl>      <dbl>
## 1 DC             0.0625       47.5
## 2 KY             0.0625       31.5
## 3 MI             0.062        35  
## 4 NM             0.062        51  
## 5 WV             0.062        57.5
```


The top 5 states for bitterness


```r
### CORRECT DATA ###
head(plot1[order(plot1$Bitterness, decreasing = T, na.last = T),],5) 
```

```
## # A tibble: 5 x 3
##   State Alcohol_content Bitterness
##   <chr>           <dbl>      <dbl>
## 1 ME              0.051       61  
## 2 WV              0.062       57.5
## 3 FL              0.057       55  
## 4 GA              0.055       55  
## 5 DE              0.055       52
```


The bottom 5 states for median alcohol content


```r
### CORRECT DATA ###
head(plot1[order(plot1$Alcohol_content, decreasing = F, na.last = T),],5) 
```

```
## # A tibble: 5 x 3
##   State Alcohol_content Bitterness
##   <chr>           <dbl>      <dbl>
## 1 UT              0.04        34  
## 2 NJ              0.046       34.5
## 3 KS              0.05        20  
## 4 ND              0.05        32  
## 5 WY              0.05        21
```


The bottom 5 states for bitterness


```r
### CORRECT DATA ###
head(plot1[order(plot1$Bitterness, decreasing = F, na.last = T),],5) 
```

```
## # A tibble: 5 x 3
##   State Alcohol_content Bitterness
##   <chr>           <dbl>      <dbl>
## 1 WI              0.052       19  
## 2 KS              0.05        20  
## 3 AZ              0.055       20.5
## 4 WY              0.05        21  
## 5 HI              0.054       22.5
```



## QUESTION 5: MAX IBU & ABV STATES

The following code show the beer with the highest raw [NOT median] alcohol content and bitterness. Additionally shows its various attributes such as which brewery distills it, what state that brewery is in, and the style of beer. 


Individual Beer with Highest Alcohol Content**  
_**Note that bitterness information is unavailable_


```r
head(beers[order(beers$ABV, decreasing = T, na.last = T),],1)
```

```
##                                                      Name Beer_ID   ABV
## 2279 Lee Hill Series Vol. 5 - Belgian Style Quadrupel Ale    2565 0.128
##      IBU Brewery_id            Style Ounces
## 2279  NA         52 Quadrupel (Quad)   19.2
```


Individual Beer with Highest Bitterness


```r
head(beers[order(beers$IBU, decreasing = T, na.last = T),],1)
```

```
##                          Name Beer_ID   ABV IBU Brewery_id
## 148 Bitter Bitch Imperial IPA     980 0.082 138        375
##                              Style Ounces
## 148 American Double / Imperial IPA     12
```




## QUESTION 6: SUMMARY STATS FOR ABV

Below we show the distribution of alcohol content across all beers in our data set. Again the NA values need to be removed from our data. Additionally we represent this visually with a boxplot.



```r
#find NA
which( !is.na(beers$ABV) ) -> beers_na

#assign santized data to new variable
beers$ABV[beers_na] -> beers_abv

#summerize & plot sanitized data
summary(beers_abv)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.00100 0.05000 0.05600 0.05977 0.06700 0.12800
```

```r
boxplot(beers_abv, main=toupper("Alcohol Content in Beers"), font.main=3, cex.main=1.2, xlab="Beers", ylab="Alcohol Content", font.lab=3, col="blue")
```

![](Casestudy_DDS_JeremyOtsap_files/figure-html/unnamed-chunk-13-1.png)<!-- -->



## QUESTION 7: ABV & IBU RELATIONSHIP

Here we examine to see if there is evidence of a correlation between a beer's alcohol content and its bitterness. We do a correlation test which shows there is evidence of a moderate correlation of 67.06% between them [p-value < 0.0001]. We also plot this and calculate the regression line, with an estimated R-squared is 44.97%, which means almost 45% of a beer's bitterness can be accounted for by its alcohol content. As with prior analysis the NA values will be ignored



```r
# tests R correlation between ABV & IBU ignoring NA entries
cor.test(combined$ABV, combined$IBU, na.action(na.omit("NA")))
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  combined$ABV and combined$IBU
## t = 33.863, df = 1403, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.6407982 0.6984238
## sample estimates:
##       cor 
## 0.6706215
```

```r
#predicting bitterness based on alcohol content
beer.lm <- lm(combined$IBU ~ combined$ABV, na.action(na.omit("NA")))
summary(beer.lm)
```

```
## 
## Call:
## lm(formula = combined$IBU ~ combined$ABV, data = na.action(na.omit("NA")))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -78.849 -11.977  -0.721  13.997  93.458 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   -34.099      2.326  -14.66   <2e-16 ***
## combined$ABV 1282.037     37.860   33.86   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.26 on 1403 degrees of freedom
##   (1005 observations deleted due to missingness)
## Multiple R-squared:  0.4497,	Adjusted R-squared:  0.4493 
## F-statistic:  1147 on 1 and 1403 DF,  p-value: < 2.2e-16
```


Scatterplot and regression line between alcohol & bitterness


```r
plot(combined$ABV, combined$IBU, xlab = "Alcohol Content", ylab = "Bitterness")
abline( lm(combined$IBU ~ combined$ABV)  , col = "red")
```

![](Casestudy_DDS_JeremyOtsap_files/figure-html/unnamed-chunk-15-1.png)<!-- -->




Additionally we looked at the most popular style of beer across the US which turned out to be American IPA; having close to twice as many different individual beers as the second most popular.



```r
beers %>% count(Style) %>% arrange(desc(n)) -> beers_style
head(beers_style[order(beers_style$n, decreasing = T, na.last = T),],5)
```

```
## # A tibble: 5 x 2
##   Style                              n
##   <fct>                          <int>
## 1 American IPA                     424
## 2 American Pale Ale (APA)          245
## 3 American Amber / Red Ale         133
## 4 American Blonde Ale              108
## 5 American Double / Imperial IPA   105
```



## CONCLUSION

Given the combination of factors, my recommendation is for West Virginia. Currently there is only 1 brewery for the entire state, and of the bottom four states which all had only 1 brewery, West Virginia has the highest population.  Nationwide the average ratio of brewery to people was 1-2 breweries per million residents. West Virginia was below this at around 0.553. Furthermore this brewery does not make an American IPA, the most popular style.

As shown by the prior analysis, it was in the top for both highest median ABV and highest median IBU. Thus this may be a good opportunity to have a native brewery making a less bitter American IPA. 

There are also additional factors: West Virginia has one of the lowest economies of any US state. This means high unemployment and very inexpensive real-estate. Its fairly reasonable to anticipate a high level of cooperation from the local government given their need as well as recent appeals for more industry. On a final note they also have large natural gas reserves, which could be an added bonus to the company looking for a more eco-friendly energy source.

