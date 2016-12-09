library(WDI)
library(stringr)
library(dplyr)

### Country-level variables

#NOOOO
# We start by reading from disk the 'countryvariables' data frame. It already
# contains, from when I first created it, the columns for indicators for gdp per
# capita, gini, and governance (and some of the data for them, some of it added
# by hand because it was not readily accessible), but we need to add those for
# political stability and polarization
# NOOOO

# Let's first load the wvsAll data set without the country variables, which we
# will use to create our countryvariables data frame by extracting the all the
# country-year combinations

load("wvsAll-no.country.vars.Rda")

countryvariables <- unique(wvsAll[,c('country', 'year')])
rownames(countryvariables) <- NULL # reset rownames to consecutive numbers
countryvariables <- arrange(countryvariables, country) # sort by country name

# Add variable columns for all the indicators we want to include, as well as the
# year of the data (we can't always find the data for the exact year of the
# survey), and the source of the data. The variables are GDP, governance, 
# gini coefficient, and political stability

countryvariables$gdp <- NA
countryvariables$gdpyear <- NA
countryvariables$gdpsource <- ""

countryvariables$governance <- NA
countryvariables$govyear <- NA
countryvariables$govsource <- ""

countryvariables$gini <- NA
countryvariables$giniyear <- NA
countryvariables$ginisource <- ""

countryvariables$polstab <- NA
countryvariables$polstabyear <- NA
countryvariables$polstabsource <- ""

countryvariables$mleftright <- NA
countryvariables$mleft <- NA
countryvariables$mright <- NA
countryvariables$lryear <- NA
countryvariables$lrsource <- ""


### GDP per capita, from the World Bank's World Development Indicators (WDI)

gdp = WDI(indicator = 'NY.GDP.PCAP.PP.KD', start = 2005, end = 2015)

# Let's rename some countries so that we can match the downloaded GDP data to
# the name of the countries in our data set

gdp$country[gdp$country == "Russian Federation"] <- "Russia"
gdp$country[gdp$country == "Korea, Rep."] <- "South Korea"
gdp$country[gdp$country == "Iran, Islamic Rep."] <- "Iran"
gdp$country[gdp$country == "Egypt, Arab Rep."] <- "Egypt"
gdp$country[gdp$country == "Hong Kong SAR, China"] <- "Hong Kong"
gdp$country[gdp$country == "Kyrgyz Republic"] <- "Kyrgyzstan"
gdp$country[gdp$country == "Yemen, Rep."] <- "Yemen"

# Let's add the GDP per capita information to countryvariables

nodatagdp <- vector(mode = "character", 0) # For not-found countries

for(row in 1:nrow(countryvariables)) {
  country <- countryvariables[row, 1]
  year <- countryvariables[row, 2]
  if(sum(gdp$country %in% country) == 0) {
    nodatagdp <- c(nodatagdp, levels(country)[country])
    next
  }
  if(is.na(countryvariables[row, 3])) {
    if(!is.na(gdp$NY.GDP.PCAP.PP.KD[gdp$country == country & gdp$year == year])) {
      countryvariables[row, 3] <- gdp$NY.GDP.PCAP.PP.KD[gdp$country == country & gdp$year == year]
      countryvariables[row, 4] <- year
      countryvariables[row, 5] <- "WDI"
    } else {
      values <- gdp$NY.GDP.PCAP.PP.KD[gdp$country %in% country]
      years <- gdp$year[gdp$country %in% country]
      nas <- !is.na(values)
      if(sum(nas) == 0) {
        countryvariables[row, 3] <- NA
        countryvariables[row, 4] <- NA
      } else {
        values <- values[nas]
        years <- years[nas]
        dist <- abs(years - year)
        countryvariables[row, 3] <- values[which.min(dist)]
        countryvariables[row, 4] <- years[which.min(dist)]
        countryvariables[row, 5] <- "WDI" 
      }
    }
  }
}

# Countries not available in the WDIs for GDP:

nodatagdp # Palestine and Taiwan (for both waves)

# Countries without GDP data:

countryvariables$country[is.na(countryvariables$gdp)]

# Andorra, Palestine and Taiwan (for both waves)


### GINI coefficient, also from the World Bank's WDI

gini = WDI(indicator = 'SI.POV.GINI', start = 1990, end = 2015)

gini$country[gini$country == "Russian Federation"] <- "Russia"
gini$country[gini$country == "Korea, Rep."] <- "South Korea"
gini$country[gini$country == "Iran, Islamic Rep."] <- "Iran"
gini$country[gini$country == "Egypt, Arab Rep."] <- "Egypt"
gini$country[gini$country == "Hong Kong SAR, China"] <- "Hong Kong"
gini$country[gini$country == "Kyrgyz Republic"] <- "Kyrgyzstan"
gini$country[gini$country == "Yemen, Rep."] <- "Yemen"

# Here we add the GINI information to countryvariables

nodatagini <- vector(mode = "character", 0)

for(row in 1:nrow(countryvariables)) {
  country <- countryvariables[row, 1]
  year <- countryvariables[row, 2]
  if(sum(gini$country %in% country) == 0) {
    nodatagini <- c(nodatagini, levels(country)[country])
    next
  }
  if(is.na(countryvariables[row, 9])) {
    if(!is.na(gini$SI.POV.GINI[gini$country == country & gini$year == year])) {
      countryvariables[row, 9] <- (gini$SI.POV.GINI[gini$country == country & gini$year == year])/100
      countryvariables[row, 10] <- year
      countryvariables[row, 11] <- "WDI"
    } else {
      values <- gini$SI.POV.GINI[gini$country %in% country]
      years <- gini$year[gini$country %in% country]
      nas <- !is.na(values)
      if(sum(nas) == 0) {
        countryvariables[row, 9] <- NA
        countryvariables[row, 10] <- NA
      } else {
        values <- values[nas]
        years <- years[nas]
        dist <- abs(years - year)
        countryvariables[row, 9] <- (values[which.min(dist)])/100
        countryvariables[row, 10] <- years[which.min(dist)]
        countryvariables[row, 11] <- "WDI" 
      }
    }
  }
}

# Countries not available in the WDIs for GINI:

nodatagini # Palestine and Taiwan (for both waves)

# Countries without GINI data:

countryvariables$country[is.na(countryvariables$gini)]

# Algeria, Andorra, Bahrain, Egypt, Hong Kong, Iraq, Jordan, Lebanon, Libya,
# Palestine, South Korea (for both waves), Taiwan (for both waves), and Yemen


## GOVERNANCE: Now we prepare the data on government effectiveness, which comes
# from the Worldwide Governance Indicators.
# http://info.worldbank.org/governance/wgi/index.aspx#home

gov <- read.csv("governance.csv", na.strings = "#N/A")

gov$country <- str_to_title(gov$country)

gov$country[gov$country == "Russian Federation"] <- "Russia"
gov$country[gov$country == "Korea, Rep."] <- "South Korea"
gov$country[gov$country == "Iran, Islamic Rep."] <- "Iran"
gov$country[gov$country == "Egypt, Arab Rep."] <- "Egypt"
gov$country[gov$country == "Hong Kong Sar, China"] <- "Hong Kong"
gov$country[gov$country == "Kyrgyz Republic"] <- "Kyrgyzstan"
gov$country[gov$country == "Taiwan, China"] <- "Taiwan"
gov$country[gov$country == "Trinidad And Tobago"] <- "Trinidad and Tobago"
gov$country[gov$country == "Yemen, Rep."] <- "Yemen"

# Now we add the government effectiveness information to countryvariables

nodatagov <- vector(mode = "character", 0)

for(row in 1:nrow(countryvariables)) {
  country <- countryvariables[row, 1]
  year <- countryvariables[row, 2]
  if(sum(gov$country %in% country) == 0) {
    nodatagov <- c(nodatagov, levels(country)[country])
    next
  }
  cols <- which(gov[1,] == year)
  if(is.na(countryvariables[row, 6])) {
    if(!is.na(gov[gov$country == levels(country)[country], cols[4]])) {
      countryvariables[row, 6] <- gov[gov$country == levels(country)[country], cols[4]]
      countryvariables[row, 7] <- year
      countryvariables[row, 8] <- "WGI"
    } else {
      countryvariables[row, 6] <- NA
      countryvariables[row, 7] <- NA
    }
  }
}

# Countries not available in the WGIs for governance:

nodatagov # Palestine

# Countries without governance data:

countryvariables$country[is.na(countryvariables$governance)] # Palestine


## POLITICAL STABILITY: Now we prepare the data on political stability, also
# from the Worldwide Governance Indicators.

polstab <- read.csv("politicalstability.csv", na.strings = "#N/A")

polstab$country <- str_to_title(polstab$country) # Change case to title

polstab$country[polstab$country == "Russian Federation"] <- "Russia"
polstab$country[polstab$country == "Korea, Rep."] <- "South Korea"
polstab$country[polstab$country == "Iran, Islamic Rep."] <- "Iran"
polstab$country[polstab$country == "Egypt, Arab Rep."] <- "Egypt"
polstab$country[polstab$country == "Hong Kong Sar, China"] <- "Hong Kong"
polstab$country[polstab$country == "Kyrgyz Republic"] <- "Kyrgyzstan"
polstab$country[polstab$country == "Taiwan, China"] <- "Taiwan"
polstab$country[polstab$country == "Trinidad And Tobago"] <- "Trinidad and Tobago"
polstab$country[polstab$country == "Yemen, Rep."] <- "Yemen"

# Now we add the political stability information to countryvariables

nodatapol <- vector(mode = "character", 0)

for(row in 1:nrow(countryvariables)) {
  country <- countryvariables[row, 1]
  year <- countryvariables[row, 2]
  if(sum(polstab$country %in% country) == 0) {
    nodatapol <- c(nodatapol, levels(country)[country])
    next
  }
  cols <- which(polstab[1,] == year)
  if(is.na(countryvariables[row, 12])) {
    if(!is.na(polstab[polstab$country == levels(country)[country], cols[4]])) {
      countryvariables[row, 12] <- polstab[polstab$country == levels(country)[country], cols[4]]
      countryvariables[row, 13] <- year
      countryvariables[row, 14] <- "WGI"
    } else {
      countryvariables[row, 12] <- NA
      countryvariables[row, 13] <- NA
    }
  }
}

# Countries not available in the WDIs for POLITICAL STABILITY:

nodatapol # Palestine

# Countries without POLITICAL STABILITY data:

countryvariables$country[is.na(countryvariables$polstab)] # Palestine



## POLITICAL POLARIZATION: Now we get to the data on political polarization
# This comes from the Manifesto Project, which looks at the party programs of
# political parties to figure out their ideological position. Basically, here
# I take the ideological score (-100 -left- to 100 - right) of the parties'
# program for a given election year, weigh it by its popular support, and find
# the mean for that election (mleftright variable). Then I take that mean value and
# subtract it from each party's value, square them, add them together, and do the square root.
# Then I find the mean of that value for each party, which gives me the average
# polarization, how much distance the parties are from each other (or the 
# mean) for that election.
# https://manifestoproject.wzb.eu/

pol <- read.csv("MPDataset_MPDS2016a.csv", stringsAsFactors = FALSE)
pol$edate <- as.Date(pol$edate, format = "%d/%m/%Y")

# Remove all data from before 2005. We go from 3924 entries to 734

pol <- pol[pol$edate >= "2005-01-01",]

# Select only the variables we need for the analysis. From 169 to 5

pol <- select(pol, countryname, edate, party, pervote, rile)

# We now do the same for the South America dataset, which is a separate file

polSA <- read.csv("MPDataset_MPDSSA2015a.csv", stringsAsFactors = FALSE)
polSA$edate <- as.Date(polSA$edate, format = "%d/%m/%Y")
polSA <- polSA[polSA$edate >= "2005-01-01",]
polSA <- select(polSA, countryname, edate, party, pervote, rile)

# We merge both datasets into one
pol <- rbind(pol, polSA)
pol <- arrange(pol, countryname, edate) # Sort by country and election
rm(polSA) # Remove the South America dataset

# Let's see if there's any missing data in the dataset, since that could affect
# the size of the final dataset that we will use for our analysis

pol[is.na(pol$pervote),]
pol[is.na(pol$rile),]

# There are 7 cases in which the percentage of the vote is missing (Brazil and
# Hungary) and 12 in which party ideology is missing. It makes most sense to
# delete these entries

pol <- pol[complete.cases(pol),]

# Before we move on, let's check to see if the percentage of votes in each
# election is close to 100%. If not, that could be problematic, since the
# ideological score for the country at that point in time would be calculated
# missing a lot of the votes, and therefore might be very inaccurate/biased

percVotes <- pol %>% group_by(countryname, edate) %>% dplyr::summarise(totalPerc = sum(pervote))

# Let's single out the cases in which the total percentage of the vote accounted
# for in the dataset for each election is lower than 75%

problemElections <- percVotes[percVotes$totalPerc < 75,]

# There are 3 cases very close to 75% (Croatia 2007, Macedonia 2006 and South
# Korea 2008). Since these are close, I will keep them here. However, if we
# chose 85% instead of 75%, the number of cases climbs to 24, so we might want
# to think whether this is what we want to do

# The other three cases below 75% are for Argentina 2013, where the three are
# set on consecutive days (10/27, 28 & 29). We could just count them as part of
# the same election, but even then they would just add up to 54% of the vote,
# so it makes sense to remove them

pol <- pol[!(pol$countryname == 'Argentina' & as.numeric(format(pol$edate,'%Y')) > 2012),]

# 'pol' now has a row for the percentage of votes that each party got in each
# election for each country, as well as the ideological score for that party
# at that point in time (based on their program, which can be found in the rile
# variable with a score between -100 for extreme left, and 100 for extreme
# right). We need to do some magic to find the average ideological score for
# the country at each election, which we get by taking the rows for each
# election/country, and summarizing them into one row that calculates the mean
# of the ideological score weighted by the percentage of the vote

# I first create a dataset that contains that data frame for each
# country/election combination

meanlr <- pol %>% group_by(countryname, edate) %>% dplyr::summarise(meanlr = sum(rile*pervote/100, na.rm = TRUE))

# And now I add a column to the 'pol' data frame with the corresponding matching
# data. The value will be repeated for each row that refers to a
# country/election (there's a line for each party). We do this (instead of just
# using the meanlr data frame or collapsing all the entries for a given
# country/election into one) so that we can add more variables that distinguish
# between right and left parties

pol$meanlr <- meanlr$meanlr[match(interaction(pol$countryname, pol$edate), interaction(meanlr$countryname, meanlr$edate))]

# The problem with the mean ideological score for each country is that we lose
# the information on how much political polarization there in the country's
# political system. In order to keep some of that information, we will first
# create two variables that capture the average ideological score for each
# country/election only for parties on the right (mright) and only for parties
# on the left (mleft).

pol$left <- ifelse(pol$rile < 0, pol$rile, 0)
pol$left <- -pol$left

pol$right <- ifelse(pol$rile > 0, pol$rile, 0)

### THINK ABOUT: Should I use the percentage of total vote, or should I
# transform this to be the percentage of all votes on the left & right?

mleft <- pol%>% group_by(countryname, edate) %>% dplyr::summarise(mleft = sum(left*pervote/100, na.rm = TRUE))

pol$mleft <- mleft$mleft[match(interaction(pol$countryname, pol$edate), interaction(mleft$countryname, mleft$edate))]

mright <- pol%>% group_by(countryname, edate) %>% dplyr::summarise(mright = sum(right*pervote/100, na.rm = TRUE))

pol$mright <- mright$mright[match(interaction(pol$countryname, pol$edate), interaction(mright$countryname, mright$edate))]



mleftright <- pol %>% group_by(countryname, edate) %>% dplyr::summarise(mleftright = max(meanlr), mleft = max(mleft), mright = max(mright))

mleftright$year <- as.numeric(format(mleftright$edate, "%Y"))

mleftright <- filter(mleftright, !(countryname == "Greece" & edate == "2012-05-06"))
mleftright <- filter(mleftright, !(countryname == "Argentina" & edate == "2013-10-27"))
mleftright <- filter(mleftright, !(countryname == "Argentina" & edate == "2013-10-28"))


nodataideology <- vector(mode = "character", 0)

for(row in 1:nrow(countryvariables)) {
  country <- countryvariables[row, 1]
  year <- countryvariables[row, 2]
  if(sum(mleftright$countryname %in% country) == 0) {
    nodataideology <- c(nodataideology, levels(country)[country])
    next
  }
  if(is.na(countryvariables[row, 15])) {
    if(length(mleftright$mleftright[mleftright$countryname == country & mleftright$year == year]) != 0) {
      if(!is.na(mleftright$mleftright[mleftright$countryname == country & mleftright$year == year])) {
        countryvariables[row, 15] <- mleftright$mleftright[mleftright$countryname == country & mleftright$year == year]
        countryvariables[row, 16] <- mleftright$mleft[mleftright$countryname == country & mleftright$year == year]
        countryvariables[row, 17] <- mleftright$mright[mleftright$countryname == country & mleftright$year == year]
        countryvariables[row, 18] <- year
        countryvariables[row, 19] <- "MP"
        next
      }
    } else {
      values <- mleftright$mleftright[mleftright$countryname %in% country]
      values2 <- mleftright$mleft[mleftright$countryname %in% country]
      values3 <- mleftright$mright[mleftright$countryname %in% country]
      years <- mleftright$year[mleftright$countryname %in% country]
      nas <- !is.na(values)
      if(sum(nas) == 0) {
        countryvariables[row, 15] <- NA
        countryvariables[row, 16] <- NA
        countryvariables[row, 17] <- NA
        countryvariables[row, 18] <- NA
      } else {
        values <- values[nas]
        values2 <- values2[nas]
        values3 <- values3[nas]
        years <- years[nas]
        dist <- abs(years - year)
        countryvariables[row, 15] <- values[which.min(dist)]
        countryvariables[row, 16] <- values2[which.min(dist)]
        countryvariables[row, 17] <- values3[which.min(dist)]
        countryvariables[row, 18] <- years[which.min(dist)]
        countryvariables[row, 19] <- "MP" 
      }
    }
  }
}

# Finally, we save countryvariables so that it can be loaded in the main
# script and used to add the values to the main data frame with the World
# Values Survey data

save(countryvariables, file = "countryvariables.Rda")
