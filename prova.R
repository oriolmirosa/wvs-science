library(WDI)
library(stringr)
library(dplyr)

### Country-level variables

# We start by reading from disk the 'countryindicators' data frame. It already
# contains, from when I first created it, the columns for indicators for gdp per
# capita, gini, and governance (and some of the data for them, some of it added
# by hand because it was not readily accessible), but we need to add those for
# political stability and polarization

countryindicators2 <- countryyear

countryindicators2$gdp <- NA
countryindicators2$gdpyear <- NA
countryindicators2$gdpsource <- ""

countryindicators2$governance <- NA
countryindicators2$govyear <- NA
countryindicators2$govsource <- ""

countryindicators2$gini <- NA
countryindicators2$giniyear <- NA
countryindicators2$ginisource <- ""

countryindicators2$polstab <- NA
countryindicators2$polstabyear <- NA
countryindicators2$polstabsource <- ""

countryindicators2$mleftright <- NA
countryindicators2$mleft <- NA
countryindicators2$mright <- NA
countryindicators2$mideologyyear <- NA
countryindicators2$ideologysource <- ""

### Gdp per capita, from the World Bank's World Development Indicators (WDI)

gdp = WDI(indicator = 'NY.GDP.PCAP.PP.KD', start = 2005, end = 2015)

gdp$country[gdp$country == "Russian Federation"] <- "Russia"
gdp$country[gdp$country == "Korea, Rep."] <- "South Korea"
gdp$country[gdp$country == "Iran, Islamic Rep."] <- "Iran"
gdp$country[gdp$country == "Egypt, Arab Rep."] <- "Egypt"
gdp$country[gdp$country == "Hong Kong SAR, China"] <- "Hong Kong"
gdp$country[gdp$country == "Kyrgyz Republic"] <- "Kyrgyzstan"
gdp$country[gdp$country == "Yemen, Rep."] <- "Yemen"
gdp$country[gdp$country == "United Kingdom"] <- "Great Britain"

# Let's add the GDP per capita information to countryindicators2

nodatagdp <- vector(mode = "character", 0)

for(row in 1:nrow(countryindicators2)) {
  country <- countryindicators2[row, 1]
  year <- countryindicators2[row, 2]
  if(sum(gdp$country %in% country) == 0) {
    nodatagdp <- c(nodatagdp, levels(country)[country])
    next
  }
  if(is.na(countryindicators2[row, 3])) {
    if(!is.na(gdp$NY.GDP.PCAP.PP.KD[gdp$country == country & gdp$year == year])) {
      countryindicators2[row, 3] <- gdp$NY.GDP.PCAP.PP.KD[gdp$country == country & gdp$year == year]
      countryindicators2[row, 4] <- year
      countryindicators2[row, 5] <- "WDI"
    } else {
      values <- gdp$NY.GDP.PCAP.PP.KD[gdp$country %in% country]
      years <- gdp$year[gdp$country %in% country]
      nas <- !is.na(values)
      if(sum(nas) == 0) {
        countryindicators2[row, 3] <- NA
        countryindicators2[row, 4] <- NA
      } else {
        values <- values[nas]
        years <- years[nas]
        dist <- abs(years - year)
        countryindicators2[row, 3] <- values[which.min(dist)]
        countryindicators2[row, 4] <- years[which.min(dist)]
        countryindicators2[row, 5] <- "WDI" 
      }
    }
  }
}

## Now we prepare the gini data, also from the World Bank's WDI

gini = WDI(indicator = 'SI.POV.GINI', start = 1990, end = 2015)

gini$country[gini$country == "Russian Federation"] <- "Russia"
gini$country[gini$country == "Korea, Rep."] <- "South Korea"
gini$country[gini$country == "Iran, Islamic Rep."] <- "Iran"
gini$country[gini$country == "Egypt, Arab Rep."] <- "Egypt"
gini$country[gini$country == "Hong Kong SAR, China"] <- "Hong Kong"
gini$country[gini$country == "Kyrgyz Republic"] <- "Kyrgyzstan"
gini$country[gini$country == "Yemen, Rep."] <- "Yemen"
gini$country[gini$country == "United Kingdom"] <- "Great Britain"

# Here we add the GINI information to countryindicators2

nodatagini <- vector(mode = "character", 0)

for(row in 1:nrow(countryindicators2)) {
  country <- countryindicators2[row, 1]
  year <- countryindicators2[row, 2]
  if(sum(gini$country %in% country) == 0) {
    nodatagini <- c(nodatagini, levels(country)[country])
    next
  }
  if(is.na(countryindicators2[row, 9])) {
    if(!is.na(gini$SI.POV.GINI[gini$country == country & gini$year == year])) {
      countryindicators2[row, 9] <- (gini$SI.POV.GINI[gini$country == country & gini$year == year])/100
      countryindicators2[row, 10] <- year
      countryindicators2[row, 11] <- "WDI"
    } else {
      values <- gini$SI.POV.GINI[gini$country %in% country]
      years <- gini$year[gini$country %in% country]
      nas <- !is.na(values)
      if(sum(nas) == 0) {
        countryindicators2[row, 9] <- NA
        countryindicators2[row, 10] <- NA
      } else {
        values <- values[nas]
        years <- years[nas]
        dist <- abs(years - year)
        countryindicators2[row, 9] <- (values[which.min(dist)])/100
        countryindicators2[row, 10] <- years[which.min(dist)]
        countryindicators2[row, 11] <- "WDI" 
      }
    }
  }
}

countryindicators2$gini[countryindicators2$country == "South Korea" & countryindicators2$year == 2010] <- 0.307
countryindicators2$giniyear[countryindicators2$country == "South Korea" & countryindicators2$year == 2010] <- 2012
countryindicators2$ginisource[countryindicators2$country == "South Korea" & countryindicators2$year == 2010] <- "OECD"

countryindicators2$gini[countryindicators2$country == "South Korea" & countryindicators2$year == 2005] <- 0.307
countryindicators2$giniyear[countryindicators2$country == "South Korea" & countryindicators2$year == 2005] <- 2012
countryindicators2$ginisource[countryindicators2$country == "South Korea" & countryindicators2$year == 2005] <- "OECD"

## Now we prepare the data on government effectiveness, which comes from the
# Worldwide Governance Indicators.
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
gov$country[gov$country == "United Kingdom"] <- "Great Britain"

# Now we add the government effectiveness information to countryindicators2

nodatagov <- vector(mode = "character", 0)

for(row in 1:nrow(countryindicators2)) {
  country <- countryindicators2[row, 1]
  year <- countryindicators2[row, 2]
  if(sum(gov$country %in% country) == 0) {
    nodatagov <- c(nodatagov, country)
    next
  }
  cols <- which(gov[1,] == year)
  if(is.na(countryindicators2[row, 6])) {
    if(!is.na(gov[gov$country == country, cols[4]])) {
      countryindicators2[row, 6] <- gov[gov$country == country, cols[4]]
      countryindicators2[row, 7] <- year
      countryindicators2[row, 8] <- "WGI"
    } else {
      countryindicators2[row, 6] <- NA
      countryindicators2[row, 7] <- NA
    }
  }
}
## Now we prepare the data on political stability, also from the Worldwide 
# Governance Indicators.

polstab <- read.csv("politicalstability.csv", na.strings = "#N/A")

polstab$country <- str_to_title(polstab$country)

polstab$country[polstab$country == "Russian Federation"] <- "Russia"
polstab$country[polstab$country == "Korea, Rep."] <- "South Korea"
polstab$country[polstab$country == "Iran, Islamic Rep."] <- "Iran"
polstab$country[polstab$country == "Egypt, Arab Rep."] <- "Egypt"
polstab$country[polstab$country == "Hong Kong Sar, China"] <- "Hong Kong"
polstab$country[polstab$country == "Kyrgyz Republic"] <- "Kyrgyzstan"
polstab$country[polstab$country == "Taiwan, China"] <- "Taiwan"
polstab$country[polstab$country == "Trinidad And Tobago"] <- "Trinidad and Tobago"
polstab$country[polstab$country == "Yemen, Rep."] <- "Yemen"
polstab$country[polstab$country == "United Kingdom"] <- "Great Britain"

# Now we add the political stability information to countryindicators2

nodatapol <- vector(mode = "character", 0)

for(row in 1:nrow(countryindicators2)) {
  country <- countryindicators2[row, 1]
  year <- countryindicators2[row, 2]
  if(sum(polstab$country %in% country) == 0) {
    nodatapol <- c(nodatapol, country)
    next
  }
  cols <- which(polstab[1,] == year)
  if(is.na(countryindicators2[row, 12])) {
    if(!is.na(polstab[polstab$country == country, cols[4]])) {
      countryindicators2[row, 12] <- polstab[polstab$country == country, cols[4]]
      countryindicators2[row, 13] <- year
      countryindicators2[row, 14] <- "WGI"
    } else {
      countryindicators2[row, 12] <- NA
      countryindicators2[row, 13] <- NA
    }
  }
}

## Now we get to the data on political polarization
# This comes from the Manifesto Project, which looks at the party programs of
# political parties to figure out their ideological position. Basically, here
# I take the ideological score (-100 -left- to 100 - right) of the parties 
# program for a given election year, weigh it by its popular support, and find
# the mean for that election. Then I take the mean value and subtract it from
# each party's value, square them, add them together, and do the square root.
# Then I find the mean of that value for each party, which gives me the average
# polarization, how much distance the parties are from each other (or the 
# mean) for that election.
# https://manifestoproject.wzb.eu/

pol <- read.csv("MPDataset_MPDS2015a.csv", stringsAsFactors = FALSE)
pol$edate <- as.Date(pol$edate, format = "%d/%m/%Y")

pol <- pol[pol$edate >= "2005-01-01",]

pol$countryname[pol$countryname == "Slovakia"] <- "Slovak Republic"

pol <- select(pol, countryname, edate, party, pervote, rile)

polSA <- read.csv("MPDataset_MPDSSA2015a.csv", stringsAsFactors = FALSE)
polSA$edate <- as.Date(polSA$edate, format = "%d/%m/%Y")

polSA <- polSA[polSA$edate >= "2005-01-01",]
polSA <- select(polSA, countryname, edate, party, pervote, rile)

pol <- rbind(pol, polSA)
pol <- arrange(pol, countryname, edate)
rm(polSA)

meanlr <- pol %>% group_by(countryname, edate) %>% dplyr::summarise(meanlr = sum(rile*pervote/100, na.rm = TRUE))

pol$meanlr <- meanlr$meanlr[match(interaction(pol$countryname, pol$edate), interaction(meanlr$countryname, meanlr$edate))]

pol$left <- ifelse(pol$rile < 0, pol$rile, 0)
pol$left <- -pol$left

pol$right <- ifelse(pol$rile > 0, pol$rile, 0)

mleft <- pol%>% group_by(countryname, edate) %>% dplyr::summarise(mleft = sum(left*pervote/100, na.rm = TRUE))

pol$mleft <- mleft$mleft[match(interaction(pol$countryname, pol$edate), interaction(mleft$countryname, mleft$edate))]

mright <- pol%>% group_by(countryname, edate) %>% dplyr::summarise(mright = sum(right*pervote/100, na.rm = TRUE))

pol$mright <- mright$mright[match(interaction(pol$countryname, pol$edate), interaction(mright$countryname, mright$edate))]

ideology <- pol %>% group_by(countryname, edate) %>% dplyr::summarise(mleftright = max(meanlr), mleft = max(mleft), mright = max(mright))

ideology$year <- as.numeric(format(ideology$edate, "%Y"))

ideology <- ideology[ideology$countryname != "Greece" & ideology$countryname != "Argentina",]

nodataideology <- vector(mode = "character", 0)

for(row in 1:nrow(countryindicators2)) {
  country <- countryindicators2[row, 1]
  year <- countryindicators2[row, 2]
  if(sum(ideology$countryname %in% country) == 0) {
    nodataideology <- c(nodataideology, levels(country)[country])
    next
  }
  if(is.na(countryindicators2[row, 15])) {
    if(length(ideology$mleftright[ideology$countryname == country & ideology$year == year]) != 0) {
      if(!is.na(ideology$mleftright[ideology$countryname == country & ideology$year == year])) {
        countryindicators2[row, 15] <- ideology$mleftright[ideology$countryname == country & ideology$year == year]
        countryindicators2[row, 16] <- ideology$mleft[ideology$countryname == country & ideology$year == year]
        countryindicators2[row, 17] <- ideology$mright[ideology$countryname == country & ideology$year == year]
        countryindicators2[row, 18] <- year
        countryindicators2[row, 19] <- "MP"
        next
      }
    } else {
      values <- ideology$mleftright[ideology$countryname %in% country]
      values2 <- ideology$mleft[ideology$countryname %in% country]
      values3 <- ideology$mright[ideology$countryname %in% country]
      years <- ideology$year[ideology$countryname %in% country]
      nas <- !is.na(values)
      if(sum(nas) == 0) {
        countryindicators2[row, 15] <- NA
        countryindicators2[row, 16] <- NA
        countryindicators2[row, 17] <- NA
        countryindicators2[row, 18] <- NA
      } else {
        values <- values[nas]
        values2 <- values2[nas]
        values3 <- values3[nas]
        years <- years[nas]
        dist <- abs(years - year)
        countryindicators2[row, 15] <- values[which.min(dist)]
        countryindicators2[row, 16] <- values2[which.min(dist)]
        countryindicators2[row, 17] <- values3[which.min(dist)]
        countryindicators2[row, 18] <- years[which.min(dist)]
        countryindicators2[row, 19] <- "MP" 
      }
    }
  }
}

# Finally, we save countryindicators2 so that it can be loaded in the main
# script and used to add the values to the main data frame with the World
# Values Survey data

save(countryindicators2, file = "countryindicators2.Rda")
