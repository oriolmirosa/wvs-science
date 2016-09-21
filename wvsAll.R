library(plyr)
library(dplyr)
library(stringr)
library(psych)
library(lavaan)
library(MplusAutomation)

# Wave 5

load("WV5_Data_R_v_2015_04_18.rdata")
wvs5 <- WV5_Data_r_v_2015_04_18
rm(WV5_Data_r_v_2015_04_18)

names(wvs5)[names(wvs5)=="V2"] <- "country"
names(wvs5)[names(wvs5)=="V235"] <- "female"
names(wvs5)[names(wvs5)=="V256"] <- "race"
names(wvs5)[names(wvs5)=="V237"] <- "age"
names(wvs5)[names(wvs5)=="V253"] <- "incdecile"
names(wvs5)[names(wvs5)=="V187"] <- "religious"
names(wvs5)[names(wvs5)=="V186"] <- "religserv"
names(wvs5)[names(wvs5)=="V238"] <- "education"
names(wvs5)[names(wvs5)=="V114"] <- "leftright"
names(wvs5)[names(wvs5)=="Y001"] <- "postmater"
names(wvs5)[names(wvs5)=="V46"] <- "control"
names(wvs5)[names(wvs5)=="V47"] <- "trust"
names(wvs5)[names(wvs5)=="V260"] <- "year"

names(wvs5)[names(wvs5)=="V91"] <- "scibetlife"
names(wvs5)[names(wvs5)=="V92"] <- "scinextgen"
names(wvs5)[names(wvs5)=="V93"] <- "scichangefast"
names(wvs5)[names(wvs5)=="V94"] <- "scinotenfaith"
names(wvs5)[names(wvs5)=="V123"] <- "scibetoff"

wvs5 <- select(wvs5, country, year, female, race, age, incdecile, religious, religserv, education, leftright, postmater, control, trust, scibetlife, scinextgen, scichangefast, scinotenfaith, scibetoff)

cc <- read.csv(file = "Country Codes.csv", header = FALSE)
cc$code <- NA
cc$country <- NA
cc$code <- sapply(cc[,1], function(x) as.numeric(str_split(x, "##")[[1]][1]))
cc$country <- sapply(cc[,1], function(x) str_split(x, "##")[[1]][2])

wvs5$country <- cc$country[match(wvs5$country, cc$code)]

sum(sapply(wvs5, function(x) sum(x %in% c(-5, -4, -3, -2, -1))))
wvs5[wvs5 == -5 | wvs5 == -4 | wvs5 == -3 | wvs5 == -2 | wvs5 == -1] <- NA
sum(sapply(wvs5, function(x) sum(x %in% c(-5, -4, -3, -2, -1))))

wvs5$female <- mapvalues(wvs5$female, c(1, 2), c(0, 1))
wvs5$female <- as.factor(wvs5$female)
wvs5$religious <- mapvalues(wvs5$religious, c(1, 2, 3), c(1, 0, 0))
wvs5$religious <- as.factor(wvs5$religious)
wvs5$religserv <- mapvalues(wvs5$religserv, c(1,2,3,4,5,6,7),
                            c(7,6,5,4,3,2,1))

wvs5$scichangefast <- mapvalues(wvs5$scichangefast, c(1,2,3,4,5,6,7,8,9,10),
                                c(10,9,8,7,6,5,4,3,2,1))
wvs5$scinotenfaith <- mapvalues(wvs5$scinotenfaith, c(1,2,3,4,5,6,7,8,9,10),
                                c(10,9,8,7,6,5,4,3,2,1))

wvs5$wave <- "five"


# Wave 6

load("WV6_Data_R_v_2016_01_01.rdata")
wvs6 <- WV6_Data_R
rm(WV6_Data_R)

names(wvs6)[names(wvs6)=="V2"] <- "country"
names(wvs6)[names(wvs6)=="V240"] <- "female"
names(wvs6)[names(wvs6)=="V254"] <- "race"
names(wvs6)[names(wvs6)=="V242"] <- "age"
names(wvs6)[names(wvs6)=="V239"] <- "incdecile"
names(wvs6)[names(wvs6)=="V147"] <- "religious"
names(wvs6)[names(wvs6)=="V145"] <- "religserv"
names(wvs6)[names(wvs6)=="V248"] <- "education"
names(wvs6)[names(wvs6)=="V95"] <- "leftright"
names(wvs6)[names(wvs6)=="Y001"] <- "postmater"
names(wvs6)[names(wvs6)=="V55"] <- "control"
names(wvs6)[names(wvs6)=="V56"] <- "trust"
names(wvs6)[names(wvs6)=="V262"] <- "year"

names(wvs6)[names(wvs6)=="V192"] <- "scibetlife"
names(wvs6)[names(wvs6)=="V193"] <- "scinextgen"
names(wvs6)[names(wvs6)=="V194"] <- "scinotenfaith"
names(wvs6)[names(wvs6)=="V195"] <- "scirightwrong"
names(wvs6)[names(wvs6)=="V196"] <- "sciimportant"
names(wvs6)[names(wvs6)=="V197"] <- "scibetoff"

wvs6 <- select(wvs6, country, year, female, race, age, incdecile, religious, religserv, education, leftright, postmater, control, trust, scibetlife, scinextgen, scinotenfaith, scirightwrong, sciimportant, scibetoff)

wvs6$country <- cc$country[match(wvs6$country, cc$code)]

sum(sapply(wvs6, function(x) sum(x %in% c(-5, -4, -3, -2, -1))))
wvs6[wvs6 == -5 | wvs6 == -4 | wvs6 == -3 | wvs6 == -2 | wvs6 == -1] <- NA
sum(sapply(wvs6, function(x) sum(x %in% c(-5, -4, -3, -2, -1))))

wvs6$female <- mapvalues(wvs6$female, c(1, 2), c(0, 1))
wvs6$female <- as.factor(wvs6$female)
wvs6$religious <- mapvalues(wvs6$religious, c(1, 2, 3), c(1, 0, 0))
wvs6$religious <- as.factor(wvs6$religious)
wvs6$religserv <- mapvalues(wvs6$religserv, c(1,2,3,4,5,6,7),
                            c(7,6,5,4,3,2,1))

wvs6$scinotenfaith <- mapvalues(wvs6$scinotenfaith, c(1,2,3,4,5,6,7,8,9,10),
                                c(10,9,8,7,6,5,4,3,2,1))
wvs6$scirightwrong <- mapvalues(wvs6$scirightwrong, c(1,2,3,4,5,6,7,8,9,10),
                                c(10,9,8,7,6,5,4,3,2,1))
wvs6$sciimportant <- mapvalues(wvs6$sciimportant, c(1,2,3,4,5,6,7,8,9,10),
                               c(10,9,8,7,6,5,4,3,2,1))

wvs6$wave <- "six"

# We start by using as many countries as possible. For this, we will get rid of
# the countries that do not have any values on the science questions

c5.1 <- unique(wvs5$country)
c6.1 <- unique(wvs6$country)

length(c5.1)
length(c6.1)

length(intersect(c5.1, c6.1))

nrow(wvs5)
nrow(wvs6)

# At this point we have 58 countries in wave 5 and 60 in wave 6, of which 38
# are common to both waves. In total, we have 83975 observations in wave 5 and
# 90350 in wave 6. We delete the observations without values in any of the
# science questions

wvs5.sci <- filter(wvs5, !is.na(scibetlife) & !is.na(scinextgen) & !is.na(scichangefast) & !is.na(scinotenfaith) & !is.na(scibetoff))

wvs6.sci <- filter(wvs6, !is.na(scibetlife) & !is.na(scinextgen) & !is.na(scirightwrong) & !is.na(scinotenfaith) & !is.na(scibetoff) & !is.na(sciimportant))

c5.2 <- unique(wvs5.sci$country)
c6.2 <- unique(wvs6.sci$country)

length(c5.2)
length(c6.2)

length(intersect(c5.2, c6.2))

nrow(wvs5.sci)
nrow(wvs6.sci)

# We lost 11 countries in wave 5, and none in wave 6. We now have 59580
# observations in wave 5, and 81279 in wave 6. Now we are left with only 32
# countries in both waves. The countries we lost because they don't ask the
# science questions are:

setdiff(c5.1, c5.2)

# "Argentina"     "France"        "Guatemala"     "Hong Kong"     "Iran"         
# "Iraq"          "Netherlands"   "New Zealand"   "Russia"        "Switzerland"  
# "Great Britain"

# Now let's remove the observations that do not have values in any of our main
# covariates

wvs5.sci.no.na <- filter(wvs5.sci, !is.na(female) & !is.na(age) & !is.na(incdecile) & !is.na(religious) & !is.na(education) & !is.na(leftright) & !is.na(trust))

nrow(wvs5.sci.no.na)

wvs6.sci.no.na <- filter(wvs6.sci, !is.na(female) & !is.na(age) & !is.na(incdecile) & !is.na(religious) & !is.na(education) & !is.na(leftright) & !is.na(trust))

nrow(wvs6.sci.no.na)

# This is leaving us with 41644 observations in wave 5 and 59692 in wave 6.
# Since we've lost plenty of observations, let's see these imply the loss of
# any specific countries, or if these are random across the data set

c5.3 <- unique(wvs5.sci.no.na$country)
length(c5.3)

c6.3 <- unique(wvs6.sci.no.na$country)
length(c6.3)

# In wave 5, we went from 47 countries to 44, and in wave 6 we went from 60 to
# 53 countries, so we lost 3 and 7 respectively. Let's see which they are:

setdiff(c5.2, c5.3)

# The three that we lost in wave 5 are China, India, and Malaysia. Let's see why
# we lost them

sapply(wvs5.sci, function(x) sum(!is.na(x[wvs5.sci$country == "China"])))
sapply(wvs5.sci, function(x) sum(!is.na(x[wvs5.sci$country == "India"])))
sapply(wvs5.sci, function(x) sum(!is.na(x[wvs5.sci$country == "Malaysia"])))

# We see that China is missing the 'leftright' variable, India is missing the
# trust (and race) variable, and Malaysia is also missing the leftright variable

setdiff(c6.2, c6.3)

# In wave 6, we go from 60 to 53, countries, so we lost 7. These are: China,
# Egypt, Jordan, Kuwait, New Zealand, Qatar, Singapore. Again, let's see why
# we lost them:

sapply(wvs6.sci, function(x) sum(!is.na(x[wvs6.sci$country == "China"])))
sapply(wvs6.sci, function(x) sum(!is.na(x[wvs6.sci$country == "Egypt"])))
sapply(wvs6.sci, function(x) sum(!is.na(x[wvs6.sci$country == "Jordan"])))
sapply(wvs6.sci, function(x) sum(!is.na(x[wvs6.sci$country == "Kuwait"])))
sapply(wvs6.sci, function(x) sum(!is.na(x[wvs6.sci$country == "New Zealand"])))
sapply(wvs6.sci, function(x) sum(!is.na(x[wvs6.sci$country == "Qatar"])))
sapply(wvs6.sci, function(x) sum(!is.na(x[wvs6.sci$country == "Singapore"])))

# We lost China, Jordan, Kuwait (also missing race, religserv and postmater),
# Qater (also missing race nd religserv) and Singapore because they are missing
# the 'leftright' variable

# Egypt misses 'race' and religious' (but it does have religserv!)
# New Zealand misses 'trust'

length(intersect(c5.3, c6.3))

# SO, to sum up, we are left with 44 countries in wave 5 and 53 in wave 6, of
# which 27 are common to both. In total, then, we have...

44+53-27

# ... 70 countries

# Now we bind together waves 5 and 6 into a single dataset

wvsAll <- rbind.fill(wvs5.sci.no.na, wvs6.sci.no.na)

wvsAll$wave <- as.factor(wvsAll$wave)

wvsAll$age2 <- wvsAll$age * wvsAll$age

wvsAll$country[wvsAll$country == "Cyprus (G)"] <- "Cyprus"
wvsAll$country[wvsAll$country == "Viet Nam"] <- "Vietnam"
wvsAll$country[wvsAll$country == "Serbia and Montenegro"] <- "Serbia"
wvsAll$US <- ifelse(wvsAll$country == "United States", 1, 0)
wvsAll$US <- as.factor(wvsAll$US)

wvsAll$country <- as.factor(wvsAll$country)

save(wvsAll, file = "wvsAll-no.country.vars.Rda")

# Now we need to bring in the country-level variables

load("countryvariables.Rda")
wvsAll <- left_join(wvsAll, countryvariables, by = c("country", "year"))

# Before we can scale the variables, we need to know what the final analytical
# frame is, so that we actually scale based on the cases that are used in the
# analysis

# ALL: we start by using all the data we have

wvsAll$age <- scale(wvsAll$age)
wvsAll$age2 <- scale(wvsAll$age2)
wvsAll$incdecile <- scale(wvsAll$incdecile)
wvsAll$education <- scale(wvsAll$education)
wvsAll$leftright <- scale(wvsAll$leftright)
wvsAll$religserv <- scale(wvsAll$religserv)
wvsAll$postmater <- scale(wvsAll$postmater)
wvsAll$control <- scale(wvsAll$control)
wvsAll$trust <- scale(wvsAll$trust)

wvsAll$scibetlife <- scale(wvsAll$scibetlife)
wvsAll$scinextgen <- scale(wvsAll$scinextgen)
wvsAll$scichangefast <- scale(wvsAll$scichangefast)
wvsAll$scinotenfaith <- scale(wvsAll$scinotenfaith)
wvsAll$scibetoff <- scale(wvsAll$scibetoff)
wvsAll$scirightwrong <- scale(wvsAll$scirightwrong)
wvsAll$sciimportant <- scale(wvsAll$sciimportant)

wvsAll$gdp <- scale(wvsAll$gdp)
wvsAll$gini <- scale(wvsAll$gini)
wvsAll$governance <- scale(wvsAll$governance)
wvsAll$polstab <- scale(wvsAll$polstab)
wvsAll$mleftright <- scale(wvsAll$mleftright)
wvsAll$mleft <- scale(wvsAll$mleft)
wvsAll$mright <- scale(wvsAll$mright)

for(i in 1:ncol(wvsAll)) {
  if(class(wvsAll[,i]) == "matrix") attributes(wvsAll[,i]) <- NULL
}

prepareMplusData(wvsAll, "mplusAll", keepCols = c("country", "year", "female", "age", "incdecile", "religious", "religserv", "education", "leftright", "trust", "scibetlife", "scinextgen", "scichangefast", "scinotenfaith", "scibetoff", "wave", "scirightwrong", "sciimportant", "age2", "US", "gdp", "gini", "mleftright", "mleft", "mright"))
