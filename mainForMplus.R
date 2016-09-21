library(plyr)
library(dplyr)
library(stringr)
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

names(wvs5)[names(wvs5)=="V91"] <- "scibetterlife"
names(wvs5)[names(wvs5)=="V92"] <- "scinextgen"
names(wvs5)[names(wvs5)=="V93"] <- "scichangefast"
names(wvs5)[names(wvs5)=="V94"] <- "scinotenfaith"
names(wvs5)[names(wvs5)=="V123"] <- "scibetteroff"

wvs5 <- select(wvs5, country, year, female, race, age, incdecile, religious, religserv, education, leftright, postmater, control, trust, scibetterlife, scinextgen, scichangefast, scinotenfaith, scibetteroff)

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

names(wvs6)[names(wvs6)=="V192"] <- "scibetterlife"
names(wvs6)[names(wvs6)=="V193"] <- "scinextgen"
names(wvs6)[names(wvs6)=="V194"] <- "scinotenfaith"
names(wvs6)[names(wvs6)=="V195"] <- "scirightwrong"
names(wvs6)[names(wvs6)=="V196"] <- "sciimportant"
names(wvs6)[names(wvs6)=="V197"] <- "scibetteroff"

wvs6 <- select(wvs6, country, year, female, race, age, incdecile, religious, religserv, education, leftright, postmater, control, trust, scibetterlife, scinextgen, scinotenfaith, scirightwrong, sciimportant, scibetteroff)

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

wvs5.sci <- filter(wvs5, !is.na(scibetterlife) & !is.na(scinextgen) & !is.na(scichangefast) & !is.na(scinotenfaith) & !is.na(scibetteroff))

wvs6.sci <- filter(wvs6, !is.na(scibetterlife) & !is.na(scinextgen) & !is.na(scirightwrong) & !is.na(scinotenfaith) & !is.na(scibetteroff) & !is.na(sciimportant))

# Let's see how many countries are left here in each wave and in total

c5.1 <- unique(wvs5.sci$country)
length(c5.1)

c6.1 <- unique(wvs6.sci$country)
length(c6.1)

# Here we have 47 countries in wave 5 and 60 in wave 6. Let's now keep only the
# OECD countries

wvs5.OECD <- wvs5.sci[wvs5.sci$country == "Australia" | wvs5.sci$country == "Austria" | wvs5.sci$country == "Belgium" | wvs5.sci$country == "Canada" | wvs5.sci$country == "Chile" | wvs5.sci$country == "Czech Republic" | wvs5.sci$country == "Denmark" | wvs5.sci$country == "Estonia" | wvs5.sci$country == "Finland" | wvs5.sci$country == "France" | wvs5.sci$country == "Germany" | wvs5.sci$country == "Greece" | wvs5.sci$country == "Hungary" | wvs5.sci$country == "Iceland" | wvs5.sci$country == "Ireland" | wvs5.sci$country == "Israel" | wvs5.sci$country == "Italy" | wvs5.sci$country == "Japan" | wvs5.sci$country == "South Korea" | wvs5.sci$country == "Luxembourg" | wvs5.sci$country == "Mexico" | wvs5.sci$country == "Netherlands" | wvs5.sci$country == "New Zealand" | wvs5.sci$country == "Norway" | wvs5.sci$country == "Poland" | wvs5.sci$country == "Portugal" | wvs5.sci$country == "Slovak Republic" | wvs5.sci$country == "Slovenia" | wvs5.sci$country == "Spain" | wvs5.sci$country == "Sweden" | wvs5.sci$country == "Switzerland" | wvs5.sci$country == "Turkey" | wvs5.sci$country == "Great Britain" | wvs5.sci$country == "United States",]

wvs6.OECD <- wvs6.sci[wvs6.sci$country == "Australia" | wvs6.sci$country == "Austria" | wvs6.sci$country == "Belgium" | wvs6.sci$country == "Canada" | wvs6.sci$country == "Chile" | wvs6.sci$country == "Czech Republic" | wvs6.sci$country == "Denmark" | wvs6.sci$country == "Estonia" | wvs6.sci$country == "Finland" | wvs6.sci$country == "France" | wvs6.sci$country == "Germany" | wvs6.sci$country == "Greece" | wvs6.sci$country == "Hungary" | wvs6.sci$country == "Iceland" | wvs6.sci$country == "Ireland" | wvs6.sci$country == "Israel" | wvs6.sci$country == "Italy" | wvs6.sci$country == "Japan" | wvs6.sci$country == "South Korea" | wvs6.sci$country == "Luxembourg" | wvs6.sci$country == "Mexico" | wvs6.sci$country == "Netherlands" | wvs6.sci$country == "New Zealand" | wvs6.sci$country == "Norway" | wvs6.sci$country == "Poland" | wvs6.sci$country == "Portugal" | wvs6.sci$country == "Slovak Republic" | wvs6.sci$country == "Slovenia" | wvs6.sci$country == "Spain" | wvs6.sci$country == "Sweden" | wvs6.sci$country == "Switzerland" | wvs6.sci$country == "Turkey" | wvs6.sci$country == "Great Britain" | wvs6.sci$country == "United States",]

c5.2 <- unique(wvs5.OECD$country)
length(c5.2)

c6.2 <- unique(wvs6.OECD$country)
length(c6.2)

# Now we have 17 countries on wave 5 and 15 on wave 6

# We will now filter the data further to make sure that we do not have missing
# information, and we will check how many (and which) countries we lose from
# this

wvs5.OECD.no.na <- filter(wvs5.OECD, !is.na(female) & !is.na(age) & !is.na(incdecile) & !is.na(religious) & !is.na(education) & !is.na(leftright) & !is.na(postmater) & !is.na(control) & !is.na(trust))

wvs6.OECD.no.na <- filter(wvs6.OECD, !is.na(female) & !is.na(age) & !is.na(incdecile) & !is.na(religious) & !is.na(education) & !is.na(leftright) & !is.na(postmater) & !is.na(control) & !is.na(trust))

c5.3 <- unique(wvs5.OECD.no.na$country)
length(c5.3)

c6.3 <- unique(wvs6.OECD.no.na$country)
length(c6.3)

setdiff(c5.2, c5.3)
setdiff(c6.2, c6.3)

# We have lost New Zealand in wave 6 because it does not have information on
# trust

# Now we join the waves together and make the 'wave' variable a factor

wvsOECD <- rbind.fill(wvs5.OECD.no.na, wvs6.OECD.no.na)

wvsOECD$wave <- as.factor(wvsOECD$wave)

unique(wvsOECD$country)
distinct(wvsOECD, country, year)

# So, in the end, we have 19 different countries and 31 different
# country/wave combinations

# Now we add the country-level indicators

load("countryindicators2.Rda")
wvsOECD <- left_join(wvsOECD, countryindicators2, by = c("country", "year"))

# Next we standardize all the continuous variables

wvsOECD$age <- scale(wvsOECD$age)
wvsOECD$incdecile <- scale(wvsOECD$incdecile)
wvsOECD$education <- scale(wvsOECD$education)
wvsOECD$leftright <- scale(wvsOECD$leftright)
wvsOECD$religserv <- scale(wvsOECD$religserv)
wvsOECD$postmater <- scale(wvsOECD$postmater)
wvsOECD$control <- scale(wvsOECD$control)
wvsOECD$trust <- scale(wvsOECD$trust)
wvsOECD$gdp <- scale(wvsOECD$gdp)
wvsOECD$gini <- scale(wvsOECD$gini)
wvsOECD$governance <- scale(wvsOECD$governance)
wvsOECD$polstab <- scale(wvsOECD$polstab)
wvsOECD$mleftright <- scale(wvsOECD$mleftright)
wvsOECD$mleft <- scale(wvsOECD$mleft)
wvsOECD$mright <- scale(wvsOECD$mright)

wvsOECD$age2 <- wvsOECD$age * wvsOECD$age
wvsOECD$leftright2 <- wvsOECD$leftright * wvsOECD$leftright

prepareMplusData(wvsOECD, "mplus1", keepCols = c("country", "female", "age", "incdecile", "religious", "education", "leftright", "postmater", "control", "trust", "scibetterlife", "scinextgen", "scichangefast", "scinotenfaith", "scibetteroff", "wave", "scirightwrong", "sciimportant", "gdp", "governance", "gini", "polstab", "mleftright", "mleft", "mright", "age2"))
