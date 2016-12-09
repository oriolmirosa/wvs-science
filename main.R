library(plyr)
library(dplyr)
library(stringr)
library(psych)
library(lavaan)

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

# Let's first remove the country codes data so that we are just left with the
# two main data sets for each wave

rm(cc)

## Now that we have the main data sets, we will create different data sets for
# different numbers of countries. For each of them we will calculate the science
# factors, mean values of variables and introduce them as country indicators, 
# as well as standardize the variables, and do multiple imputation for the
# missing values, at least for incdecile

# ALL COUNTRIES

# We start by using as many countries as possible. For this, we will get rid of
# the countries that do not have any values on the science questions

wvs5.sci <- filter(wvs5, !is.na(scibetterlife) & !is.na(scinextgen) & !is.na(scichangefast) & !is.na(scinotenfaith) & !is.na(scibetteroff))

wvs6.sci <- filter(wvs6, !is.na(scibetterlife) & !is.na(scinextgen) & !is.na(scirightwrong) & !is.na(scinotenfaith) & !is.na(scibetteroff) & !is.na(sciimportant))

# Let's see how many countries are left here in each wave and in total

c5.1 <- unique(wvs5.sci$country)
length(c5.1)

c6.1 <- unique(wvs6.sci$country)
length(c6.1)

# Here we have 47 countries in wave 5 and 60 in wave 6.

# We will now filter the data further to make sure that we do not have missing
# information, and we will check how many (and which) countries we lose from
# this

wvs5.all.no.na <- filter(wvs5.sci, !is.na(female) & !is.na(age) & !is.na(incdecile) & !is.na(religious) & !is.na(education) & !is.na(leftright) & !is.na(postmater) & !is.na(control) & !is.na(trust))

wvs6.all.no.na <- filter(wvs6.sci, !is.na(female) & !is.na(age) & !is.na(incdecile) & !is.na(religious) & !is.na(education) & !is.na(leftright) & !is.na(postmater) & !is.na(control) & !is.na(trust))

c5.2 <- unique(wvs5.all.no.na$country)
length(c5.2)

c6.2 <- unique(wvs6.all.no.na$country)
length(c6.2)

setdiff(c5.1, c5.2)
setdiff(c6.1, c6.2)

# We are now left with 43 countries in wave 5 (we lost Colombia, China, India,
# and Malaysia), and 53 in wave 6 (we lost China, Egypt, Jordan, Kuwait, New
# Zealand, Qatar, and Singapore)

# Now we join the waves together

wvsAll <- rbind.fill(wvs5.all.no.na, wvs6.all.no.na)

# And we make the wave variable a factor

wvsAll$wave <- as.factor(wvsAll$wave)

# Let's save this data set so that we can use it in the countryvariables.R
# script to create the countryindicators data frame

save(wvsAll, file = "wvsAll-no.country.vars.Rda")

# Before we move forward, we load the countryindicators that were put together
# in the countryvariables.R script

load("countryindicators2.Rda")
wvsOECD <- left_join(wvsOECD, countryindicators2, by = c("country", "year"))

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

means <- wvsOECD %>% group_by(country) %>% dplyr::summarise(mleftright = mean(leftright, na.rm = TRUE), meducation = mean(education, na.rm = TRUE), mpostmater = mean(postmater, na.rm = TRUE), mcontrol = mean(control, na.rm = TRUE), mtrust = mean(trust, na.rm = TRUE))

countries <- unique(wvsOECD$country)

religideo <- sapply(countries, function(x) cor(wvsOECD$religserv[wvsOECD$country == x], wvsOECD$leftright[wvsOECD$country == x], use = "complete.obs"))

religideo <- as.data.frame(religideo)

religideo$country <- rownames(religideo)

wvsOECD$meanleftright <- means$mleftright[match(wvsOECD$country, means$country)]
wvsOECD$meaneducation <- means$meducation[match(wvsOECD$country, means$country)]
wvsOECD$meanpostmater <- means$mpostmater[match(wvsOECD$country, means$country)]
wvsOECD$meancontrol <- means$mcontrol[match(wvsOECD$country, means$country)]
wvsOECD$meantrust <- means$mtrust[match(wvsOECD$country, means$country)]
wvsOECD$religideo <- religideo$religideo[match(wvsOECD$country, religideo$country)]
















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
# trust. At some point, I should do multiple imputation on that variable so
# that I don't lose a whole country

# Now we join the waves together

wvsOECD <- rbind.fill(wvs5.OECD.no.na, wvs6.OECD.no.na)

wvsOECD$wave <- as.factor(wvsOECD$wave)

load("countryindicators2.Rda")
wvsOECD <- left_join(wvsOECD, countryindicators2, by = c("country", "year"))

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

means <- wvsOECD %>% group_by(country) %>% dplyr::summarise(mleftright = mean(leftright, na.rm = TRUE), meducation = mean(education, na.rm = TRUE), mpostmater = mean(postmater, na.rm = TRUE), mcontrol = mean(control, na.rm = TRUE), mtrust = mean(trust, na.rm = TRUE))

countries <- unique(wvsOECD$country)

religideo <- sapply(countries, function(x) cor(wvsOECD$religserv[wvsOECD$country == x], wvsOECD$leftright[wvsOECD$country == x], use = "complete.obs"))

religideo <- as.data.frame(religideo)

religideo$country <- rownames(religideo)

wvsOECD$meanleftright <- means$mleftright[match(wvsOECD$country, means$country)]
wvsOECD$meaneducation <- means$meducation[match(wvsOECD$country, means$country)]
wvsOECD$meanpostmater <- means$mpostmater[match(wvsOECD$country, means$country)]
wvsOECD$meancontrol <- means$mcontrol[match(wvsOECD$country, means$country)]
wvsOECD$meantrust <- means$mtrust[match(wvsOECD$country, means$country)]
wvsOECD$religideo <- religideo$religideo[match(wvsOECD$country, religideo$country)]








# Let's now do the CFA

CFAmodel.wvs5a <- "factor1 =~ scibetterlife + scinextgen + scibetteroff
                   factor2 =~ scinotenfaith + scichangefast"

CFAmodel.wvs5b <- "factor1 =~ scibetterlife + scinextgen
                   factor2 =~ scinotenfaith + scichangefast"

CFAmodel.wvs5c <- "factor1 =~ scibetterlife + scinextgen + scibetteroff
                   factor2 =~ scinotenfaith
                   factor3 =~ scichangefast"

CFAmodel.wvs5d <- "factor1 =~ scibetterlife + scinextgen
                   factor2 =~ scinotenfaith
                   factor3 =~ scichangefast"

CFAmodel.wvs6 <- "factor1 =~ scibetterlife + scinextgen + scibetteroff
                  factor2 =~ scinotenfaith + scirightwrong + sciimportant"

cfa5 <- cfa(CFAmodel.wvs5a, mimic = "Mplus", data = wvs5.OECD.no.na)
cfa5 <- cfa(CFAmodel.wvs5b, mimic = "Mplus", data = wvs5.OECD.no.na)

summary(cfa5, standardized = TRUE, fit.measures = TRUE)

cfa6 <- cfa(CFAmodel.wvs6, group = "country", mimic = "Mplus", data = wvs6.OECD.no.na)

summary(cfa6, standardized = TRUE, fit.measures = TRUE)


cfa5.weak <- cfa(CFAmodel.wvs5, data = wvs5.OECD.no.na, group = "country", group.equal = "loadings")

cfa5.strong <- cfa(CFAmodel.wvs5, data = wvs5.OECD.no.na, group = "country", group.equal = c("loadings", "intercepts"))

cfa5.strict <- cfa(CFAmodel.wvs5, data = wvs5.OECD.no.na, group = "country", group.equal = c("loadings", "intercepts", "residuals"))

anova(cfa5, cfa5.weak, cfa5.strong, cfa5.strict)






fa <- dplyr::select(wvs5.OECD.no.na, scibetterlife, scinextgen, scinotenfaith, scibetteroff, scichangefast)

fa.parallel(fa, fm = "ml")

scifa2.wvs5.OECD <- fa(fa, nfactors = 2, rotate = "oblimin", fm = "ml")
scifa2.wvs5.OECD

scifa3.wvs5.OECD <- fa(fa, nfactors = 3, rotate = "oblimin", fm = "ml")
scifa3.wvs5.OECD

# We choose the 2 factors, and add them to the data frame

wvs5.OECD.no.na$sci21 <- scifa2.wvs5.OECD$scores[,1]
wvs5.OECD.no.na$sci22 <- scifa2.wvs5.OECD$scores[,2]

fa <- dplyr::select(wvs6.OECD.no.na, scibetterlife, scinextgen, scinotenfaith, scibetteroff, scirightwrong, sciimportant)

fa.parallel(fa, fm = "ml")

scifa2.wvs6.OECD <- fa(fa, nfactors = 2, rotate = "oblimin", fm = "ml")
scifa2.wvs6.OECD

scifa3.wvs6.OECD <- fa(fa, nfactors = 3, rotate = "oblimin", fm = "ml")
scifa3.wvs6.OECD

wvs6.OECD.no.na$sci21 <- scifa2.wvs6.OECD$scores[,1]
wvs6.OECD.no.na$sci22 <- scifa2.wvs6.OECD$scores[,2]

wvsOECD <- rbind.fill(wvs5.OECD.no.na, wvs6.OECD.no.na)

wvsOECD$wave <- as.factor(wvsOECD$wave)





CFAmodel.wvsOECD <- "factor1 =~ NA*scibetterlife + scinextgen + scibetteroff
                     factor2 =~ NA*scinotenfaith + scichangefast + scirightwrong + sciimportant

                     factor1 ~~ 1*factor1
                     factor2 ~~ 1*factor2"

CFAmodel2.wvsOECD <- "factor1 =~ scibetterlife + scinextgen + scibetteroff
                     factor2 =~ scinotenfaith

                     factor1 ~~ 1*factor1
                     factor2 ~~ 1*factor2"

SEMmodel1.wvsOECD <- "factor1 =~ scibetterlife + scinextgen + scibetteroff
                     factor2 =~ scinotenfaith + scichangefast + scirightwrong + sciimportant

                     factor1 ~ countryAustralia + countryCanada + countryChile + countryEstonia + countryFinland + countryGermany + countryHungary + countryItaly + countryJapan + countryMexico + countryNetherlands + countryNorway + countryPoland + countrySlovenia + countrySouthKorea + countrySpain + countrySweden + countryTurkey
                     factor2 ~ countryAustralia + countryCanada + countryChile + countryEstonia + countryFinland + countryGermany + countryHungary + countryItaly + countryJapan + countryMexico + countryNetherlands + countryNorway + countryPoland + countrySlovenia + countrySouthKorea + countrySpain + countrySweden + countryTurkey"

SEMmodel2.wvsOECD <- "factor1 =~ NA*scibetterlife + scinextgen + scibetteroff
                      factor2 =~ NA*scinotenfaith + scichangefast + scirightwrong + sciimportant

                      factor1 ~ female + age + age2 + incdecile + religious + education + leftright + trust + wave + gdp + gini + US + mleft + mright + mleft*mright + leftright*mleft + countryAustralia + countryCanada + countryChile + countryEstonia + countryFinland + countryGermany + countryHungary + countryItaly + countryJapan + countryMexico + countryNetherlands + countryNorway + countryPoland + countrySlovenia + countrySouthKorea + countrySpain + countrySweden + countryTurkey
                      factor2 ~ female + age + age2 + incdecile + religious + education + leftright + trust + wave + religideo + gdp + gini + US + mleft + mright + mleft*mright + leftright * US + countryAustralia + countryCanada + countryChile + countryEstonia + countryFinland + countryGermany + countryHungary + countryItaly + countryJapan + countryMexico + countryNetherlands + countryNorway + countryPoland + countrySlovenia + countrySouthKorea + countrySpain + countrySweden + countryTurkey

                      factor1 ~~ 1*factor1
                      factor2 ~~ 1*factor2"

cfaOECD <- cfa(CFAmodel.wvsOECD, missing = "FIML", data = wvsOECD)
cfaOECD2 <- cfa(CFAmodel2.wvsOECD, data = wvsOECD)

semOECD1 <- sem(SEMmodel1.wvsOECD, missing = "FIML", data = wvsOECD2)
semOECD2 <- sem(SEMmodel2.wvsOECD, missing = "FIML", data = wvsOECD2)


summary(cfaOECD, standardized = TRUE, fit.measures = TRUE)
summary(cfaOECD2, standardized = TRUE, fit.measures = TRUE)

summary(semOECD1, standardized = TRUE, fit.measures = TRUE)
summary(semOECD2, standardized = TRUE, fit.measures = TRUE)

predicted <- predict(cfaOECD)
predicted2 <- predict(cfaOECD2)

library(dummies)
wvsOECD2 <- dummy.data.frame(wvsOECD, verbose = TRUE, names = "country")
wvsOECD2 <- cbind(wvsOECD$country, wvsOECD2)
names(wvsOECD2)[names(wvsOECD2) == "countrySouth Korea"] <- "countrySouthKorea"
names(wvsOECD2)[names(wvsOECD2) == "countryUnited States"] <- "countryUnitedStates"






sapply(countries, function(x) cor(wvs5.OECD.no.na$scibetterlife[wvs5.OECD.no.na$country == x], wvs5.OECD.no.na$scinextgen[wvs5.OECD.no.na$country == x], use = "complete.obs"))

for(i in length(unique(wvsOECD$country))) {
  
}

load("countryindicators2.Rda")
wvsOECD <- left_join(wvsOECD, countryindicators2, by = c("country", "year"))

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
wvsOECD$US <- ifelse(wvsOECD$country == "United States", 1, 0)
wvsOECD$US <- as.factor(wvsOECD$US)

means <- wvsOECD %>% group_by(country) %>% dplyr::summarise(mleftright = mean(leftright, na.rm = TRUE), meducation = mean(education, na.rm = TRUE), mpostmater = mean(postmater, na.rm = TRUE), mcontrol = mean(control, na.rm = TRUE), mtrust = mean(trust, na.rm = TRUE), religideo = cor(leftright, religserv, use = "complete.obs"))

wvsOECD$meanleftright <- means$mleftright[match(wvsOECD$country, means$country)]
wvsOECD$meaneducation <- means$meducation[match(wvsOECD$country, means$country)]
wvsOECD$meanpostmater <- means$mpostmater[match(wvsOECD$country, means$country)]
wvsOECD$meancontrol <- means$mcontrol[match(wvsOECD$country, means$country)]
wvsOECD$meantrust <- means$mtrust[match(wvsOECD$country, means$country)]
wvsOECD$religideo <- means$religideo[match(wvsOECD$country, means$country)]

# Let's also add the mean for each country of the mean distance of all
# individuals leftright position to the mean of leftright for that country,
# giving us a different measure of polarization than the one about political
# party programs introduced in the 'polari' variable

wvsOECD$distleftright <- (wvsOECD$leftright - wvsOECD$mleftright)^2

distmleftright <- wvsOECD %>% group_by(country) %>% summarise(distmleftright = sqrt(mean(distleftright, na.rm = TRUE)))

wvsOECD$distmleftright <- distmleftright$distmleftright[match(wvsOECD$country, distmleftright$country)]

wvsOECD <- select(wvsOECD, -distleftright)

save(wvsOECD, file = "wvsOECD.Rda")

###################






















# Let's start doing the factor analysis and adding the factors to the dataset

fa <- dplyr::select(wvs5.43, scibetterlife, scinextgen, scinotenfaith, scibetteroff, scichangefast)

fa.parallel(fa, fm = "ml")

scifa2.wvs5.43 <- fa(fa, nfactors = 2, rotate = "oblimin", fm = "ml")
scifa2.wvs5.43

# We choose the 2 factors, and add them to the data frame

wvs5.43$sci21 <- scifa2.wvs5.43$scores[,1]
wvs5.43$sci22 <- scifa2.wvs5.43$scores[,2]

fa <- dplyr::select(wvs6.53, scibetterlife, scinextgen, scinotenfaith, scibetteroff, scirightwrong, sciimportant)

fa.parallel(fa, fm = "ml")

scifa2.wvs6.53 <- fa(fa, nfactors = 2, rotate = "oblimin", fm = "ml")
scifa2.wvs6.53

wvs6.53$sci21 <- scifa2.wvs6.53$scores[,1]
wvs6.53$sci22 <- scifa2.wvs6.53$scores[,2]

save(wvs5.43, file = "wvs5.43.Rda")
save(wvs6.53, file = "wvs6.53.Rda")

# Now we can join them together and do all the standardizing. Since, once
# together, there are 70 countries in total for the analysis, I call the
# resulting data set wvs70

wvs70 <- rbind.fill(wvs5.43, wvs6.53)

wvs70$wave <- as.factor(wvs70$wave)

wvs70$age <- scale(wvs70$age)
wvs70$incdecile <- scale(wvs70$incdecile)
wvs70$education <- scale(wvs70$education)
wvs70$leftright <- scale(wvs70$leftright)
wvs70$postmater <- scale(wvs70$postmater)
wvs70$control <- scale(wvs70$control)
wvs70$trust <- scale(wvs70$trust)
wvs70$gdp <- scale(wvs70$gdp)
wvs70$gini <- scale(wvs70$gini)
wvs70$governance <- scale(wvs70$governance)
wvs70$polstab <- scale(wvs70$polstab)
wvs70$polari <- scale(wvs70$polari)

wvs70$age2 <- wvs70$age * wvs70$age

means <- wvs70 %>% group_by(country) %>% dplyr::summarise(mleftright = mean(leftright, na.rm = TRUE), meducation = mean(education, na.rm = TRUE), mpostmater = mean(postmater, na.rm = TRUE), mcontrol = mean(control, na.rm = TRUE), mtrust = mean(trust, na.rm = TRUE))

wvs70$mleftright <- means$mleftright[match(wvs70$country, means$country)]
wvs70$meducation <- means$meducation[match(wvs70$country, means$country)]
wvs70$mpostmater <- means$mpostmater[match(wvs70$country, means$country)]
wvs70$mcontrol <- means$mcontrol[match(wvs70$country, means$country)]
wvs70$mtrust <- means$mtrust[match(wvs70$country, means$country)]

# Let's also add the mean for each country of the mean distance of all
# individuals leftright position to the mean of leftright for that country,
# giving us a different measure of polarization than the one about political
# party programs introduced in the 'polari' variable

wvs70$distleftright <- (wvs70$leftright - wvs70$mleftright)^2

distmleftright <- wvs70 %>% group_by(country) %>% summarise(distmleftright = sqrt(mean(distleftright, na.rm = TRUE)))

wvs70$distmleftright <- distmleftright$distmleftright[match(wvs70$country, distmleftright$country)]

wvs70 <- select(wvs70, -distleftright)

save(wvs70, file = "wvs70.Rda")

# Let's now do all of this (scaling, country-level variables) for the separate
# wvs5 and wvs6 data sets so that we can see if the model with both combined
# is similar to what we obtain separately.

load("wvs5.43.Rda")

wvs5.43$age <- scale(wvs5.43$age)
wvs5.43$incdecile <- scale(wvs5.43$incdecile)
wvs5.43$education <- scale(wvs5.43$education)
wvs5.43$leftright <- scale(wvs5.43$leftright)
wvs5.43$postmater <- scale(wvs5.43$postmater)
wvs5.43$control <- scale(wvs5.43$control)
wvs5.43$trust <- scale(wvs5.43$trust)
wvs5.43$gdp <- scale(wvs5.43$gdp)
wvs5.43$gini <- scale(wvs5.43$gini)
wvs5.43$governance <- scale(wvs5.43$governance)
wvs5.43$polstab <- scale(wvs5.43$polstab)
wvs5.43$polari <- scale(wvs5.43$polari)

wvs5.43$age2 <- wvs5.43$age * wvs5.43$age

means <- wvs5.43 %>% group_by(country) %>% dplyr::summarise(mleftright = mean(leftright, na.rm = TRUE), meducation = mean(education, na.rm = TRUE), mpostmater = mean(postmater, na.rm = TRUE), mcontrol = mean(control, na.rm = TRUE), mtrust = mean(trust, na.rm = TRUE))

wvs5.43$mleftright <- means$mleftright[match(wvs5.43$country, means$country)]
wvs5.43$meducation <- means$meducation[match(wvs5.43$country, means$country)]
wvs5.43$mpostmater <- means$mpostmater[match(wvs5.43$country, means$country)]
wvs5.43$mcontrol <- means$mcontrol[match(wvs5.43$country, means$country)]
wvs5.43$mtrust <- means$mtrust[match(wvs5.43$country, means$country)]

# Let's also add the mean for each country of the mean distance of all
# individuals leftright position to the mean of leftright for that country,
# giving us a different measure of polarization than the one about political
# party programs introduced in the 'polari' variable

wvs5.43$distleftright <- (wvs5.43$leftright - wvs5.43$mleftright)^2

distmleftright <- wvs5.43 %>% group_by(country) %>% summarise(distmleftright = sqrt(mean(distleftright, na.rm = TRUE)))

wvs5.43$distmleftright <- distmleftright$distmleftright[match(wvs5.43$country, distmleftright$country)]

wvs5.43 <- select(wvs5.43, -distleftright)

save(wvs5.43, file = "wvs5.43.Rda")

##

load("wvs6.53.Rda")

wvs6.53$age <- scale(wvs6.53$age)
wvs6.53$incdecile <- scale(wvs6.53$incdecile)
wvs6.53$education <- scale(wvs6.53$education)
wvs6.53$leftright <- scale(wvs6.53$leftright)
wvs6.53$postmater <- scale(wvs6.53$postmater)
wvs6.53$control <- scale(wvs6.53$control)
wvs6.53$trust <- scale(wvs6.53$trust)
wvs6.53$gdp <- scale(wvs6.53$gdp)
wvs6.53$gini <- scale(wvs6.53$gini)
wvs6.53$governance <- scale(wvs6.53$governance)
wvs6.53$polstab <- scale(wvs6.53$polstab)
wvs6.53$polari <- scale(wvs6.53$polari)

wvs6.53$age2 <- wvs6.53$age * wvs6.53$age

means <- wvs6.53 %>% group_by(country) %>% dplyr::summarise(mleftright = mean(leftright, na.rm = TRUE), meducation = mean(education, na.rm = TRUE), mpostmater = mean(postmater, na.rm = TRUE), mcontrol = mean(control, na.rm = TRUE), mtrust = mean(trust, na.rm = TRUE))

wvs6.53$mleftright <- means$mleftright[match(wvs6.53$country, means$country)]
wvs6.53$meducation <- means$meducation[match(wvs6.53$country, means$country)]
wvs6.53$mpostmater <- means$mpostmater[match(wvs6.53$country, means$country)]
wvs6.53$mcontrol <- means$mcontrol[match(wvs6.53$country, means$country)]
wvs6.53$mtrust <- means$mtrust[match(wvs6.53$country, means$country)]

# Let's also add the mean for each country of the mean distance of all
# individuals leftright position to the mean of leftright for that country,
# giving us a different measure of polarization than the one about political
# party programs introduced in the 'polari' variable

wvs6.53$distleftright <- (wvs6.53$leftright - wvs6.53$mleftright)^2

distmleftright <- wvs6.53 %>% group_by(country) %>% summarise(distmleftright = sqrt(mean(distleftright, na.rm = TRUE)))

wvs6.53$distmleftright <- distmleftright$distmleftright[match(wvs6.53$country, distmleftright$country)]

wvs6.53 <- select(wvs6.53, -distleftright)

save(wvs6.53, file = "wvs6.53.Rda")

## Let's focus on OECD countries

OECDcountries <- (country == "Australia" | country == "Austria" | country == "Belgium" | country == "Canada" | country == "Chile" | country == "Czech Republic" | country == "Denmark" | country == "Estonia" | country == "Finland" | country == "France" | country == "Germany" | country == "Greece" | country == "Hungary" | country == "Iceland" | country == "Ireland" | country == "Israel" | country == "Italy" | country == "Japan" | country == "South Korea" | country == "Luxembourg" | country == "Mexico" | country == "Netherlands" | country == "New Zealand" | country == "Norway" | country == "Poland" | country == "Portugal" | country == "Slovak Republic" | country == "Slovenia" | country == "Spain" | country == "Sweden" | country == "Switzerland" | country == "Turkey" | country == "Great Britain" | country == "United States")






















wvs5 <- filter(wvs5, country == "Australia" | country == "Brazil" | country == "Canada" | country == "Chile" | country == "Finland" | country == "Germany" | country == "Italy" | country == "Japan" | country == "Norway" | country == "South Korea" | country == "Spain" | country == "Sweden" | country == "United States")

wvs6 <- filter(wvs6, country == "Australia" | country == "Brazil" | country == "Chile" | country == "Germany" | country == "Japan" | country == "Netherlands" | country == "New Zealand" | country == "South Korea" | country == "Spain" | country == "Sweden" | country == "United States")





####

# Countries without science info are France, Great Britain, Guatemala, Iran and Switzerland
# Let's remove them from the data set. The original data set has 80 countries. Since we will now
# remove 5, we will call the new data set wvs75

wvs75 <- filter(wvs, country != "France", country != "Great Britain", country != "Guatemala", country != "Iran", country != "Switzerland")

wvs75$country[wvs75$country == "Cyprus (G)"] <- "Cyprus"
wvs75$country[wvs75$country == "Viet Nam"] <- "Vietnam"

countryyear <- unique(wvs75[,c('country', 'year')])
countryyear <- arrange(countryyear, country)
write.csv(countryyear, file = "countryyear.csv")




