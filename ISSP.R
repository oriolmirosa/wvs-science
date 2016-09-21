library(foreign)
library(plyr)
library(dplyr)
library(stringr)
library(MuMIn)

# V4: Country ISO 3166 Code
# C_ALPHAN: Country/Sample Prefix ISO 3166 Code - alphanumeric
# SEX: Sex of Respondent (1 Male, 2 Female, 9 no answer)
# AGE: Age of respondent (998 Don't know, 999 No answer)
# EDUCYRS: Education I: years of schooling
# DEGREE: Education II: Highest education level: categories
# RELIGGRP: Groups of religious affiliations (0 No religion
  # 1 Roman Catholic, 2 Protestant, 3 Christian Orthodox, 4 Other Christian,
  # 5 Jewish, 6 Islamic, 7 Buddhist, 8 Hindu, 9 Other Asian Religions,
  # 10 Other Religions, 97 Refused, 98 Information insufficient, 99 No answer)
# ATTEND: Attendance of religious services (0 NAP, no religion, TR: no islamic religion
  # 1 Several times a week or more often, 2 Once a week, 3 2 or 3 times a month,
  # 4 Once a month, 5 Several times a year, 6 Once a year,
  # 7 Less frequently than once a year, 8 Never, 97 Refused, 98 Don't know
  # 99 No answer)
# TOPBOT: Top-Bottom self-placement (1 - 10)
# PARTY_LR: R: Party affiliation: left-right scale (0 NAP, did not vote, not eligible
  # 1 Far left (communist etc.), 2 Left, center left, 3 Center, liberal
  # 4 Right, conservative, 5 Far right (fascist etc.), 6 Other, no specification,
  # 7 No party affiliation, no preference, would not vote, 97 Refused,
  # 98 Insufficient information, 99 No answer)
# V11: Q4a Amount of trust in most people (1 - 5)
# V12: Q4b Try to take advantage or be fair (1 - 5)
# V20: Q9a Science: believe too often in science, not enough in feelings and faith
# V21: Q9b Science: more harm than good
# V22: Q9c Science: solve environmental problems
# V24: Q10b Modern life harms the environment
# V25: Q10c Worry: progress harming environment

issp <- read.dta("ZA5500_v2-0-0.dta", convert.factors = FALSE)

issp <- select(issp, V4, C_ALPHAN, SEX, AGE, EDUCYRS, DEGREE, RELIGGRP, ATTEND, TOPBOT, PARTY_LR, V11, V12, V20, V21, V22, V24, V25)

cc <- read.csv("countrycodesissp.csv", header = FALSE)
cc$code <- NA
cc$country <- NA
cc$code <- sapply(cc[,1], function(x) str_split(x, " = ")[[1]][1])
cc$country <- sapply(cc[,1], function(x) str_split(x, " = ")[[1]][2])

issp$country <- cc$country[match(issp$C_ALPHAN, cc$code)]

isspOECD <- issp[issp$country == "Australia" | issp$country == "Austria" | issp$country == "Belgium" | issp$country == "Canada" | issp$country == "Chile" | issp$country == "Czech Republic" | issp$country == "Denmark" | issp$country == "Estonia" | issp$country == "Finland" | issp$country == "France" | issp$country == "Germany" | issp$country == "Greece" | issp$country == "Hungary" | issp$country == "Iceland" | issp$country == "Ireland" | issp$country == "Israel" | issp$country == "Italy" | issp$country == "Japan" | issp$country == "South Korea" | issp$country == "Luxembourg" | issp$country == "Mexico" | issp$country == "Netherlands" | issp$country == "New Zealand" | issp$country == "Norway" | issp$country == "Poland" | issp$country == "Portugal" | issp$country == "Slovak Republic" | issp$country == "Slovenia" | issp$country == "Spain" | issp$country == "Sweden" | issp$country == "Switzerland" | issp$country == "Turkey" | issp$country == "Great Britain" | issp$country == "United States",]

isspOECD$year <- 2010

load("countryindicatorsissp.Rda")

isspOECD <- left_join(isspOECD, countryindicatorsissp)

isspOECD$female <- mapvalues(isspOECD$SEX, c(1, 2), c(0, 1))
isspOECD$female <- as.factor(isspOECD$female)

isspOECD$age <- isspOECD$AGE

isspOECD$age2 <- isspOECD$age * isspOECD$age

isspOECD$education <- isspOECD$DEGREE

isspOECD$religiousA <- isspOECD$ATTEND
isspOECD$religiousA <- mapvalues(isspOECD$religiousA, c(1, 2, 3, 4, 5, 6, 7, 8),
                                 c(1, 1, 1, 1, 1, 1, 1, 0))
isspOECD$religiousA <- as.factor(isspOECD$religiousA)

isspOECD$religiousB <- isspOECD$RELIGGRP
isspOECD$religiousB <- mapvalues(isspOECD$religiousB, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                 c(0, rep(1, 10)))
isspOECD$religiousB <- as.factor(isspOECD$religiousB)

isspOECD$decile <- isspOECD$TOPBOT

# For leftright, 1 through 5 is the scale from left to right, but there are two
# more values: "6 Other, no specification"; and "7 No party affiliation, no 
# preference, would not vote." We put 6 as NA, but we build two versions of the
# leftright variable: leftright1 sets value 7 as NA, while leftright2 sets it
# to the middle value 3.

isspOECD$leftright1 <- isspOECD$PARTY_LR
isspOECD$leftright1 <- mapvalues(isspOECD$leftright1, c(1, 2, 3, 4, 5, 6, 7), c(1, 2, 3, 4, 5, NA, NA))

isspOECD$leftright2 <- isspOECD$PARTY_LR
isspOECD$leftright2 <- mapvalues(isspOECD$leftright2, c(1, 2, 3, 4, 5, 6, 7), c(1, 2, 3, 4, 5, NA, 3))

# For trust, we also use two variables. trust1 is pretty much the same question
# that we had in the WVS, as it captures whether the respondent thinks that
# other people 'try to take advantage or be fair' in a scale from 1 to 5.
# trust2 is a question about the 'amount of trust in most people', also
# measured from 1 to 5

isspOECD$trust1 <- isspOECD$V12

isspOECD$trust2 <- isspOECD$V11

# V20: Q9a Science: We believe too often in science, and not enough in feelings 
# and faith

isspOECD$scinotenfaith <- isspOECD$V20

# V21: Q9b Science: Overall, modern science does more harm than good

isspOECD$scimoreharm <- isspOECD$V21

# V22: Q9c Science: Modern science will solve our environmental problems with 
# little change to our way of life

isspOECD$scisolvenv <- isspOECD$V22
isspOECD$scisolvenv <- mapvalues(isspOECD$scisolvenv, c(1, 2, 3, 4, 5), c(5, 4, 3, 2, 1))

# Now that we have all the variables as we want them, we remove all the NAs
# from the data set to see how they look.

# In order to see if we lose any countries, let's check how many (and which),
# we have now:

c1 <- unique(isspOECD$country)

# [1] "Austria"         "Belgium"         "Canada"          "Chile"           "Czech Republic" 
# [6] "Denmark"         "Finland"         "France"          "Germany"         "Japan"          
# [11] "South Korea"     "Mexico"          "New Zealand"     "Norway"          "Slovak Republic"
# [16] "Slovenia"        "Spain"           "Sweden"          "Switzerland"     "Turkey"         
# [21] "Great Britain"   "United States"

# We now have 22 countries and 30477 observations

# In the first removal of NAs, I will use leftright1, for which I classified
# one value as NA, which makes it the variable with the highest number of NAs
# (11373), so we will lose a lot of observations here. Later, I will create a
# different data set in which I keep leftright2, and which will contain more
# data points

isspOECD.na1 <- filter(isspOECD, !is.na(female) & !is.na(age) & !is.na(decile) & !is.na(religiousA) & !is.na(education) & !is.na(leftright1) & !is.na(trust1))

c2 <- unique(isspOECD.na1$country)

setdiff(c1, c2)

# Now we have 20 countries and only 16103 observations. The countries we lost
# are New Zealand and Great Britain

# Let's see what the problem for these was

missing1 <- isspOECD %>% dplyr::filter(country == "Great Britain" | country == "New Zealand") %>% dplyr::group_by(country) %>% dplyr::summarise(n(), sum(is.na(female)),  sum(is.na(age)), sum(is.na(decile)), sum(is.na(religiousA)), sum(is.na(education)), sum(is.na(leftright1)), sum(is.na(leftright2)), sum(is.na(trust1)))

View(missing1)

# The missing variable for these countries is 'decile'.

isspOECD.na2 <- filter(isspOECD, !is.na(female) & !is.na(age) & !is.na(decile) & !is.na(religiousA) & !is.na(education) & !is.na(leftright2) & !is.na(trust1))

# Before we remove the NAs for the DVs, since we will actually run three
# different regressions, one for each of the DVs, I create three data sets,
# labeled A, B, and C, to make sure that we are not removing rows that contain
# all the information

isspOECD.na1$leftright1sq <- isspOECD.na1$leftright1 * isspOECD.na1$leftright1
isspOECD.na1$US <- ifelse(isspOECD.na1$country == "United States", 1, 0)

isspOECD.na1.A <- isspOECD.na1
isspOECD.na1.B <- isspOECD.na1
isspOECD.na1.C <- isspOECD.na1

# Now we can remove the NAs in a different DV for each of the three data sets



isspOECD.na1.A <- filter(isspOECD.na1.A, !is.na(scinotenfaith))
isspOECD.na1.B <- filter(isspOECD.na1.B, !is.na(scimoreharm))
isspOECD.na1.C <- filter(isspOECD.na1.C, !is.na(scisolvenv))

# Now we just need to scale the variables for each of them, and we will be
# ready to run our regressions

isspOECD.na1.A$age <- scale(isspOECD.na1.A$age)
isspOECD.na1.A$age2 <- scale(isspOECD.na1.A$age2)
isspOECD.na1.A$decile <- scale(isspOECD.na1.A$decile)
isspOECD.na1.A$education <- scale(isspOECD.na1.A$education)
isspOECD.na1.A$leftright1 <- scale(isspOECD.na1.A$leftright1)
isspOECD.na1.A$leftright1 <- scale(isspOECD.na1.A$leftright1)
isspOECD.na1.A$trust1 <- scale(isspOECD.na1.A$trust1)
isspOECD.na1.A$gdp <- scale(isspOECD.na1.A$gdp)
isspOECD.na1.A$mleft <- scale(isspOECD.na1.A$mleft)
isspOECD.na1.A$mright <- scale(isspOECD.na1.A$mright)
isspOECD.na1.A$gini <- scale(isspOECD.na1.A$gini)
isspOECD.na1.A$scinotenfaith <- scale(isspOECD.na1.A$scinotenfaith)

isspOECD.na1.B$age <- scale(isspOECD.na1.B$age)
isspOECD.na1.B$age2 <- scale(isspOECD.na1.B$age2)
isspOECD.na1.B$decile <- scale(isspOECD.na1.B$decile)
isspOECD.na1.B$education <- scale(isspOECD.na1.B$education)
isspOECD.na1.B$leftright1 <- scale(isspOECD.na1.B$leftright1)
isspOECD.na1.B$trust1 <- scale(isspOECD.na1.B$trust1)
isspOECD.na1.B$gdp <- scale(isspOECD.na1.B$gdp)
isspOECD.na1.B$mleft <- scale(isspOECD.na1.B$mleft)
isspOECD.na1.B$mright <- scale(isspOECD.na1.B$mright)
isspOECD.na1.B$gini <- scale(isspOECD.na1.B$gini)
isspOECD.na1.B$scimoreharm <- scale(isspOECD.na1.B$scimoreharm)

isspOECD.na1.C$age <- scale(isspOECD.na1.C$age)
isspOECD.na1.C$age2 <- scale(isspOECD.na1.C$age2)
isspOECD.na1.C$decile <- scale(isspOECD.na1.C$decile)
isspOECD.na1.C$education <- scale(isspOECD.na1.C$education)
isspOECD.na1.C$leftright1 <- scale(isspOECD.na1.C$leftright1)
isspOECD.na1.C$trust1 <- scale(isspOECD.na1.C$trust1)
isspOECD.na1.C$gdp <- scale(isspOECD.na1.C$gdp)
isspOECD.na1.C$mleft <- scale(isspOECD.na1.C$mleft)
isspOECD.na1.C$mright <- scale(isspOECD.na1.C$mright)
isspOECD.na1.C$gini <- scale(isspOECD.na1.C$gini)
isspOECD.na1.C$scisolvenv <- scale(isspOECD.na1.C$scisolvenv)

# Let's run the models

M1a.isspOECD.na1.A <- lmer(scinotenfaith ~ female + age + age2 + decile + religiousA + education + leftright1 + trust1 + gdp + gini + US + mleft + mright + religiousA * mright + (1 + leftright1 + religiousA | country), data = isspOECD.na1.A)

R2m        R2c 
0.09403288 0.14086813 

ICCa.ISSP <- 0.045883 / (0.045883 + 0.866371)

MA <- lmer(scinotenfaith ~ female + age + age2 + decile + religiousA + education + leftright1 + trust1 + gdp + gini + US + mleft + mright + (1 | country), data = isspOECD.na1.A)

M2a.isspOECD.na1.B <- lmer(scimoreharm ~ female + age + age2 + decile + religiousA + education + leftright1 + trust1 + gdp + gini + US + mleft + mright + mleft*mright + leftright1*gdp + (1 + leftright1 | country), data = isspOECD.na1.B)

R2m       R2c 
0.1465806 0.1738831 

ICCb.ISSP <- 0.0274214 / (0.0274214 + 0.8379556)

M3a.isspOECD.na1.C <- lmer(scisolvenv ~ female + age + age2 + decile + religiousA + education + leftright1 + trust1 + gdp + gini + US + mleft + mright + mleft*mright + leftright1*gdp + leftright1*US + (1 + leftright1 | country), data = isspOECD.na1.C)

R2m        R2c 
0.04009205 0.12794520 

ICCc.ISSP <- 0.0905008 / (0.0905008 + 0.9059678)


M1a.isspOECD.na1.A.X1 <- olmm(scinotenfaith.fa ~ ge(female) + ge(age) + ge(age2) + ge(decile) + ge(religiousA) + ge(education) + ge(leftright1) + ge(trust1) + ge(gdp) + ge(gini) + ge(US) + ge(mleft) + ge(mright) + ge(religiousA*mright) + re(1 + ge(leftright1) + ge(religiousA) | country), data = isspOECD.na1.A, family = cumulative())


##########################

##########################


isspOECD.na2.A <- isspOECD.na2
isspOECD.na2.B <- isspOECD.na2
isspOECD.na2.C <- isspOECD.na2

# Now we can remove the NAs in a different DV for each of the three data sets

isspOECD.na2.A <- filter(isspOECD.na2.A, !is.na(scinotenfaith))
isspOECD.na2.B <- filter(isspOECD.na2.B, !is.na(scimoreharm))
isspOECD.na2.C <- filter(isspOECD.na2.C, !is.na(scisolvenv))

# Now we just need to scale the variables for each of them, and we will be
# ready to run our regressions

isspOECD.na2.A$age <- scale(isspOECD.na2.A$age)
isspOECD.na2.A$age2 <- scale(isspOECD.na2.A$age2)
isspOECD.na2.A$decile <- scale(isspOECD.na2.A$decile)
isspOECD.na2.A$education <- scale(isspOECD.na2.A$education)
isspOECD.na2.A$leftright2 <- scale(isspOECD.na2.A$leftright2)
isspOECD.na2.A$trust1 <- scale(isspOECD.na2.A$trust1)
isspOECD.na2.A$gdp <- scale(isspOECD.na2.A$gdp)
isspOECD.na2.A$mleft <- scale(isspOECD.na2.A$mleft)
isspOECD.na2.A$mright <- scale(isspOECD.na2.A$mright)
isspOECD.na2.A$gini <- scale(isspOECD.na2.A$gini)
isspOECD.na2.A$scinotenfaith <- scale(isspOECD.na2.A$scinotenfaith)

isspOECD.na2.B$age <- scale(isspOECD.na2.B$age)
isspOECD.na2.B$age2 <- scale(isspOECD.na2.B$age2)
isspOECD.na2.B$decile <- scale(isspOECD.na2.B$decile)
isspOECD.na2.B$education <- scale(isspOECD.na2.B$education)
isspOECD.na2.B$leftright2 <- scale(isspOECD.na2.B$leftright2)
isspOECD.na2.B$trust1 <- scale(isspOECD.na2.B$trust1)
isspOECD.na2.B$gdp <- scale(isspOECD.na2.B$gdp)
isspOECD.na2.B$mleft <- scale(isspOECD.na2.B$mleft)
isspOECD.na2.B$mright <- scale(isspOECD.na2.B$mright)
isspOECD.na2.B$gini <- scale(isspOECD.na2.B$gini)
isspOECD.na2.B$scimoreharm <- scale(isspOECD.na2.B$scimoreharm)

isspOECD.na2.C$age <- scale(isspOECD.na2.C$age)
isspOECD.na2.C$age2 <- scale(isspOECD.na2.C$age2)
isspOECD.na2.C$decile <- scale(isspOECD.na2.C$decile)
isspOECD.na2.C$education <- scale(isspOECD.na2.C$education)
isspOECD.na2.C$leftright2 <- scale(isspOECD.na2.C$leftright2)
isspOECD.na2.C$trust1 <- scale(isspOECD.na2.C$trust1)
isspOECD.na2.C$gdp <- scale(isspOECD.na2.C$gdp)
isspOECD.na2.C$mleft <- scale(isspOECD.na2.C$mleft)
isspOECD.na2.C$mright <- scale(isspOECD.na2.C$mright)
isspOECD.na2.C$gini <- scale(isspOECD.na2.C$gini)
isspOECD.na2.C$scisolvenv <- scale(isspOECD.na2.C$scisolvenv)

# Let's run the models

M1a.isspOECD.na2.A <- lmer(scinotenfaith ~ female + age + age2 + decile + religiousA + education + leftright2 + trust1 + gdp + gini + mleft + mright + mleft*mright + (1 + leftright2 | country), data = isspOECD.na2.A)

M2a.isspOECD.na2.B <- lmer(scimoreharm ~ female + age + age2 + decile + religiousA + education + leftright2 + trust1 + gdp + gini + mleft + mright + mleft*mright + (1 + leftright1 | country), data = isspOECD.na2.B)

M3a.isspOECD.na2.C <- lmer(scisolvenv ~ female + age + age2 + decile + religiousA + education + leftright2 + trust1 + gdp + gini + mleft + mright + mleft*mright + (1 + leftright1 | country), data = isspOECD.na2.C)

library(ordinal)
library(VGAM)


parallel <- vglm(scinotenfaith ~ female + age + age2 + decile + religiousA + education + leftright2 + trust1 + country, family=cumulative(parallel=TRUE), data=isspOECD.na1.A)

nonparallel <- vglm(scinotenfaith ~ female + age + age2 + decile + religiousA + education + leftright2 + trust1 + country, family=cumulative, data=isspOECD.na1.A)

pchisq(deviance(parallel)-deviance(nonparallel),
       df=df.residual(parallel)-df.residual(nonparallel), lower.tail=FALSE)

write.dta(isspOECD.na1.A, file = "isspOECD-A.dta")
write.dta(isspOECD.na1.B, file = "isspOECD-B.dta")
write.dta(isspOECD.na1.C, file = "isspOECD-C.dta")
