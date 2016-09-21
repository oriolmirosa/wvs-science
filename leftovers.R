missing <- wvs %>% group_by(country, wave) %>% dplyr::summarise(n = n(), scibetterlife = sum(is.na(scibetterlife)), scinextgen = sum(is.na(scinextgen)), scichangefast = sum(is.na(scichangefast)), scinotenfaith = sum(is.na(scinotenfaith)), scibetteroff = sum(is.na(scibetteroff)), scirightwrong = sum(is.na(scirightwrong)), sciimportant = sum(is.na(sciimportant)))

View(missing)


countryyear <- unique(wvsOECD[,c('country', 'year')])
rownames(countryyear) <- NULL
countryyear <- arrange(countryyear, country)

c5.2 <- unique(wvs5.no.na$country)
length(c5.2)

c6.2 <- unique(wvs6.no.na$country)
length(c6.2)

# The countries that we lose here are found thus:

setdiff(c5.1, c5.2)
setdiff(c6.1, c6.2)

# In wave 5, they are: Colombia, China, India and Malaysia.
# In wave 6, they are: China, Egypt, Jordan, Kuwait, New Zealand, Qatar, and
# Singapore
# Let's find out what the problems with these countries are:

missing5 <- wvs5.sci %>% filter(country == "Colombia" | country == "China" | country == "India" | country == "Malaysia") %>% group_by(country) %>% dplyr::summarise(n(), sum(is.na(female)),  sum(is.na(age)), sum(is.na(incdecile)), sum(is.na(religious)), sum(is.na(education)), sum(is.na(leftright)), sum(is.na(postmater)), sum(is.na(control)), sum(is.na(trust)))

View(missing5)

missing6 <- wvs6.sci %>% filter(country == "China" | country == "Egypt" | country == "Jordan" | country == "Kuwait" | country == "New Zealand" | country == "Qatar" | country == "Singapore") %>% group_by(country) %>% dplyr::summarise(n(), sum(is.na(female)),  sum(is.na(age)), sum(is.na(incdecile)), sum(is.na(religious)), sum(is.na(education)), sum(is.na(leftright)), sum(is.na(postmater)), sum(is.na(control)), sum(is.na(trust)))

View(missing6)

# From this we see that, in wave 5, China and Malaysia are missing data on the
# 'leftright' variable, whereas Colombia is missing the 'postmater' variable
# and India is missing the 'trust' variable

# In wave 6, Egypt is missing the 'religious' variable, China, Jordan, Kuwait,
# Qatar, and Singapore are missing the 'leftright' variable, Kuwait is also
# missing the 'postmater' variable, and New Zealand is missing the 'trust'
# variable

# Before moving on, we will save the data set that we have now as 'raw', so
# that they can be changed (with new standardization and FAs) when we limit
# them to only some countries

save(wvs5.no.na, file = "wvs5raw.Rda")
save(wvs6.no.na, file = "wvs6raw.Rda")


wvs5 <- left_join(wvs5, countryindicators, by = c("country", "year"))
wvs6 <- left_join(wvs6, countryindicators, by = c("country", "year"))



# Let's keep only the OECD countries in each wave

wvs5.OECD <- wvs5.no.na[wvs5.no.na$country == "Australia" | wvs5.no.na$country == "Austria" | wvs5.no.na$country == "Belgium" | wvs5.no.na$country == "Canada" | wvs5.no.na$country == "Chile" | wvs5.no.na$country == "Czech Republic" | wvs5.no.na$country == "Denmark" | wvs5.no.na$country == "Estonia" | wvs5.no.na$country == "Finland" | wvs5.no.na$country == "France" | wvs5.no.na$country == "Germany" | wvs5.no.na$country == "Greece" | wvs5.no.na$country == "Hungary" | wvs5.no.na$country == "Iceland" | wvs5.no.na$country == "Ireland" | wvs5.no.na$country == "Israel" | wvs5.no.na$country == "Italy" | wvs5.no.na$country == "Japan" | wvs5.no.na$country == "South Korea" | wvs5.no.na$country == "Luxembourg" | wvs5.no.na$country == "Mexico" | wvs5.no.na$country == "Netherlands" | wvs5.no.na$country == "New Zealand" | wvs5.no.na$country == "Norway" | wvs5.no.na$country == "Poland" | wvs5.no.na$country == "Portugal" | wvs5.no.na$country == "Slovak Republic" | wvs5.no.na$country == "Slovenia" | wvs5.no.na$country == "Spain" | wvs5.no.na$country == "Sweden" | wvs5.no.na$country == "Switzerland" | wvs5.no.na$country == "Turkey" | wvs5.no.na$country == "Great Britain" | wvs5.no.na$country == "United States",]

wvs6.OECD <- wvs6.no.na[wvs6.no.na$country == "Australia" | wvs6.no.na$country == "Austria" | wvs6.no.na$country == "Belgium" | wvs6.no.na$country == "Canada" | wvs6.no.na$country == "Chile" | wvs6.no.na$country == "Czech Republic" | wvs6.no.na$country == "Denmark" | wvs6.no.na$country == "Estonia" | wvs6.no.na$country == "Finland" | wvs6.no.na$country == "France" | wvs6.no.na$country == "Germany" | wvs6.no.na$country == "Greece" | wvs6.no.na$country == "Hungary" | wvs6.no.na$country == "Iceland" | wvs6.no.na$country == "Ireland" | wvs6.no.na$country == "Israel" | wvs6.no.na$country == "Italy" | wvs6.no.na$country == "Japan" | wvs6.no.na$country == "South Korea" | wvs6.no.na$country == "Luxembourg" | wvs6.no.na$country == "Mexico" | wvs6.no.na$country == "Netherlands" | wvs6.no.na$country == "New Zealand" | wvs6.no.na$country == "Norway" | wvs6.no.na$country == "Poland" | wvs6.no.na$country == "Portugal" | wvs6.no.na$country == "Slovak Republic" | wvs6.no.na$country == "Slovenia" | wvs6.no.na$country == "Spain" | wvs6.no.na$country == "Sweden" | wvs6.no.na$country == "Switzerland" | wvs6.no.na$country == "Turkey" | wvs6.no.na$country == "Great Britain" | wvs6.no.na$country == "United States",]