library(WDI)

new_cache = WDIcache()

WDIsearch('gdp.*capita.*ppp', cache = new_cache)

# The name of the GDP per capita, PPP (constant 2011 international $) is: NY.GDP.PCAP.PP.KD

WDIsearch('gini', cache = new_cache)

# GINI index (World Bank estimate): SI.POV.GINI

gdp = WDI(indicator = 'NY.GDP.PCAP.PP.KD', country = wvscountries, start = 2005, end = 2016)
gini = WDI(indicator = 'SI.POV.GINI', country = wvscountries, start = 2005, end = 2016)

country = 'all'
indicator = 'NY.GDP.PCAP.PP.KD'
start = 2005
end = 2016

daturl = paste0("http://api.worldbank.org/countries/", country, "/indicators/", indicator,
               "?date=", start, ":", end, "&per_page=25000", "&format=json")

dat = RJSONIO::fromJSON(daturl, nullValue=NA)[[2]]

dat = lapply(dat, function(j) cbind(j$country[[1]], j$country[[2]], j$value, j$date))

dat = data.frame(do.call('rbind', dat))


wvs5, country == "Australia" | country == "Brazil" | country == "Canada" | country == "Chile" | country == "Finland" | country == "Germany" | country == "Italy" | country == "Japan" | country == "Norway" | country == "South Korea" | country == "Spain" | country == "Sweden" | country == "Taiwan" | country == "United States"

wvs6, country == "Australia" | country == "Brazil" | country == "Chile" | country == "Germany" | country == "Hong Kong" | country == "Japan" | country == "Netherlands" | country == "New Zealand" | country == "Singapore" | country == "South Korea" | country == "Spain" | country == "Sweden" | country == "Taiwan" | country == "United States")

wvs, country == "Australia" | country == "Brazil" | country == "Canada" | country == "Chile" | country == "Finland" | country == "Germany" | country == "Hong Kong" | country == "Italy" | country == "Japan" | country == "Netherlands" | country == "New Zealand" | country == "Norway" | country == "Singapore" | country == "South Korea" | country == "Spain" | country == "Sweden" | country == "Taiwan" | country == "United States"

wvscountries <- c('AU', 'BR', 'CA', 'CL', 'FI', 'DE', 'HK', 'IT', 'JP', 'NL', 'NZ', 'NO', 'SG', 'KR', 'ES', 'SE', 'TW', 'US')
