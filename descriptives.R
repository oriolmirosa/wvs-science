# Descriptives

# Let's find basic descriptives for the DVs for both waves

dvdescrip5 <- summarise(group_by(wvs5, country),
                        n = n(),
                        n_sci21 = sum(!is.na(sci21)),
                        sci21 = mean(sci21, na.rm = T), 
                        n_sci22 = sum(!is.na(sci22)),
                        sci22 = mean(sci22, na.rm = T))

dvdescrip6 <- summarise(group_by(wvs6, country),
                        n = n(),
                        n_sci21 = sum(!is.na(sci21)),
                        sci21 = mean(sci21, na.rm = T), 
                        n_sci22 = sum(!is.na(sci22)),
                        sci22 = mean(sci22, na.rm = T))

# Let's now find the descriptives for the IVs for both waves

ivdescrip5 <- summarise(group_by(wvs5, country),
                        n = n(),
                        female = mean(as.numeric(female)-1, na.rm = T),
                        age = mean(age, na.rm = T),
                        incdecile = mean(incdecile, na.rm = T),
                        religious = mean(as.numeric(religious)-1, na.rm = T),
                        education = mean(education, na.rm = T),
                        leftright = mean(leftright, na.rm = T),
                        postmater = mean(postmater, na.rm = T),
                        control = mean(control, na.rm = T),
                        trust = mean(trust, na.rm = T))

ivdescrip6 <- summarise(group_by(wvs6, country),
                        n = n(),
                        female = mean(as.numeric(female)-1, na.rm = T),
                        age = mean(age, na.rm = T),
                        incdecile = mean(incdecile, na.rm = T),
                        religious = mean(as.numeric(religious)-1, na.rm = T),
                        education = mean(education, na.rm = T),
                        leftright = mean(leftright, na.rm = T),
                        postmater = mean(postmater, na.rm = T),
                        control = mean(control, na.rm = T),
                        trust = mean(trust, na.rm = T))