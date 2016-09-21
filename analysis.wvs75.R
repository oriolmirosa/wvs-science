library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(broom)
library(ggplot2)
library(gridExtra)

load("wvs75.Rda")

Mod1a.wvs75.1 <- lmer(sci21 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 + leftright | country), data = wvs75)

Mod1a.wvs75.2 <- lmer(sci21 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 | country), data = wvs75)

anova(Mod1a.wvs75.1, Mod1a.wvs75.2)

# The chisquare test is significant, which means that adding 'leftright' to the
# second level makes it a better model. We keep the first one as the
# benchmark then

# Now we do the same for the second factor

Mod1b.wvs75.1 <- lmer(sci22 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 + leftright | country), data = wvs75)

Mod1b.wvs75.2 <- lmer(sci22 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 | country), data = wvs75)

anova(Mod1b.wvs75.1, Mod1b.wvs75.2)

# The test is significant again, so we keep 'leftright' at the second level as
# well

# We have a problem here though, since 'wavesix' is significant, which indicates
# that maybe we cannot add together both waves. It could indicate that the
# economic crisis, which took place between the two waves, does not really
# change the affect for science, whereas it does reduce the trust in science
# as generating ontological insecurity

# Let's start by looking at the graphs of the slopes for both factors, before
# we get to try different specifications of the models

ideology <- data.frame("country" = character(70), "slope" = numeric(70))

ideology$country <- rownames(coef(Mod1a.wvs75.1)$country)
ideology$slope <- coef(Mod1a.wvs75.1)$country$leftright
ideology$intercept <- coef(Mod1a.wvs75.1)$country$"(Intercept)"

xmax <- max(wvs75$leftright, na.rm = TRUE)
xmin <- min(wvs75$leftright, na.rm = TRUE)

ggplot(ideology) + geom_abline(aes(slope = slope, intercept = intercept, color = country)) + geom_text(aes(x = 0, y = intercept, label = country), hjust = 0, vjust = 0) + scale_x_continuous(limits = c(xmin, xmax)) + scale_y_continuous(limits = c(-1, 1)) + theme(legend.position = "none")

ideology2 <- data.frame("country" = character(70), "slope" = numeric(70))

ideology2$country <- rownames(coef(Mod1b.wvs75.1)$country)
ideology2$slope <- coef(Mod1b.wvs75.1)$country$leftright
ideology2$intercept <- coef(Mod1b.wvs75.1)$country$"(Intercept)"

ggplot(ideology2) + geom_abline(aes(slope = slope, intercept = intercept, color = country)) + geom_text(aes(x = 0, y = intercept, label = country), hjust = 0, vjust = 0) + scale_x_continuous(limits = c(xmin, xmax)) + scale_y_continuous(limits = c(-0.5, 0.75)) + theme(legend.position = "none")

superideology <- merge(ideology, ideology2, by = "country")
superideology$diff <- superideology$slope.x - superideology$slope.y
arrange(superideology, desc(diff))

# Let's see what happens when we add some country-level variables to the
# intercept

Mod2a.wvs75 <- lmer(sci21 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + meanlr + (1 + leftright | country), data = wvs75)

Mod2b.wvs75 <- lmer(sci22 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + meanlr + (1 + leftright | country), data = wvs75)

# meanlr is not significant in either

# Let's now add age2 to both models to see what happens

Mod3a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 + leftright | country), data = wvs75)

Mod3b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 + leftright | country), data = wvs75)

# It is supersignificant in a, and significant in b, but in this case it makes
# age not significant anymore. Why?

# Let's now add gdppc

Mod4a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + (1 + leftright | country), data = wvs75)

Mod4b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + (1 + leftright | country), data = wvs75)

# It is significant and negative in a, meaning that the wealthier a country is,
# the least its citizens trust science. It is also significant, but positive in
# b, meaning that the wealthier the country is, the more its citizens value
# science

Mod5a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + (1 + leftright | country), data = wvs75)

Mod5b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + (1 + leftright | country), data = wvs75)

# Gini is again significant for both factors, but this time it's negative for
# both, meaning that the more inequality, the less trust in science

Mod6a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + governance + (1 + leftright | country), data = wvs75)

Mod6b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + governance + (1 + leftright | country), data = wvs75)

# Governance is not significant in a or b, and in b it makes gini insignificant,
# so I remove it from the models. I also tried (although is not here because I
# only got to it later) the effect of the country's political stability, but it
# is also not significant for both. Models 5, then, seem to be so far the most
# appropriate

# From the basis of models 5, I will now introduce some interactions, to see
# what happens

Mod7a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + religious * leftright + (1 + leftright | country), data = wvs75)

Mod7b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + religious * leftright + (1 + leftright | country), data = wvs75)

# Only b is significant, and positive: when people are religious, the negative
# effect of conservatism on trust in science (in terms of ontological
# insecurity) is tamed

# Let's add the interaction of leftright with postmaterial

Mod8a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + postmater * leftright + (1 + leftright | country), data = wvs75)

Mod8b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + religious * leftright + postmater * leftright + (1 + leftright | country), data = wvs75)

# It is significant in both, positive in a, and negative in b. That is, the more
# postmaterial values the individual has, the effect of leftright on trust in
# science is incremented in a (it was positive already), and it is decreased
# (it was negative already) in b. That is, postmaterialism amplifies the effect
# of ideology in both cases

Mod8a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + postmater * leftright + (1 + leftright | country), data = wvs75)

Mod8b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + religious * leftright + postmater * leftright + (1 + leftright | country), data = wvs75)

# Now we try the interactions with control

Mod9a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + postmater * leftright + control * leftright + (1 + leftright | country), data = wvs75)

Mod9b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + religious * leftright + postmater * leftright + control * leftright + (1 + leftright | country), data = wvs75)

# It's only significant (and negative) in a, which means that more 'control'
# reduces the effect of conservatism on trust in science

# Finally, let's look at the interaction with trust

Mod10a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + postmater * leftright + control * leftright + trust * leftright + (1 + leftright | country), data = wvs75)

Mod10b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + religious * leftright + postmater * leftright + trust * leftright + (1 + leftright | country), data = wvs75)

# Trust is significant and negative in both cases, which means that the more
# trusting an individual is, the less positive the effect of ideology is on a,
# and the more negative the effect of ideology is on b

# Now we'll try interactions with gdppc

Mod11a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + postmater * leftright + control * leftright + trust * leftright + gdppc * leftright + (1 + leftright | country), data = wvs75)

Mod11b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + religious * leftright + postmater * leftright + trust * leftright + gdppc * leftright + (1 + leftright | country), data = wvs75)

# Neither is significant, so we move to interactions with gini

Mod12a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + postmater * leftright + control * leftright + trust * leftright + gini * leftright + (1 + leftright | country), data = wvs75)

Mod12b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + religious * leftright + postmater * leftright + trust * leftright + gini * leftright + (1 + leftright | country), data = wvs75)

# Neither is significant, so we move to interactions with education

Mod13a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + postmater * leftright + control * leftright + trust * leftright + education * leftright + (1 + leftright | country), data = wvs75)

Mod13b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + religious * leftright + postmater * leftright + trust * leftright + education * leftright + (1 + leftright | country), data = wvs75)

# It's significant and negative for a, which means that the effect of 
# conservatism on trust in science is less positive when education increases.
# This seems to be the effect that we would expect from sophistication. The
# interaction is not significant, however, which could be interpreted as
# meaning that factor b is more intuitive than a, which requires more reasining.
# However, we see that education has a positive effect on a, which makes this
# rationale suspect

# Now we'll try the interaction with age

Mod14a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + postmater * leftright + control * leftright + trust * leftright + education * leftright + age * leftright + (1 + leftright | country), data = wvs75)

Mod14b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + religious * leftright + postmater * leftright + trust * leftright + age * leftright + (1 + leftright | country), data = wvs75)

# Again, the effect is only significant for a, and negative, so the positive
# effect of conservatism on trust in science is reduced with age

# We'll now check the interaction with age2

Mod15a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + postmater * leftright + control * leftright + trust * leftright + education * leftright + age * leftright + age2 * leftright + (1 + leftright | country), data = wvs75)

Mod15b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + religious * leftright + postmater * leftright + trust * leftright + age * leftright + age2 * leftright + (1 + leftright | country), data = wvs75)

# This interaction has no effect on either

# As a last interaction, we'll see the effect of female on leftright

Mod16a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + postmater * leftright + control * leftright + trust * leftright + education * leftright + age * leftright + female * leftright + (1 + leftright | country), data = wvs75)

Mod16b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + religious * leftright + postmater * leftright + trust * leftright + female * leftright + (1 + leftright | country), data = wvs75)

# Neither is significant. Let's recapitulate and make Model 17 the main one for
# each factor

Mod17a.wvs75 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + postmater * leftright + control * leftright + trust * leftright + education * leftright + age * leftright + (1 + leftright | country), data = wvs75)

Mod17b.wvs75 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdppc + gini + religious * leftright + postmater * leftright + trust * leftright + (1 + leftright | country), data = wvs75)

# Now let's create the graphs again for the final models

ideology3 <- data.frame("country" = character(60), "slope" = numeric(60))

ideology3$country <- rownames(coef(Mod17a.wvs75)$country)
ideology3$slope <- coef(Mod17a.wvs75)$country$leftright
ideology3$intercept <- coef(Mod17a.wvs75)$country$"(Intercept)"

xmax <- max(wvs75$leftright, na.rm = TRUE)
xmin <- min(wvs75$leftright, na.rm = TRUE)

ggplot(ideology3) + geom_abline(aes(slope = slope, intercept = intercept, color = country)) + geom_text(aes(x = 0, y = intercept, label = country), hjust = 0, vjust = 0) + scale_x_continuous(limits = c(xmin, xmax)) + scale_y_continuous(limits = c(-1, 0.75)) + theme(legend.position = "none")

ideology4 <- data.frame("country" = character(60), "slope" = numeric(60))

ideology4$country <- rownames(coef(Mod17b.wvs75)$country)
ideology4$slope <- coef(Mod17b.wvs75)$country$leftright
ideology4$intercept <- coef(Mod17b.wvs75)$country$"(Intercept)"

ggplot(ideology4) + geom_abline(aes(slope = slope, intercept = intercept, color = country)) + geom_text(aes(x = 0, y = intercept, label = country), hjust = 0, vjust = 0) + scale_x_continuous(limits = c(xmin, xmax)) + scale_y_continuous(limits = c(-0.3, 1)) + theme(legend.position = "none")
