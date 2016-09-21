library(lme4)
library(lmerTest)
library(MuMIn)
library(broom)
library(ggplot2)
library(gridExtra)

load("wvs70.Rda")

M1a.wvs70.1 <- lmer(sci21 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 | country), data = wvs70)

M1a.wvs70.2 <- lmer(sci21 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 + leftright| country), data = wvs70)

anova(M1a.wvs70.1, M1a.wvs70.2)

# The chisquare test is significant, which means that adding 'leftright' to the
# second level makes it a better model. We keep the second one as the
# benchmark then

summary(M1a.wvs70.1)
r.squaredGLMM(M1a.wvs70.1)

# Now we do the same for the second factor

M1b.wvs70.1 <- lmer(sci22 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 | country), data = wvs70)

M1b.wvs70.2 <- lmer(sci22 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 + leftright | country), data = wvsOECD)

anova(M1b.wvs70.1, M1b.wvs70.2)

# The test is significant again, so we keep 'leftright' at the second level as
# well

summary(M1b.wvs70.1)
r.squaredGLMM(M1b.wvs70.1)

# We have a problem here though, since 'wavesix' is significant, which indicates
# that maybe we cannot add together both waves. It could indicate that the
# economic crisis, which took place between the two waves, does not really
# change the affect for science, whereas it does reduce the trust in science
# as generating ontological insecurity

# Let's start by looking at the graphs of the slopes for both factors, before
# we get to try different specifications of the models

ideology <- data.frame("country" = character(70), "slope" = numeric(70))

ideology$country <- rownames(coef(M1a.wvs70.2)$country)
ideology$slope <- coef(M1a.wvs70.2)$country$leftright
ideology$intercept <- coef(M1a.wvs70.2)$country$"(Intercept)"

xmax <- max(wvsOECD$leftright, na.rm = TRUE)
xmin <- min(wvsOECD$leftright, na.rm = TRUE)

ggplot(ideology) + geom_abline(aes(slope = slope, intercept = intercept, color = country)) + geom_text(aes(x = 0, y = intercept, label = country), hjust = 0, vjust = 0) + scale_x_continuous(limits = c(xmin, xmax)) + scale_y_continuous(limits = c(-1, 1)) + theme(legend.position = "none")

ideology2 <- data.frame("country" = character(19), "slope" = numeric(19))

ideology2$country <- rownames(coef(M1b.wvs70.2)$country)
ideology2$slope <- coef(M1b.wvs70.2)$country$leftright
ideology2$intercept <- coef(M1b.wvs70.2)$country$"(Intercept)"

ggplot(ideology2) + geom_abline(aes(slope = slope, intercept = intercept, color = country)) + geom_text(aes(x = 0, y = intercept, label = country), hjust = 0, vjust = 0) + scale_x_continuous(limits = c(xmin, xmax)) + scale_y_continuous(limits = c(-0.5, 0.75)) + theme(legend.position = "none")

# Let's see what happens when we add some country-level variables to the
# intercept

M2a.wvs70 <- lmer(sci21 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + mleftright + (1 + leftright | country), data = wvs70)

M2b.wvs70 <- lmer(sci22 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + mleftright + (1 + leftright | country), data = wvs70)

# mleftright is not significant in either

# Let's now add age2 to both models to see what happens

M3a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 + leftright | country), data = wvs70)

M3b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 + leftright | country), data = wvs70)

# It is significant in a, and significant in b, but in this case it makes
# age not significant anymore. Why? In any case, we will leave it in in all the
# following models

# Now we try meducation

M4a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + meducation + (1 + leftright | country), data = wvs70)

M4b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + meducation + (1 + leftright | country), data = wvs70)

# meducation is not significant in either

# Now we try mpostmater

M5a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + (1 + leftright | country), data = wvs70)

M5b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + (1 + leftright | country), data = wvs70)

# mpostmater is significant in a, but not in b, so we will keep it in the a
# models

# Now we try mcontrol

M6a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + mcontrol + (1 + leftright | country), data = wvs70)

M6b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mcontrol + (1 + leftright | country), data = wvs70)

# mcontrol is not significant in either, so I will not include it. However,
# it is significant in a if mpostmater is not included, so I need to think
# about what that means

# Now we try mtrust

M7a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + mtrust + (1 + leftright | country), data = wvs70)

M7b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mtrust + (1 + leftright | country), data = wvs70)

# In a, mtrust on its own is not significant, nor is it with mpostmater
# included, so I will not include it. In b, it is not significant (just as trust
# is not in the basic model), so I will not include it.

# Let's now add gdp

M8a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + (1 + leftright | country), data = wvs70)

r.squaredGLMM(M8a.wvs70)

M8b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + (1 + leftright | country), data = wvs70)

# It is significant and positive in both, meaning that the wealthier the country
# is, the more its citizens value science (I tried it in a without postmater,
# and the result is the same). I will therefore keep gdp in all models. In the
# previous version of the analysis, gdp was negative in a, so I should check
# what's happening here.

# Let's now try gini

M9a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + (1 + leftright | country), data = wvs70)

M9b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + (1 + leftright | country), data = wvs70)

# Gini is again significant for both factors, but this time it's negative for
# a and positive for b (before it was negative in both).

# Now we try governance

M10a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + (1 + leftright | country), data = wvs70)

r.squaredGLMM(M10a.wvs70)

M10b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + (1 + leftright | country), data = wvs70)

# In a, governance was not significant if we put it without gdp or gini, but
# it becomes significant (and positive) when I add gpd and gini, although then
# gdp is not significant anymore. Ugh.

# In b, it is significant (and positive) both alone and with gdp and gini, and
# as in a it makes gdp insignificant.

# Now we add political stability

M11a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + polstab + (1 + leftright | country), data = wvs70)

M11b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + (1 + leftright | country), data = wvs70)

# In a, it is not significant on its own or with gdp and gini, so we leave it
# out. In b, it was significant (and positive) on its own, as well as with the
# other governance indicators, so we keep it in.

# Now we add the polarization variable, although we need to be careful
# here because it implies having many fewer countries (because of lack of data).

M12a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + polari + (1 + leftright | country), data = wvs70)

M12b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + polari + (1 + leftright | country), data = wvs70)

# In a, it was not significant on its own or with the other governance
# variables, so I will not use it.

# In b, it is significant and negative on its own, but insignificant when added
# to the other governance variables, so we remove it. Actually, if we add all
# other governance variables except polstab, then polari becomes significant
# and positive, which is strange.

# Finally, the last measure for us to try is the other variable dealing with
# polarization, distmleftright

M13a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + distmleftright + (1 + leftright | country), data = wvs70)

M13b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + distmleftright + (1 + leftright | country), data = wvs70)

# In both a and b, it was not significant on its own or with the other
# governance variables, so we exclude it from both.

## Ok, so at this point, before we start trying interaction effects, the main
# models are the following

M14a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + (1 + leftright | country), data = wvs70)

M14b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + (1 + leftright | country), data = wvs70)

# Let's now look at the interactions of leftright with the other variables

# First, with female

M15a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + female * leftright + (1 + leftright | country), data = wvs70)

M15b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + female * leftright + (1 + leftright | country), data = wvs70)

# It is not significant in either.

# Now, interaction with age

M16a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + age * leftright + (1 + leftright | country), data = wvs70)

M16b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + age * leftright + (1 + leftright | country), data = wvs70)

# It is not significant in either.

# Now, interaction with age2

M17a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + age2 * leftright + (1 + leftright | country), data = wvs70)

M17b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + age2 * leftright + (1 + leftright | country), data = wvs70)

# It is not significant in either.

# Now, interaction with incdecile

M18a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + incdecile * leftright + (1 + leftright | country), data = wvs70)

M18b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + incdecile * leftright + (1 + leftright | country), data = wvs70)

# It is significant and negative in both, so we keep it

# Now, interaction with religious

M19a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + incdecile * leftright + religious * leftright + (1 + leftright | country), data = wvs70)

M19b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + incdecile * leftright + religious * leftright + (1 + leftright | country), data = wvs70)

# In a, either with the other interaction or without, this one is not
# significant. In b, it is in both cases positive and significant, so we keep it

# Now, interaction with education

M20a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + incdecile * leftright + religious * leftright + education * leftright + (1 + leftright | country), data = wvs70)

M20b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + incdecile * leftright + religious * leftright + education * leftright + (1 + leftright | country), data = wvs70)

# In a, the interaction is significant and negative on its own. When put with
# the others it stays the same, but it makes the one with religion insignificant

# In b, the interaction is significant and negative on its own, but it is not
# significant with the others included in the model, so I leave it out.

# Now, interaction with postmater

M21a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + incdecile * leftright + religious * leftright + education * leftright + postmater * leftright + (1 + leftright | country), data = wvs70)

M21b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + incdecile * leftright + religious * leftright + postmater * leftright + (1 + leftright | country), data = wvs70)

# In a, on its own, the interaction is significant and positive, and it remains
# so with the others, so I leave it

# In b, on its own, it is significant and negative, and it remains so with the
# others, so I leave it

# Now, interaction with control

M22a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + incdecile * leftright + religious * leftright + education * leftright + postmater * leftright + control * leftright + (1 + leftright | country), data = wvs70)

M22b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + incdecile * leftright + religious * leftright + postmater * leftright + control * leftright + (1 + leftright | country), data = wvs70)

# In a, on its own, the interaction is significant and negative. The same when
# included with the other interactions, although it does make some of them
# not significant.

# In b, it is not significant on its own or with the others, so we exclude it.

# Now, we try the interaction with trust

M23a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + incdecile * leftright + religious * leftright + education * leftright + postmater * leftright + control * leftright + trust * leftright + (1 + leftright | country), data = wvs70)

M23b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + incdecile * leftright + religious * leftright + postmater * leftright + control * leftright + trust * leftright + (1 + leftright | country), data = wvs70)

# In a, on its own, the interaction is significant and negative. The same when
# included with the other interactions, although it does make some of them
# not significant.

# In b, it is negative and significant on its own, and the same with the others,
# although it makes the interaction with control insignificant.

# Now, let's try the interaction with gdp

M24a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + incdecile * leftright + religious * leftright + education * leftright + postmater * leftright + control * leftright + trust * leftright + gdp * leftright + (1 + leftright | country), data = wvs70)

M24b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + incdecile * leftright + religious * leftright + postmater * leftright + control * leftright + trust * leftright + gdp * leftright + (1 + leftright | country), data = wvs70)

# In a, it is not significant on its own or with the other interactions, so we
# leave it out. The same in b.

# Now, let's try the interaction with gini

M25a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + incdecile * leftright + religious * leftright + education * leftright + postmater * leftright + control * leftright + trust * leftright + gini * leftright + (1 + leftright | country), data = wvs70)

M25b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + incdecile * leftright + religious * leftright + postmater * leftright + control * leftright + trust * leftright + gini * leftright + (1 + leftright | country), data = wvs70)

# In a, it is not significant on its own or with the other interactions, so we
# leave it out. The same in b.

# Next, we try the interaction with governance

M26a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + incdecile * leftright + religious * leftright + education * leftright + postmater * leftright + control * leftright + trust * leftright + governance * leftright + (1 + leftright | country), data = wvs70)

M26b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + incdecile * leftright + religious * leftright + postmater * leftright + control * leftright + trust * leftright + governance * leftright + (1 + leftright | country), data = wvs70)

# In a, it is not significant on its own or with the other interactions, so we
# leave it out. The same in b.

# Finally, we try the interaction with polstab, but only for b since that is the
# only model in which we have the main effects for polstab

M27b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + incdecile * leftright + religious * leftright + postmater * leftright + control * leftright + trust * leftright + polstab * leftright + (1 + leftright | country), data = wvs70)

# It is not significant on its own or with the other interactions, so we leave
# it out.

# OK!! Given all this, the final models for each are the following ones:

MFa.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + mpostmater + gdp + gini + governance + incdecile * leftright + religious * leftright + education * leftright + postmater * leftright + control * leftright + trust * leftright + (1 + leftright | country), data = wvs70)

MFb.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + incdecile * leftright + religious * leftright + postmater * leftright + control * leftright + trust * leftright + (1 + leftright | country), data = wvs70)

r.squaredGLMM(MFa.wvs70)
r.squaredGLMM(MFb.wvs70)

ICC.MFa.wvs70 <- 0.051445 / (0.051445 + 0.004679 + 0.694866)
ICC.MFb.wvs70 <- 0.320746 / (0.320746 + 0.003689 + 0.512379)

# In order to figure out if this makes sense, let's do the same analysis but
# with wvs5.43 and wvs6.53

MFa.wvs5.43 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + mpostmater + gdp + gini + governance + incdecile * leftright + religious * leftright + education * leftright + postmater * leftright + control * leftright + trust * leftright + (1 + leftright | country), data = wvs5.43)

MFb.wvs5.43 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + gdp + gini + governance + polstab + incdecile * leftright + religious * leftright + postmater * leftright + control * leftright + trust * leftright + (1 + leftright | country), data = wvs5.43)

r.squaredGLMM(MFa.wvs5.43)
r.squaredGLMM(MFb.wvs5.43)

MFa.wvs6.53 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + mpostmater + gdp + gini + governance + incdecile * leftright + religious * leftright + education * leftright + postmater * leftright + control * leftright + trust * leftright + (1 + leftright | country), data = wvs6.53)

MFb.wvs6.53 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + gdp + gini + governance + polstab + incdecile * leftright + religious * leftright + postmater * leftright + control * leftright + trust * leftright + (1 + leftright | country), data = wvs6.53)

r.squaredGLMM(MFa.wvs6.53)
r.squaredGLMM(MFb.wvs6.53)











# Only b is significant, and positive: when people are religious, the negative
# effect of conservatism on trust in science (in terms of ontological
# insecurity) is tamed

# Let's add the interaction of leftright with postmaterial

M8a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + postmater * leftright + (1 + leftright | country), data = wvs70)

M8b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + religious * leftright + postmater * leftright + (1 + leftright | country), data = wvs70)

# It is significant in both, positive in a, and negative in b. That is, the more
# postmaterial values the individual has, the effect of leftright on trust in
# science is incremented in a (it was positive already), and it is decreased
# (it was negative already) in b. That is, postmaterialism amplifies the effect
# of ideology in both cases

M8a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + postmater * leftright + (1 + leftright | country), data = wvs70)

M8b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + religious * leftright + postmater * leftright + (1 + leftright | country), data = wvs70)

# Now we try the interactions with control

M9a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + postmater * leftright + control * leftright + (1 + leftright | country), data = wvs70)

M9b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + religious * leftright + postmater * leftright + control * leftright + (1 + leftright | country), data = wvs70)

# It's only significant (and negative) in a, which means that more 'control'
# reduces the effect of conservatism on trust in science

# Finally, let's look at the interaction with trust

M10a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + postmater * leftright + control * leftright + trust * leftright + (1 + leftright | country), data = wvs70)

M10b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + religious * leftright + postmater * leftright + trust * leftright + (1 + leftright | country), data = wvs70)

# Trust is significant and negative in both cases, which means that the more
# trusting an individual is, the less positive the effect of ideology is on a,
# and the more negative the effect of ideology is on b

# Now we'll try interactions with gdp

M11a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + postmater * leftright + control * leftright + trust * leftright + gdp * leftright + (1 + leftright | country), data = wvs70)

M11b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + religious * leftright + postmater * leftright + trust * leftright + gdp * leftright + (1 + leftright | country), data = wvs70)

# Neither is significant, so we move to interactions with gini

M12a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + postmater * leftright + control * leftright + trust * leftright + gini * leftright + (1 + leftright | country), data = wvs70)

M12b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + religious * leftright + postmater * leftright + trust * leftright + gini * leftright + (1 + leftright | country), data = wvs70)

# Neither is significant, so we move to interactions with education

M13a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + postmater * leftright + control * leftright + trust * leftright + education * leftright + (1 + leftright | country), data = wvs70)

M13b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + religious * leftright + postmater * leftright + trust * leftright + education * leftright + (1 + leftright | country), data = wvs70)

# It's significant and negative for a, which means that the effect of 
# conservatism on trust in science is less positive when education increases.
# This seems to be the effect that we would expect from sophistication. The
# interaction is not significant, however, which could be interpreted as
# meaning that factor b is more intuitive than a, which requires more reasining.
# However, we see that education has a positive effect on a, which makes this
# rationale suspect

# Now we'll try the interaction with age

M14a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + postmater * leftright + control * leftright + trust * leftright + education * leftright + age * leftright + (1 + leftright | country), data = wvs70)

M14b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + religious * leftright + postmater * leftright + trust * leftright + age * leftright + (1 + leftright | country), data = wvs70)

# Again, the effect is only significant for a, and negative, so the positive
# effect of conservatism on trust in science is reduced with age

# We'll now check the interaction with age2

M15a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + postmater * leftright + control * leftright + trust * leftright + education * leftright + age * leftright + age2 * leftright + (1 + leftright | country), data = wvs70)

M15b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + religious * leftright + postmater * leftright + trust * leftright + age * leftright + age2 * leftright + (1 + leftright | country), data = wvs70)

# This interaction has no effect on either

# As a last interaction, we'll see the effect of female on leftright

M16a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + postmater * leftright + control * leftright + trust * leftright + education * leftright + age * leftright + female * leftright + (1 + leftright | country), data = wvs70)

M16b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + religious * leftright + postmater * leftright + trust * leftright + female * leftright + (1 + leftright | country), data = wvs70)

# Neither is significant. Let's recapitulate and make Mel 17 the main one for
# each factor

M17a.wvs70 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + postmater * leftright + control * leftright + trust * leftright + education * leftright + age * leftright + (1 + leftright | country), data = wvs70)

M17b.wvs70 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + religious * leftright + postmater * leftright + trust * leftright + (1 + leftright | country), data = wvs70)

# Now let's create the graphs again for the final Mels

ideology3 <- data.frame("country" = character(60), "slope" = numeric(60))

ideology3$country <- rownames(coef(M17a.wvs70)$country)
ideology3$slope <- coef(M17a.wvs70)$country$leftright
ideology3$intercept <- coef(M17a.wvs70)$country$"(Intercept)"

xmax <- max(wvs70$leftright, na.rm = TRUE)
xmin <- min(wvs70$leftright, na.rm = TRUE)

ggplot(ideology3) + geom_abline(aes(slope = slope, intercept = intercept, color = country)) + geom_text(aes(x = 0, y = intercept, label = country), hjust = 0, vjust = 0) + scale_x_continuous(limits = c(xmin, xmax)) + scale_y_continuous(limits = c(-1, 0.75)) + theme(legend.position = "none")

ideology4 <- data.frame("country" = character(60), "slope" = numeric(60))

ideology4$country <- rownames(coef(M17b.wvs70)$country)
ideology4$slope <- coef(M17b.wvs70)$country$leftright
ideology4$intercept <- coef(M17b.wvs70)$country$"(Intercept)"

ggplot(ideology4) + geom_abline(aes(slope = slope, intercept = intercept, color = country)) + geom_text(aes(x = 0, y = intercept, label = country), hjust = 0, vjust = 0) + scale_x_continuous(limits = c(xmin, xmax)) + scale_y_continuous(limits = c(-0.3, 1)) + theme(legend.position = "none")
