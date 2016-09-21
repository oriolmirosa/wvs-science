library(lme4)
library(lmerTest)
library(MuMIn)
library(broom)
library(ggplot2)
library(gridExtra)

load("wvsOECD.Rda")

M1a.wvsOECD <- lmer(sci21 ~ (1 | country), data = wvsOECD)

totVar1a <- 0.02484 + 0.78237
0.80721

ICC1a <- 0.02484 / totVar
0.03077266

M1b.wvsOECD <- lmer(sci22 ~ (1 | country), data = wvsOECD)

totVar1b <- 0.0711 + 0.5581

ICC1b <- 0.0711 / totVar1b 
0.1130006


M2a.wvsOECD.1 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 | country), data = wvsOECD)

M2a.wvsOECD.2 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + leftright2 + leftright3 + postmater + control + trust + wave + (1 | country), data = wvsOECD)

totVar2a <- 0.03301 + 0.74784
0.78085

ICC2a <- 0.03301 / totVar2a
0.04227444

Rsquared <- 1 - (totVar2a / totVar1a)
0.03265569

r.squaredGLMM(M2a.wvsOECD)
R2m        R2c 
0.04683846 0.08713460


M2b.wvsOECD.1 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 | country), data = wvsOECD)

M2b.wvsOECD.2 <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + leftright*leftright + postmater + control + trust + wave + (1 | country), data = wvsOECD)


totVar2b <- 0.0561 + 0.5326
0.5887

ICC2b <- 0.0561 / totVar2b
0.09529472

Rsquared2b <- 1 - (totVar2b / totVar1b)
0.06436745

r.squaredGLMM(M2b.wvsOECD)
R2m        R2c 
0.04928446 0.13987569


M3a.wvsOECD <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 + leftright | country), data = wvsOECD)

M3b.wvsOECD <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 + leftright | country), data = wvsOECD)

anova(M3a.wvsOECD, M2a.wvsOECD)
anova(M3b.wvsOECD, M2b.wvsOECD)



M4a.wvsOECD <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + (1 | country), data = wvsOECD)

totVar4a <- 0.1191 + 0.7466
0.8657

ICC4a <- 0.1191 / totVar4a
0.1375765

Rsquared4a <- 1 - (totVar4a / totVar1a)
-0.07245946

r.squaredGLMM(M4a.wvsOECD)
R2m        R2c 
0.07980347 0.20644108


M4b.wvsOECD <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + gini + governance + polstab + (1 | country), data = wvsOECD)

totVar4b <- 9.6040 + 0.5278
10.1318

ICC4b <- 9.6040 / totVar4b
0.9479066

r.squaredGLMM(M4b.wvsOECD)
R2m       R2c 
0.6269836 0.9805693

M5a.wvsOECD <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + leftright2 + postmater + control + trust + wave + gdp + ginisd + US + mleft + mright + mleft*mright + leftright*ginisd + leftright*mleft + (1 + leftright | country), data = wvsOECD)

R2m      R2c 
0.182618 0.394288

M6a.wvsOECD <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + trust + wave + gdp + gini + US + mleft + mright + mleft*mright + leftright*mleft + (1 + leftright | country), data = wvsOECD)

# Only for waves 5 and 6

M6a.wvsOECDw5 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + trust + gdp + gini + US + mleft + mright + mleft*mright + leftright*mleft + (1 + leftright | country), data = wvsOECD5)

M6a.wvsOECDw6 <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + trust + gdp + gini + US + mleft + mright + mleft*mright + leftright*mleft + (1 + leftright | country), data = wvsOECD6)

R2m       R2c 
0.1760722 0.3998850

ICCa <- 0.276866 / (0.276866 + 0.752683)

# Adding US dummy

wvsOECD$US <- 0

wvsOECD$US[wvsOECD$country == "United States"] <- 1

wvsOECD$US <- as.factor(wvsOECD$US)

US <- wvsOECD[wvsOECD$country == "United States",]


M5b.wvsOECD <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + ginisd + US + mleft + mright + mleft*mright + leftright * US + leftright * mleft + (1 + leftright | country), data = wvsOECD)

M6b.wvsOECD <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + gdp + ginisd + US + mleft + mright + mleft*mright + leftright * US + leftright * mleft + religious*gdp + religious*ginisd + religious*US + religious*mleft + (1 + leftright | country), data = wvsOECD)

R2m       R2c 
0.1410495 0.1870984

M7b.wvsOECD <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + trust + wave + gdp + ginisd + US + mleft + mright + mleft*mright + leftright * US + religious*mright + (1 + leftright + religious | country), data = wvsOECD)

R2m       R2c 
0.1023258 0.1944185

ICCb <- 0.112993 / (0.112993 + 0.521832)

M8b.wvsOECD <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + trust + wave + gdp + gini + US + mleft + mright + mleft*mright + leftright * US + (1 + leftright + religious | country), data = wvsOECD)

R2m       R2c 
0.1097652 0.1694966

M8b.wvsOECD <- lmer(sci22 ~ female + age + age2 + incdecile + religious + education + leftright + trust + wave + religideo + gdp + gini + US + mleft + mright + mleft*mright + leftright * US + (1 + leftright + religious | country), data = wvsOECD)
  

wvsOECD5 <- wvsOECD[wvsOECD$wave == "five",]
wvsOECD6 <- wvsOECD[wvsOECD$wave == "six",]






  
newsci21df <- as.data.frame(predict(M8b.wvsOECD, wvsOECD))

new <- cbind(wvsOECD, newsci21df)

names(new)[names(new) == "predict(M8b.wvsOECD, wvsOECD)"] <- "yhat"

newcountry <- new %>% dplyr::group_by(country) %>% dplyr::summarise(meanyhat = mean(yhat), medianyhat = median(yhat), mleft = mean(mleft), mright = mean(mright))

newcountrydf <- slice(newcountry, 1:19)

newcountrydf



ideology <- data.frame("country" = character(19), "slope" = numeric(19))

ideology$country <- rownames(coef(M8b.wvsOECD)$country)
ideology$slope <- coef(M8b.wvsOECD)$country$leftright
ideology$intercept <- coef(M8b.wvsOECD)$country$"(Intercept)"

xmax <- max(wvsOECD$leftright, na.rm = TRUE)
xmin <- min(wvsOECD$leftright, na.rm = TRUE)

ggplot(ideology) + geom_abline(aes(slope = slope, intercept = intercept, color = country)) + geom_text(aes(x = 0, y = intercept, label = country), hjust = 0, vjust = 0) + scale_x_continuous(limits = c(xmin, xmax)) + scale_y_continuous(limits = c(-1, 1)) + theme(legend.position = "none")

yhats <- read.csv("yhats.csv")
