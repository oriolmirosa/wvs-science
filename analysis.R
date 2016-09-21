library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(broom)
library(ggplot2)
library(gridExtra)

Mod1a.wvs5 <- lmer(sci21 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + (1 + leftright + meanpolari | country), data = wvs5sd)

Mod1a.wvs5b <- lmer(sci21 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + (1 + leftright | country), data = wvs5sd)

Mod1b.wvs5 <- lmer(sci22 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + (1 + leftright | country), data = wvs5sd)

Mod2a.wvs5 <- lmer(sci21 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + (1 | country), data = wvs5sd)

Mod2b.wvs5 <- lmer(sci22 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + (1 | country), data = wvs5sd)

anova(Mod1a.wvs5, Mod2a.wvs5)
anova(Mod1b.wvs5, Mod2b.wvs5)

# In order to check R^2 for mixed effect models, there is the sem.model.fits
# function from the piecewiseSEM package. There are two versions of R^2:
# the marginal R2 and describes the proportion of variance explained by the 
# fixed factor(s) alone. The conditional R^2 describes the proportion of 
# variance explained by both the fixed and random factors.
# See http://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/ for
# the reference to the paper where the idea of these 2 R^2s comes from

sem.model.fits(Mod1a.wvs5)

Mod1a.wvs6 <- lmer(sci21 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + (1 + leftright | country), data = wvs6sd)

Mod1b.wvs6 <- lmer(sci22 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + (1 + leftright | country), data = wvs6sd)

Mod2a.wvs6 <- lmer(sci21 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + (1 | country), data = wvs6sd)

Mod2b.wvs6 <- lmer(sci22 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + (1 | country), data = wvs6sd)

anova(Mod1a.wvs6, Mod2a.wvs6)
anova(Mod1b.wvs6, Mod2b.wvs6)

tidy.a.wvs5 <- tidy(Mod1a.wvs5)
tidy.a.wvs5$source <- "sci21"

tidy.b.wvs5 <- tidy(Mod1b.wvs5)
tidy.b.wvs5$source <- "sci22"

tidy.wvs5 <- rbind(tidy.a.wvs5, tidy.b.wvs5)
tidy.wvs5 <- filter(tidy.wvs5, term != "(Intercept)" & group == "fixed")

tidy.a.wvs6 <- tidy(Mod1a.wvs6)
tidy.a.wvs6$source <- "sci21"

tidy.b.wvs6 <- tidy(Mod1b.wvs6)
tidy.b.wvs6$source <- "sci22"

tidy.wvs6 <- rbind(tidy.a.wvs6, tidy.b.wvs6)
tidy.wvs6 <- filter(tidy.wvs6, term != "(Intercept)" & group == "fixed")

plot1 <- ggplot(tidy.wvs5, aes(y = source, x = estimate, xmin = estimate - std.error * 2,
                            xmax = estimate + std.error * 2)) + 
  geom_vline(xintercept = 0, colour = "black", lty = 2) +
  geom_point() + 
  geom_errorbarh(height = 0) + 
  ggtitle("World Values Survey - Wave 5") +
  theme(axis.title.y = element_blank()) +
  facet_grid(. ~ term, scales = "free")

plot2 <- ggplot(tidy.wvs6, aes(y = source, x = estimate, xmin = estimate - std.error * 2,
                 xmax = estimate + std.error * 2)) + 
  geom_vline(xintercept = 0, colour = "black", lty = 2) +
  geom_point() + 
  geom_errorbarh(height = 0) + 
  ggtitle("World Values Survey - Wave 6") +
  theme(axis.title.y = element_blank()) +
  facet_grid(. ~ term, scales = "free")

png("graph0.png", width = 10, height = 4, units = "in", res = 300)
grid.arrange(plot1, plot2)
dev.off()

ideology <- data.frame("country" = character(14), "slope" = numeric(14))

ideology$country <- rownames(coef(Mod1a.wvs5)$country)
ideology$slope <- coef(Mod1a.wvs5)$country$leftright
ideology$intercept <- coef(Mod1a.wvs5)$country$"(Intercept)"

ggplot(wvs5sd, aes(x = leftright, y = sci21, color = country)) + geom_jitter(size = .5) + geom_abline(data = ideology, aes(slope = slope, intercept = intercept, color = country))

xmax <- max(wvs5sd$leftright, na.rm = TRUE)
xmin <- min(wvs5sd$leftright, na.rm = TRUE)

ymax <- max(wvs5sd$sci21, na.rm = TRUE)
ymin <- min(wvs5sd$sci21, na.rm = TRUE)

ggplot(ideology) + geom_abline(aes(slope = slope, intercept = intercept, color = country)) + geom_text(aes(x = -1.5, y = intercept, label = country), hjust = 0, vjust = 0) + scale_x_continuous(limits = c(xmin, xmax)) + scale_y_continuous(limits = c(-0.3, 0.75))

####

load("wvssd.Rda")

Mod1a.wvssd <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + meanlr + governance + (1 + leftright | country), data = wvssd)

Mod1a.wvssd <- lmer(sci21 ~ female + age + age2 + incdecile + religious + education + leftright + postmater + control + trust + wave + lfslopes + (1 + leftright | country), data = wvssd)

Mod1a.wvssd <- lmer(sci21 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + meanlr + (1 + leftright | country), data = wvssd)

Mod1a.wvssd2 <- lmer(sci21 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 + leftright | country), data = wvssd)

anova(Mod1a.wvssd, Mod1a.wvssd2)

Mod1b.wvssd2 <- lmer(sci22 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + governance + (1 + leftright | country), data = wvssd)

Mod1b.wvssd2 <- lmer(sci22 ~ female + age + incdecile + religious + education + leftright + postmater + control + trust + wave + (1 + leftright | country), data = wvssd)

anova(Mod1b.wvssd, Mod1b.wvssd2)

ideology <- data.frame("country" = character(16), "slope" = numeric(16))

ideology$country <- rownames(coef(Mod1a.wvssd)$country)
ideology$slope <- coef(Mod1a.wvssd)$country$leftright
ideology$intercept <- coef(Mod1a.wvssd)$country$"(Intercept)"

xmax <- max(wvssd$leftright, na.rm = TRUE)
xmin <- min(wvssd$leftright, na.rm = TRUE)

ggplot(ideology) + geom_abline(aes(slope = slope, intercept = intercept, color = country)) + geom_text(aes(x = 0, y = intercept, label = country), hjust = 0, vjust = 0) + scale_x_continuous(limits = c(xmin, xmax)) + scale_y_continuous(limits = c(-0.75, 2.5))

ideology2 <- data.frame("country" = character(16), "slope" = numeric(16))

ideology2$country <- rownames(coef(Mod1b.wvssd2)$country)
ideology2$slope <- coef(Mod1b.wvssd2)$country$leftright
ideology2$intercept <- coef(Mod1b.wvssd2)$country$"(Intercept)"

ggplot(ideology2) + geom_abline(aes(slope = slope, intercept = intercept, color = country)) + geom_text(aes(x = 0, y = intercept, label = country), hjust = 0, vjust = 0) + scale_x_continuous(limits = c(xmin, xmax)) + scale_y_continuous(limits = c(-0.5, 0.75))

superideology <- merge(ideology, ideology2, by = "country")
superideology$diff <- superideology$slope.x - superideology$slope.y
