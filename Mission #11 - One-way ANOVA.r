# STAT E-100
# Mission #11
# One-way Analysis of Variance

library(dplyr)
group_by(NCbirths, MomRace) %>%
  summarise(
    count = n(),
    mean = mean(BirthWeightGm, na.rm = TRUE),
    sd = sd(BirthWeightGm, na.rm = TRUE)
  )

boxplot(BirthWeightGm ~ MomRace,
        data = NCbirths)

oneway <- aov(BirthWeightGm ~ MomRace, data = NCbirths)
summary(oneway)

library("car")
leveneTest(BirthWeightGm ~ MomRace, NCbirths)

qqnorm(oneway$residuals)
qqline(oneway$residuals, col = 2,lwd=2,lty=2)

x2 <- seq(min(oneway$residuals), max(oneway$residuals), length = 40)
fun <- dnorm(x2, mean = mean(oneway$residuals), sd = sd(oneway$residuals))
hist(oneway$residuals, prob = TRUE, col = "white",
     ylim = c(0, max(fun)),
     main = "Histogram with normal curve")
lines(x2, fun, col = 2, lwd = 2)

# Kolmogorov-Smirnov Test of Normality
ks.test(oneway$residuals, "pnorm", mean=mean(oneway$residuals), sd=sd(oneway$residuals))

# Shapiro-Wilk Test for Normality
shapiro.test(oneway$residuals)

# Bonferroni's correction
pairwise.t.test(NCbirths$BirthWeightGm, NCbirths$MomRace, p.adjust.method="bonferroni")

