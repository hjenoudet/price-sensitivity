########################################
## Load the necessary libraries
########################################
library(readr)
library(ggplot2)

########################################
## Load the data
## The data frame is called Expedia.
########################################
load("price-sensitivity-data.Rdata")
Table1 <- summary(Expedia)
Table1

########################################
## EDA
##
## For Booked?
########################################
PricePerNight <- Expedia$PricePerNight
Booked <- Expedia$`Booked?`

## Base R scatterplot + linear regression line
plot(
  PricePerNight,
  Booked,
  main = "Figure 1. Booked versus Price Per Night (Linear Relationship)",
  xlab = "Price Per Night ($)",
  ylab = "Booked?"
)
abline(lm(Booked ~ PricePerNight))

## ggplot2 with LOESS
ggplot(Expedia, aes(x = PricePerNight, y = `Booked?`)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Figure 2. Booked versus Price Per Night (loess Relationship)",
    x = "Price Per Night ($)",
    y = "Booked?"
  ) +
  theme_minimal()

## ggplot2 by Region
ggplot(Expedia, aes(x = PricePerNight, y = `Booked?`, color = Region)) +
  geom_point() +
  geom_smooth(method = lm) +
  scale_color_manual(values = c("Hawaii" = "blue", "Miami" = "red", "Washinton DC" = "yellow", "Las Vegas" = "green")) +
  labs(
    title = "Figure 3. Booked versus Price Per Night Per Region",
    x = "Price Per Night ($)",
    y = "Booked?",
    color = "Region"
  ) +
  theme_minimal()

########################################
## For Nights
########################################
Nights <- Expedia$Nights

## Base R scatterplot + linear regression line
plot(
  PricePerNight,
  Nights,
  main = "Figure 4. Nights Booked versus Price Per Night (Linear Relationship",
  xlab = "Price Per Night ($)",
  ylab = "Nights Booked"
)
abline(lm(Nights ~ PricePerNight))

## ggplot2 with LOESS
ggplot(Expedia, aes(x = PricePerNight, y = Nights)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Figure 5. Nights Booked versus Price Per Night (loess Relationship)",
    x = "Price Per Night ($)",
    y = "Nights"
  ) +
  theme_minimal()

## ggplot2 by Region
ggplot(Expedia, aes(x = PricePerNight, y = Nights, color = Region)) +
  geom_point() +
  geom_smooth(method = lm) +
  scale_color_manual(values = c("Hawaii" = "blue", "Miami" = "red", "Washinton DC" = "yellow", "Las Vegas" = "green")) +
  labs(
    title = "Figure 6. Nights Booked versus Price Per Night Per Region",
    x = "Price Per Night ($)",
    y = "Nights Booked",
    color = "Region"
  ) +
  theme_minimal()

########################################
## Developing one or more tentative regression models
##
## For Booked?
########################################
cor(Booked, PricePerNight)
cor(log10(Booked), PricePerNight)
cor(Booked, log10(PricePerNight))
cor(Booked, PricePerNight + PricePerNight^2)
cor(Booked, PricePerNight + PricePerNight^2 + PricePerNight^3)
cor(exp(Booked), PricePerNight)
cor(Booked, exp(PricePerNight))

Table2 <- lm(Booked ~ PricePerNight)
summary(Table2)

########################################
## For Nights
########################################
cor(Nights, PricePerNight)
cor(log10(Nights), PricePerNight)
cor(Nights, log10(PricePerNight))
cor(Nights, PricePerNight + PricePerNight^2)
cor(Nights, PricePerNight + PricePerNight^2 + PricePerNight^3)
cor(exp(Nights), PricePerNight)
cor(Nights, exp(PricePerNight))

Table3 <- lm(Nights ~ PricePerNight)
summary(Table3)

########################################
## Is the initial observational study accurate?
##
## For Booked?
########################################
InterceptBooked <- 0.4463765
SlopeBooked <- -0.0007498

FractionDifferenceBooked <- -0.0007498 * 100
PercentDifferenceBooked <- FractionDifferenceBooked * 100
PercentDifferenceBooked

########################################
## For Nights
########################################
InterceptNights <- 1.4867377
SlopeNights <- -0.0028909

NightsBookedEvery100usd <- -0.0028909 * 100
NightsBookedEvery100usd

########################################
## Heterogenous interaction models for UserIncome and Region
##
## Regions
## For Booked
########################################
unique(Expedia$Region)
Expedia$RegionFactor <- as.factor(Expedia$Region)

## Create dummies
Expedia$Hawaii <- ifelse(Expedia$RegionFactor == "Hawaii", 1, 0)
Expedia$LasVegas <- ifelse(Expedia$RegionFactor == "Las Vegas", 1, 0)
Expedia$Miami <- ifelse(Expedia$RegionFactor == "Miami", 1, 0)
Expedia$DC <- ifelse(Expedia$RegionFactor == "Washinton DC", 1, 0)

Expedia$TreatRegH <- Expedia$PricePerNight * Expedia$Hawaii
Expedia$TreatRegLV <- Expedia$PricePerNight * Expedia$LasVegas
Expedia$TreatRegM <- Expedia$PricePerNight * Expedia$Miami
Expedia$TreatRegDC <- Expedia$PricePerNight * Expedia$DC

Table4 <- lm(
  Expedia$`Booked?` ~ Expedia$TreatRegH + Expedia$TreatRegLV +
    Expedia$TreatRegM + Expedia$TreatRegDC +
    Expedia$Hawaii + Expedia$LasVegas + Expedia$Miami + Expedia$DC
)
summary(Table4)

########################################
## For Nights
########################################
Table5 <- lm(
  Expedia$Nights ~ Expedia$TreatRegH + Expedia$TreatRegLV +
    Expedia$TreatRegM + Expedia$TreatRegDC +
    Expedia$Hawaii + Expedia$LasVegas + Expedia$Miami + Expedia$DC
)
summary(Table5)

########################################
## For UserIncome
## For Booked
########################################
Expedia$UserIncomeFactor <- as.factor(Expedia$UserIncome)

## Income Groups
Expedia$IncGrp <- "<$50k"
Expedia$IncGrp <- ifelse(Expedia$UserIncome >= 50000, "$50k-75k", Expedia$IncGrp)
Expedia$IncGrp <- ifelse(Expedia$UserIncome >= 75000, "$75k-100k", Expedia$IncGrp)
Expedia$IncGrp <- ifelse(Expedia$UserIncome > 100000, ">$100k", Expedia$IncGrp)

Expedia$INC1 <- ifelse(Expedia$IncGrp == "<$50k", 1, 0)
Expedia$INC2 <- ifelse(Expedia$IncGrp == "$50k-75k", 1, 0)
Expedia$INC3 <- ifelse(Expedia$IncGrp == "$75k-100k", 1, 0)
Expedia$INC4 <- ifelse(Expedia$IncGrp == ">$100k", 1, 0)

Expedia$TreatInc1 <- Expedia$PricePerNight * Expedia$INC1
Expedia$TreatInc2 <- Expedia$PricePerNight * Expedia$INC2
Expedia$TreatInc3 <- Expedia$PricePerNight * Expedia$INC3
Expedia$TreatInc4 <- Expedia$PricePerNight * Expedia$INC4

Table6 <- lm(
  Expedia$`Booked?` ~ Expedia$INC1 + Expedia$INC2 + Expedia$INC3 + Expedia$INC4 +
    Expedia$TreatInc1 + Expedia$TreatInc2 + Expedia$TreatInc3 + Expedia$TreatInc4
)
summary(Table6)

## EDA for IncGrp
ggplot(Expedia, aes(x = PricePerNight, y = `Booked?`, color = IncGrp)) +
  geom_point() +
  geom_smooth(method = lm) +
  scale_color_manual(values = c("<$50k" = "blue", "$50k-75k" = "red", "$75k-100k" = "orange", ">$100k" = "green")) +
  labs(
    title = "Figure 7. Booked versus Price Per Night Per Income Group",
    x = "Price Per Night ($)",
    y = "Booked?",
    color = "Income Group"
  ) +
  theme_minimal()

########################################
## For Nights
########################################
ggplot(Expedia, aes(x = PricePerNight, y = Nights, color = IncGrp)) +
  geom_point() +
  geom_smooth(method = lm) +
  scale_color_manual(values = c("<$50k" = "blue", "$50k-75k" = "red", "$75k-100k" = "orange", ">$100k" = "green")) +
  labs(
    title = "Figure 8. Nights Booked versus Price Per Night Per Income Group",
    x = "Price Per Night ($)",
    y = "Nights Booked",
    color = "Income Group"
  ) +
  theme_minimal()

Table7 <- lm(
  Expedia$Nights ~ Expedia$INC1 + Expedia$INC2 + Expedia$INC3 + Expedia$INC4 +
    Expedia$TreatInc1 + Expedia$TreatInc2 + Expedia$TreatInc3 + Expedia$TreatInc4
)
summary(Table7)

########################################
## For both UserIncome and Region
##
## For Booked
########################################
Table8 <- lm(
  Expedia$`Booked?` ~ Expedia$INC1 + Expedia$INC2 + Expedia$INC3 + Expedia$INC4 +
    Expedia$TreatInc1 + Expedia$TreatInc2 + Expedia$TreatInc3 + Expedia$TreatInc4 +
    Expedia$TreatRegH + Expedia$TreatRegLV + Expedia$TreatRegM + Expedia$TreatRegDC +
    Expedia$Hawaii + Expedia$LasVegas + Expedia$Miami + Expedia$DC
)
summary(Table8)

########################################
## For Nights
########################################
Table9 <- lm(
  Expedia$Nights ~ Expedia$INC1 + Expedia$INC2 + Expedia$INC3 + Expedia$INC4 +
    Expedia$TreatInc1 + Expedia$TreatInc2 + Expedia$TreatInc3 + Expedia$TreatInc4 +
    Expedia$TreatRegH + Expedia$TreatRegLV + Expedia$TreatRegM + Expedia$TreatRegDC +
    Expedia$Hawaii + Expedia$LasVegas + Expedia$Miami + Expedia$DC
)
summary(Table9)
