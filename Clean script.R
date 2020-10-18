## Nødvendige pakker:
# install.packages("rsdmx")
# install.packages("arsenal")
#install.packages("tidyverse")
library(tidyverse)
library(rsdmx)

# Henter data fra OECD ved bruk av rsdmx, reduserer koden ved bruk av pipe-operator
shortBonds <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI_FIN/IR3TIB.AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA19+SDR+NMEC+ARG+BRA+CHN+CRI+IND+IDN+RUS+ZAF.A/all?startTime=1956&endTime=2020" %>% 
  readSDMX() %>% 
  as.data.frame() %>% 
  select(obsTime, LOCATION, obsValue) %>% 
  spread(LOCATION, obsValue, convert = FALSE)

longBonds <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI_FIN/IRLT.AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA19+SDR+NMEC+ARG+BRA+CHN+CRI+IND+IDN+RUS+ZAF.A/all?startTime=1956&endTime=2020" %>% 
  readSDMX() %>% 
  as.data.frame() %>% 
  select(obsTime, LOCATION, obsValue) %>% 
  spread(LOCATION, obsValue, convert = FALSE) 

sharePrices <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI_FIN/SP.AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA19+SDR+NMEC+ARG+BRA+CHN+CRI+IND+IDN+RUS+ZAF.A/all?startTime=1956&endTime=2020" %>% 
  readSDMX() %>% 
  as.data.frame() %>% 
  select(obsTime, LOCATION, obsValue) %>% 
  spread(LOCATION, obsValue, convert = FALSE)

# Sammenligner datasettene ved bruk av comparedf og oppsummerer
library(arsenal)                          
summary(comparedf(sharePrices, longBonds))
#Table: Variables not shared
# version   variable    position  class   
# --------  ---------  ---------  --------
# x         BRA                5  numeric 
# x         CHN                9  numeric 
# x         EST               16  numeric 
# x         IDN               22  numeric 
# x         TUR               42  numeric 
# y         CRI                9  numeric 
# y         LTU               27  numeric

summary(comparedf(longBonds, shortBonds))
# Table: Variables not shared
# version   variable    position  class   
# --------  ---------  ---------  --------
# y         CHN                8  numeric 
# y         EST               16  numeric 
# y         IDN               22  numeric 

summary(comparedf(sharePrices, shortBonds))
# Table: Variables not shared
# version   variable    position  class   
# --------  ---------  ---------  --------
# x         BRA                5  numeric 
# x         TUR               42  numeric 
# y         CRI               10  numeric 
# y         LTU               30  numeric 

# Fjerner kolonner som ikke matcher
sharePrices <- within(sharePrices, rm(BRA, CHN, EST, IDN, TUR))
longBonds <- within(longBonds, rm(CRI, LTU))
shortBonds <- within(shortBonds, rm(CHN, EST, IDN, CRI, LTU))

# Kontrollerer symmetri i datasettene
summary(comparedf(sharePrices, shortBonds))
summary(comparedf(longBonds, shortBonds))
summary(comparedf(sharePrices, longBonds))
####################################
# Endrer navn på kolonner i hvert datasett for utregningen
colnames(sharePrices) <- c("sharePrices_obsTime", "sharePrices_AUS","sharePrices_AUT", "sharePrices_BEL", "sharePrices_CAN", "sharePrices_CHE", "sharePrices_CHL", "sharePrices_COL", "sharePrices_CZE", "sharePrices_DEU", "sharePrices_DNK", "sharePrices_EA19", "sharePrices_ESP", "sharePrices_FIN", "sharePrices_FRA", "sharePrices_GBR", "sharePrices_GRC", "sharePrices_HUN", "sharePrices_IND", "sharePrices_IRL", "sharePrices_ISL", "sharePrices_ISR", "sharePrices_ITA", "sharePrices_JPN", "sharePrices_KOR", "sharePrices_LUX", "sharePrices_LVA", "sharePrices_MEX", "sharePrices_NLD", "sharePrices_NOR", "sharePrices_NZL", "sharePrices_POL", "sharePrices_PRT", "sharePrices_RUS", "sharePrices_SVK", "sharePrices_SVN", "sharePrices_SWE", "sharePrices_USA", "sharePrices_ZAF")
colnames(longBonds) <- c("obsTime", "longBonds_AUS", "longBonds_AUT", "longBonds_BEL", "longBonds_CAN", "longBonds_CHE", "longBonds_CHL", "longBonds_COL", "longBonds_CZE", "longBonds_DEU", "longBonds_DNK", "longBonds_EA19", "longBonds_ESP", "longBonds_FIN", "longBonds_FRA", "longBonds_GBR", "longBonds_GRC", "longBonds_HUN", "longBonds_IND", "longBonds_IRL", "longBonds_ISL", "longBonds_ISR", "longBonds_ITA", "longBonds_JPN", "longBonds_KOR", "longBonds_LUX", "longBonds_LVA", "longBonds_MEX", "longBonds_NLD", "longBonds_NOR", "longBonds_NZL", "longBonds_POL", "longBonds_PRT", "longBonds_RUS", "longBonds_SVK", "longBonds_SVN", "longBonds_SWE", "longBonds_USA", "longBonds_ZAF")
colnames(shortBonds) <- c("shortBonds_obsTime", "shortBonds_AUS", "shortBonds_AUT", "shortBonds_BEL", "shortBonds_CAN", "shortBonds_CHE", "shortBonds_CHL", "shortBonds_COL", "shortBonds_CZE", "shortBonds_DEU", "shortBonds_DNK", "shortBonds_EA19", "shortBonds_ESP", "shortBonds_FIN", "shortBonds_FRA", "shortBonds_GBR", "shortBonds_GRC", "shortBonds_HUN", "shortBonds_IND", "shortBonds_IRL", "shortBonds_ISL", "shortBonds_ISR", "shortBonds_ITA", "shortBonds_JPN", "shortBonds_KOR", "shortBonds_LUX", "shortBonds_LVA", "shortBonds_MEX", "shortBonds_NLD", "shortBonds_NOR", "shortBonds_NZL", "shortBonds_POL", "shortBonds_PRT", "shortBonds_RUS", "shortBonds_SVK", "shortBonds_SVN", "shortBonds_SWE", "shortBonds_USA", "shortBonds_ZAF")

# https://snl.no/interpolasjon_-_matematikk
library(zoo)
sharePrices[1, c("sharePrices_AUS", "sharePrices_AUT", "sharePrices_BEL", "sharePrices_CHL", "sharePrices_COL", "sharePrices_CZE", "sharePrices_DEU", "sharePrices_DNK", "sharePrices_EA19", "sharePrices_ESP", "sharePrices_FIN", "sharePrices_GBR", "sharePrices_GRC", "sharePrices_HUN", "sharePrices_IND", "sharePrices_ISL", "sharePrices_ISR", "sharePrices_ITA", "sharePrices_JPN", "sharePrices_KOR", "sharePrices_LUX", "sharePrices_LVA", "sharePrices_MEX", "sharePrices_NLD", "sharePrices_NOR", "sharePrices_NZL", "sharePrices_POL" ,"sharePrices_PRT", "sharePrices_RUS", "sharePrices_SVK", "sharePrices_SVN", "sharePrices_USA", "sharePrices_ZAF")] <- 1.0
sharePrices <- na.approx(sharePrices)
sharePrices <- as.data.frame(sharePrices)
# Beregner fra pris til prosentvis endring
sharePrices <- mutate((sharePrices[2:nrow(sharePrices), 2:ncol(sharePrices)]
                       - sharePrices[1:(nrow(sharePrices)-1), 2:ncol(sharePrices)])
                      /sharePrices[1:(nrow(sharePrices)-1), 2:ncol(sharePrices)])*100
longBonds[1, c("longBonds_AUS", "longBonds_AUT", "longBonds_CHL", "longBonds_COL", "longBonds_CZE", "longBonds_DEU", "longBonds_DNK", "longBonds_EA19", "longBonds_ESP", "longBonds_FIN", "longBonds_FRA", "longBonds_GBR", "longBonds_GRC", "longBonds_HUN", "longBonds_IND", "longBonds_IRL", "longBonds_ISL", "longBonds_ISR", "longBonds_ITA", "longBonds_JPN", "longBonds_KOR", "longBonds_LUX", "longBonds_LVA", "longBonds_MEX", "longBonds_NLD", "longBonds_NOR", "longBonds_NZL", "longBonds_POL", "longBonds_PRT", "longBonds_RUS", "longBonds_SVK", "longBonds_SVN", "longBonds_SWE", "longBonds_ZAF")] <- 1.0
longBonds[63,34] <- 8
longBonds <- na.approx(longBonds)
longBonds <- as.data.frame(longBonds[-c(1),])
shortBonds[1, c("shortBonds_AUS", "shortBonds_AUT", "shortBonds_BEL", "shortBonds_CHE", "shortBonds_CHL", "shortBonds_COL", "shortBonds_CZE", "shortBonds_DEU", "shortBonds_DNK", "shortBonds_EA19", "shortBonds_ESP", "shortBonds_FIN", "shortBonds_FRA", "shortBonds_GBR", "shortBonds_GRC", "shortBonds_HUN", "shortBonds_IND", "shortBonds_IRL", "shortBonds_ISL", "shortBonds_ISR", "shortBonds_ITA", "shortBonds_JPN", "shortBonds_KOR", "shortBonds_LUX", "shortBonds_LVA", "shortBonds_MEX", "shortBonds_NLD", "shortBonds_NOR", "shortBonds_NZL", "shortBonds_POL", "shortBonds_PRT", "shortBonds_RUS", "shortBonds_SVK", "shortBonds_SVN", "shortBonds_SWE", "shortBonds_USA", "shortBonds_ZAF")] <- 1.0
shortBonds <- na.approx(shortBonds)
shortBonds <- as.data.frame(shortBonds[-c(1),])

# Kombinerer datasett til én frame
allObs <- cbind(longBonds, shortBonds, sharePrices)
allObs <- within(allObs, rm(shortBonds_obsTime))

# Kontrollerer at all data er riktig kategorisert
sapply(allObs, class)

# Lager funksjon for å beregne aksjepremie
calc_premium <- function(x, y){
  premium <- (x - y)
  return(premium)
}
long <- allObs[2:39]
short <- allObs[40:77]
shares <- allObs[78:115]

# Beregner aksjepremie og gir kolonner beskrivende navn
premiumShort <- calc_premium(shares, short)
premiumLong <- calc_premium(shares, long)
colnames(premiumShort) <- c("premiumShort_AUS", "premiumShort_AUT", "premiumShort_BEL","premiumShort_CAN", "premiumShort_CHE", "premiumShort_CHL", "premiumShort_COL", "premiumShort_CZE", "premiumShort_DEU", "premiumShort_DNK", "premiumShort_EA19", "premiumShort_ESP", "premiumShort_FIN", "premiumShort_FRA", "premiumShort_GBR", "premiumShort_GRC", "premiumShort_HUN", "premiumShort_IND", "premiumShort_IRL", "premiumShort_ISL", "premiumShort_ISR", "premiumShort_ITA", "premiumShort_JPN", "premiumShort_KOR", "premiumShort_LUX", "premiumShort_LVA", "premiumShort_MEX", "premiumShort_NLD", "premiumShort_NOR", "premiumShort_NZL", "premiumShort_POL", "premiumShort_PRT", "premiumShort_RUS", "premiumShort_SVK", "premiumShort_SVN", "premiumShort_SWE", "premiumShort_USA", "premiumShort_ZAF")
colnames(premiumLong) <- c("premiumLong_AUS", "premiumLong_AUT", "premiumLong_BEL","premiumLong_CAN", "premiumLong_CHE", "premiumLong_CHL", "premiumLong_COL", "premiumLong_CZE", "premiumLong_DEU", "premiumLong_DNK", "premiumLong_EA19", "premiumLong_ESP", "premiumLong_FIN", "premiumLong_FRA", "premiumLong_GBR", "premiumLong_GRC", "premiumLong_HUN", "premiumLong_IND", "premiumLong_IRL", "premiumLong_ISL", "premiumLong_ISR", "premiumLong_ITA", "premiumLong_JPN", "premiumLong_KOR", "premiumLong_LUX", "premiumLong_LVA", "premiumLong_MEX", "premiumLong_NLD", "premiumLong_NOR", "premiumLong_NZL", "premiumLong_POL", "premiumLong_PRT", "premiumLong_RUS", "premiumLong_SVK", "premiumLong_SVN", "premiumLong_SWE", "premiumLong_USA", "premiumLong_ZAF")

## Tester hvorvidt data egner seg for å kommunisere informasjon
premiumShort <- gather(premiumShort)
dates <- as.data.frame(1957:2019)
premiumShort <- cbind(dates, premiumShort)
colnames(premiumShort) <- c("Date", "Country", "Premium")

ggplot(data = premiumShort) + 
  geom_line(mapping = aes(x = Date, y = Premium, col = Country)) + 
  labs(title = "Premium: Calculated by Short-term bonds")

premiumLong <- gather(premiumLong)
dates <- as.data.frame(1957:2019)
premiumLong <- cbind(dates, premiumLong)
colnames(premiumLong) <- c("Date", "Country", "Premium")

ggplot(data = premiumLong) + 
  geom_line(mapping = aes(x = Date, y = Premium, col = Country)) + 
  labs(title = "Premium: Calculated by Long-term bonds")

## Klargjør data for grafer: Tidshorisont og kategorisering
# Reduserer tidshorisont (1995-2019)
premiumShort <- premiumShort %>% 
  filter(Date >= 1995)
premiumLong <- premiumLong %>% 
  filter(Date >= 1995)
dates <- as.data.frame(1995:2019)

# Country members of the EU 
PremiumShort_E19 <- premiumShort %>% 
  spread(Country, Premium, convert = FALSE) %>% 
  select("premiumShort_AUT", "premiumShort_BEL", "premiumShort_FIN", "premiumShort_FRA", "premiumShort_DEU", "premiumShort_GRC", "premiumShort_IRL", "premiumShort_ITA", "premiumShort_LVA", "premiumShort_LUX", "premiumShort_NLD", "premiumShort_PRT", "premiumShort_SVK", "premiumShort_SVN", "premiumShort_ESP") 
colnames(PremiumShort_E19) <- c("Austria", "Belgium", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Latvia", "Luxembourg", "Netherlands", "Portugal", "SlovakRepublic", "Slovenia", "Spain")
PremiumShort_E19 <- gather(PremiumShort_E19)
PremiumShort_E19 <- cbind(dates, PremiumShort_E19)
colnames(PremiumShort_E19) <- c("Date", "Country", "Premium")

PremiumLong_E19 <- premiumLong %>% 
  spread(Country, Premium, convert = FALSE) %>% 
  select("premiumLong_AUT", "premiumLong_BEL", "premiumLong_FIN", "premiumLong_FRA", "premiumLong_DEU", "premiumLong_GRC", "premiumLong_IRL", "premiumLong_ITA", "premiumLong_LVA", "premiumLong_LUX", "premiumLong_NLD", "premiumLong_PRT", "premiumLong_SVK", "premiumLong_SVN", "premiumLong_ESP")
colnames(PremiumLong_E19) <- c("Austria", "Belgium", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Latvia", "Luxembourg", "Netherlands", "Portugal", "SlovakRepublic", "Slovenia", "Spain")
PremiumLong_E19 <- gather(PremiumLong_E19)
PremiumLong_E19 <- cbind(dates, PremiumLong_E19)
colnames(PremiumLong_E19) <- c("Date", "Country", "Premium")

# Countries outside the EU
PremiumShort_notE19 <- premiumShort %>% 
  spread(Country, Premium, convert = FALSE) %>% 
  select("premiumShort_AUS", "premiumShort_CAN", "premiumShort_CHL", "premiumShort_COL", "premiumShort_CZE", "premiumShort_DNK", "premiumShort_HUN", "premiumShort_ISL", "premiumShort_ISR", "premiumShort_JPN", "premiumShort_KOR", "premiumShort_MEX", "premiumShort_NZL", "premiumShort_NOR", "premiumShort_POL", "premiumShort_SWE", "premiumShort_CHE", "premiumShort_GBR", "premiumShort_USA", "premiumShort_RUS", "premiumShort_ZAF")
colnames(PremiumShort_notE19) <- c("Australia", "Canada", "Chile", "Colombia", "CzechRepublic", "Denmark", "Hungary", "Iceland", "Israel", "Japan", "Korea", "Mexico", "New Zealand", "Norway", "Poland", "Sweden", "Switzerland", "GBR", "USA", "Russia", "SouthAfrica")
PremiumShort_notE19 <- gather(PremiumShort_notE19)
PremiumShort_notE19 <- cbind(dates, PremiumShort_notE19)
colnames(PremiumShort_notE19) <- c("Date", "Country", "Premium")

PremiumLong_notE19 <- premiumLong %>% 
  spread(Country, Premium, convert = FALSE) %>% 
  select("premiumLong_AUS", "premiumLong_CAN", "premiumLong_CHL", "premiumLong_COL", "premiumLong_CZE", "premiumLong_DNK", "premiumLong_HUN", "premiumLong_ISL", "premiumLong_ISR", "premiumLong_JPN", "premiumLong_KOR", "premiumLong_MEX", "premiumLong_NZL", "premiumLong_NOR", "premiumLong_POL", "premiumLong_SWE", "premiumLong_CHE", "premiumLong_GBR", "premiumLong_USA", "premiumLong_RUS", "premiumLong_ZAF") 
colnames(PremiumLong_notE19) <- c("Australia", "Canada", "Chile", "Colombia", "CzechRepublic", "Denmark", "Hungary", "Iceland", "Israel", "Japan", "Korea", "Mexico", "New Zealand", "Norway", "Poland", "Sweden", "Switzerland", "GBR", "USA", "Russia", "SouthAfrica")
PremiumLong_notE19 <- gather(PremiumLong_notE19)
PremiumLong_notE19 <- cbind(dates, PremiumLong_notE19)
colnames(PremiumLong_notE19) <- c("Date", "Country", "Premium")

## Plots
#install.packages("gghighlight")
library(gghighlight)
# Countries within EU
PremiumShort_E19 %>% 
  group_by(Country) %>% 
  mutate(varians = (var(Premium)) > 920) %>%
  ungroup() %>% 
  ggplot(aes(x = Date, y = Premium, col = Country)) +
  geom_line(aes(linetype = varians)) +
  scale_linetype_manual(values = c("solid", "solid"), guide = "none") +
  gghighlight(varians) +
  scale_x_continuous(breaks = seq(1995, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "gray99"),
        panel.grid.major = element_line(colour = "gray50", size=0.1)) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  labs(title = "Share premium: Countries within the European Union",
       subtitle = "Premium in comparison of Short-Term Bonds - Highlighted by variance")

PremiumLong_E19 %>% 
  group_by(Country) %>% 
  mutate(varians = (var(Premium)) > 920) %>%
  ungroup() %>% 
  ggplot(aes(x = Date, y = Premium, col = Country)) +
  geom_line(aes(linetype = varians)) +
  scale_linetype_manual(values = c("solid", "solid"), guide = "none") +
  #gghighlight(varians) +
  scale_x_continuous(breaks = seq(1995, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "gray99"),
           panel.grid.major = element_line(colour = "gray50", size=0.1)) +
   geom_hline(yintercept = 0, color = "black", size = 0.5) +
   labs(title = "Share premium: Countries within the European Union",
          subtitle = "Premium in comparison of Long-Term Bonds - Highlighted by variance")

# Countries outside the EU
PremiumLong_notE19 %>% 
  group_by(Country) %>% 
  mutate(varians = (var(Premium)) > 900) %>%
  ungroup() %>% 
  ggplot(aes(x = Date, y = Premium, col = Country)) +
  geom_line(aes(linetype = varians)) +
  scale_linetype_manual(values = c("solid", "solid"), guide = "none") +
  gghighlight(varians) +
  scale_x_continuous(breaks = seq(1995, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "gray99"),
        panel.grid.major = element_line(colour = "gray50", size=0.1)) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  labs(title = "Share premium: Countries outside the European Union",
       subtitle = "Premium in comparison of Short-Term Bonds - Highlighted by variance")

PremiumLong_notE19 %>% 
  group_by(Country) %>% 
  mutate(varians = (var(Premium)) > 900) %>%
  ungroup() %>% 
  ggplot(aes(x = Date, y = Premium, col = Country)) +
  geom_line(aes(linetype = varians)) +
  scale_linetype_manual(values = c("solid", "solid"), guide = "none") +
  gghighlight(varians) +
  scale_x_continuous(breaks = seq(1995, 2019, by = 1)) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "gray99"),
        panel.grid.major = element_line(colour = "gray50", size=0.1)) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  labs(title = "Share premium: Countries outside the European Union",
       subtitle = "Premium in comparison of Long-Term Bonds - Highlighted by variance")
