# This script will read in the Wapusk microclimate, snow, and needle data
# and run some simple analyses for the the year-end report

# Libraries ----
rm(list = ls())

library(dplyr)
library(lubridate)
library(pscl)
library(lmtest)
library(scales)
library(tidyverse)
library(wesanderson)

curr_year <- 2025

# Source functions
source("~/Desktop/Workspace/earthwatch/R_functions/R_functions.R")

# Set working directory
setwd("~/Desktop/Workspace/")

#__________________________________-----
# Microclimate ----

## Read in the data ----
# ch <- read_csv("./Earthwatch/Churchill/data/microclimate_20000101_20221010_filled.csv", header = T)[-1]
microclimate <- read_latest_dated_file(
  dir     = "./Earthwatch/Churchill/data/",
  pattern = "^microclimate_\\d{8}_\\d{8}_filled\\.csv$",
  drop_first_col = TRUE
)

ch <- microclimate$data

## Data wrangling ----
### Format date, add seasons/season_year ----
ch <- ch %>% 
  mutate(Date = ymd(Date), 
         season_year = ifelse(month(Date) == 10 | month(Date) == 11 | month(Date) == 12,
                              year(Date) + 1, year(Date)),
         season = case_when(month(Date) %in% c(10,11,12,1,2,3,4,5) ~ "cool",
                            month(Date) %in% c(6, 7, 8, 9) ~ "warm",
                            T ~ NA_character_
         ))

ch_temps_season <- ch %>% 
  group_by(season_year, season) %>% 
  summarise(across(ends_with(c("airp150","bsw150","tis150","wsu150","mlk150","fen150","bfr150","pfr150",
                               "bwp150","ppa150","ppd150","tun150","rlk150",
                               "airp0","bsw0","tis0","wsu0","mlk0","fen0","bfr0","pfr0",
                               "bwp0","ppa0","ppd0","tun0","rlk0", # No ground surface for RLK
                               "neg80")), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  select(season_year, season, mlk150, mlkneg80, rlk150, rlkneg80)

### Subsets ----
ch_temps_w <- subset(ch_temps_season, season == "warm")
ch_temps_c <- subset(ch_temps_season, season == "cool")
matplot(ch_temps_w[,-c(1:3)], type = "l")
matplot(ch_temps_c[,-c(1:3)], type = "l")

ch_annual <- ch %>% 
  group_by(season_year) %>% 
  summarise(mean150 = mean(c(mlk150,rlk150), na.rm = T))

mean2006_2024 <- ch_annual %>% 
  filter(season_year >= 2006, season_year <= (!!curr_year - 1)) %>% 
  summarise(mean150 = mean(mean150, na.rm = T))

mean_150 <- ch_annual %>% 
  filter(season_year == !!curr_year - 1) %>% 
  mutate(diff = mean150 - pull(mean2006_2024))

ch_annual %>% 
  filter(season_year >= 2006) %>% 
  ggplot(aes(x = season_year, y = mean150)) +
  geom_line() +
  geom_hline(
    data = mean2006_2024,
    aes(yintercept = mean150),
    linetype = "dashed"
  )

### Regressions ----
#### Air ----
summary(mlk150w <- lm(mlk150 ~ season_year, data = ch_temps_w[-c(1,nrow(ch_temps_w)),]))     # ns
summary(rlk150w <- lm(rlk150 ~ season_year, data = ch_temps_w[-c(1,nrow(ch_temps_w)),]))     # ns

summary(mlk150c <- lm(mlk150 ~ season_year, data = ch_temps_c[-c(1,nrow(ch_temps_c)),]))     # ns
summary(rlk150c <- lm(rlk150 ~ season_year, data = ch_temps_c[-c(1,nrow(ch_temps_c)),]))     # ns

#### Permafrost ----
summary(mlkneg80w <- lm(mlkneg80 ~ season_year, data = ch_temps_w[-c(1,nrow(ch_temps_w)),]))     # ns
summary(rlkneg80w <- lm(rlkneg80 ~ season_year, data = ch_temps_w[-c(1,nrow(ch_temps_w)),]))     # 2025 SIG

summary(mlkneg80c <- lm(mlkneg80 ~ season_year, data = ch_temps_c[-c(1,nrow(ch_temps_c)),]))     # ns
summary(rlkneg80c <- lm(rlkneg80 ~ season_year, data = ch_temps_c[-c(1,nrow(ch_temps_c)),]))     # 2025 SIG

ch_temps_w %>% 
  ggplot(aes(x = season_year, y = mlk150)) + geom_line() + 
  geom_line(aes(y = rlk150), color = "blue")

ch_temps_w %>% 
  ggplot(aes(x = season_year, y = mlkneg80)) + geom_line() + 
  geom_line(aes(y = rlkneg80), color = "blue")


ch_temps_c %>% 
  filter(season_year < !!curr_year) %>% 
  ggplot(aes(x = season_year, y = mlk150)) + geom_line() + 
  geom_line(aes(y = rlk150), color = "blue")

ch_temps_c %>% 
  filter(season_year < !!curr_year) %>% 
  ggplot(aes(x = season_year, y = mlkneg80)) + geom_line() + 
  geom_line(aes(y = rlkneg80), color = "blue")

#__________________________________-----
# Snow ----

## Read in the data ----
snow <- read_csv("~/Desktop/Workspace/Parks/data/parks_snow_2006_2023.csv")

## Data wrangling ----
# [1] "forest"  "beach"   "fen"     "shrub"   "wedge"   "polygon" "island" 
snow %>% 
  # filter(type == "forest") %>% 
  ggplot(aes(x = factor(year), y = depth)) +
  geom_boxplot() + 
  facet_wrap(~type, scale="free")

### Mean depth (all years != t) ----
snow_mean <- snow %>% 
  filter(year != !!curr_year) %>% 
  group_by(type) %>% 
  reframe(mean = mean(depth, na.rm = T))

# # A tibble: 7 × 2
# type        mean
# <chr>       <dbl>
# 1 beach     17.6 
# 2 fen       41.6 
# 3 forest    63.4 
# 4 island    94.6 
# 5 polygon   9.03
# 6 shrub     85.8 
# 7 wedge     35.2 

### Mean depth (year == t) ----
snow_2023 <- snow %>% 
  filter(year == !!curr_year) %>% 
  group_by(type) %>% 
  reframe(mean = mean(depth, na.rm = T))

# # A tibble: 7 × 2
# type        mean
# <chr>       <dbl>
# 1 beach     31.8 
# 2 fen       45.0 
# 3 forest    78.0 
# 4 island    141.  
# 5 polygon   7.52
# 6 shrub     90.5 
# 7 wedge     30.8 

### Join global and current year mean ----
df_snow <- snow_mean %>% 
  left_join(snow_2023, by = "type") %>% 
  rename(snow_mean = mean.x,
         snow_2023 = mean.y) %>% 
  rowwise() %>% 
  mutate(percent_diff = ((snow_2023 - snow_mean)/snow_mean)*100)

## Stats ----
### Test (year == t) & control (year != t) datasets ----
beach_tg <- snow %>% filter(type == "beach", year == !!curr_year, !is.na(depth)) %>% select(depth) %>% deframe()
beach_cg <- snow %>% filter(type == "beach", year != !!curr_year, !is.na(depth)) %>% select(depth) %>% deframe()
fen_tg <- snow %>% filter(type == "fen", year == !!curr_year, !is.na(depth)) %>% select(depth) %>% deframe()
fen_cg <- snow %>% filter(type == "fen", year != !!curr_year, !is.na(depth)) %>% select(depth) %>% deframe()
forest_tg <- snow %>% filter(type == "forest", year == !!curr_year, !is.na(depth)) %>% select(depth) %>% deframe()
forest_cg <- snow %>% filter(type == "forest", year != !!curr_year, !is.na(depth)) %>% select(depth) %>% deframe()
island_tg <- snow %>% filter(type == "island", year == !!curr_year, !is.na(depth)) %>% select(depth) %>% deframe()
island_cg <- snow %>% filter(type == "island", year != !!curr_year, !is.na(depth)) %>% select(depth) %>% deframe()
polygon_tg <- snow %>% filter(type == "polygon", year == !!curr_year, !is.na(depth)) %>% select(depth) %>% deframe()
polygon_cg <- snow %>% filter(type == "polygon", year != !!curr_year, !is.na(depth)) %>% select(depth) %>% deframe()
shrub_tg <- snow %>% filter(type == "shrub", year == !!curr_year, !is.na(depth)) %>% select(depth) %>% deframe()
shrub_cg <- snow %>% filter(type == "shrub", year != !!curr_year, !is.na(depth)) %>% select(depth) %>% deframe()
wedge_tg <- snow %>% filter(type == "wedge", year == !!curr_year, !is.na(depth)) %>% select(depth) %>% deframe()
wedge_cg <- snow %>% filter(type == "wedge", year != !!curr_year, !is.na(depth)) %>% select(depth) %>% deframe()

### Variance tests ----
var.test(beach_cg, beach_tg)      # hetero
var.test(fen_cg, fen_tg)          # homo
var.test(forest_cg, forest_tg)    # hetero
var.test(island_cg, island_tg)    # hetero
var.test(polygon_cg, polygon_tg)  # hetero
var.test(shrub_cg, shrub_tg)      # hetero
var.test(wedge_cg, wedge_tg)      # hetero

### t-tests ----
t.test(forest_cg, forest_tg, var.equal = FALSE)     # t = -13.542, df = 157.97, p-value < 2.2e-16
t.test(island_cg, island_tg, var.equal = FALSE)     # t = -26.55, df = 73.336, p-value < 2.2e-16
t.test(shrub_cg, shrub_tg, var.equal = FALSE)       # t = -3.3377, df = 42.596, p-value = 0.001761
t.test(wedge_cg, wedge_tg, var.equal = FALSE)       # t = 3.6751, df = 44.703, p-value = 0.000634
t.test(beach_cg, beach_tg, var.equal = FALSE)       # t = -4.668, df = 67.03, p-value = 1.506e-05
t.test(fen_cg, fen_tg, var.equal = TRUE)            # t = -1.5322, df = 1611, p-value = 0.1257
t.test(polygon_cg, polygon_tg, var.equal = FALSE)   # t = 2.2651, df = 37.458, p-value = 0.02938

#__________________________________-----
# Needles ----

## Read in the data ----
needles <- read_csv("~/Desktop/Workspace/Parks/data/parks_needles_2013_2020.csv")

## Data wrangling ----
### Subset to only parks sites ----
levels(as.factor(needles$Site))
needles <- needles %>% 
  filter(Site %in% c("BR","BRR","BSF","MLK","OR","OSF","RLK")) %>% 
  mutate(gmin = as.numeric(gmin),
         Zone = ifelse(Zone == "R","F",Zone)) %>% 
  filter(Zone == "F")

### Visual inspection ----
needles %>% 
  ggplot(aes(x=Aspect, y=gmin, fill=as.factor(year))) +
  geom_boxplot()

### Percent difference between years & ranks ----
#### Epidermal conductance ----
n_df <- needles %>% 
  group_by(year) %>% 
  summarise(gmin = mean(gmin, na.rm = T)) %>% 
  mutate(prop = (gmin - mean(gmin)) / mean(gmin),
         rank = rank(gmin))

#### Air temperatures ----
t_df <- ch_temps_w %>% 
  filter(season_year >= 2013, season_year < 2021) %>% 
  rowwise() %>% 
  mutate(mean150 = mean(c(mlk150, rlk150), na.rm = T)) %>% 
  group_by(season_year) %>% 
  summarise(mean150 = mean(mean150, na.rm = T)) %>% 
  mutate(prop = (mean150 - mean(mean150)) / mean(mean150),
         rank = rank(-mean150))

#__________________________________-----
# Archived code ----

# ## Export at 6 x 3.5
# 
# treed_cols <- rep(wes_palette("BottleRocket2")[4], 4)
# disturbed_cols <- rep(wes_palette("FantasticFox1")[4], 3)
# fen_cols <- wes_palette("FantasticFox1")[2]
# treeless_cols <- rep(wes_palette("FantasticFox1")[3], 3)
# col_pal <- c(treed_cols, disturbed_cols, fen_cols, treeless_cols)
# col_pal2 <- c(treed_cols[1], disturbed_cols[1], fen_cols[1], treeless_cols[1])
# 
# # Warm
# jpeg("~/Desktop/Workspace/Earthwatch/Churchill/figures/warmT_2022.jpg", width = 5, height = 7, units = "in", res = 300)
# par(xpd = F)
# par(mar = c(2,2,1,1), oma = c(2,2,0,0))
# par(mfrow = c(3,1))
# ## Air T
# matplot(ch_temps_w$season_year[-c(1,23)], ch_temps_w[-c(1,23),c(3:6,8:14)], 
#         type = "l", col = col_pal, 
#         lty = 1, ylab = "", xlab = "", ylim = c(6,15))
# mtext("Air (°C)", side = 2, line = 2.5, cex = 0.7)
# abline(coef(bwp150w), lty=2, col = wes_palette("FantasticFox1")[4])
# ## Grd T
# matplot(ch_temps_w$season_year[-c(1,23)], ch_temps_w[-c(1,23),c(16:19,21:27)], 
#         type = "l", col = col_pal, 
#         lty = 1, ylab = "", xlab = "", ylim = c(5,14))
# mtext("Ground surface (°C)", side = 2, line = 2.5, cex = 0.7)
# abline(coef(ppd0w), lty=2, col = wes_palette("FantasticFox1")[3])
# abline(coef(wsu0w), lty=2, col = wes_palette("BottleRocket2")[4])
# abline(coef(bfr0w), lty=2, col = wes_palette("BottleRocket2")[4])
# abline(coef(pfr0w), lty=2, col = wes_palette("BottleRocket2")[4])
# ## -80 T
# matplot(ch_temps_w$season_year[-c(1,23)], ch_temps_w[-c(1,23),c(28:31,33:39)], type = "l", 
#         col = col_pal, 
#         lty = 1, ylab = "", xlab = "", ylim = c(-2,12))
# mtext("Subsurface (°C)", side = 2, line = 2.5, cex = 0.7)
# mtext("Year", side = 1, line = 2.5, cex = 0.7)
# abline(coef(ppaneg80w), lty=2, col = wes_palette("FantasticFox1")[3])
# abline(coef(fenneg80w), lty=2, col = wes_palette("FantasticFox1")[2])
# abline(coef(wsuneg80w), lty=2, col = wes_palette("BottleRocket2")[4])
# abline(coef(pfrneg80w), lty=2, col = wes_palette("FantasticFox1")[4])
# abline(coef(bwpneg80w), lty=2, col = wes_palette("FantasticFox1")[4])
# 
# par(xpd = NA)
# legend(2004,12, c("Treed","Fen","Disturbed","Treeless"), 
#        col = col_pal2, 
#        horiz = T, lty = 1, cex = 1, bty = "n", y.intersp = 1, text.width = 2)
# dev.off()
# 
# # Cool
# jpeg("~/Desktop/Workspace/Earthwatch/Churchill/figures/coolT_2022.jpg", width = 5, height = 7, units = "in", res = 300)
# par(xpd = F)
# par(mar = c(2,2,1,1), oma = c(2,2,0,0))
# par(mfrow = c(3,1))
# ## Air T
# matplot(ch_temps_c$season_year[-c(1,23,24)], ch_temps_c[-c(1,23,24),c(3:6,8:14)], 
#         type = "l", col = col_pal, 
#         lty = 1, ylab = "", xlab = "", ylim = c(-16.5,-9))
# mtext("Air (°C)", side = 2, line = 2.5, cex = 0.7)
# ## Grd T
# matplot(ch_temps_c$season_year[-c(1,23,24)], ch_temps_c[-c(1,23,24),c(16:19,21:27)], 
#         type = "l", col = col_pal, 
#         lty = 1, ylab = "", xlab = "", ylim = c(-16,1))
# mtext("Ground surface (°C)", side = 2, line = 2.5, cex = 0.7)
# abline(coef(tun0c), lty=2, col = wes_palette("FantasticFox1")[3])
# abline(coef(ppa0c), lty=2, col = wes_palette("FantasticFox1")[3])
# abline(coef(ppd0c), lty=2, col = wes_palette("FantasticFox1")[3])
# abline(coef(tis0c), lty=2, col = wes_palette("BottleRocket2")[4])
# abline(coef(airp0c), lty=2, col = wes_palette("BottleRocket2")[4])
# abline(coef(bfr0c), lty=2, col = wes_palette("BottleRocket2")[4])
# abline(coef(pfr0c), lty=2, col = wes_palette("BottleRocket2")[4])
# abline(coef(fen0c), lty=2, col = wes_palette("FantasticFox1")[2])
# ## -80 T
# matplot(ch_temps_c$season_year[-c(1,23,24)], ch_temps_c[-c(1,23,24),c(28:31,33:39)], type = "l", 
#         col = col_pal, 
#         lty = 1, ylab = "", xlab = "", ylim = c(-9,4))
# mtext("Subsurface (°C)", side = 2, line = 2.5, cex = 0.7)
# mtext("Year", side = 1, line = 2.5, cex = 0.7)
# abline(coef(ppaneg80c), lty=2, col = wes_palette("FantasticFox1")[3])
# abline(coef(fenneg80c), lty=2, col = wes_palette("FantasticFox1")[2])
# abline(coef(tisneg80c), lty=2, col = wes_palette("BottleRocket2")[4])
# abline(coef(bfrneg80c), lty=2, col = wes_palette("FantasticFox1")[4])
# abline(coef(pfrneg80c), lty=2, col = wes_palette("FantasticFox1")[4])
# abline(coef(bwpneg80c), lty=2, col = wes_palette("FantasticFox1")[4])
# 
# par(xpd = NA)
# legend(2004,4, c("Treed","Fen","Disturbed","Treeless"), 
#        col = col_pal2, 
#        horiz = T, lty = 1, cex = 1, bty = "n", y.intersp = 1, text.width = 2)
# dev.off()