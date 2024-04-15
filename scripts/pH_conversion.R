# convert free-scale Ph from 2013/2017 experiments to total-scale

library(tidyverse)
library(seacarb)

theme_set(theme_bw())
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# load 2013 data

dat <- read.csv("./data/RKC_water_chemistry_2017.csv") %>%
  rename(pH_free = pH)

head(dat)

# check range of S and T to confirm they'll work with the Dockson constant
range(dat$Salinity)
range(dat$Temp.C)
# both look good

# convert free-scale to total scale and get difference between the two

dat <- dat %>%
  mutate(pH_total = pHconv(flag = 2, pH = pH_free, S = Salinity, T = Temp.C),
         pH_diff = pH_total - pH_free)

ggplot(dat, aes(pH_free, pH_total)) +
  geom_point() +
  geom_abline(lty = 2)

ggplot(dat, aes(pH_diff)) +
  geom_density(alpha = 0.2, color = NA, fill = cb[2]) +
  xlim(-0.08, -0.045) +
  ggtitle("pH_diff = pH total scale - pH free scale")

ggsave("./figs/pH_diff_2017_data.png", units = 'in', width = 5, height = 4)



# load other sheet with 2013 pH data
dat2 <- read.csv("./data/pH_temp_RKC_2013.csv") 
unique(dat2$Treatment)

# plot 2013 treatments on free scale
ggplot(dat2, aes(pH, fill = Treatment)) +
  geom_density(alpha = 0.2, color = NA) +
  scale_fill_manual(values = cb[c(2,4,6)]) +
  coord_cartesian(xlim = c(7.4, 8.2))

# load other sheet with 2017 pH data
dat3 <- read.csv("./data/RKC_pH_temp_2017.csv") 
unique(dat3$Treatment.pH)

# plot 2017 treatments on free scale
ggplot(dat3, aes(pH, fill = Treatment.pH)) +
  geom_density(alpha = 0.2, color = NA) +
  scale_fill_manual(values = cb[c(2,4,6)]) +
  coord_cartesian(xlim = c(7.6, 8.1)) +
  ggtitle("2017 pH treatments (free scale)")

# change dat3 "Ambient" to "control
change <- dat3$Treatment.pH == "Ambient"
dat3$Treatment.pH[change] <- "Control"

# combine and plot
dat2 <- dat2 %>%
  select(Treatment, pH) %>%
  mutate(Year = 2013)

dat3 <- dat3 %>%
  select(Treatment.pH, pH) %>%
  rename(Treatment = Treatment.pH) %>%
  mutate(Year = 2017)

dat.both <- rbind(dat2, dat3)

range(dat.both$pH) # 7.03!!

dat.both <- dat.both %>%
  rename(`pH (free scale)` = pH)

# plot both years on free scale
ggplot(dat.both, aes(`pH (free scale)`, fill = Treatment)) +
  geom_density(alpha = 0.2, color = NA) +
  scale_fill_manual(values = cb[c(2,4,6)]) +
  facet_wrap(~Year, ncol = 1, scale = "free_y") +
  coord_cartesian(xlim = c(7.4, 8.15)) 

ggsave("./figs/pH_treatments_2013_2017.png", units = 'in', width = 5, height = 5)
