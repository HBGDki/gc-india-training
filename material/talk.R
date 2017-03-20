library(hbgd)
library(tidyverse)
library(trelliscopejs)
library(plotly)

enroll <- read_csv("data/enrollment.csv")
anthro <- read_csv("data/anthro.csv")
post <- read_csv("data/post.csv")
anthro_enroll <- full_join(anthro, enroll)
dat <- full_join(anthro_enroll, post)

check_data(dat)

names(dat) <- tolower(names(dat))
dat <- rename(dat, agedays = ageday)

check_data(dat)

p <- ggplot(dat, aes(x = agedays, y = htcm)) +
  geom_line(aes(group = subjid), alpha = 0.5) +
  geom_smooth(aes(group = 1), colour = "red", size = 1,
    method = "gam", formula = y ~ s(x))

ggplotly(p)

dat$haz <- who_htcm2zscore(dat$agedays, dat$htcm, dat$sex)
dat$waz <- who_wtkg2zscore(dat$agedays, dat$wtkg, dat$sex)

p <- ggplot(dat, aes(x = agedays, y = haz)) +
  geom_line(aes(group = subjid), alpha = 0.5) +
  geom_smooth(aes(group = 1), colour = "red", size = 1,
    method = "gam", formula = y ~ s(x))

ggplotly(p)

subjdat <- by_subject(dat)
subjdat %>% select(subjid, longi) # just look at two columns

plot(subjdat, subjid = 1)

plot(subjdat, subjid = 1, y_var = "haz")

subjdat <- add_longi_cogs(subjdat)

low_early <- filter(subjdat, min_haz < -4 & min_haz_age < 150) %>% unnest()

ggplot(low_early, aes(x = agedays, y = haz)) +
  geom_line(aes(group = subjid), alpha = 0.5)

subjdat <- add_trajectory_plot(subjdat, y_var = "haz")
subjdat %>% select(subjid, longi, panel) # just look at three columns
trelliscope(subjdat, name = "haz", path = "displays", nrow = 2, ncol = 4)

plot_missing(dat, subject = TRUE)

plot_missing(dat, subject = FALSE)

plot_univar(dat, subject = TRUE)

plot_univar(dat, subject = FALSE)

plot_visit_distn(dat, width = 350, height = 350)

plot_first_visit_age(dat, width = 350, height = 350)

agefreq <- get_agefreq(dat)
plot_agefreq(agefreq)

allfits <- fit_all_trajectories(dat, datfit)

plot(allfits$fit[[2]])

allfits %>%
  add_all_cogs() %>%
  add_trajectory_plot(z = TRUE) %>%
  trelliscope(name = "brokenstick_haz", path = "display_fits", nrow = 2, ncol = 4)
