install.packages("cjoint")
library(cjoint)
library(ggplot2)
library(tidyr)
library(dplyr)
library(rcompanion)
library(FSA)
library(car)
library(MASS)
library(reshape)
library(foreign)
library(readstata13)
library(panelr)
library(data.table)
library(tidyverse)


rm(list = ls())
dat <- read.csv("/Users/sergiobejar/Desktop/conjoint.csv")
dat <- na.omit(dat)
dat <- dat[, -c(10)]

library(tidyverse)

dat$dv1_ptychoice1 <- ifelse(dat$dv1_ptychoice == 2, 1, 0)
dat$dv1_ptychoice2 <- ifelse(dat$dv1_ptychoice1  == 1, 0, 1)
dat$dv2_ptysupp1   <- dat$dv2_ptysupp
dat$dv2_ptysupp2   <- 100 - dat$dv2_ptysupp1

names_vars <- c("id", "party", "econ", "ethnic", "gender", "age", "choice", "rating", "id_party", "times")
dat1 <- dat[, c(1:4, 10, 11, 12, 14)]
dat2 <- dat[, c(1, 5:7, 10, 11, 13, 15)]

dat1$id_party <- "A"
dat2$id_party <- "B"

dat1$times <- 1:3
dat2$times <- 1:3

names(dat1) <- names_vars
names(dat2) <- names_vars

dat_full <- rbind(dat1, dat2) %>% arrange(id, times)

rownames(dat_full) <- NULL

dat_full$gender <- factor(dat_full$gender)

dat_full$party  <- factor(ifelse(dat_full$party == 1, "Ruling", "Opposition"))
dat_full$econ   <- factor(ifelse(dat_full$econ  == 1, "ProMarket",
                                 ifelse(dat_full$econ  == 2, "Indifferent", "Opposes Markets")))
dat_full$ethnic <- factor(ifelse(dat_full$ethnic == 1, "SingleEthnic", "MultiEthni"))

##Call library "cregg"
library(cregg)

##Marginal Means (Mean outcomes across all conjoint feature attributes)
f1 <- choice ~ party + econ + ethnic
plot(mm(dat_full, f1, id = ~id), vline = 0.5)

##AMCEs

amces <- cj(dat_full, f1, id = ~id)
plot(amces)

##Diagnostic Reference Category (Allows to choose most appropriate reference category)
amce_diagnostic <- amce_by_reference(dat_full, choice ~ econ, ~econ, id = ~id)
plot(amce_diagnostic, group = "REFERENCE", legend_title = "Reference Category")

# calculate marginal means and order output by estimate
econ_mms <- mm(dat_full, choice ~ econ, id = ~id)
econ_mms <- econ_mms[order(econ_mms$estimate), ]


mm_by <- cj(dat_full, choice ~ party + econ + ethnic, id = ~id, estimate = "mm", by = ~gender)
plot(mm_by, group = "gender", vline = 0.5)

# calculate interaction AMCEs (ACIEs)
amces_2 <- cj(dat_full, choice ~ party, id = ~id, estimate = "amce", by = ~gender)
diff_amces_2 <- cj(dat_full, choice ~ party, id = ~id, estimate = "amce_diff", by = ~gender)
plot(rbind(amces_2, diff_amces_2)) + ggplot2::facet_wrap(~BY, ncol = 3L)

cj_anova(dat_full, choice ~ party + econ + ethnic, by = ~gender)

