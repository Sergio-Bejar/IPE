library(RRreg)
library(ggplot2)
library(tidyr)
library(dplyr)




set.seed(1234)

turkey <- read.csv("/Users/sergiobejar/Desktop/tky.csv")

turkey <- na.omit(turkey)

unimod <- RRuni(response = r2_s48CW, data = turkey, model = "Warner", p= .3, group = r2_s48SE)

summary(unimod)

mod_log <- RRlog(formula = r2_s48CW ~ ed + X0ge + m0le, data = turkey, model = "Crosswise", p= .32, group = r2_s48SE)

summary(mod_log)
plot(mod_log, "X0ge" ,ci=.95, type = "response", ylim = 0:1)

cormod <- RRcor(x= turkey[,c("r2_s48CW", "ed", "X0ge", "m0le")], models = c("Crosswise", "d", "d", "d"), p.list = list(c(1/3)), group = turkey[,"r2_s48SE"], bs.type = c("se.n", "pval"), bs.n = 1000, nCPU = 1)
cormod

modlog <- RRlog(formula = r2_s48CW ~ ed * X0ge, data = turkey, model = "Crosswise", p= .32, group = r2_s48SE)
summary(modlog)
plot(modlog, "ed:X0ge" ,ci=.95, type = "attribute", ylim = 0:1)



sim.power <- RRsimu(numRep = 1000, n = 214, pi, cor = .3, model = "Crosswise", p = .32, groupRatio = .45, complyRates = c(1,1), sysBias = c(0,0), nCPU = 1)

power <- powerplot(1000, model = "Crosswise", p = .32, pi = .6, n = c(10, 250), cor = c(0, .3, .5), method = "RRlog", nCPU = 1)
plot(power)








