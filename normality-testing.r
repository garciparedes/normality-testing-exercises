rm(list = ls())

library(magrittr)
library(IRdisplay, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(Bhat)
library(ggplot2)
library(reshape2)
library(tidyr, warn.conflicts = FALSE)
library(nortest)

alpha <- 0.05
PowerTest <- function(fn, n, nsim){
  mean(replicate(nsim, fn(rt(n, df = 2))$p.value < alpha))
}


LillieforsPower <- function(n, nsim = 1000) {
  PowerTest(lillie.test, n, nsim)
}

CramerVonMisesPower <- function(n, nsim = 1000) {
  PowerTest(cvm.test, n, nsim)
}

AndersonDarlingPower <- function(n, nsim = 1000) {
  PowerTest(ad.test, n, nsim)
}


ShapiroFranciaPower <- function(n, nsim = 1000) {
  PowerTest(sf.test, n, nsim)
}

alpha <- 0.05
results <- data.frame(n = c(10, 30, 60, 100, 150, 300, 500))
results$Lilliefors <-
  results$n %>%
  sapply(function(n) {
    LillieforsPower(n)
  })
results$Cramer.Von.Mises <-
  results$n %>%
  sapply(function(n) {
    suppressWarnings(CramerVonMisesPower(n))
  })
results$Anderson.Darling <-
  results$n %>%
  sapply(function(n) {
    AndersonDarlingPower(n)
  })

results$Shapiro.Francia <-
  results$n %>%
  sapply(function(n) {
    ShapiroFranciaPower(n)
  })

results

results.plot <- results %>%
    gather(method, value, - n) %>%
    ggplot(aes(x = n, y = value, colour = method)) +
        geom_line() +
        theme(legend.position = "top") +
        xlab("Tamaño Muestral") +
        ylab("Potencia") +
        labs(colour = "Método")
results.plot

ggsave("plot.png", results.plot, width = 7, height = 5, dpi = 120)
