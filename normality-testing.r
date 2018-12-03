rm(list = ls())

library(nortest)

library(magrittr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(reshape2, warn.conflicts = FALSE)
library(ggplot2)

AlternativeHypothesisGenerator <- function(n) {
  rt(n, df = 2)  # Generador de valores procedentes de una distribución t_2.
}

PowerTest <- function(test, n, alpha = 0.05, nsim = 1000,
                      generator = AlternativeHypothesisGenerator){
  mean(replicate(nsim, test(generator(n))$p.value < alpha))
}

LillieforsPower <- function(n, ...) {
  PowerTest(lillie.test, n, ...)
}

CramerVonMisesPower <- function(n, ...) {
  suppressWarnings(PowerTest(cvm.test, n, ...))
}

AndersonDarlingPower <- function(n, ...) {
  PowerTest(ad.test, n, ...)
}

ShapiroFranciaPower <- function(n, ...) {
  PowerTest(sf.test, n, ...)
}

results <-
  data.frame(n = c(10, 30, 60, 100, 150, 300, 500)) %>%
  rowwise() %>%
  mutate(Lilliefors      = LillieforsPower(n),
         CramerVonMises  = CramerVonMisesPower(n),
         AndersonDarling = AndersonDarlingPower(n),
         ShapiroFrancia  = ShapiroFranciaPower(n))

results.plot <- results %>%
    gather(method, value, - n) %>%
    ggplot(aes(x = n, y = value, colour = method)) +
        geom_line() +
        theme(legend.position = "top") +
        xlab("Tamaño Muestral") +
        ylab("Potencia") +
        labs(colour = "Método")

write.csv(results, "normality-testing.csv", row.names = FALSE, quote = FALSE)
ggsave("normality-testing.png", results.plot, width = 7, height = 4, dpi = 120)
