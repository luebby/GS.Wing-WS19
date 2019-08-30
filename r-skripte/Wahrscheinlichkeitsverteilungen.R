### Normalverteilung

library(mosaic)

xpnorm(44.5, mean = 55, sd = 15)

zscore(c(0,1,2))

zscore(c(100,200,300))

xpnorm(60, mean = 55, sd = 15)

xpnorm(c(-2,2))

xqnorm(0.9, mean = 55, sd = 15)


### t-Verteilung

# Normalverteilung, Zeichnen von x = -5 bis +5
gf_dist("norm", xlim = c(-5, 5)) %>%
  # Darstellungsbereich für x auf -5 bis +5 einschränken
  gf_lims(x = c(-5, 5)) %>%
  # t-Verteilung mit 1 Freiheitsgrad
  gf_dist("t", color = "blue", df = 1) %>%
  # t-Verteilung mit 5 Freiheitsgrad
  gf_dist("t", color = "blue", df = 5, linetype = 2) %>%
  # t-Verteilung mit 20 Freiheitsgrad
  gf_dist("t", color = "blue", df = 20, linetype = 3)


## Exponentialverteilung

gf_dist("exp")

pexp(6, rate = 1/10)

qexp(0.5, rate = 1/10)


## X^2- un F-Verteilung

# Chi2-Verteilung mit einem Freiheitsgrad, Zeichnen von x = 0 bis +25
gf_dist("chisq", df = 1, xlim = c(0, 25)) %>%
  # Grenzen der y-Achse festlegen
  gf_lims(y = c(0, 0.5)) %>%
  # Chi2-Verteilung mit 3 Freiheitsgraden
  gf_dist("chisq", df = 3, xlim = c(0, 25), linetype = 2) %>%
  # Chi2-Verteilung mit 6 Freiheitsgraden
  gf_dist("chisq", df = 6, xlim = c(0, 25), color = "blue") %>%
  # Chi2-Verteilung mit 9 Freiheitsgraden
  gf_dist("chisq", df = 9, xlim = c(0, 25), color = "blue", linetype = 2)

# F-Verteilung mit 1 und 9 Freiheitsgraden
gf_dist("f", df1 = 1, df2 = 9) %>%
  # Grenzen der Achsen festlegen
  gf_lims(x = c(0, 8), y = c(0, 0.8)) %>%
  # F-Verteilung mit 4 und 6 Freiheitsgraden
  gf_dist("f", df1 = 4, df2 = 6, linetype = 2) %>%
  # F-Verteilung mit 8 und 2 Freiheitsgraden
  gf_dist("f", df1 = 8, df2 = 2, color = "blue", xlim = c(0, 8)) %>%
  # F-Verteilung mit 12 und 1 Freiheitsgraden
  gf_dist("f", df1 = 12, df2 = 1, color = "blue", linetype = 2,
          xlim = c(0, 8))


### Binomialverteilung

gf_dist("binom", size = 10, prob = 0.3)

xpbinom(1, size = 20, prob = 0.05)

xqbinom(0.5, size = 20, prob = 0.1)

