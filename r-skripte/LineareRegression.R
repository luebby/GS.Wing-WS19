heli <- read.csv("data/heli.csv")
library(mosaic) # Paket laden


### Einfache lineare Regression

gf_point(Time ~ RotorL, data = heli)

erglm1 <- lm(Time ~ # abhängige Variable
               RotorL, # unabhängige Variable(n)
             data = heli) # Datensatz
erglm1

plotModel(erglm1)

summary(erglm1)

predict(erglm1, # Modell
        # Neue Beobachtung mit x=10:
        newdata = data.frame(RotorL = 10),
        # Prognoseintervall:
        interval = "prediction")


### Inferenz – Einführungsbeispiel: Ist die Münze gezinkt?

set.seed(1896) # Reproduzierbarkeit
muenzverteilung <- do(100) *
  rflip(n = 10)


### Inferenz in der linearen Regression

# Original
heli %>% head()

# Reproduzierbarkeit
set.seed(1896)
# Resample
heli %>% head() %>% resample()

# Original
do(1) * lm(Time ~ RotorL, data = heli)

# Reproduzierbarkeit
set.seed(1896)
# Resample
do(3) * lm(Time ~ RotorL, data = resample(heli))

set.seed(1896)
Bootvtlg <- do(10000) *
  lm(Time ~ RotorL, data = resample(heli))

gf_histogram( ~ RotorL, data = Bootvtlg)

sd( ~ RotorL, data = Bootvtlg)

quantile( ~ RotorL, data = Bootvtlg,
          probs = c(0.025, 0.975))

# Original
heli %>% head()

# Reproduzierbarkeit
set.seed(1896)
# Shuffle
heli %>% head() %>% shuffle()

# Original
do(1) * lm(Time ~ RotorL, data = heli)

# Reproduzierbarkeit
set.seed(1896)
# Resample
do(3) * lm(Time ~ shuffle(RotorL), data = heli)

set.seed(1896) # Reproduzierbarkeit
Nullvtlg <- do(10000) *
  lm(Time ~ shuffle(RotorL), data = heli)

gf_histogram( ~ RotorL, data = Nullvtlg)

coef(erglm1) # Koeffizienten
coef(erglm1)[2] # Steigung
abs(coef(erglm1))[2] # Absolutbetrag der Steigung
effektdach <- abs(coef(erglm1))[2] # Zuweisung
effektdach

Nullvtlg <- Nullvtlg %>%
  mutate(effekt0 = abs(RotorL))

prop( ~ (effekt0 >= effektdach), data = Nullvtlg)


### Voraussetzungen Lineare Regression

gf_point(resid(erglm1) ~ fitted(erglm1))

predict(erglm1, # Modell
        # Neue Beobachtung mit x=1000:
        newdata = data.frame(RotorL = 100),
        # Prognoseintervall:
        interval = "prediction")


### Regression mit kategorialer unabhängiger Variable

mean(Time ~ Paper, data = heli)

diffmean(Time ~ Paper, data = heli)

gf_point(Time ~ Paper, data = heli,
         position = "jitter",
         width = 0.1, height = 0)

erglm2 <- lm(Time ~ Paper, data = heli)
summary(erglm2)

set.seed(1896)
Bootvtlg <- do(10000)* lm(Time ~ Paper, data = resample(heli))
gf_histogram( ~ PaperLight, data = Bootvtlg) %>%
  gf_vline(xintercept = ~0)

quantile( ~ PaperLight, probs = c(0.025, 0.975), data = Bootvtlg)

set.seed(1896)
Nullvtlg <- do(10000) * lm(Time ~ shuffle(Paper), data = heli)
dachbeta_PaperLight <- coef(lm(Time ~ Paper, data = heli))[2]
gf_histogram( ~ PaperLight, data = Nullvtlg) %>%
  gf_vline(xintercept = ~dachbeta_PaperLight)

quantile( ~ PaperLight, probs = c(0.025, 0.975), data = Nullvtlg)

lm(Time ~ DirFold, data = heli)


### Multiple Regression

mean(Time ~ 1, data = heli)

lm(Time ~ 1, data = heli)

erglm3 <- lm(Time ~ # abbhängige Variable
               RotorL + Paper, # unabhängige Variablen
             data = heli) # Datensatz
summary(erglm3)

plotModel(erglm3)

set.seed(1896) # Reproduzierbarkeit
Bootvtlg <- do(10000) * lm(Time ~ RotorL + Paper,
                           data = resample(heli))
confint(Bootvtlg)


### Wechselwirkung

erglm4 <- lm(Time ~ RotorL + Paper + RotorL:Paper,
             data = heli)
plotModel(erglm4)

summary(erglm4)

