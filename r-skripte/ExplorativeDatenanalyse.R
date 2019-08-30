### Daten

# ggf. einmalig installieren
#install.packages("SixSigma")
# Vignette herunterladen
vignette("HelicopterInstructions")
# Bauplan generieren
SixSigma::ss.heli()

heli <- read.csv("data/heli.csv", header=TRUE)

library(mosaic)

str(heli)


### Analyse kategorialer Daten

gf_bar( ~ Paper, # (unabhängige) Variable, die analysiert wird
        data = heli) # Datensatz

prop( ~ Paper, # (unabhängige) Variable, die analysiert wird
      data = heli) # Datensatz

prop( ~ Paper, # (unabhängige) Variable, die analysiert wird
      success = "Light", # wovon den Anteil bestimmen
      data = heli) # Datensatz

tally( ~ Paper, # Variable, die analysiert wird
       data = heli) # Datensatz

tally( ~ Paper, # Variable, die analysiert wird
       format = "proportion", # Option: Anteile
       data = heli) # Datensatz

gf_bar( ~ DirFold # Variable, die analysiert wird
        | Paper, # Variable, nach der bedingt wird
        data = heli) # Datensatz

tally( ~ DirFold # Variable, die analysiert wird
       | Paper, # Variable, nach der bedingt wird
       data = heli) # Datensatz

tally( ~ DirFold # Variable, die analysiert wird
       | Paper, # Variable, nach der bedingt wird
       format = "proportion", # Option: Anteile
       data = heli) # Datensatz

tally( ~ Paper | DirFold, format = "proportion", data = heli)

tally( ~ DirFold | Paper, format = "proportion", data = heli)

mosaicplot(DirFold ~ Paper, data = heli, color = TRUE)


### Analyse numerischer (metrischer) Daten

gf_histogram( ~ Time, data = heli,
              binwidth = 2, center = 1, color = "white")

pdata( ~ Time, q = 10, data = heli)

qdata( ~ Time, p = 0.9, data = heli)

mean( ~ Time, data = heli)

inspect(heli)

favstats( ~ Time, # Variable, die analysiert wird
          data = heli) # Datensatz

gf_boxplot(Time ~ 1, data = heli)

gf_boxplot(Time ~ # abhängige Variable
             Paper, # unabhängige Variable
           data = heli) # Datensatz

favstats(Time ~ Paper, data = heli)

gf_point(Time ~ Paper, stat = "summary", size = 5, data = heli)

favstats(Time ~ 1, data = heli)

favstats(Time ~ Paper, data = heli)


### Zusammenhang zwischen numerischen Variablen

gf_point( Time # Variable auf y-Achse
          ~ RotorL, # Variable auf x-Achse
          data = heli) # Datensatz

cor(Time ~ RotorL, # Variablen
    data = heli) # Datensatz


