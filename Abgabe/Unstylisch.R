### Übung 6 GRM


# Laden von benötigten Paketen:
library(tidyverse)


## d)

# Laden des Datensatzes (siehe vorheriges Blatt)
foodstamp <- read.table("foodstamp.dat", header = FALSE)
colnames(foodstamp) <- c("y", "TEN", "SUP", "INC")
head(foodstamp)
summary(foodstamp)

# Entfernen der Beobachtungen mit einem Einkommen von 0:
foodstamp <- foodstamp %>% filter(INC > 0)

# Schätzung eines GLMs mit Logit-Link (Logit-Modell):
model_logit <- glm(
  formula = y ~ TEN + SUP + INC,
  family = binomial(link = "logit"), data = foodstamp
)
summary(model_logit)

# Funktion, die die Diagonalelemente der generalisierten Hat-Matrix ausgibt
hatglm <- function(glmobject) {
  # Gewichte aus dem terminalen IWLS-Schritt
  w <- glmobject$weights
  # Extrahieren der Design-Matrix X
  X <- model.matrix(glmobject)
  # Berechnen von sqrt(W^(t))*X
  wsqrtX <- sqrt(w) * X
  # Inverse Fisher-Matrix: hier gleich der skalierten Kovarianzmatrix von \beta
  Finv <- summary(glmobject)$cov.scaled
  hii <- diag(wsqrtX %*% Finv %*% t(wsqrtX))
  return(hii)
}
hii <- hatglm(model_logit)

# Anzahl Beobachtungen im Datensatz
n <- length(foodstamp$y)
# Plotten der Leverages (Diagonalelemente) gegen die Indizes
par(mfrow = c(1, 1))
plot(1:n, hii, type = "b", xlab = "Index", ylab = expression(h[ii]))
p <- length(model_logit$coef)
# H >= 2*p/n sind sogenannte high-leverage Punkte
abline(h = 2 * p / n, lty = 3)

# Identifikation der high-leverage Punkte d.h. wenn man die Daten dieser
# Indizes herausnimmt kann man erwarten, dass sich die Schätzer stärker ändert,
# als wenn andere Datenpunkte herausgelassen werden (Fausregel: h_ii > 2*p'/n):
candHL <- which(hii > 2 * p / n)
candHL
# Beobachtungen 33 und 108

# Vergleich mit in R implementierter Funktion "hatvalues"
hii_R <- hatvalues(model_logit)
sum((hii - hii_R)^2)
all.equal(hii, hii_R)
# gleiches Ergebnis wie bei der in R implementierten Funktion


## e)

# Funktion zur Bestimmung von Cook's Distance:
CookGLM <- function(glmobject) {
  # Response-Werte
  y <- as.vector(model.frame(glmobject)[, 1])
  # Stichprobenumfang
  n <- length(y)
  # Definition des Ergebnisvektors
  cookres <- numeric(n)
  # mit allen Daten geschätzte Koeffizienten
  beta0 <- glmobject$coef
  # Kovarianzmatrix von beta0
  cov0 <- summary(glmobject)$cov.scaled
  # Inversion: Fisher-Matrix des vollen Modells
  covchol <- chol(cov0)
  Fish0 <- chol2inv(covchol)
  # Berechnung aller n Cook's Distances
  for (i in 1:n) {
    # Schätzung bei Weglassen der i-ten Beobachtung
    logittemp <- update(glmobject, data = model.frame(glmobject)[-i, ])
    # zugehöriger Parametervektor
    betatemp <- logittemp$coef
    # Cook's Distance
    cookres[i] <- t(betatemp - beta0) %*% Fish0 %*% (betatemp - beta0)
  }
  return(cookres)
}

cookdistance <- CookGLM(model_logit)
plot(1:n, cookdistance, type = "b", xlab = "Index", ylab = "Cook's Distance")
abline(h = 0.2, lty = 3, col = 2)
# nicht so gleichmäßig verteilt wie die leverages

# 8 Werte mit c_i > 0.2
which(cookdistance > 0.2)

# ein Index, für den beides groß ist
which(cookdistance > 0.2 & hii > 2 * p / n)
# Auffäällige Beobachtung: 108

# Eine große Cook-Distanz impliziert in den meisten Fällen auch einen hohen
# Leverage und umgekehrt.
plot(hii, log(cookdistance))
points(hii[which(cookdistance > 0.2)],
  log(cookdistance)[which(cookdistance > 0.2)],
  col = "red", pch = 3
)
points(hii[which(hii > 2 * p / n)], log(cookdistance)[which(hii > 2 * p / n)],
  col = "red", pch = 4
)
