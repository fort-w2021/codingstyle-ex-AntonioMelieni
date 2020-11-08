### Übung 6


# Laden von benötigten Paketen:
library(tidyverse)


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
  x <- model.matrix(glmobject)
  # Berechnen von sqrt(W^(t))*X
  wsqrtX <- sqrt(w) * x
  # Inverse Fisher-Matrix: hier gleich der skalierten Kovarianzmatrix von \beta
  finv <- summary(glmobject)$cov.scaled
  hii <- diag(wsqrtX %*% finv %*% t(wsqrtX))
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
candhl <- which(hii > 2 * p / n)
candhl
# Beobachtungen 33 und 108

# Vergleich mit in R implementierter Funktion "hatvalues"
hii_r <- hatvalues(model_logit)
sum((hii - hii_r)^2)
all.equal(hii, hii_r)
# gleiches Ergebnis wie bei der in R implementierten Funktion
