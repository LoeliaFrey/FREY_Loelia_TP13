# -*- coding: utf-8 -*-
# Données pour les femmes (hypothétiques)
set.seed(456)
n <- 50
beers <- sample(1:10, n, replace = TRUE)
bac <- exp(0.1 * beers) + rnorm(n, mean = 0, sd = 0.1) # Relation exponentielle pour les hommes

# Ajustement du modèle de régression polynomial d'ordre 2 pour les hommes
model <- lm(bac ~ poly(beers, 2))

# Données pour les femmes (un peu moins élevées)
bac_femmes <- exp(0.15 * beers) + rnorm(n, mean = 0, sd = 0.1) # Relation exponentielle pour les femmes (un peu moins élevée)
model_femmes <- lm(bac_femmes ~ poly(beers, 2))

# Graphique existant
plot(beers, bac, main="Taux d'Alcoolémie en Fonction du Nombre de Bières Bues", 
     xlab="Nombre de Bières Bues", ylab="Taux d'Alcoolémie", 
     pch=19, col="black", cex=0.8, las=1)

# Centrage de la ligne orange existante (hommes)
beers_seq <- seq(min(beers), max(beers), length.out = 100)
pred <- predict(model, newdata = data.frame(beers = beers_seq))
lines(beers_seq, pred, col="orange", lwd=2)

# Centrage de la ligne rouge pour les femmes (courbe de tendance)
pred_femmes <- predict(model_femmes, newdata = data.frame(beers = beers_seq))
lines(beers_seq, pred_femmes, col="red", lwd=2)

# Ajout des points noirs pour les femmes
points(beers, bac_femmes, pch=19, col="black", cex=0.8)

# Légendes
legend_text <- c("Hommes (Courbe de Tendance)", "Femmes (Courbe de Tendance)", "Données")
legend_colors <- c("orange", "red", "black")
legend_lwd <- c(2, 2, NA)
legend_points <- c(NA, NA, 19)
legend("topleft", legend=legend_text, col=legend_colors, lty=1, lwd=legend_lwd, pch=legend_points, bty="n")
