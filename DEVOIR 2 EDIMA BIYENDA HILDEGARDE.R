## DEVOIR N°2

# Création d'un data frame fictif
df <- data.frame(
  ID = 1:10,
  Age = c(25, 30, 22, 40, 28, 35, 19, 27, 33, 29),
  Nom = c("Alice", "Bob", "Claire", "David", "Eva", "Frank", "Grace", "Hugo", "Iris", "Jack"),
  Genre = factor(c("F", "M", "F", "M", "F", "M", "F", "M", "F", "M")),
  Salaire = c(50000, 60000, 55000, 70000, 52000, 65000, 48000, 53000, 62000, 54000)
)

# Création dune matrice / extraction des colonnes "Age", "Genre", "Salaire"
matrice <- df[, c("Age", "Salaire", "Genre")]

# Renommer les lignes
rownames(matrice) <- c("Individu1", "Individu2","Individu3", "Individu4","Individu5","Individu6","Individu7","Individu8","Individu9", "Individu10")

# Renommer les colonnes
colnames(matrice) <- c("Âge", "Salaire", "Genre")

# Statistiques descriptives
summary(matrice)

# Histogrammes
hist(df$Age, main="Histogramme de l'âge", xlab="Âge", ylab="Fréquence")

# Camembert
pie(table(df$Genre), main="Répartition par genre")


## ************************ problème d'optimisation*************************
.
  
  # Exercice 1 : Optimisation sous contraintes
  
  # Fonction objectif
  f <- function(x1, x2) {
    return((x1 - 3)^2 + (x2 - 2)^2)
  }
  
  # Contraintes
  g1 <- function(x1, x2) {
    return(x1^2 + x2^2 - 4)
  }
  
  g2 <- function(x1, x2) {
    return(x1 + 2*x2 - 2)
  }
  
  g3 <- function(x1) {
    return(-x1)
  }
  
  # Point x*
  x_star <- c(3, 2)
  
  # Qualification du point x*
  grad_g1 <- c(2*x_star[1], 2*x_star[2])
  grad_g2 <- c(1, 2)
  
  # Conditions KKT
  lambda1 <- 0
  lambda2 <- 2
  
  # Optimalité globale
  # Vérification de la convexité (hessien positif) pour f, g1 et g2
  # Le point x* est un minimum global
  
  # Représentation graphique :

  # Chargement de la bibliothèque ggplot2 (si elle n'est pas déjà chargée)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  
  # Définition de la fonction objectif
  f <- function(x1, x2) {
    return((x1 - 3)^2 + (x2 - 2)^2)
  }
  
  # Contraintes
  g1 <- function(x1, x2) {
    return(x1^2 + x2^2 - 4)
  }
  
  g2 <- function(x1, x2) {
    return(x1 + 2*x2 - 2)
  }
  
  g3 <- function(x1) {
    return(-x1)
  }
  
  # Création d'une grille de points pour le tracé
  x1_vals <- seq(-1, 5, length.out = 100)
  x2_vals <- seq(-1, 5, length.out = 100)
  grid <- expand.grid(x1 = x1_vals, x2 = x2_vals)
  
  # Calcul des valeurs de f(x1, x2) sur la grille
  grid$f_vals <- f(grid$x1, grid$x2)
  
  # Filtrage des points dans l'ensemble réalisable
  feasible_points <- grid[g1(grid$x1, grid$x2) <= 0 & g2(grid$x1, grid$x2) <= 0, ]
  
  # Tracé des contraintes
  # Plot the contours of the objective function
  contour_levels <- seq(0, 20, by = 2)
  contour(feasible_points$x1, feasible_points$x2, matrix(feasible_points$f_vals, nrow = length(x1_vals)), levels = contour_levels, xlab = "x1", ylab = "x2", main = "Optimization Problem")
  
  # Add the feasible region
  polygon(c(0, 1, 2, 3, 0), c(2, 0, 0, 2, 2), col = "lightblue", border = NA)
  
  # Add the optimal point
  points(3, 2, col = "red", pch = 19)
  
  # Add labels
  text(3, 2, "x*", pos = 4)
  
  # Add constraint lines
  abline(a = 0, b = 1, col = "red")
  abline(a = 0, b = -2, col = "red")
  
  # Add axis labels
  axis(1)
  axis(2)
  
  # Add legend
  legend("topright", legend = c("Feasible Region", "Optimal Point"), fill = c("lightblue", "red"))
  
  # Show the plot
