#DEVOIR 1 EDIMA BIYENDA HILDEGARDE ISEP2_ENSAE

# D�finition de la fonction

f <- function(x, y) {
  return(x^2 + cos(x+y)+(y^2+1/(1+x^2)^(1/2)))
}
# Cr�ation d'une grille de valeurs pour x et y

x <- seq(-2, 2, length.out = 100)
y <- seq(-2, 2, length.out = 100)
grid <- expand.grid(x = x, y = y)

#calcul du minimum et du maximum de la fonction

max <- max(f(x,y))
min <- min(f(x,y))
print(paste("Le Maximum de f(x)est:", max))
print(paste("Le Minimum de f(x)est:", min))

# Calcule de la d�riv�e partielle par rapport � x

df_dx <- function(x, y) {
  h <- 0.00001
  return((f(x + h, y) - f(x, y)) / h)
}
# Calcule de la d�riv�e partielle par rapport � y
df_dy <- function(x, y) {
  h <- 0.00001
  return((f(x, y + h) - f(x, y)) / h)
}
#affectation de la 3e dimension

z <- outer(x, y, df_dx)  
t <- outer(x, y, df_dy) 
r <- outer(x, y, f)

# afficher la fonction f

persp(x, y, r, theta = 30, phi = 30, expand = 0.5, col = "yellow", xlab = "x", ylab = "y", zlab = "f(x, y)", main = "représentation de f")

# pour la d�riv�e x
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "red", xlab = "x", ylab = "y", zlab = "df/dx", main = "représentation de df/dx")
# pour la d�riv� y
persp(x, y, t, theta = 30, phi = 30, expand = 0.5, col = "green", xlab = "x", ylab = "y", zlab = "df/dy", main = "représentation de df/dy")

