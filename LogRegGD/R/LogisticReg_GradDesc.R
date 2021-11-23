#rm(list=ls())

#Fonction pour la prediction  avec la regression logistique
RegLogFonction <- function(a)
{
  return(1/(1+exp(-a)))
}

# Fonction pour le calcul du gradient
Gradient <- function(expli, cible, coeff)
{
  p = RegLogFonction(expli %*% t(coeff))
  grad = (1/nrow(cible)) * (t(expli) %*% (cible - p))
  return(t(grad))
}

# Fonction pour la fonction de cout de la regression logistique
FonctionCout <- function(expli, cible, coeff)
{
  p = RegLogFonction(expli %*% t(coeff))
  J = cible * log(p) + (1-cible) * log(1-p)
  return(J)
}

# Fonction pour la mise à jour des coefficients
MajCoefficient <- function(expli, cibles, coeff, learning_rate, tolerance, convergence)
{
  coefBis = coeff + learning_rate * Gradient(expli,cibles,coeff)

  # Verification
  if (sum(abs(coefBis-coeff)) < tolerance)
  {
    convergence = TRUE
  }
  return(list(coefficient = coefBis, convergence = convergence ))
}

# Fonction pour l'evolution du taux d'apprentissage
EvolEta <- function(learning_rate, nb_iter)
{
  return(learning_rate / nb_iter^0.25)
}



# Regression logistique avec la descente de gradient
GradDescente <- function(x, y, eta = 0.3, max_iter = 500, tol = 1e-4, mode_desc="batch", batch_size=32)
{
  # Standardisation des variables explicatives
  x = scale(x)

  #Dans le cas où il n'y a qu'une seule variable explicative
  if (is.null(ncol(x)))
  {
    x = as.matrix(x, ncol=1)
  }

  # Ajout d'une colonne de 1 au debut de la matrice des variables explicatives pour l'intercept
  #intercept =
  x = as.matrix(cbind(rep(1,nrow(x)),x))

  # Modification de y en matrice
  y = matrix(y)

  # Initialisation de la matrice des coefficients de depart de taille nombre de variables + 1 (intercept)
  coef = matrix(runif(n = ncol(x)), nrow = 1)

  # Initialisation du nombre d'iterations
  iter = 0

  # Fonction de cout
  fonctionLoglos=c()

  # Initialisation de la convergence
  converge = FALSE

  while ((iter < max_iter) && (converge == FALSE))
  {
    # Iteration suivante
    iter = iter +1

    # Identifiants des individus melange (essentiel pour batch et mini-batch)
    id = sample(nrow(x), nrow(x))
    x_tmp = as.matrix(x[id,])
    y_tmp = as.matrix(y[id,])

    # Descente de gradient
    if (mode_desc=="batch")
    {
      # Fonction de cout calcule à chaque iteration
      cout = FonctionCout(x,y,coef)
      fonctionLoglos = c(fonctionLoglos, (-1/nrow(x))*sum(cout))
      plot(fonctionLoglos, type='l', xlab ='Iterations', ylab='Fonction de cout', main='Fonction de cout au fil des iterations')

      # Mise à jour des coefficients
      res = MajCoefficient(x, y, coef, eta, tol, converge)
      converge = res$convergence
      coef = res$coefficient
    }

    # Descente de gradient stochastique online
    if (mode_desc == "online")
    {
      # Initialisation du vecteur contenant les valeurs de la fonction de cout
      fonctionCost = c()

      # Pour chaque observation
      for (i in 1:nrow(x_tmp))
      {
        xi = t(as.matrix(x_tmp[i,]))
        yi = as.matrix(y_tmp[i,])

        # Cout pour chaque individu
        cout = FonctionCout(xi,yi,coef)
        fonctionCost = c(fonctionCost, cout)

        # Mise à jour des coefficients
        res = MajCoefficient(xi, yi, coef, eta, tol, converge)

        coef = res$coefficient
      }
      converge = res$convergence
      # Fonction de cout calculee à chaque iteration
      loglos = (-1/nrow(x_tmp))*sum(fonctionCost)
      fonctionLoglos = c(fonctionLoglos,loglos)
      plot(fonctionLoglos, type='l', xlab ='Iterations', ylab='Fonction de cout', main='Fonction de cout au fil des iterations')

      # Evolution du eta au fil des iterations
      eta = EvolEta(eta, iter)

    }

    # Descente de gradient stochastique mini-batch
    if (mode_desc == "mini-batch")
    {
      # Initialisation du vecteur contenant les valeurs de la fonction de cout
      fonctionCost = c()

      # Pour chaque bloc d'individus
      for(j in seq(1,nrow(x_tmp),batch_size))
      {
        # Quand il reste moins d'individus que la taille du batch renseigne
        if ((nrow(x_tmp)-j) < batch_size)
        {
          break
        }
        # Un individu après l'autre
        if (batch_size == 1)
        {
          xj = t(as.matrix(x_tmp[j:(j+batch_size-1),]))
        } else # Bloc d'individus
          {
          xj = x_tmp[j:(j+batch_size-1),]
          }

        yj = as.matrix(y_tmp[j:(j+batch_size-1),])

        # Cout pour chaque batch
        cout = FonctionCout(xj,yj,coef)
        fonctionCost = c(fonctionCost, cout)

        # Mise à jour des coefficients
        res = MajCoefficient(xj, yj, coef, eta, tol, converge)
        converge = res$convergence
        coef = res$coefficient

      }
      # Evolution du eta au fil des iterations
      eta = EvolEta(eta, iter)

      # Fonction de cout calculee à chaque iteration
      loglos = (-1/nrow(x_tmp))*sum(fonctionCost)
      fonctionLoglos = c(fonctionLoglos,loglos)
      plot(fonctionLoglos, type='l', xlab ='Iterations', ylab='Fonction de cout', main='Fonction de cout au fil des iterations')

    }
  }
  return(list(coef=coef,nbIter=iter))
}

# class(pima_indians_diabetes[,c("X1")])
# class(pima_indians_diabetes$X9)
# print(pima_indians_diabetes)
# class(pima_indians_diabetes)
# GradDescente(pima_indians_diabetes[,c("X1","X2","X3","X4","X5")],pima_indians_diabetes$X9, max_iter = 100, eta = 0.31, mode_desc="batch")
# modele = glm(X9 ~ X1+X2+X3+X4+X5, data = pima_indians_diabetes, family = binomial)
# print(modele)
