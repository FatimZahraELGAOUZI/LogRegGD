rm(list=ls())

#Fonction pour la prédiction  avec la régression logistique
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

# Fonction pour la fonction de coût de la régression logistique
FonctionCout <- function(expli, cible, coeff)
{
  p = RegLogFonction(expli %*% t(coeff))
  J = (-1/nrow(cible)) * sum(cible * log(p) + (1-cible) * log(1-p))
  return(J)
}

# Fonction pour la mise à jour des coefficients
MajCoefficient <- function(expli, cibles, coeff, learning_rate, tolerance, convergence)
{
  coefBis = coeff + learning_rate * Gradient(expli,cibles,coeff)

  # Vérification
  if (sum(abs(coefBis-coeff)) < tolerance)
  {
    convergence = TRUE
  }
  return(list(coefficient = coefBis, convergence = convergence ))
}

# Fonction pour l'évolution du taux d'apprentissage
EvolEta <- function(learning_rate, nb_iter)
{
  return(learning_rate / nb_iter^0.25)
}



# Régression logistique avec la descente de gradient
GradDescente <- function(x, y, eta = 0.3, max_iter = 500, tol = 1e-4, mode_desc="batch", batch_size=32, seed = NULL)
{
  # Standardisation des variables explicatives
  x = scale(x)

  #Dans le cas où il n'y a qu'une seule variable explicative
  if (is.null(ncol(x)))
  {
    x = as.matrix(x, ncol=1)
  }

  # Ajout d'une colonne de 1 au début de la matrice des variables explicatives pour l'intercept
  #intercept =
  x = as.matrix(cbind(rep(1,nrow(x)),x))

  # Modification de y en matrice
  y = matrix(y)

  # Initialisation de la matrice des coefficients de départ de taille nombre de variables + 1 (intercept)
  coef = matrix(runif(n = ncol(x)), nrow = 1)

  # Initialisation du nombre d'itérations
  iter = 0

  # Initialisation du vecteur contenant les valeurs de la fonction de coût
  fonctionCost = c()

  # Initialisation de la convergence
  converge = FALSE

  while ((iter < max_iter) && (converge == FALSE))
  {
    # Itération suivante
    iter = iter +1

    # Pour reproduire les mêmes résultats sur plusieurs appels de la fonction
    if (!is.null(seed))
    {
      set.seed(seed)
    }

    # Identifiants des individus mélange (essentiel pour batch et mini-batch)
    id = sample(nrow(x), nrow(x))
    x_tmp = as.matrix(x[id,])
    y_tmp = as.matrix(y[id,])

    # Descente de gradient
    if (mode_desc=="batch")
    {
      # Fonction de coût calculé à chaque itération
      cout = FonctionCout(x,y,coef)
      fonctionCost = c(fonctionCost, cout)
      plot(fonctionCost, type='l', xlab ='Itérations', ylab='Fonction de coût', main='Fonction de coût au fil des itérations')

      # Mise à jour des coefficients
      res = MajCoefficient(x, y, coef, eta, tol, converge)
      converge = res$convergence
      coef = res$coefficient
    }

    # Descente de gradient stochastique online
    if (mode_desc == "online")
    {
      # Pour chaque observation
      for (i in 1:nrow(x_tmp))
      {
        xi = t(as.matrix(x_tmp[i,]))
        yi = as.matrix(y_tmp[i,])

        # Mise à jour des coefficients
        res = MajCoefficient(xi, yi, coef, eta, tol, converge)
        converge = res$convergence
        coef = res$coefficient
      }

      # Evolution du eta au fil des itérations
      eta = EvolEta(eta, iter)

      # Fonction de coût calculée à chaque itération
      cout = FonctionCout(xi,yi,coef)
      fonctionCost = c(fonctionCost, cout)
      plot(fonctionCost, type='l', xlab ='Itérations', ylab='Fonction de coût', main='Fonction de coût au fil des itérations')

    }

    # Descente de gradient stochastique mini-batch
    if (mode_desc == "mini-batch")
    {
      # Pour chaque bloc d'individus
      for(j in seq(1,nrow(x_tmp),batch_size))
      {
        # Quand il reste moins d'individus que la taille du batch renseigné
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

        # Mise à jour des coefficients
        res = MajCoefficient(xj, yj, coef, eta, tol, converge)
        converge = res$convergence
        coef = res$coefficient
      }
      # Evolution du eta au fil des itérations
      eta = EvolEta(eta, iter)

      # Fonction de coût calculée à chaque itération
      cout = FonctionCout(xj,yj,coef)
      fonctionCost = c(fonctionCost, cout)
      plot(fonctionCost, type='l', xlab ='Itérations', ylab='Fonction de coût', main='Fonction de coût au fil des itérations')

    }
  }
  return(list(coef=coef,nbIter=iter))
}



#setwd('/Users/learegazzetti/Library/Mobile Documents/com~apple~CloudDocs/M2_SISE/Prog R/Projet/Test_diabete')
#d = read.csv('pima-indians-diabetes.csv', sep=",", header = FALSE)
#d = d[0:760,]



# test du mini-batch
#GradDescente(d[,c('V1')],d$V9, mode_desc="mini-batch", max_iter = 500, eta=0.31, seed = 1) # OK mêmes valeurs que glm
#GradDescente(d[,c('V1','V2','V3','V4','V5','V6','V7','V8')],d$V9, mode_desc="mini-batch", max_iter = 500, eta=0.31, batch_size = 76) #  OK mêmes valeurs que glm


# test du online
#GradDescente(d[,c('V1')],d$V9, max_iter = 500, eta = 0.31, mode_desc="online", seed = 1) # pas les bons résultats
#GradDescente(d[,c('V1','V2')],d$V9, max_iter = 500, eta = 0.31, mode_desc="online")  # résultats moyens comparés à glm


# test du batch
#GradDescente(d[,c('V1')],d$V9, max_iter = 500, eta = 0.31, mode_desc="batch", seed = 1) # OK mêmes valeurs que glm
#GradDescente(d[,c('V1','V2')],d$V9, max_iter = 500, eta = 0.31, mode_desc="batch") # OK mêmes valeurs que glm


#d = data.frame(cbind(scale(d[,1:8]),d$V9))


#modele = glm(V9 ~ V1, data = d, family = binomial)
#print(modele$coefficients)

