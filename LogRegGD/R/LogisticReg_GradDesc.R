

#Fonction pour la prediction  avec la regression logistique
#' Segmoid funcion
#'
#' @param a linear combination of predictor variables
#'
#' @return a vector of probabilities of belonging to the positive or negative class
#' @export
#'

RegLogFonction <- function(a)
{
  return(1/(1+exp(-a)))
}

# Fonction pour le calcul du gradient
#' Gradient function
#'
#' @param expli  the predictor variables
#' @param cible  the target (the variable to predict)
#' @param coeff  the logistic regression coefficients to be estimated
#'
#' @return a gradient vector
#' @export
#'

Gradient <- function(expli, cible, coeff)
{
  p = RegLogFonction(expli %*% t(coeff))
  grad = (1/nrow(cible)) * (t(expli) %*% (cible - p))
  return(t(grad))
}

# Fonction pour la fonction de cout de la regression logistique
#' Logistic regression loss function
#'
#' @param expli the predictor variables
#' @param cible the target (the variable to predict)
#' @param coeff the logistic regression coefficients to be estimated
#'
#' @return a vector
#' @export
#'

FonctionCout <- function(expli, cible, coeff)
{
  p = RegLogFonction(expli %*% t(coeff))
  J = ifelse(p==1, cible * log(p), cible * log(p) + (1-cible) * log(1-p))
  return(J)
}

# Fonction pour la mise a jour des coefficients
#' Update coefficients function
#'
#' @param expli the predictor variables
#' @param cibles the target (the variable to predict)
#' @param coeff the logistic regression coefficients to be estimated
#' @param learning_rate is the learning rate for gradient descent
#' @param tolerance is the minimum movement allowed for each iteration
#' @param convergence   logical true/false
#'
#' @return  a list containing updated coefficients and convergence status
#' @export

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
#' the function of changes in the learning rate
#'
#' @param learning_rate learning rate for gradient descent
#' @param nb_iter number of iterationn
#'
#' @return
#' @export

#'
EvolEta <- function(learning_rate, nb_iter)
{
  return(learning_rate / nb_iter^0.25)
}



# Regression logistique avec la descente de gradient
#' Logistic regression function with gradient descent
#'
#' @param x the predictor variables
#' @param y the target (the variable to predict)
#' @param eta the learning rate for gradient descent. Default value: 0.3
#' @param max_iter the number of iterations,default value:: 500
#' @param tol the minimum movement allowed for each iteration.Default value: 1e-3
#' @param mode_desc the mode chosen to be used to update coefficients of stochastic gradient descent.We have 3 posiblities: {« batch », « online », « mini_batch »}
#' @param batch_size the number of observations for the mini-batch mode; Default value: 32
#' @param nc indicates the number of cores to be used in parallel programming
#'
#'
#' @return a list of (theta(coefficients), a number of iteration and deviances)
#'
#' @export
#'

GradDescente <- function(x, y, eta = 0.3, max_iter = 500, tol = 1e-3, mode_desc="batch", batch_size=32, nc)
{
  # Standardisation des variables explicatives
  x = scale(as.matrix(x))

  #Dans le cas où il n'y a qu'une seule variable explicative
  if (is.null(ncol(x)))
  {
    x = as.matrix(x, ncol=1)
  }

  # Ajout d'une colonne de 1 au debut de la matrice des variables explicatives pour l'intercept
  x = as.matrix(cbind(intercept = (rep(1,nrow(x))),x))

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
      # Fonction de cout calculee a chaque iteration
      cout = sapply(list(x_tmp), FonctionCout, cible = y_tmp, coeff = coef)
      deviance = -2 * sum(cout)
      fonctionLoglos = c(fonctionLoglos, (-1/nrow(x_tmp))*sum(cout, na.rm = T))
      plot(fonctionLoglos, type='l', xlab ='Iterations', ylab='Cost function', main='Cost function during iterations')

      # Pour la parallelisation
      if(nc > 1)
      {
        d = data.frame(y=(y_tmp),x_tmp)

        # Programmation parallele
        doParallel::registerDoParallel()
        blocs = split.data.frame(d,1+(1:nrow(d))%%nc)

        res_interm = foreach::foreach(b=blocs, .combine = 'cbind',.export = c("MajCoefficient","Gradient","RegLogFonction")) %dopar%
          {
            y_para = as.matrix(b[,1])
            x_para = as.matrix(b[,2:ncol(d)])
            cop <- MajCoefficient(x_para, y_para, coef, eta, tol, converge)
            return(cop)
          }

        doParallel::stopImplicitCluster()

        # Mise a jour des coefficients
        coef = res_interm[1,]
        coef= do.call(rbind, coef)
        coef = apply(as.matrix(coef), MARGIN = 2, FUN=mean)
        coef = t(as.matrix(coef))

        # Convergence
        converge = res_interm[2,]
        converge = do.call(rbind,converge)

        if (TRUE %in% converge)
        {
          converge=TRUE
        } else
        {
          converge=FALSE
        }

      } else # Pour la programmation sequentielle
      {
        # Mise a jour des coefficients
        res = MajCoefficient(x_tmp, y_tmp, coef, eta, tol, converge)
        converge = res$convergence
        coef = res$coefficient
      }
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
        cout = sapply(list(xi), FonctionCout, cible = yi, coeff = coef)
        fonctionCost = c(fonctionCost, cout)

        # Mise a jour des coefficients
        res = MajCoefficient(xi, yi, coef, eta, tol, converge)
        coef = res$coefficient
      }

      # Calcul de la deviance residuelle
      deviance = -2 * sum(fonctionCost)

      # Coritere de convergence
      converge = res$convergence

      # Fonction de cout calculee a chaque iteration
      loglos = (-1/nrow(x_tmp))*sum(fonctionCost, na.rm = T)
      fonctionLoglos = c(fonctionLoglos,loglos)
      plot(fonctionLoglos, type='l', xlab ='Iterations', ylab='Cost Function', main='Cost function during iterations')

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
        # Un individu apres l'autre
        if (batch_size == 1)
        {
          xj = t(as.matrix(x_tmp[j:(j+batch_size-1),]))
        } else # Bloc d'individus
          {
          xj = x_tmp[j:(j+batch_size-1),]
          }

        yj = as.matrix(y_tmp[j:(j+batch_size-1),])

        # Cout pour chaque batch
        cout = sapply(list(xj), FonctionCout, cible = yj, coeff = coef)
        fonctionCost = c(fonctionCost, cout)

        # Mise a jour des coefficients
        res = MajCoefficient(xj, yj, coef, eta, tol, converge)
        converge = res$convergence
        coef = res$coefficient

      }

      # Calcul de la deviance residuelle
      deviance = -2 * sum(fonctionCost, na.rm = T)

      # Evolution du eta au fil des iterations
      eta = EvolEta(eta, iter)

      # Fonction de cout calculee à chaque iteration
      loglos = (-1/nrow(x_tmp))*sum(fonctionCost, na.rm = T)
      fonctionLoglos = c(fonctionLoglos,loglos)
      plot(fonctionLoglos, type='l', xlab ='Iterations', ylab='Cost Function', main='Cost function during iterations')

    }

  }
  return(list(coef=coef,nbIter=iter, dev = deviance))
}

