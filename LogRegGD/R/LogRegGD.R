
#Definition Objet S3

#' Fit function
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.“formula” describes the problem to be solved and must correspond to “target ~feature_1 + feature_2” if explanatory are explicitly specified, or “target ~ .” if all available variables
#' @param data the dataframe containing the variables in the model
#' @param mode the mode chosen to be used to update coefficients of stochastic gradient descent.We have 3 posiblities: {« batch », « online », « mini_batch »}
#' @param batch_size the number of observations for the mini-batch mode
#' @param max_iter is the number of iterations
#' @param tol the minimum movement allowed for each iteration
#' @param eta the learning rate for gradient descent
#' @param ncores indicates the number of cores to be used in parallel programming
#'
#' @return The function returns an object of TYPE S3 that will be used later in the prediction function
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'  fit(formula, data)
#'  fit(formule, data, mode="batch",ncores=2)
#'  fit(formule, data, mode="mini-batch",max_iter = 500, tol = 1e-3, eta = 0.3,batch_size=32)
#'  }
#'
fit <- function(formula, data, mode="batch", ncores=NULL, max_iter = 500, tol = 1e-3, eta = 0.3, batch_size = 32)
{
  #On en profite pour introduire les contrôles
  #Contrôles:
  controlerFormula(formula)
  controlerDataType(data)
  controlerMode(mode)
  controlerItermax(max_iter)
  batch_size=controlerBatch(batch_size, data,mode)
  ncores=ncores(ncores)



  #Creation de l'instance
  instance <- list(call=NULL,model=NULL,coefficient=NULL,formula=NULL,input_data=NULL, y=NULL,iter=NULL,mode=NULL,var=NULL,var_names=NULL, resdev = NULL, resdf = NULL, totdf = NULL, aic = NULL)
  # les donnees
  instance$input_data = data
  #Recuperation et transformation des x et y
  y = formule_extract_cible(formula, data)
  xtemp = formule_donnees_exp(formula, data)
  data = tranformDataset(xtemp,y)
  x = data[["x"]]
  y = data[["y"]]

  #le modele
  instance$model= GradDescente(x, y, eta = eta, max_iter = max_iter, tol = tol, mode_desc=mode, batch_size=batch_size, nc = ncores)
  #l'appel a la fonction fit
  call<-match.call()
  instance$call=call
  #les coefficients
  instance$coefficient=instance$model$coef
  #la formule
  instance$formula=formula
  #la variable a expliquer
  instance$y=y
  #le nombre de variables explicatives
  instance$var <- ncol(x)
  #les noms des variables explicatives
  instance$var_names <- colnames(x)
  #Les moyennes et les ecarts-types des variables explicatives
  instance$Xmeans <- apply(x, 2, mean)
  instance$Xsd<-apply(x, 2, sd)
  # le nombre d'iteration
  instance$iter=instance$model$nbIter
  # le mode de descente de gradient
  instance$mode = controlerMode(mode)
  # deviance residuelle
  instance$resdev = instance$model$dev
  instance$resdf = nrow(x)-(length(instance$coefficient)-1)-1
  #deviance nulle
  instance$totdf = nrow(x)-1
  # AIC
  instance$aic= instance$resdev + 2*length(instance$coefficient)

  class(instance) <- "ObjectLogRegGD"

  return(instance)
}

############################################################ Surcharge Print #####################################################

#' the generic "print" method
#'
#' @param objet is the fitted object
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'  print(objet)
#'  }


print.ObjectLogRegGD <- function(objet)
{
  cat("############################################################################################################### \n")
  cat("\n")
  cat("Call : \n", paste(deparse(objet$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  print(objet$formula)
  cat("Gradient descent : ", objet$mode, "\n")
  cat("Nombre de variables explicatives : ", objet$var, "\n")
  cat("Noms des variables : ", objet$var_names,"\n")
  cat("Nombre d'itérations : ", objet$iter, "\n")
  print("Coefficients :")
  print(objet$coefficient)
  cat("Residual deviance : ", objet$resdev, "\n")
  cat("Degrees of freedom (residual) : ", objet$resdf, "\n")
  cat("Degrees of freedom (total) : ", objet$totdf, "\n")
  cat("AIC : ", objet$aic, "\n")
  cat("\n")
  cat("############################################################################################################### \n")

}

############################################################ Surcharge Summary ###############################################
#' the generic "Summary" method
#'
#' @param objet is the fitted object
#'
#' @return summary of the results
#' @export
#'
#' @examples
#'  \dontrun{
#'  summary(objet)
#'  }
#'
summary.ObjectLogRegGD <- function(objet)
{
  cat("############################################################################################################### \n")
  cat("\n")
  cat("Call : \n", paste(deparse(objet$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  print("Coefficients :")
  print(objet$coefficient)
  cat("Nombre de variables explicatives : ", objet$var, "\n")
  cat("Noms des variables : ", objet$var_names,"\n")
  cat("Nombre d'itérations : ", objet$iter, "\n")
  cat("Residual deviance : ", objet$resdev, "\n")
  cat("Degrees of freedom (residual) : ", objet$resdf, "\n")
  cat("Degrees of freedom (total) : ", objet$totdf, "\n")
  cat("AIC : ", objet$aic, "\n")
  cat("\n")
  cat("############################################################################################################### \n")

}



######################################################## Fonction Predict ###############################################

#' Predict function
#'
#' @param objet_Reg is a S3 object provided by the function fit()
#' @param newdata the dataframe (DataTest) that we will use to make the prediction
#' @param type indicates the type of prediction {“class”: predicted class , “posterior”:belonging class probability}
#'
#' @return Function that return a list (the predicted probs,confusion matrix and error rate) or the belonging class probability
#' @export
#'
#' @examples
#'  \dontrun{
#'  predict(objet,newdata,type= "class")
#'  predict(objet,newdata,type= "posterior")
#'  }



predict<-function (objet_Reg, newdata, type)
{
  #controle de l'objet
  if (class(objet_Reg)!="ObjectLogRegGD")
  {
    stop("Object's class is not ObjectLogRegGD")
  }

  # controle des donnees
  if (FALSE %in% (colnames(newdata) %in% colnames(objet_Reg$input_data)))
  {
    stop(" Number of variables must be the same")
  }

  # controle du type
  if (type !="class"  &&  type != "posterior"){
    stop("type  is not correct, you must be {\"class\" or \"posterior\"}")
  }

  # recuperer x et y
  px = formule_donnees_exp(objet_Reg$formula, newdata)
  py = formule_extract_cible(objet_Reg$formula, newdata)

  #transformation des donnees
  pquali = transformQuali(px) # remplacer NA quali par mode
  pquanti = transformQuanti(px) # on remplace les NA par mean(newdata)
  y = tranformDataset(px,py)$y
  colnames(y) = "y"

  #centrer et reduire
  # Recuperation des moyennes et des ecarts types des donnees
  means <- objet_Reg$Xmeans
  stdvs<-objet_Reg$Xsd
  # On utilise les moyennes et les ecarts types des donnees d'entrainement pour centrer et reduire les donnees tests
  res = as.data.frame(combine(pquali,pquanti))
  res2 = as.data.frame(scale(res, center = means[colnames(res)], scale = stdvs[colnames(res)])[,])
  colnames(res2) = colnames(res)
  newdata = as.data.frame(cbind(res2,y))

  x = as.data.frame(newdata[,intersect(objet_Reg$var_names, colnames(newdata))])
  colnames(x) = intersect(objet_Reg$var_names, colnames(newdata))

  #recuperer les variables dans l'ordre
  pnewdata <- as.data.frame(x[,colnames(x) %in% objet_Reg$var_names])

  #Ajouter l'intercept
  pnewdata<-as.data.frame(cbind(Intercept=(rep(1,nrow(pnewdata))), pnewdata))

  #calculer les proba
  coefficient <-as.vector(objet_Reg$coefficient[objet_Reg$var_names %in% colnames(x)])
  #ne selectionner que les coefficients correspondant aux colonnes de x
  #car parfois certaines modalites sont dans le train et pas dans le test ou inversement
  pnewdata<-as.matrix(pnewdata)

  pi <- RegLogFonction(pnewdata %*% coefficient)

  if (type=="class")
  {
    #classe predite
    pred = ifelse(pi>=0.5,1,0)
    mc = table(y, pred)
    err = 1.0-sum(diag(mc))/sum(mc)
    return(list(pred = pred, mat_conf = mc, error = err))
  }
  else if (type=="posterior")
  {
    # probabilite d appartenance aux classes
    pred <- pi
    return(pred = pred)
  }
}
