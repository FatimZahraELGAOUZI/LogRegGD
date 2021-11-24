 #source("R/Check_Data.R")
#source("R/LogisticReg_GradDesc.R")

#Consttantes

#Affichage
TYPE_MODES <- c('batch','online','mini-batch')
MODE_POSSIBLES = "Modes possibles: "
#Message d'erreur
ERREUR_MODE_INCORRECT="Mode incorrect"
ERREUR_BATCH_SIZE="Le batch-size depasse le nombre de donnees"
ERREUR_NOMBRE_MODALITE="La variable cible a plus de  2 modalites"
#ERREUR_SEED="TRUE/FALSE"
ERREUR_ITER_MAX="iter_max doit imperativement etre positif"
ERREUR_TYPE_DATA="The input is not a dataframe"
# message derreur formula


#Definition classe
definitionClasse <- list(model=NULL)
class(definitionClasse ) <- "CalculLogRegGD"

#' @Title  le titre
#'
#' @param formula la formule
#' @param data les données
#' @param mode le mode d'execution
#' @param batch_size size of batch
#' @param max_iter bla bla
#' @param tol bla bli
#' @param eta bla blo
#' @param ncores number of CPU cores
#'
#' @return test
#' @export
#'
fit <- function(formula, data, mode="batch", ncores=NULL, max_iter = 500, tol = 1e-4, eta = 0.3, batch_size = 32){


  #On en profite pour introduire les contrôles
  #Contrôles:
  controlerDataType(data)
  controlerMode(mode)
  controlerItermax(max_iter)
  batch_size=controlerBatch(batch_size, data,mode)
  ncores=ncores(ncores)

  #Récupération et transformation des x et y
  y = formule_extract_cible(formula, data)
  x = formule_donnees_exp(formula, data)
  data = tranformDataset(x,y)
  x = data[["x"]]
  y = data[["y"]]


  #Creation de l'instance

  instance <- list(call=NULL,model=NULL,coefficient=NULL,formula=NULL,y=NULL,modalites= NULL,iter=NULL,var=NULL,var_names=NULL)
  instance$model= GradDescente(x, y, eta = eta, max_iter = max_iter, tol = tol, mode_desc=mode, batch_size=batch_size)
  #le modele
  #instance$call<- paste(deparse(formula$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  #les coefficients
  instance$coefficient=instance$model$coef
  #la formule
  instance$formula=formula
  #print(formula$call)
  #la variable à expliquer
  instance$y=y
  #la liste des modalités
  instance$modalites <- nlevels(y)
  # le nombre d'iteration
  instance$iter=instance$model$nbIter
  #log des probas a priori

  #le nombre de variables explicatives
  instance$var <- ncol(x)
  #les noms des  variables explicatives
  instance$var_names <- colnames(x)

  #instance$aic= AIC()
  #print(instance$model)
  # à remplir par les autres modules faits par l'equipe
  class(instance) <- "CalculLogRegGD"

  return(instance)
}

######## Surcharge Print###################

print.CalculLogRegGD <- function(objet){
  #cat("Call : ",objet$cat,"\n")
  print(objet$formula)
  print("Coefficients :")
  print(objet$coefficient)
  #cat("y= ",objet$y,"\n")
  #cat("modalites= ",objet$modalites,"\n")
  #print(objet$modalites)
  cat("Nombre de variables explicatives : ", objet$var, "\n")
  cat("Noms des variables : ", objet$var_names,"\n")
  cat("Nombre d'itérations : ", objet$iter, "\n")
  #cat("p-value = ",objet$pvalue,"\n")
}

######## Surcharge summary###################



###########################################les fonctions ############################################

# controlerFormula <- function(formula){
#   as.formula(paste("y ~ x1 + x2", "x3", sep = "+"))
# }

#if (!inherits(formule,"formula")) {formule <- as.formula(formule)}
controlerDataType <- function (data){
  if (is.data.frame(data)==FALSE){
    stop(ERREUR_TYPE_DATA)
  }
}
controlerMode<-function(mode){
  if(is.na (match(mode,TYPE_MODES))){
    print (MODE_POSSIBLES)
    print (TYPE_MODES)
    stop(ERREUR_MODE_INCORRECT)
  }
  else{
    print (paste("Mode: ",mode))
  }

}

controlerNombreModalites<-function(y){
  #print(y)
  if(length(unique(y))!=2)
    stop (ERREUR_NOMBRE_MODALITE)

}

controlerBatch<-function(batch_size,donnees,mode){
  if(mode=="online"){
    return (NULL);
  }
  if( batch_size>=nrow(donnees) || batch_size<0 ){
    stop (ERREUR_BATCH_SIZE)
  }
  return(batch_size)
}


controlerItermax<-function(max_iter){
  if( max_iter <0)
    stop (ERREUR_ITER_MAX)
}

controlerSeed<-function(seed)
{
  if(!is.boolean(seed))
    stop (ERREUR_SEED)
  }


formule_donnees_exp<- function (formula, donnees){

  # recuperation des variables explicatives (right of tilde)
  x <- model.frame(formula, donnees)
  return (x[,-1])
}

formule_extract_cible<- function (formula, donnees){

  # recuperation de Y (left of tilde)
  var<-model.frame(formula,donnees)
  # controler Y
  Y<-as.matrix.data.frame(var[1])
  controlerNombreModalites(Y)
  return (Y)
}



#Recuperation et controle des coeurs
ncores<-function(cores_choice){
  #print(cores_choice)
  if (is.null(cores_choice) || cores_choice > parallel::detectCores() || cores_choice <= 0 ){
    cores_choice = parallel::detectCores()-1
  }
  else{
    cores_choice = cores_choice
  }
  return (cores_choice)
}

#reflexion
# pima_indians_diabetes = read.csv("pima-indians-diabetes.csv", header = T, sep=',')
obj=fit(X9 ~ ., pima_indians_diabetes, mode="batch")
 obj
# d = data.frame(cbind(scale(pima_indians_diabetes[,1:8]),pima_indians_diabetes$X9))
# model<-glm(V9~.,family=binomial,data=d)
# model$coefficients
#

# library(liver)
# data("adult")
# adult[adult=="?"] <- NA
# obj=fit(income ~ age + workclass + demogweight + gender + capital.gain + capital.loss, adult, mode="mini-batch")
# obj
# ind <- sapply(adult, is.numeric)
# adult[ind] <- lapply(adult[ind], scale)
# model<-glm(income~ age + workclass + demogweight + gender + capital.gain + capital.loss,family=binomial,data=adult)
# model$coefficients


# d = read.csv("Test.csv", header = T, sep=";", dec = ",")
# obj=fit(diabete ~ ., d, mode="online")
# print(obj)
# d$diabete = dplyr::recode(d$diabete, "presence"=1, "absence"=0)
# d$triceps = replace(d$triceps, is.na(d$triceps), mean(d$triceps, na.rm = TRUE))
# d = as.data.frame(cbind(scale(d[,1:8]),d$diabete))
# model<-glm(V9~ .,family=binomial,data=d)
# model$coefficients

# num_params = length(obj$coef) + 1
# print(num_params)
# tc <- lapply(obj$var,function(x,y){return(log(prop.table(table(y,x)+1,1)))},obj$y)
# aic = 2* log(tc) + 2 * 10


########################################################Fonction Predict###########################"""""

  predict<-function (objet_Reg, newdata, type){
   # controle des données
   if (length(intersect(objet_Reg$var_names,colnames(newdata))) < objet_Reg$var){stop("Erreur Variables")}
   #récupérer les variables dans l'ordre
    pnewdata <- newdata[objet_Reg$var_names]
     mpnewdata <- matrix(unlist(pnewdata))
     pnewdata$intercept=1
     pi <- RegLogFonction(mpnewdata %*% objet_Reg$coef)
       if (type=="class"){
           Classpred<- ifelse(pi>0.5, 1,0)
           print(Classpred)
         }
     else if (type=="Prior") {
           prob <- pi
           print(prob)
    }
  }

pred<-predict( obj,pima_indians_diabetes,"class")
 #   #prediction
 #   Pred <- model$modalites[apply(scores,1,which.max)]
 #   return(mPred)
 # }





#
#  }
