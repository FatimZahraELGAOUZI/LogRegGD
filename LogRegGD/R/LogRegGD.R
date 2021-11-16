source("R/Check_Data.R", chdir=T)
source("R/LogisticReg_GradDesc_Fonctions.R", chdir=T)

#Consttantes

#Affichage
TYPE_MODES <- c('batch','online','mini_batch')
MODE_POSSIBLES = "Modes possibles: "

#Message d'erreur
ERREUR_MODE_INCORRECT="Mode incorrect"
ERREUR_BATCH_SIZE="Le batch-size depasse le nombre de donnees"
ERREUR_NOMBRE_MODALITE="La variable cible a plus de  2 modalites"
ERREUR_SEED="TRUE/FALSE"
ERREUR_ITER_MAX="iter_max doit etre imperativement positif"



#Definition classe
definitionClasse <- list(model=NULL)
class(definitionClasse ) <- "CalculLogRegGD"

#' Title
#'
#' @param formula le formula
#' @param data les données
#' @param mode le mode d'execution
#' @param batch_size size of batch
#' @param ncores number of CPU cores
#' @param itermax iteration max
#'
#' @return test
#' @export
#'
fit <- function(formula,data, mode, ncores=NULL,max_iter = 500,tol = 1e-4,eta = 0.3,seed = TRUE, batch_size=32){


  #On en profite pour introduire les contrôles
  #Controle:- Mode
  controlerMode(mode)
  controlerItermax(max_iter)
  batch_size=controlerBatch(batch_size, data,mode)
  x = formule_donnees_exp(formula, data)
  y= formule_extract_cible(formula, data)
  controlerSeed(seed)
  ncores=ncores(ncores)

  #Creation de l'instance
  instance <- list(model=NULL)
  instance$model= GradDescente(x, y, eta = eta, max_iter = max_iter, tol = tol, mode_desc=mode, batch_size=batch_size, seed = seed)
  # à remplir par les autres modules faits par l'equipe
  class(instance) <- "CalculLogRegGD"

  return(instance)
}

###########################################les fonctions ############################################

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
  if(length(unique(y))!=2)
    stop (ERREUR_NOMBRE_MODALITE)

}



controlerBatch<-function(batch_size,donnees,mode){
  if(mode=="online"){
    return (NULL);
  }
  if( batch_size>=nrow(donnees) | batch_size<0 ){
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

# # Recuperation des donnees
# formule<- function (formula, donnees){
#
#   # recuperation des variables explicatives (right of tilde)
#   var_exp <- model_matrix(donnees, formula)
#
#   # recuperation de Y (left of tilde)
#   var<-get_all_vars(formula,donnees)
#
#   # controler Y
#   controlerNombreModalites(var[1])
#   #Y<-as.matrix.data.frame()
#
#   # regrouper les variables explicatives et la cible dans une seule matrice
#   var_exp <- cbind(var_exp, var[1])
#
#   return (var_exp)
# }


formule_donnees_exp<- function (formula, donnees){

  # recuperation des variables explicatives (right of tilde)
  x <- model_matrix(donnees, formula)

  return (x)
}

formule_extract_cible<- function (formula, donnees){

  # recuperation de Y (left of tilde)
  var<-get_all_vars(formula,donnees)

  # controler Y
  Y<-as.matrix.data.frame(var[1])
  controlerNombreModalites(Y)
  return (Y)
}



#Recuperation des coeurs et controle
ncores<-function(cores_choice){
  print(cores_choice)
  if (is.null(cores_choice) || cores_choice > parallel::detectCores() || cores_choice <= 0 ){
    cores_choice = parallel::detectCores()-1
  }
  else{
    cores_choice = cores_choice
  }
  return (cores_choice)
}

# c=c(1,2,3,4)
# a=c(1,0,1,0)
# b=c(1,2,3,3)
# tab=c(a,b,c)
# tab=data.frame(a=a, b=b, c=c)
# fit(a~b, tab, mode="online")

