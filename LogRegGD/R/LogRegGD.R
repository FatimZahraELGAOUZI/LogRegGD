#Consttantes

#Affichage
TYPE_MODES <- c('batch','online','mini_batch')
MODE_POSSIBLES = "Modes possibles: "

#Message d'erreur
ERREUR_MODE_INCORRECT="Mode incorrect"
ERREUR_BATCH_SIZE="Le batch-size depasse le nombre de donnees"
ERREUR_NOMBRE_MODALITE="La variable cible a plus de  2 modalites"



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
fit <- function(formula,data, mode, batch_size="32", ncores,itermax){


  #On en profite pour introduire les contrôles
  #Controle:- Mode
  controlerMode(mode)
  controlerBatch(batch_size, data)
  donneesModel=formule(formula,data)
  ncores=ncores(ncores)

  #Creation de l'instance
  instance <- list()
  instance$model="test"
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

controlerBatch<-function(batch_size,donnees){
  if(batch_size>=nrow(donnees))
    stop (ERREUR_BATCH_SIZE)

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
  var_exp <- model_matrix(donnees, formula)

  return (var_exp)
}

formule_extract_cible<- function (formula, donnees){

  # recuperation de Y (left of tilde)
  var<-get_all_vars(formula,donnees)

  # controler Y
  Y<-as.matrix.data.frame(var[1])
  controlerNombreModalites(Y)
  print(class(Y))
  return (var[1])
}



#Recuperation des coeurs et controle
ncores<-function(cores_choice){
  if (cores_choice > parallel::detectCores() | cores_choice <= 0 ){
    cores_choice = parallel::detectCores()#utiliser max-1
  }
  else{
    cores_choice = cores_choice
  }
  return (cores_choice)
}
