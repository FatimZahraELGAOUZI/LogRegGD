################################################### Les fonctions de controles ################################################################

#Messages d'erreurs
ERREUR_TYPE_DATA="The input is not a dataframe"
ERREUR_FORMULA="Formula must be R formula !"
TYPE_MODES <- c('batch','online','mini-batch')
MODE_POSSIBLES = "the possible modes : "
ERREUR_MODE_INCORRECT="Incorrect mode"
ERREUR_BATCH_SIZE="The batch-size must be positive and does not exceed the number of data"
ERREUR_NOMBRE_MODALITE="Target variable has more than 2 modalities"
ERREUR_ITER_MAX="max_iter must imperatively be positive"


#' Formula's Control function
#'
#' @param formula a symbolic description of the model to be fitted
#'
#' @return Error message:"Formula must be R formula !"
#'
controlerFormula <- function(formula)
{
  if(plyr::is.formula(formula)==F)
  {
    stop(ERREUR_FORMULA)
  }
}


#' DataType's Control function
#'
#' @param data the dataframe containing the variables in the model
#'
#' @return Error message:"The input is not a dataframe !"
#'
#'
controlerDataType <- function (data)
{
  if (is.data.frame(data)==FALSE)
  {
    stop(ERREUR_TYPE_DATA)
  }
}


#' Mode's Control function
#'
#' @param mode the mode chosen to be used to update coefficients of stochastic gradient descent
#'
#' @return Error message:"Incorrect mode" and the list of possible modes
#' @export
#'

controlerMode<-function(mode)
{
  if(is.na (match(mode,TYPE_MODES)))
  {
    print (MODE_POSSIBLES)
    print (TYPE_MODES)
    stop(ERREUR_MODE_INCORRECT)
  }else{
    return(mode)
  }
}

#' the target modality control function
#'
#' @param y the target variable
#'
#' @return Error message:"The target variable has more than 2 modalities"
#' @export
#'

controlerNombreModalites<-function(y)
{
  if(length(unique(y))!=2)
  {
    stop (ERREUR_NOMBRE_MODALITE)
  }
}

#' Batch_Size's Control function
#'
#' @param batch_size is the number of observations for the mini-batch mode
#' @param donnees is the dataset
#' @param mode the mode chosen to be used to update coefficients of stochastic gradient descent
#'
#' @return Error message:"The batch-size must be positive and does not exceed the number of data"
#' @export
#'


controlerBatch<-function(batch_size,donnees,mode)
{
  if(mode=="online" | mode == "batch")
  {
    return (NULL);
  }
  if( batch_size>=nrow(donnees) || batch_size<0 )
  {
    stop (ERREUR_BATCH_SIZE)
  }
  return(batch_size)
}



#' max_iter's Control function
#'
#' @param max_iter is the number of iterations
#'
#' @return Error message:"max_iter must imperatively be positive"
#' @export
#'
controlerItermax<-function(max_iter)
{
  if( max_iter <0)
  {
    stop (ERREUR_ITER_MAX)
  }
}

#' Function to extract predictor variables from formula
#'
#' @param formula a symbolic description of the model to be fitted
#' @param donnees the dataset
#'
#' @return the explanatory variables
#' @import stats
#' @export
#'
#'
formule_donnees_exp<- function (formula, donnees)
{
  # recuperation des variables explicatives (right of tilde)
  x <- model.frame(formula, donnees, na.action = na.pass)
  x = subset.data.frame(x, select = -1)
  return (x)
}

#' Function to extract the target from formula
#'
#' @param formula a symbolic description of the model to be fitted
#' @param donnees the dataset
#'
#' @return the target variable
#' @importFrom stats model.frame
#' @export
#'

formule_extract_cible<- function (formula, donnees)
{
  # recuperation de Y (left of tilde)
  var<-model.frame(formula,donnees, na.action = na.pass)
  # controler Y
  Y<-as.matrix.data.frame(var[1])
  controlerNombreModalites(Y)
  return (Y)
}



#Recuperation et controle des coeurs

#' Function to control and get cores
#'
#' @param cores_choice the number of CPU cores chosen by the user
#'
#' @return cores_choice the number of CPU cores that will be used in parallelisation
#' @import parallel
#' @export
#'

ncores<-function(cores_choice)
{

  if (is.null(cores_choice))
  {
    cores_choice = 1
  } else if (cores_choice >= parallel::detectCores() || cores_choice <= 0 )
  {
    cores_choice = parallel::detectCores()-1
  } else
  {
    cores_choice = cores_choice
  }
  return (cores_choice)
}



################################################Fonctions utilitaires#############################################

#Récupération du mode et remplacement des NA par le mode
#'Function that replaces the NA with the mode
#'
#' @param v a vector containing the NA values
#'
#' @return
#' @import tidyr
#' @export
#'

getmode <- function(v) {
  uniqv <- unique(v)
  tidyr::replace_na(v,uniqv[which.max(tabulate(match(v, uniqv)))])
}


#' Transformation function on qualitative variables
#'
#' @param d dataset containing qualitative predictor variables
#'
#' @return  qualitative variables recoded in 0/1
#' @import  fastDummies
#' @export
#'
#'
#'
transformQuali <- function(d)
{
  # Récupérer la liste des variables explicatives qualitatives et les recoder en 0/1
  id_quali = sapply(d, function(x){is.factor(x)})
  if (sum(id_quali)>0)
  {
    quali = as.data.frame(d[id_quali])
    quali <- as.data.frame(apply(quali,2,getmode))
    quali = fastDummies::dummy_cols(quali, remove_selected_columns = TRUE, remove_first_dummy=TRUE)
  } else
  {
    quali <- NULL
  }
  return(quali)
}

#' Transformation function on quantitative variables
#'
#' @param d dataset containing qualitative predictor variable
#'
#' @return the NA values of the quantitative variables replaced by the average of each variable
#' @export
#'

transformQuanti <- function(d)
{
  #Récupérer la liste des variables explicatives quantitatives
  id_quanti = sapply(d,function(x){is.numeric(x)|is.double(x)})
  if (sum(id_quanti)>0)
  {
    quanti = as.data.frame(d[id_quanti])
    quanti <- replace(quanti, TRUE, lapply(quanti, function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
  } else
  {
    quanti <- NULL
  }
  return(quanti)
}


#' the function that groups together the quantitative and qualitative variables
#'
#' @param qual qualitative variables
#' @param quant quantitative variables
#'
#' @return a matrix of predictor variables
#' @export
#'

combine <- function(qual, quant)
{
  # Recombiner les explicatives
  if (is.null(qual)){
    expli <- quant
  } else if (is.null(quant)) {
    expli <- qual
  } else {
    expli <- cbind(qual, quant)
  }
  return(expli)
}

# Fonction de vérification des données
#' Checkdata function
#'
#' @param expli predictor variables
#' @param cible target variable
#'
#' @return x and y tranformed
#' @import fastDummies
#' @export
#'

tranformDataset<- function(expli, cible)
{

  # Recoder la variable cible en 0/1 si ce n'est pas le cas
  if (is.factor(cible) | length(unique(cible))==2)
  {
    cible = as.factor(cible)
    w <- levels(cible)
    cible<-fastDummies::dummy_cols(cible)
    cible<-as.matrix(cible[,3])
    print(paste(w[1], "correspond a la modalite 0"))
    print(paste(w[2], "correspond a la modalite 1"))
  }

  # Transformer les variables explicatives charactères en factor
  donnees = as.data.frame(unclass(expli), stringsAsFactor = TRUE, col.names = names(expli))

  qual = transformQuali(donnees)
  quant = transformQuanti(donnees)

  result = combine(qual, quant)
  return(list(x = result, y = cible))
}




