
#modele <- glm(income ~ ., data = adult, family = binomial)
#print(modele$coefficients)

# parcer les formules
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


# Fonction de vérification des données
CheckDataset <- function(d, formula)
{
  # Vérifier que le jeu de données est bien un dataframe
  if (is.data.frame(d)==FALSE)
  {
    stop("The input is not a dataframe")
  } else
    {
      # Parser la formule
      x=formule_donnees_exp(formula,d)
      y=formule_extract_cible(formula,d)

      # Gérer les valeurs manquantes


      # Recoder la variable cible en 0/1 si ce n'est pas le cas
      cible = 0

      # Transformer les variables explicatives charactères en factor
      d = as.data.frame(unclass(d), stringsAsFactor = TRUE)

      # Récupérer la liste des variables explicatives qualitatives et les recoder en 0/1
      id_quali = sapply(d, function(x){is.factor(x)}) #remplacer d par expli
      quali = d[,id_quali]
      quali_dummies = fastDummies::dummy_cols(quali)

      #Récupérer la liste des variables explicatives quantitatives
      id_quanti = sapply(d,function(x){is.numeric(x)|is.double(x)}) #remplacer d par expli
      quanti = d[,id_quanti]

      # Recombiner les explicatives
      expli = cbind(quali_dummies, quanti)
    }
  return(list(x = expli, y = cible))
}


#data("adult")
#x=1
#CheckDataset(adult)
