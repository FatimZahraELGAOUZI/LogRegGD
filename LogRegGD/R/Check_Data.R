
#modele <- glm(income ~ ., data = adult, family = binomial)
#print(modele$coefficients)

#Récupération du mode et remplacement des NA par le mode
getmode <- function(v) {
  uniqv <- unique(v)
  replace_na(v,uniqv[which.max(tabulate(match(v, uniqv)))])
}

# Fonction de vérification des données
tranformDataset<- function(d, cible)
{
  # Recoder la variable cible en 0/1 si ce n'est pas le cas

  if (is.factor(as.factor(cible))){
    w <- levels(cible)
    cible<-fastDummies::dummy_cols(cible)
    cible<-cible[,2]
    print(paste(w[2], "correspond a la modalite 0"))
    print(paste(w[1], "correspond a la modalite 1"))

  } #A gérer dans les errors si c'est pas un factor ou si c'est pas en 0 ou 1

  # Transformer les variables explicatives charactères en factor
  d = as.data.frame(unclass(d), stringsAsFactor = TRUE)

  # Récupérer la liste des variables explicatives qualitatives et les recoder en 0/1
  id_quali = sapply(d, function(x){is.factor(x)}) #remplacer d par expli
  if (sum(id_quali)>0){
     quali = d[,id_quali]
     quali <- as.data.frame(apply(quali,2,getmode))
     quali = fastDummies::dummy_cols(quali)
  } else {
     quali <- NULL
  }

  #Récupérer la liste des variables explicatives quantitatives
  id_quanti = sapply(d,function(x){is.numeric(x)|is.double(x)}) #remplacer d par expli
  if (sum(id_quanti)>0){
    quanti = d[,id_quanti]
    quanti <- replace(quanti, TRUE, lapply(quanti, function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
  } else {
    quanti <- NULL
  }

  # Recombiner les explicatives
  if (is.null(quali)){
    expli <- quanti
  } else if (is.null(quanti)) {
    expli <- quali
  } else {
    expli <- cbind(quali, quanti)
  }
  print(expli)
  return(list(x = expli, y = cible))
}


