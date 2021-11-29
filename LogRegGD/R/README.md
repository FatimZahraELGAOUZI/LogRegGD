

LogRegGD package R
===================
<p>This project is part of our programming course R.The main objective is to develop an R package offering: Binary logistic regression with stochastic gradient descent method with the ability to exploit the capabilities of cores processors.
The LogRegGD is a package which allow you to do a logistic regression using the stochastic gradient descent with 3 modes :
<ul><li>Batch mode with the possibility of using parallel programming</li><li>Mini-batch mode</li></ul><li>Online mode</li></ul>
This package allow you to fit, to predict and  </p>

Installing the package
----------------------

 '''sh 
 devtools::install_github("FatimZahraELGAOUZI/projetR") 
 '''

Tutorial for package usage
--------------------------

### Loading the library
<p>First, you need to import a dataset (with numerical, categorical
variables or both). In this example, we’ll be using the "Adult"
dataset. This dataset is included in the dataset Rstudio. 
 </p>
###  Import Dataset 


###  Transform Dataset 
The NA of our dataset is in the form of "?" we need to transform them into "NA".This step concerns only this data set


