
LogRegGD package R
===================
<p>This project is part of our programming course R.The main objective is to develop an R package offering: Binary logistic regression with stochastic gradient descent method with the ability to exploit the capabilities of cores processors.
The LogRegGD is a package which allow you to do a logistic regression using the stochastic gradient descent with 3 modes :
<ul>
  <li>Batch mode with the possibility of using parallel programming</li>
  <li>Mini-batch mode</li>
  <li>Online mode</li>
 </ul>
This package allow you to fit, to predict and  </p>

Installing the package
----------------------
 ```sh 
 devtools::install_github("FatimZahraELGAOUZI/projetR") 
 ```

Tutorial for package usage
--------------------------

### Loading the library

<p>First, you need to import a dataset (with numerical, categorical
variables or both). In this example, we’ll be using the "Adult"
dataset. This dataset is included in the dataset Rstudio. </p>

###  Import Dataset

```sh
data("adult")
```
###  Transform Dataset 
The NA of our dataset is in the form of "?" we need to transform them into "NA".This step concerns only this dataset
 ```sh 
 adult[adult=="?"] <- NA
 ```
### Split Data  
We need to split data in a train and a test set :  
```sh 
df= sample(nrow(adult), nrow(adult)*.7)
train<-adult[df,]
test<-adult[-df,]
```
Now you are ready to use our package: 
### Fit function 
Let's start with batch mode:
```sh 
 objet <- fit(income ~  age + workclass + demogweight + gender, train, mode="batch", eta = 0.3, max_iter = 500, tol = 1e-3)
 ```
 You can also use a parallel programming to make execution faster. Just add the ncores parameter and specify the number of cores to use:
 ```sh 
 system.time(objet <- fit(income ~  age + workclass + demogweight + gender, train, mode="batch",eta = 0.3, max_iter = 500, tol = 1e-3, ncores = 3))
 ```
 If you want to see the result of your training dataset, you can use the print() or the summary() functions.
 ```sh 
 print(objet)
 summary(objet)
 ```
 <p>You can also access the different components of your object using $ (example : objet$coefficient). You can find a list of componenets calling the **_`attributes`_**
  Rfunction </p>
 
 Also, you can show a plot to look at the how the cost varies with iterations
 
```sh 

 ```
 For the mini-batch mode, you need to specify "batch size" (Default value is 32):
```sh 
 objet <- fit(income ~  age + workclass + demogweight + gender, train, mode="mini-batch", batch_size = 32, eta = 0.3, max_iter = 500, tol = 1e-3)
 ```
 And if you want to use the online mode:
```sh 
 objet <- fit(income ~  age + workclass + demogweight + gender, train, mode="online", eta = 0.3, max_iter = 500, tol = 1e-3)
 ```
 For more details about the fit function, you can use the help function: **_`help(fit)`_**
 
 
 
