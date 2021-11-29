
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
  ```
 [image](https://user-images.githubusercontent.com/92757617/143952246-8e9f9773-ca7c-4beb-93b6-d742489de1c8.png)

 ```sh 
  summary(objet)
  ```
![image](https://user-images.githubusercontent.com/92757617/143952367-d7e8ae56-77bf-479a-a3c8-7f12f6a0e5f5.png)
You can also access the different components of your object using $ (example : objet$coefficient). You can find a list of componenets calling the  **_`attributes(objet)`_** Rfunction:
 
 ![image](https://user-images.githubusercontent.com/92757617/143952583-15234094-8b6c-49a2-a230-764e093febc2.png)

 Also, you can show a plot to look at the how the cost varies with iterations
 ![image](https://user-images.githubusercontent.com/92757617/143952619-70fecce5-ccd2-4f4b-9682-1c8d825717e9.png)

 For the mini-batch mode, you need to specify "batch size" (Default value is 32):
```sh 
 objet <- fit(income ~  age + workclass + demogweight + gender, train, mode="mini-batch", batch_size = 32, eta = 0.3, max_iter = 500, tol = 1e-3)
 ```
 ![image](https://user-images.githubusercontent.com/92757617/143952676-8a5e68f7-c777-463b-ba35-b1dc63e45eb1.png)

 And if you want to use the online mode:
```sh 
 objet <- fit(income ~  age + workclass + demogweight + gender, train, mode="online", eta = 0.3, max_iter = 500, tol = 1e-3)
 ```
 For more details about the fit function, you can use the help function: **_`help(fit)`_**
 
 ### Predict function 
 
You use the S3 object provided by the function fit() to make the prediction. You indicates the type of prediction {“class”: predicted class , “posterior”:belonging class probability}
```sh 
prediction <- predict(objet, test, "class")
 ```
The result of this function is the "predite classes", a confusion matrix and error rate to evaluate your model
You can also access the different components of your result using $ (Example)
![image](https://user-images.githubusercontent.com/92757617/143952872-7a53b558-b19d-4b03-9bef-85bf995ec1d9.png)
```sh 
prediction <- predict(objet, test, "posterior")
 ```
The result of this function is a vector of belonging class probability. You can use the head(prediction) to visualise the first rows of your membership class probability.
![image](https://user-images.githubusercontent.com/92757617/143953026-2e67051c-2e2d-4b61-a7ed-3c340d9cc6dc.png)

For more details about the predict function, you can use the help function: **_`help(predict)`_**
### It's your turn !
Now it's up to you to test LogRegGD package on your own data.

