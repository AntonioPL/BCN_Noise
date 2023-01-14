#install.packages("tensorflow")


library(reticulate)
#path_to_python <- install_python()
virtualenv_create("R-DeepLearning", python = "C:/Users/antop/AppData/Local/Programs/Python/Python310/python.exe")

library(tensorflow)
install_tensorflow(envname = "R-DeepLearning")


#install.packages("keras")

library(keras)
install_keras(envname = "R-DeepLearning")


library(tensorflow)
use_condaenv("r-reticulate") 
tf$constant("Hello Tensorflow!")


use_condaenv("R-DeepLearning") 

library(reticulate)
#use_python("C:/ProgramData/Anaconda3/python.exe")
use_condaenv("C:/ProgramData/Anaconda3/envs/tf_gpu")
library(keras)
library(tensorflow)
tf$constant("Hello Tensorflow!")









# Creación del entorno Virtual ###### ESTE ES EL QUE FUNCIONA
library(reticulate)
#path_to_python <- install_python()
virtualenv_create("R-DeepLearning", python = "C:/ProgramData/Anaconda3/python.exe")

library(tensorflow)
install_tensorflow(envname = "R-DeepLearning")


#install.packages("keras")

library(keras)
install_keras(envname = "R-DeepLearning")

# Prueba del entorno virtual
library(tensorflow)
use_virtualenv("R-DeepLearning") 
tf$constant("Hello Tensorflow!")





#### EJEMPLO #####


library(keras)
set.seed(123)
n=2000  # number of sample data
a <- sample(1:20, n, replace = T)
b <- sample(1:50, n, replace = T)
c <- sample(1:100, n, replace = T)
flag <- ifelse(a > 15 & b > 30 & c > 60, "red", 
               ifelse(a<=9 & b<25& c<=35, "yellow", "green"))
df <- data.frame(a = a,
                 b = b, 
                 c = c, 
                 flag = as.factor(flag))
tail(df,15)


indexes = sample(1:nrow(df), size = .95 * nrow(df))

train <- df[indexes, ]
test <- df[-indexes, ]


train.x <- as.matrix(train[, 1:3], c(1,3,nrow(train)))
train.x <- as.matrix(train[, 1:3], 3,nrow(train))
train.x <- as.matrix(train[, 1:3], c(1,3,nrow(train)))
train.y <- to_categorical(matrix(as.numeric(train[,4])-1))

test.x <- as.matrix(test[, 1:3], c(1,3,nrow(test)))
test.x <- as.matrix(test[, 1:3], 3,nrow(test))
test.y <- to_categorical(matrix(as.numeric(test[,4])-1))

model <- keras_model_sequential() 

model %>% 
  layer_dense(units=64, activation = "relu", input_shape = c(3)) %>% 
  layer_dense(units =3, activation = "softmax")  

model %>% compile(optimizer = "rmsprop", 
                  loss = "categorical_crossentropy",  
                  metric=c("accuracy"))

print(model)


model %>% fit(train.x, train.y,epochs = 50, batch_size = 50)


pred <- model %>% predict(test.x) 


pred <- format(round(pred, 2), nsamll = 4)
result <- data.frame("green"=pred[,1], "red"=pred[,2], "yellow"=pred[,3], 
                     "predicted" = ifelse(max.col(pred[ ,1:3])==1, "green",
                                          ifelse(max.col(pred[ ,1:3])==2, "red", "yellow")),
                     original = test[ ,4])


head(result,20)

scores <- model %>% evaluate(test.x, test.y)

print(scores)

library(caret)
cfm=caret::confusionMatrix(as.factor(result$predicted), result$original)
print(cfm)
