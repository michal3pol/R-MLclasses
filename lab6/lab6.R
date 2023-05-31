# Instalacja TensorFlow
install.packages("tensorflow")
library(tensorflow)
install_tensorflow()
# Instalacja Keras
install.packages("keras")
library(keras)
install_keras()
#dataset MNIST
mnist <- dataset_mnist()
x_train <- mnist$train$x
x_test <- mnist$test$x
y_train <- mnist$train$y
y_test <- mnist$test$y
# konwersja danych
x_train <- x_train / 255                                 
x_test <- x_test / 255
y_train <- to_categorical(y_train, num_classes = 10)
y_test <- to_categorical(y_test, num_classes = 10)
#create model
model <- keras_model_sequential() %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")
#print model
summary(model)
#set model parameters
model %>% compile(
  loss = "categorical_crossentropy",      #calculate loss
  optimizer = optimizer_adam(),           #optimization
  metrics = c("accuracy")                 #accuracy
)
#train model
history <- model %>%
  fit( x_train, y_train, epochs = 50, batch_size = 128, validation_split = 0.15)
#check model quality 
model %>% evaluate(x_test, y_test)
#predict
model %>% predict(x_test) %>% k_argmin()

