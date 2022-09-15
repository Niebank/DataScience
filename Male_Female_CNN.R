library(tidyverse)
library(tensorflow)
library(keras)
library(stringr)
library(tfdatasets)
library(readr)


data_dir <- unzip("C:/Users")

maledsp <- readImage("ConvNet_dataset/training_set/male/male.2104.jpg")
plot(dogdsp)


class_names <- c('female','male')

index <- 1:10000

par(mfcol = c(5,6), mar = rep(1, 4), oma = rep(0.2, 4))
cifar$train_set$x[index,,,] %>% 
  purrr::array_tree(1) %>%
  purrr::set_names(class_names[cifar$train_set$y[index] + 1]) %>% 
  purrr::map(as.raster, max = 255) %>%
  purrr::iwalk(~{plot(.x); title(.y)})


model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu", 
                input_shape = c(32,32,3)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu")

summary(model)

model %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 10, activation = "softmax")

summary(model)

model %>% compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = "accuracy"
)

history <- model %>% 
  fit(
    x = data_dir/ConvNet_dataset/training_set$x, y = data_dir/ConvNet_dataset/training_set$y,
    epochs = 10,
    validation_data = unname(data_dir/ConvNet_dataset/test_set),
    verbose = 2
  )

plot(history)

evaluate(model, data_dir/ConvNet_dataset/test_set$x, data_dir/ConvNet_dataset/test_set$y, verbose = 0)
