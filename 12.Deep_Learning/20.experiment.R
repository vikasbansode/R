# Flags
FLAGS <- flags(flag_integer('dense_units1',32),
               flag_integer('dense_units2',16),
               flag_numeric('dropout1',0.1),
               flag_numeric('dropout2',0.1),
               flag_integer('batch_size',32))

# Model

model <- keras_model_sequential() %>%
  layer_dense(units = FLAGS$dense_units1,activation = 'relu',
              input_shape = c(21)) %>%
  layer_dropout(rate = FLAGS$dropout1) %>%
  layer_dense(units = FLAGS$dense_units2,activation = 'relu') %>%
  layer_dropout(rate = FLAGS$dropout2)%>%
  layer_dense(units = 3,activation = 'softmax')

# Compile Modle

model %>%
  compile(loss='categorical_crossentropy',
          optimizer='adam',
          metrics='accuracy')

# Fit Model

history <- model %>%
  fit(training,
      trainLabels,
      epochs=50,
      batch_size=FLAGS$batch_size,
      validation_split=0.2)

