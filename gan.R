#===Discriminator
  discriminator_inputs<-keras::layer_input(shape = as.integer(dim(X_train)[2]))
  discriminator<-discriminator_inputs
  #1 layer
  discriminator<-keras::layer_dense(object = discriminator,units = 10)
  discriminator<-keras::layer_activation(object =discriminator,activation = 'tanh')
  #2 layer
  discriminator<-keras::layer_dense(object = discriminator,units = 10)
  discriminator<-keras::layer_activation(object =discriminator,activation = 'tanh')
  #Output
  discriminator<-keras::layer_dense(object=discriminator,units = 1,activation = 'sigmoid')


  discriminator<- keras_model(inputs = discriminator_inputs,outputs = discriminator)
  discriminator %>% keras::compile(
    loss = 'binary_crossentropy',
    optimizer = keras::optimizer_adam(lr=lr),
    metrics = list('accuracy')
  )
  summary(discriminator)
#=======  


#===Generator
  generator_inputs<-keras::layer_input(shape = as.integer(dim(X_train)[2]))
  generator<-generator_inputs
  #1 layer
  generator<-keras::layer_dense(object = generator,units = 50)
  generator<-keras::layer_activation(object =generator,activation = 'relu')
  #2 layer
  generator<-keras::layer_dense(object = generator,units = 50)
  generator<-keras::layer_activation(object =generator,activation = 'relu')
  #3 layer
  generator<-keras::layer_dense(object = generator,units = 50)
  generator<-keras::layer_activation(object =generator,activation = 'relu')
  #Output
  generator<-keras::layer_dense(object=generator,units = as.integer(dim(X_train)[2]),activation = 'tanh')

  generator<- keras_model(inputs = generator_inputs,outputs = generator)
  generator %>% keras::compile(
    loss = 'binary_crossentropy',
    optimizer = keras::optimizer_adam(lr=lr),
    metrics = list('accuracy')
  )
  summary(generator)
#=======

#===Adversarial model
  adversarial<-keras::keras_model_sequential()
  adversarial$add(layer=generator)
  adversarial$add(layer=discriminator)
  adversarial %>% keras::compile(
    loss = 'binary_crossentropy',
    optimizer = keras::optimizer_adam(),
    metrics = list('accuracy')
  )
  summary(adversarial)
#=======



iterations<-100
fake_source_cases<-which(y_train==0) #healthy
real_cases<-which(y_train==1) #unhealthy
n_real_cases<-length(real_cases)
batch_size<-20
real_X<-X_train[real_cases,] #Real unhealhty

for(i in 1:iterations){
    noise<-X_train[sample(x = fake_source_cases,size =n_real_cases,replace = F),] #Noise is sourced from real healthy cases
    real_y<- runif(n = n_alarm_cases,min = 0.7,max = 1) #Just label 1 for real but with little range
    fake_y<- runif(n = n_alarm_cases,min = 0,max = 0.3) #Just label 1 for fake but with little range
    fake_X<-generator$predict_on_batch(noise) #Generate fake X from noise
    
    #Train discriminator
    discriminator$trainable<-T #enable train
    #Discriminator Real train 1 label = 1 label
    lr<-keras::fit(object=discriminator,x = real_X, y=real_y,epochs = 100,batch_size =batch_size,verbose = verbose,view_metrics = F)

    #Discriminator Fake train 0 label = 0 label
    lf<-keras::fit(object=discriminator,x = fake_X, y=fake_y,epochs = 100,batch_size =batch_size,verbose = verbose,view_metrics = F)
    discriminator$trainable<-F #disable train
  
    #Generator train 0 label as 1 label trying to fool discriminator
    la<-adversarial %>% keras::fit(x = noise,y = real_y,epochs = 100,batch_size =batch_size,verbose = verbose,view_metrics = F)
}



