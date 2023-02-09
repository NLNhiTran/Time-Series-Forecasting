
#######################################################

#######################################################
#Training data
#######################################################
set.seed(123) # set the random number seed for reproducibility 
avocado_seed <- avocado[sample(nrow(avocado)),]

# get indices for 70% of the data set
intrain <- createDataPartition(y = avocado_seed[,3], p= 0.7)[[1]]

#splitting data into training/testing data using the train Index object
training <- avocado_seed[intrain,]
testing <- avocado[-intrain,]
head(training,5)

#Another method
train_size <- floor(0.75*nrow(avocado)) 
in_rows <- sample(c(1:nrow(avocado)), size = train_size, replace = FALSE)
training <-  avocado[in_rows,]#training data (75% of data)
testing <- avocado[-in_rows,] #testing data (25% of data)


set.seed(101)
k=1:12 # set k values
trainErr=c() # set vector for training errors
for( i in k){
  
  knnFit=knn3(x=training[,-1], # training set
              y=training[,1], # training set class labels
              k=i)
  
  # predictions on the training set
  class.res=predict(knnFit,training[,-1],type="class")
  
  # training error
  err=1-confusionMatrix(training[,1],class.res)$overall[1]
  trainErr[i]=err
}

# plot training error vs k
plot(k,trainErr,type="p",col="#CC0000",pch=20)

# add a smooth line for the trend
lines(loess.smooth(x=k, trainErr,degree=2),col="#CC0000")
