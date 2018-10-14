library(grid)
library(ggplot2)
library(reshape)
#Q1
# function to read in binary image files from a directory file_path and output a RDS file at out_file
load_training_images<-function(file_path,out_file){
  # take input directory path and add each training file to path, store as vector
  files<-file.path(file_path,c("data_batch_1.bin","data_batch_2.bin","data_batch_3.bin",
                               "data_batch_4.bin","data_batch_5.bin"))
  # read in each training binary file and store as list for each file
  d<-lapply(files,function(x)readBin(con=x,what = raw(),n=30730000))
  # unlist the training data
  d<-unlist(d)
  # generate indices of label for each image
  labelindex<-seq(1,153650000,3073)
  # get labels for all the training images
  labels<-as.integer(d[labelindex])
  # read in pixel data for each image and store as matrix
  bins<-matrix(as.integer(d[-labelindex]),50000,3072,byrow=TRUE)
  # bind the label and pixel data for each image
  images<-cbind(labels,bins)
  # save images matrix to rds file
  saveRDS(images,file=out_file)
}

# read in training image data from bin files
load_training_images("D:/R/FP",out_file = "D:/R/FP/training.rds")

# function to read in binary image file from file-path and output a RDS file at out_file
load_testing_image<-function(file_path,out_file){
  # read in testing binary file
  file<-readBin(con=file_path,what=raw(),n=30730000)
  # generate indices of label for each image
  labelindex<-seq(1,30730000,3073)
  # get labels for all the training images
  label<-as.integer(file[labelindex])
  # read in pixel data for each image and store as matrix
  bin<-matrix(as.integer(file[-labelindex]),10000,3072,byrow=TRUE)
  # bind the label and pixel data for each image
  image<-cbind(label,bin)
  # sace images matrix to rds file
  saveRDS(image,out_file)
}

# read in testing image data from bin file
load_testing_image(file_path = "D:/R/FP/test_batch.bin",out_file = "D:/R/FP/test.rds")

# scale down the training and testing datasets (code thanks to Patrick)
training<-readRDS("training.rds")
testing<-readRDS("test.rds")
data_rescale<-function(labels,k=500)sort(as.vector(sapply(unique(labels),function(i)which(labels==i))[1:k,]))
train<-training[data_rescale(training[,1],k=500),]
test<-testing[data_rescale(testing[,1],k=100),]

#Q2
# read in label metadata file
class<-read.table("batches.meta.txt")

view_images<-function(images,ref,inx){
  r <- matrix(images[inx,c(2:1025)], ncol=32,byrow = T)
  g <- matrix(images[inx,c(1026:2049)], ncol=32,byrow = T)
  b <- matrix(images[inx,c(2050:3073)], ncol=32,byrow = T)
  col<-rgb(r,g,b,maxColorValue = 255)
  dim(col) <- dim(r)
  grid.raster(col,interpolate = F)
  label=as.character(ref[images[inx,1]+1,])
  print(label)
}

view_images<-function(images,ref,inx){
  # create 32x32 pixel matrices for each color channel
  r <- matrix(images[inx,c(2:1025)], ncol=32,byrow = T)
  g <- matrix(images[inx,c(1026:2049)], ncol=32,byrow = T)
  b <- matrix(images[inx,c(2050:3073)], ncol=32,byrow = T)
  # combine colors together and normalize the pixel intensities
  col<-rgb(r,g,b,maxColorValue = 255)
  dim(col) <- dim(r)
  # Now display the image 
  grid.raster(col,interpolate = F)
  # map label integer to classification based on the reference
  label=as.character(ref[images[inx,1]+1,])
  # print classification
  print(label)
}

#Q3
# subset training pixel data (do not include labels)
train_pixel<-train[,-1]
# get standard deviation of pixels
sdpixel<-apply(train_pixel,2,sd)
# get 5 highest standard deviation pixels
head(sort(sdpixel,decreasing = T),5)
head(order(sdpixel,decreasing = T),5)
# get 5 lowest standard deviation pixels
tail(sort(sdpixel,decreasing = T),5)
tail(order(sdpixel,decreasing = T),5)

#Q4
# calculate Euclidean distance matrix or training images
eucdmatrix<-dist(train[,-1],upper=T,diag=T)
eucdmatrix<-as.matrix(eucdmatrix)
# save Euclidean distance matrix to rdS file
saveRDS(eucdmatrix,"D:/R/FP/eucdmatrix.rds")
# calculate manhattan distance matrix of training images
mandmatrix<-dist(train[,-1],upper=T,diag=T,method="manhattan")
mandmatrix<-as.matrix(mandmatrix)
# save Manhattan distance matrix to file
saveRDS(mandmatrix,"D:/R/FP/mandmatrix.rds")

# read in euclidean and manhattan distance matrices for training images
eucdmatrix<-readRDS('eucdmatrix.rds')
mandmatrix<-readRDS('mandmatrix.rds')

# function to randomly assign tie label, solving tie problem, x is vector of most likely classification(s)
vote<-function(x){
  # randomly pick a label
  label<-as.integer(sample(names(which(x==max(x))),1,replace = T))
  return(label)
}

knn_predict<-function(predict,train,Dmatrix,k){
  # initialize predicted labels vector
  labels<-c(nrow(predict))
  # for all the prediction points
  for(i in 1:nrow(predict)){
    # get indices of k nearest neighbors
    nearest_k<-head(order(Dmatrix[i,]),n=k)
    # get the labels of the k nearest neighbors
    label_k<-train[c(nearest_k),1]
    # tabulate the labels of the k nearest neighbors
    counts<-table(label_k)
    # get and store the most likely label
    labels[i]<-vote(counts)
  }
  return(labels)
}

#Q5
cv_error_knn<-function(train,Dmatrix,k){
  # vector to assign training data into 10 same size folds
  folds <- rep_len(1:10,nrow(train))
  # initialize vector containing error rate for each fold
  folderrors<-numeric()
  # for each fold
  for(i in 1:10) {
    # get indices of test points
    test_inx<-which(folds==i)
    # get test points
    testing<-train[test_inx,]
    # get training points
    training<-train[-test_inx,]
    # subset distance matrix to only look at current testing points
    ttDmatrix<-Dmatrix[test_inx,-test_inx]
    # predict the labels for test points
    pre_labels<-knn_predict(testing,training,ttDmatrix,k)
    # subset the actual labels for test points
    act_labels<-testing[,1]
    # create boolean vector indicating incorrect labels
    incorrectlabels<-pre_labels!=act_labels
    # calculate error rate for current fold and store it in the vector
    folderrors[i]<-sum(incorrectlabels)/nrow(testing)
  }
  #return the error mean
  return(mean(folderrors))
}
cv_error_knn(train,eucdmatrix,7)

#Q6
#metrics and k(1:20) combination
euc_20_error<-numeric()
man_20_error<-numeric()
# for k = 1 through 20
for(k in 1:20){
  # calculate the error rate for 10 fold cross validation for euclidian and manhattan metrics
  euc_20_error[k]<-cv_error_knn(train,eucdmatrix,k)
  man_20_error[k]<-cv_error_knn(train,mandmatrix,k)
}
#metrics and k(1:15) combination
euc_15_error<-euc_20_error[1:15]
man_15_error<-man_20_error[1:15]
#data cleaning
# generate sequence of numbers 1 through 15 for each k-value
k<-seq(1,15,1)
# bind the error rates for euclidean and manhattan distance matrices together with the k value
error_rate<-data.frame(cbind(euc_15_error,man_15_error,k))
# give column names to data frame
colnames(error_rate)<-c("Euclidean","Manhattan","k")
error_rate<-melt(error_rate,id="k")
levels(error_rate$variable)<-c('Euclidean','Manhattan')
#visualization for the error rate for different combinations if k and distance metrics
ggplot(data=error_rate,aes(x=k,y=value,col=variable,label=value))+
  geom_point()+geom_line()+
  scale_x_continuous(breaks = c(seq(1,15,1)))+
  geom_label()+
  ylab("error_rate")+ggtitle("CV_error_rate for different k and metric combination")+
  theme_classic()
#check if other k is useful
best3_euc<-head(order(euc_20_error),3)
head(order(euc_15_error),4)
best3_man<-head(order(man_20_error),3)
head(order(man_15_error),3)
cv_error_knn(train,eucdmatrix,100)
cv_error_knn(train,eucdmatrix,90)

#Q7
#10 folds confusion matrix
cv_matrix_knn<-function(train,Dmatrix,k){
  # vector to assign training data into 10 same size folds
  folds <- rep_len(1:10,nrow(train))
  #vectorization first
  folderrors<-numeric()
  pre_labels<-c()
  act_labels<-c()
  # for each fold
  for(i in 1:10) {
    # get indices of test points
    test_inx<-which(folds==i)
    # get test points
    testing<-train[test_inx,]
    # get training points
    training<-train[-test_inx,]
    # subset distance matrix to only look at current testing points
    ttDmatrix<-Dmatrix[test_inx,-test_inx]
    # get prediction labels for test points
    pre_labels<-c(pre_labels,knn_predict(testing,training,ttDmatrix,k))
    # subset the actual labels for test points
    act_labels<-c(act_labels,testing[,1])
  }
  # return confusion matrix
  return(table(pre_labels,act_labels))
}
#Euclidean top 3 k combination confusion matrix
euc_con_7<-cv_matrix_knn(train,eucdmatrix,7)
euc_con_8<-cv_matrix_knn(train,eucdmatrix,8)
euc_con_9<-cv_matrix_knn(train,eucdmatrix,9)
#Manhattan top 3 k combination confusion matrix
man_con_7<-cv_matrix_knn(train,mandmatrix,7)
man_con_8<-cv_matrix_knn(train,mandmatrix,8)
man_con_14<-cv_matrix_knn(train,mandmatrix,14)


#Q9
# combine training and testing data
total_image<-rbind(train,test)
# calculate Euclidean distance matrix for all 6000 images
eucdmatrix6<-dist(total_image[,-1],upper=T,diag=T)
eucdmatrix6<-as.matrix(eucdmatrix6)
# save the distance matrix as a RDS file
saveRDS(eucdmatrix6,"D:/R/FP/eucdmatrix6.rds")
# calculate Manhattan distance matrix for all 6000 images
mandmatrix6<-dist(total_image[,-1],upper=T,diag=T,method="manhattan")
mandmatrix6<-as.matrix(mandmatrix6)
# save the distance matrix as a RDS file
saveRDS(mandmatrix6,"D:/R/FP/mandmatrix6.rds")

# read saved distance matrices
eucdmatrix6<-readRDS('eucdmatrix6.rds')
mandmatrix6<-readRDS('mandmatrix6.rds')

# initialize error rate vectors
test_error_rate1<-numeric()
test_error_rate2<-numeric()
#use train data to predict test data
# for each k value
for(k in 1:15){
  # calculate error rate for test data set using k value and Euclidean and Manhattan distance metrics
  test_error_rate1[k]<-sum(knn_predict(test,train,eucdmatrix6[5001:6000,1:5000],k)!=test[,1])/nrow(test)
  test_error_rate2[k]<-sum(knn_predict(test,train,mandmatrix6[5001:6000,1:5000],k)!=test[,1])/nrow(test)
}
#data cleaning
# generate sequence of numbers 1 through 15 for each k-value
k<-seq(1,15,1)
# bind the error rates for euclidean and manhattan distance matrices together with the k value
test_error_rate<-data.frame(cbind(test_error_rate1,test_error_rate2,k))
# give column names to data frame
colnames(error_rate)<-c("Euclidean","Manhattan","k")
test_error_rate<-melt(test_error_rate,id="k")
levels(test_error_rate$variable)<-c('Euclidean','Manhattan')
#display the test error rate
ggplot(data=test_error_rate,aes(x=k,y=value,col=variable,label=value))+
  geom_point()+geom_line()+
  scale_x_continuous(breaks = c(seq(1,15,1)))+
  geom_label()+
  ylab("error_rate")+ggtitle("test_error_rate for different k and metric combination")+
  theme_classic()
