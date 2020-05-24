Overview
--------

### Firstly the raw training data is splited into training and testing dataset. Then the training dataset is used to build predict models. Finally we found that random forest is better model of this case.

Clean and explore data
----------------------

    library(caret)

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    trainingData <- read.csv("pml-training.csv")
    testingData <- read.csv("pml-testing.csv")
    trainingData <- as.data.frame(trainingData)
    testingData <- as.data.frame(testingData)
    trainingData$classe <- factor(trainingData$classe)
    NArat <- function(x){
        apply(x, 2, function(y) sum(is.na(y)))/nrow(x)
    }
    trainingData <- trainingData[,NArat(trainingData) < .7]
    nearZero <- nearZeroVar(trainingData, saveMetrics = TRUE)
    trainingData <- trainingData[, !nearZero$nzv]
    trainingData <- trainingData[,-grep("name|timestamp|window|^X",names(trainingData))]
    trainingNN <- trainingData[, -length(names(trainingData))]
    findCor <- findCorrelation(cor(trainingNN,use="complete.obs"), cutoff = .7)
    trainingFil <- trainingData[,-findCor]
    inTrain <- createDataPartition(y = trainingFil$classe, p = 0.6, 
                                   list = FALSE) 
    training <- trainingFil[inTrain,]
    testing <- trainingFil[-inTrain,]

rpart model
-----------

    library(rpart)
    rpartMod <- rpart(formula = classe ~ ., data = training)
    rpartPred <- predict(rpartMod,newdata = testing,
                             type = "class")
    confusionMatrix(rpartPred,testing$classe)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1833  371  119  189  168
    ##          B  120  735  101  156  281
    ##          C   95  284 1039  155  224
    ##          D  168  122  109  780  170
    ##          E   16    6    0    6  599
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.6355          
    ##                  95% CI : (0.6247, 0.6461)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.536           
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.8212  0.48419   0.7595  0.60653  0.41540
    ## Specificity            0.8491  0.89602   0.8830  0.91326  0.99563
    ## Pos Pred Value         0.6840  0.52764   0.5782  0.57821  0.95534
    ## Neg Pred Value         0.9228  0.87866   0.9456  0.92212  0.88322
    ## Prevalence             0.2845  0.19347   0.1744  0.16391  0.18379
    ## Detection Rate         0.2336  0.09368   0.1324  0.09941  0.07634
    ## Detection Prevalence   0.3416  0.17754   0.2290  0.17193  0.07991
    ## Balanced Accuracy      0.8352  0.69010   0.8212  0.75990  0.70551

random forest model
-------------------

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2229   26    1    0    0
    ##          B    3 1484   26    0    2
    ##          C    0    3 1332   27    0
    ##          D    0    3    9 1257    7
    ##          E    0    2    0    2 1433
    ## 
    ## Overall Statistics
    ##                                          
    ##                Accuracy : 0.9859         
    ##                  95% CI : (0.983, 0.9883)
    ##     No Information Rate : 0.2845         
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.9821         
    ##                                          
    ##  Mcnemar's Test P-Value : NA             
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9987   0.9776   0.9737   0.9774   0.9938
    ## Specificity            0.9952   0.9951   0.9954   0.9971   0.9994
    ## Pos Pred Value         0.9880   0.9795   0.9780   0.9851   0.9972
    ## Neg Pred Value         0.9995   0.9946   0.9944   0.9956   0.9986
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2841   0.1891   0.1698   0.1602   0.1826
    ## Detection Prevalence   0.2875   0.1931   0.1736   0.1626   0.1832
    ## Balanced Accuracy      0.9969   0.9864   0.9845   0.9873   0.9966

Conclusion
----------

### Ramdon forest model is better fit model with an accuracy 0.9927, so it’s preferable to choose ramdon forest to predict 20 cases.
