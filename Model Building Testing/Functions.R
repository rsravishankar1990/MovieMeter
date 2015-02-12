##Functions to use for Model building and Testing:
#Mostly bootstrapping functions which check consistency of predictions over many samples

#Finding Significant variables - Bootstrap mechanism
#Run the model over 500 samples from the dataset: Check which variables feature in the stepwise most times
#The variables which are present always tend to be significant
#Otherwise its due to sample variation

#Run the model 500 times backward and forward to determine which variables occur frequently in explaining the model
runModel <- function(){
  #Create dataframe to store results of simulation run for variable determination
  var <- c("Director","Writer","Actor1","Actor2","Actor3","Actor4","Actor5","Act1Dir","Act2Dir","Act3Dir","Act4Dir","Act5Dir","WriDir","Act1Act2","Act1Act3","Act1Act4","Act1Act5","Act3Act2","Act4Act2","Act5Act2")
  occ <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  varDetFor <- data.frame(var,occ)
  varDetBack <- data.frame(var,occ)
  for (i in 1:500){
    #Get the training and test data for sample
    ModelData <- Interaction2
    ModelData$X <- NULL
    Split <- sample(nrow(ModelData),nrow(ModelData)*0.9)
    TestData <- ModelData[-Split,]
    TrainData <- ModelData[Split,]
    
    #Create null model and full model for stepwise regression
    #nullModel <- glm(Class~1,data=TrainData,family=binomial)
    fullModel <-glm(Class ~ .-Movie-Rating ,data=TrainData,family=binomial)
    
    #forModel <- step(nullModel,scope = list(lower=nullModel,upper=fullModel),direction = "forward")
    backModel <- step(fullModel,direction = "backward")
    
    #forList <- attr(attr(model.frame(forModel),"terms"),"term.labels")
    backList <- attr(attr(model.frame(backModel),"terms"),"term.labels")
    
    #for (j in 1:length(forList))
    #{
    # varDetFor$occ[varDetFor$var == forList[j]] = varDetFor$occ[varDetFor$var == forList[j]] + 1
    #}
    
    for (j in 1:length(backList))
    {
      varDetBack$occ[varDetBack$var == backList[j]] = varDetBack$occ[varDetBack$var == backList[j]] + 1
    }
  }
  return(varDetBack)
}



#All subsets Model fitting and generating statistics like Fscore etc:
#Create loop for running each model:
allModel <- function(formulae,TrainData,fileInteger)
{
  #Stats to find : AUC
  # Max(Accuracy), Cutoffpoint, TrueP,FalseP,TrueN,FalseN, Precision,Recall,fScore
  # Max(Fscore), Cutoffpoint, TrueP, FalseP, TrueN, FalseN, Precision, Recall, fScore
  # Max(TrueP), Cutoffpoint, TrueP, FalseP, TrueN, FalseN, Precision, Recall, fScore
  # Max(TestAccuracy), Cutoffpoint, TrueP, FalseP,TrueN,FalseN, Precision, Recall, fScore
  # Max(TestFscore), Cutoffpoint, TrueP, FalseP, TrueN, FalseN, Precision, Recall, fScore
  # Max(TestTrueP), Cutoffpoint, TrueP, FalseP, TrueN, FalseN, Precision, Recall, fScore
  
  
  #Create Data Variables:
  form <- ""
  AUC <- 0
  MaxAcc <- 0
  AccCut <- 0
  AccTruP <- 0
  AccFalP <- 0
  AccTruN <- 0
  AccFalN <- 0
  AccPrec <- 0
  AccRec <- 0
  AccFsc <- 0
  
  MaxFsc <- 0
  FscAcc <- 0
  FscCut <- 0
  FscTruP <- 0
  FscFalP <- 0
  FscTruN <- 0
  FscFalN <- 0
  FscPrec <- 0
  FscRec <- 0
  
  MaxTruP <- 0
  TruPCut <- 0
  TruPAcc <- 0
  TruPFalP <- 0
  TruPTruN <- 0
  TruPFalN <- 0
  TruPPrec <- 0
  TruPRec <- 0
  TruPFsc <- 0
  
  
  
  
  #Run for loop for all formulae
  for (i in 1:length(formulae))
  {
    #Loop begins
    print (i)
    c <- 0.25
    cModel <- glm(as.formula(formulae[i]),data=TrainData,family=binomial)
    
    #Create prediction object
    TrainData$predict <- predict(cModel,type="response")
    predictor <- prediction(TrainData$predict, TrainData$Class)
    perf <- performance(predictor,"auc")
    AUC <- c(AUC,attr(perf,"y.values")[[1]][1])
    form<- c(form,formulae[i])
    #Add AUC to the list for the iteration
    
    acc <- c(0)
    cp <- c(0.24)
    prec <- 0
    rec <- 0
    trueP <- 0
    trueN <- 0
    falseP <- 0
    falseN <- 0
    fScore <- 0
    
    #Get Precision and recall
    
    k = 1
    while (c <= 0.9)
    {
      
      #Get the proper prediction based on C
      TrainData$predicted <- 0
      TrainData$predicted[TrainData$predict >= c] <- 1
      
      
      TrainData$forTable <- "TP"
      TrainData$forTable[TrainData$Class == 0 & TrainData$predicted == 1] <- "FP"
      TrainData$forTable[TrainData$Class == 1 & TrainData$predicted == 0] <- "FN"
      TrainData$forTable[TrainData$Class == 0 & TrainData$predicted == 0] <- "TN"
      DF<-table(TrainData$forTable)
      Var1 <- c("TP","FP","TN","FN")
      Freq <- c(0,0,0,0)
      for(j in 1:4)
      {
        if(length(DF[names(DF)==Var1[j]]) == 0)
        {
          Freq[j] <- 0
        }
        else
        {
          Freq[j] <- as.integer(DF[names(DF) == Var1[j]])
        }
      }
      
      tableDF <- data.frame(Var1,Freq)
      acc[k] <- (tableDF$Freq[tableDF$Var1 == "TP"] + tableDF$Freq[tableDF$Var1 == "TN"])/sum(tableDF$Freq)
      prec[k] <- (tableDF$Freq[tableDF$Var1 == "TP"])/(tableDF$Freq[tableDF$Var1=="TP"] + tableDF$Freq[tableDF$Var1=="FP"])
      rec[k] <-  (tableDF$Freq[tableDF$Var1 == "TP"])/(tableDF$Freq[tableDF$Var1=="TP"] + tableDF$Freq[tableDF$Var1=="FN"])
      fScore[k] <- 2*(prec[k]*rec[k])/(prec[k] + rec[k])
      cp[k] <- c
      trueP[k] <- tableDF$Freq[tableDF$Var1 == "TP"]
      trueN[k] <- tableDF$Freq[tableDF$Var1 == "TN"]
      falseP[k] <- tableDF$Freq[tableDF$Var1 == "FP"]
      falseN[k] <- tableDF$Freq[tableDF$Var1 == "FN"]
      
      
      
      c <- c+0.025
      k=k+1
    }
    
    
    MaxAcc <- c(MaxAcc,max(acc,na.rm=TRUE))
    AccCut <- c(AccCut,cp[match(max(acc,na.rm=TRUE),acc)])
    AccTruP <-c(AccTruP,trueP[match(max(acc,na.rm=TRUE),acc)])
    AccFalP <- c(AccFalP,falseP[match(max(acc,na.rm=TRUE),acc)])
    AccTruN <- c(AccTruN,trueN[match(max(acc,na.rm=TRUE),acc)])
    AccFalN <- c(AccFalN,falseN[match(max(acc,na.rm=TRUE),acc)])
    AccPrec <- c(AccPrec,prec[match(max(acc,na.rm=TRUE),acc)])
    AccRec <- c(AccRec,rec[match(max(acc,na.rm=TRUE),acc)])
    AccFsc <- c(AccFsc,fScore[match(max(acc,na.rm=TRUE),acc)])
    
    MaxFsc <- c(MaxFsc,max(fScore,na.rm=TRUE))
    FscAcc <- c(FscAcc,acc[match(max(fScore,na.rm=TRUE),fScore)])
    FscCut <- c(FscCut,cp[match(max(fScore,na.rm=TRUE),fScore)])
    FscTruP <- c(FscTruP,trueP[match(max(fScore,na.rm=TRUE),fScore)])
    FscFalP <- c(FscFalP,falseP[match(max(fScore,na.rm=TRUE),fScore)])
    FscTruN <- c(FscTruN,trueN[match(max(fScore,na.rm=TRUE),fScore)])
    FscFalN <- c(FscFalN,falseN[match(max(fScore,na.rm=TRUE),fScore)])
    FscPrec <- c(FscPrec,prec[match(max(fScore,na.rm=TRUE),fScore)])
    FscRec <- c(FscRec,rec[match(max(fScore,na.rm=TRUE),fScore)])
    
    MaxTruP <- c(MaxTruP,max(trueP,na.rm=TRUE))
    TruPCut <- c(TruPCut,cp[match(max(trueP,na.rm=TRUE),trueP)])
    TruPAcc <- c(TruPAcc,acc[match(max(trueP,na.rm=TRUE),trueP)])
    TruPFalP <- c(TruPFalP,falseP[match(max(trueP,na.rm=TRUE),trueP)])
    TruPTruN <- c(TruPTruN,trueN[match(max(trueP,na.rm=TRUE),trueP)])
    TruPFalN <- c(TruPFalN,falseN[match(max(trueP,na.rm=TRUE),trueP)])
    TruPPrec <- c(TruPPrec,prec[match(max(trueP,na.rm=TRUE),trueP)])
    TruPRec <- c(TruPRec,rec[match(max(trueP,na.rm=TRUE),trueP)])
    TruPFsc <- c(TruPFsc,fScore[match(max(trueP,na.rm=TRUE),trueP)])
    
    
    
  }
  
  outputDF <- data.frame(form,AUC,MaxAcc,AccCut,AccTruP,AccFalP,AccTruN,AccFalN,AccPrec,AccRec,AccFsc,
                         MaxFsc,FscAcc,FscCut,FscTruP,FscFalP,FscTruN,FscFalN,FscPrec,FscRec,
                         MaxTruP,TruPCut,TruPAcc,TruPFalP,TruPTruN,TruPFalN,TruPPrec,TruPRec,TruPFsc)
  
  fileName <- paste("/home/ravishankar/Projects/MovieMeter/Code/DataObjects/allModelStat",fileInteger,".RData",sep="")
  save(outputDF,file=fileName)
  return(outputDF)
}




#Get bootstrapping statistics on the Fscore and stuff for all candidate models
TrainDataChecker <- function(formulae)
{
  #Checks all the candidate models and creates a dataframe with statistics such as Fscore, Accuracy etc
  #formulae object contains the formula of all candidate models
  
  #Stats to find : AUC
  # Max(Accuracy), Cutoffpoint, TrueP,FalseP,TrueN,FalseN, Precision,Recall,fScore
  # Max(Fscore), Cutoffpoint, TrueP, FalseP, TrueN, FalseN, Precision, Recall, fScore
  # Max(TrueP), Cutoffpoint, TrueP, FalseP, TrueN, FalseN, Precision, Recall, fScore
  # Max(TestAccuracy), Cutoffpoint, TrueP, FalseP,TrueN,FalseN, Precision, Recall, fScore
  # Max(TestFscore), Cutoffpoint, TrueP, FalseP, TrueN, FalseN, Precision, Recall, fScore
  # Max(TestTrueP), Cutoffpoint, TrueP, FalseP, TrueN, FalseN, Precision, Recall, fScore
  
  
  #Average and Max statistics which we would like to see
  form <- ""
  #AUC
  meanAUC <- 0
  maxAUC <- 0
  minAUC <- 0
  #Accuracy
  meanAcc <- 0
  maxAcc <- 0
  minAcc <- 0
  maxAccPrec <- 0
  meanAccPrec <- 0
  minAccPrec <- 0
  maxAccRec <- 0
  meanAccRec <- 0
  meanAccFsc <- 0
  maxAccFsc <- 0
  
  maxAccTrueP <- 0
  meanAccTrueP <- 0
  minAccFalP <- 0
  meanAccFalP <- 0
  maxAccTrueN <- 0
  meanAccTrueN <- 0
  minAccFalN <- 0
  meanAccFalN <- 0
  
  #Fscore
  meanFsc <- 0
  maxFsc <- 0
  minFsc <- 0
  maxFscPrec <- 0
  meanFscPrec <- 0
  minFscPrec <- 0
  maxFscRec <- 0
  meanFscRec <- 0
  meanFscAcc <- 0
  maxFscAcc <- 0
  
  maxFscTrueP <- 0
  meanFscTrueP <- 0
  minFscFalP <- 0
  meanFscFalP <- 0
  maxFscTrueN <- 0
  meanFscTrueN <- 0
  minFscFalN <- 0
  meanFscFalN <- 0
  
  
  #Run for loop for all formulae
  for (j in 1:length(formulae))
  {
    #Loop begins
    print (j)
    form[j] <- formulae[j]
    print(length(form))
    #Create Data Variables:
    
    AUC <- 0
    MaxAcc <- 0
    AccCut <- 0
    AccTruP <- 0
    AccFalP <- 0
    AccTruN <- 0
    AccFalN <- 0
    AccPrec <- 0
    AccRec <- 0
    AccFsc <- 0
    
    MaxFsc <- 0
    FscAcc <- 0
    FscCut <- 0
    FscTruP <- 0
    FscFalP <- 0
    FscTruN <- 0
    FscFalN <- 0
    FscPrec <- 0
    FscRec <- 0
    
    MaxTruP <- 0
    TruPCut <- 0
    TruPAcc <- 0
    TruPFalP <- 0
    TruPTruN <- 0
    TruPFalN <- 0
    TruPPrec <- 0
    TruPRec <- 0
    TruPFsc <- 0
    
    
    for(i in 1:250)
    {
      
      ModelData <- Interaction2
      ModelData$X <- NULL
      Split <- sample(nrow(ModelData),nrow(ModelData)*0.8)
      TestData <- ModelData[-Split,]
      TrainData <- ModelData[Split,]  
      
      c <- 0.24
      cModel <- glm(as.formula(formulae[j]),data=TrainData,family=binomial)
      
      #Create prediction object
      TrainData$predict <- predict(cModel,type="response")
      predictor <- prediction(TrainData$predict, TrainData$Class)
      perf <- performance(predictor,"auc")
      AUC[i] <- attr(perf,"y.values")[[1]][1]
      
      #Create data variables for the cutoff point detection
      
      acc <- c(0)
      cp <- c(0.24)
      prec <- 0
      rec <- 0
      trueP <- 0
      trueN <- 0
      falseP <- 0
      falseN <- 0
      fScore <- 0
      
      #Get Precision and recall
      
      k = 1
      while (c <= 0.9)
      {
        
        #Get the proper prediction based on C
        TrainData$predicted <- 0
        TrainData$predicted[TrainData$predict >= c] <- 1
        
        
        TrainData$forTable <- "TP"
        TrainData$forTable[TrainData$Class == 0 & TrainData$predicted == 1] <- "FP"
        TrainData$forTable[TrainData$Class == 1 & TrainData$predicted == 0] <- "FN"
        TrainData$forTable[TrainData$Class == 0 & TrainData$predicted == 0] <- "TN"
        DF<-table(TrainData$forTable)
        Var1 <- c("TP","FP","TN","FN")
        Freq <- c(0,0,0,0)
        for(z in 1:4)
        {
          if(length(DF[names(DF)==Var1[z]]) == 0)
          {
            Freq[z] <- 0
          }
          else
          {
            Freq[z] <- as.integer(DF[names(DF) == Var1[z]])
          }
        }
        
        tableDF <- data.frame(Var1,Freq)
        acc[k] <- (tableDF$Freq[tableDF$Var1 == "TP"] + tableDF$Freq[tableDF$Var1 == "TN"])/sum(tableDF$Freq)
        prec[k] <- (tableDF$Freq[tableDF$Var1 == "TP"])/(tableDF$Freq[tableDF$Var1=="TP"] + tableDF$Freq[tableDF$Var1=="FP"])
        rec[k] <-  (tableDF$Freq[tableDF$Var1 == "TP"])/(tableDF$Freq[tableDF$Var1=="TP"] + tableDF$Freq[tableDF$Var1=="FN"])
        fScore[k] <- 2*(prec[k]*rec[k])/(prec[k] + rec[k])
        cp[k] <- c
        trueP[k] <- tableDF$Freq[tableDF$Var1 == "TP"]
        trueN[k] <- tableDF$Freq[tableDF$Var1 == "TN"]
        falseP[k] <- tableDF$Freq[tableDF$Var1 == "FP"]
        falseN[k] <- tableDF$Freq[tableDF$Var1 == "FN"]
        
        
        
        c <- c+0.01
        k=k+1
      }
      
      
      MaxAcc[i] <- (max(acc,na.rm=TRUE))
      AccCut[i] <- (cp[match(max(acc,na.rm=TRUE),acc)])
      AccTruP[i] <-(trueP[match(max(acc,na.rm=TRUE),acc)])
      AccFalP[i] <- (falseP[match(max(acc,na.rm=TRUE),acc)])
      AccTruN[i] <- (trueN[match(max(acc,na.rm=TRUE),acc)])
      AccFalN[i] <- (falseN[match(max(acc,na.rm=TRUE),acc)])
      AccPrec[i] <- (prec[match(max(acc,na.rm=TRUE),acc)])
      AccRec[i] <- (rec[match(max(acc,na.rm=TRUE),acc)])
      AccFsc[i] <- (fScore[match(max(acc,na.rm=TRUE),acc)])
      
      MaxFsc[i] <- (max(fScore,na.rm=TRUE))
      FscAcc[i] <- (acc[match(max(fScore,na.rm=TRUE),fScore)])
      FscCut[i] <- (cp[match(max(fScore,na.rm=TRUE),fScore)])
      FscTruP[i] <- (trueP[match(max(fScore,na.rm=TRUE),fScore)])
      FscFalP[i] <- (falseP[match(max(fScore,na.rm=TRUE),fScore)])
      FscTruN[i] <- (trueN[match(max(fScore,na.rm=TRUE),fScore)])
      FscFalN[i] <- (falseN[match(max(fScore,na.rm=TRUE),fScore)])
      FscPrec[i] <- (prec[match(max(fScore,na.rm=TRUE),fScore)])
      FscRec[i] <- (rec[match(max(fScore,na.rm=TRUE),fScore)])
      
      MaxTruP[i] <- (max(trueP,na.rm=TRUE))
      TruPCut[i] <- (cp[match(max(trueP,na.rm=TRUE),trueP)])
      TruPAcc[i] <- (acc[match(max(trueP,na.rm=TRUE),trueP)])
      TruPFalP[i] <- (falseP[match(max(trueP,na.rm=TRUE),trueP)])
      TruPTruN[i] <- (trueN[match(max(trueP,na.rm=TRUE),trueP)])
      TruPFalN[i] <- (falseN[match(max(trueP,na.rm=TRUE),trueP)])
      TruPPrec[i] <- (prec[match(max(trueP,na.rm=TRUE),trueP)])
      TruPRec[i] <- (rec[match(max(trueP,na.rm=TRUE),trueP)])
      TruPFsc[i] <- (fScore[match(max(trueP,na.rm=TRUE),trueP)])
      
    }
    
    
    meanAUC[j] <- mean(AUC,na.rm=TRUE)
    maxAUC[j] <- max(AUC,na.rm=TRUE)
    minAUC[j] <- min(AUC,na.rm=TRUE)
    #Accuracy
    meanAcc[j] <- mean(MaxAcc,na.rm=TRUE)
    maxAcc[j] <- max(MaxAcc,na.rm=TRUE)
    minAcc[j] <- min(MaxAcc,na.rm=TRUE)
    maxAccPrec[j] <- max(AccPrec,na.rm=TRUE)
    meanAccPrec[j] <- mean(AccPrec,na.rm=TRUE)
    minAccPrec[j] <- min(AccPrec,na.rm=TRUE)
    maxAccRec[j] <- max(AccRec,na.rm=TRUE)
    meanAccRec[j] <- mean(AccRec,na.rm=TRUE)
    meanAccFsc[j] <- mean(AccFsc,na.rm=TRUE)
    maxAccFsc[j] <- max(AccFsc,na.rm=TRUE)
    
    maxAccTrueP[j] <- max(AccTruP,na.rm=TRUE)
    meanAccTrueP[j] <- mean(AccTruP,na.rm=TRUE)
    minAccFalP[j] <- min(AccFalP,na.rm=TRUE)
    meanAccFalP[j] <- mean(AccFalP,na.rm=TRUE)
    maxAccTrueN[j] <- max(AccTruN,na.rm=TRUE)
    meanAccTrueN[j] <- mean(AccTruN,na.rm=TRUE)
    minAccFalN[j] <- min(AccFalN,na.rm=TRUE)
    meanAccFalN[j] <- mean(AccFalN,na.rm=TRUE)
    
    #Fscore
    meanFsc[j] <- mean(MaxFsc,na.rm=TRUE)
    maxFsc[j] <- max(MaxFsc,na.rm=TRUE)
    minFsc[j] <- min(MaxFsc,na.rm=TRUE)
    maxFscPrec[j] <- max(FscPrec,na.rm=TRUE)
    meanFscPrec[j] <- mean(FscPrec,na.rm=TRUE)
    minFscPrec[j] <- min(FscPrec,na.rm=TRUE)
    maxFscRec[j] <- max(FscRec,na.rm=TRUE)
    meanFscRec[j] <- mean(FscRec,na.rm=TRUE)
    meanFscAcc[j] <- mean(FscAcc,na.rm=TRUE)
    maxFscAcc[j] <- max(FscAcc,na.rm=TRUE)
    
    maxFscTrueP[j] <- max(FscTruP,na.rm=TRUE)
    meanFscTrueP[j] <- mean(FscTruP,na.rm=TRUE)
    minFscFalP[j] <- min(FscFalP,na.rm=TRUE)
    meanFscFalP[j] <- mean(FscFalP,na.rm=TRUE)
    maxFscTrueN[j] <- max(FscTruN,na.rm=TRUE)
    meanFscTrueN[j] <- mean(FscTruN,na.rm=TRUE)
    minFscFalN[j] <- min(FscFalN,na.rm=TRUE)
    meanFscFalN[j] <- mean(FscFalN,na.rm=TRUE)
    print(length(meanFscFalN))
    
    
  }
  
  returnObj <- data.frame(form,minAUC,meanAUC,maxAUC,minAcc,meanAcc,maxAcc,minAccPrec,meanAccPrec,maxAccPrec,
                          meanAccRec,maxAccRec,meanAccFsc,maxAccFsc,
                          meanAccTrueP,maxAccTrueP,meanAccFalP,minAccFalP,meanAccTrueN,maxAccTrueN,meanAccFalN,minAccFalN,
                          minFsc,meanFsc,maxFsc,minFscPrec,meanFscPrec,maxFscPrec,meanFscRec,maxFscRec,
                          meanFscAcc,maxFscAcc,meanFscTrueP,maxFscTrueP,meanFscFalP,minFscFalP,meanFscTrueN,maxFscTrueN,
                          meanFscFalN,minFscFalN)
  
  return(returnObj)  
}







FscPlotter <- function(formula)
{
  
  #Plots the different prediction statistics - Area Under the Curve, Number of True Positives, Number of False Positives, Fscore
  #etc which vary with Cut off points
  #Last Edit - 02/12/15
  
  
  
  
  ModelData <- Interaction2
  ModelData$X <- NULL
  Split <- sample(nrow(ModelData),nrow(ModelData)*0.8)
  TestData <- ModelData[-Split,]
  TrainData <- ModelData[Split,]  
  
  c <- 0
  cModel <- glm(as.formula(formula),data=TrainData,family=binomial)
  
  #Create prediction object
  TrainData$predict <- predict(cModel,type="response")
  predictor <- prediction(TrainData$predict, TrainData$Class)
  perf <- performance(predictor,"auc")
  
  
  #Create data variables for the cutoff point detection
  
  acc <- c(0)
  cp <- c(0.24)
  prec <- 0
  rec <- 0
  trueP <- 0
  trueN <- 0
  falseP <- 0
  falseN <- 0
  fScore <- 0
  
  #Get Precision and recall
  
  k = 1
  while (c <= 1)
  {
    
    #Get the proper prediction based on C
    TrainData$predicted <- 0
    TrainData$predicted[TrainData$predict >= c] <- 1
    
    
    TrainData$forTable <- "TP"
    TrainData$forTable[TrainData$Class == 0 & TrainData$predicted == 1] <- "FP"
    TrainData$forTable[TrainData$Class == 1 & TrainData$predicted == 0] <- "FN"
    TrainData$forTable[TrainData$Class == 0 & TrainData$predicted == 0] <- "TN"
    DF<-table(TrainData$forTable)
    Var1 <- c("TP","FP","TN","FN")
    Freq <- c(0,0,0,0)
    for(z in 1:4)
    {
      if(length(DF[names(DF)==Var1[z]]) == 0)
      {
        Freq[z] <- 0
      }
      else
      {
        Freq[z] <- as.integer(DF[names(DF) == Var1[z]])
      }
    }
    
    tableDF <- data.frame(Var1,Freq)
    acc[k] <- (tableDF$Freq[tableDF$Var1 == "TP"] + tableDF$Freq[tableDF$Var1 == "TN"])/sum(tableDF$Freq)
    prec[k] <- (tableDF$Freq[tableDF$Var1 == "TP"])/(tableDF$Freq[tableDF$Var1=="TP"] + tableDF$Freq[tableDF$Var1=="FP"])
    rec[k] <-  (tableDF$Freq[tableDF$Var1 == "TP"])/(tableDF$Freq[tableDF$Var1=="TP"] + tableDF$Freq[tableDF$Var1=="FN"])
    fScore[k] <- 2*(prec[k]*rec[k])/(prec[k] + rec[k])
    cp[k] <- c
    trueP[k] <- tableDF$Freq[tableDF$Var1 == "TP"]
    trueN[k] <- tableDF$Freq[tableDF$Var1 == "TN"]
    falseP[k] <- tableDF$Freq[tableDF$Var1 == "FP"]
    falseN[k] <- tableDF$Freq[tableDF$Var1 == "FN"]
    
    
    
    c <- c+0.01
    k=k+1
  }
  plot(cp,trueP)
}
