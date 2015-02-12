#Candidate Models used in the analysis:


cModel <- glm(Class ~ Director + Writer + Actor1 + Actor2 + Actor3 + Actor4 + Actor5,data=TrainData,family = binomial)
cModel1 <- glm(Class ~ Director + Writer + Actor1 + Act1Dir + WriDir, data = TrainData,family = binomial)
cModel2 <- glm(Class ~ Director + Writer + Act1Dir + WriDir,data = TrainData, family = binomial)
cModel3 <- glm(Class ~ Director + Writer + Actor3 + Act1Dir + Act2Dir + Act3Dir + WriDir + Act3Act2,data = TrainData, family = binomial)
cModel4 <- glm(Class ~ Director + Writer + Act1Dir + Act2Dir + Act3Dir + WriDir + Act3Act2, data = TrainData,family = binomial)
cModel5 <- glm(Class ~ Director + Writer + Actor3 + Act1Dir + Act3Dir + WriDir + Act3Act2,data = TrainData, family = binomial)


#Second set of candidate models:
cModel6 <- glm(Class ~ Director + Writer + WriDir,data = TrainData,family = binomial)
cModel7 <- glm(Class ~ Director + Writer + WriDir + Actor1 + Actor3,data=TrainData,family = binomial)
cModel8 <- glm(Class ~ Director + Writer + Actor3 + WriDir,data=TrainData,family= binomial)
cModel9 <- glm(Class ~ Director + Writer + Actor3,data = TrainData,family = binomial)
cModel10 <- glm(Class ~ Director + Writer + Act1Dir+Act3Dir+Actor3+WriDir,data=TrainData,family = binomial)

#Third set of models:
cModel11 <-  glm(Class ~ Director + Writer + Actor3 + Actor4 + WriDir + Act3Dir + Act5Dir + Act1Act4 + Act3Act2 + Act5Act2,data=TrainData,family=binomial)
summary(cModel11)
cModel12 <- glm(Class ~ Director + Writer + Actor3  + WriDir + Act3Dir + Act5Dir + Act3Act2 ,data=TrainData,family=binomial)
lr <- lrtest(cModel11,cModel13)
cModel13 <- glm(Class ~ Director + Writer + Actor3  + WriDir + Act3Dir + Act5Dir + Act3Act2 + Act5Act2,data=TrainData,family=binomial)
