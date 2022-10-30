################################################################################
################################################################################
########   BC2406 Analytics I: Visual and Predictive Analytics   ###############
########   PROJECT: Say NO to Myocardial Infarction Complications  #############
################################################################################
################################################################################


#### Loading Datasets

# Install Packages
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("caTools")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("jtools")
# install.packages("huxtable")
# install.packages("officer")
# install.packages("flextable")
# install.packages("broom")
# install.packages("knitr")

# Importing libraries
library("data.table")
library("ggplot2")
library("dplyr")
library("caTools")
library("rpart")
library("rpart.plot")
library("huxtable")
library("jtools")
library("officer")
library("flextable")
library("broom")
library("knitr")



###################################################################
#####################   DATA PREPARATION    #######################
###################################################################



# ATRIAL FIBRILLATION (Complication #1)
final_AF = fread("data/clean/Final_AF.csv", stringsAsFactors = T)

# Factorizing Categorical Variables
final_AF[,(colnames(final_AF)):=lapply(.SD, as.factor),.SDcols = colnames(final_AF)]
sapply(final_AF,class)
final_AF[, 1] = NULL # Drop Age Column

summary(final_AF)

colnames(final_AF)
final_AF[,c("SYS_BP_ECT", "DIA_BP_ECT", "SYS_BP_ICU", "DIA_BP_ICU")] = NULL






# CHRONIC HEART FAILURE (Complication #2)
final_CNHF = fread("Final_CN_HF.csv", stringsAsFactors = T)

# Factorizing Categorical Variables
final_CNHF[,(colnames(final_CNHF)):=lapply(.SD, as.factor),.SDcols = colnames(final_CNHF)]
sapply(final_CNHF,class)
final_CNHF[, 1] = NULL # Drop Age Column

summary(final_CNHF)

colnames(final_CNHF)
final_CNHF[,c("SYS_BP_ECT", "DIA_BP_ECT", "SYS_BP_ICU", "DIA_BP_ICU")] = NULL






# RELAPSE OF MI (Complication #3)
final_RMI = fread("Final_RMI.csv", stringsAsFactors = T)

# Factorizing Categorical Variables
final_RMI[,(colnames(final_RMI)):=lapply(.SD, as.factor),.SDcols = colnames(final_RMI)]
sapply(final_RMI,class)
final_RMI[, 1] = NULL # Drop Age Column

summary(final_RMI)

colnames(final_RMI)
final_RMI[,c("SYS_BP_ECT", "DIA_BP_ECT", "SYS_BP_ICU", "DIA_BP_ICU")] = NULL






################################################################################
#####################   DATA EXPLORATION AND ANALYSIS    #######################
################################################################################


## Check proportion of cases for each categorical X variables 
# Done in Data Cleaning R code script

# Conclusion: Due to the highly imbalanced X variables, we decided to balance out the data by
# using over & undersampling


## Check distribution of categorical Y and explain why we chose the top 3
sampled_target_df = cbind(final_AF, final_CNHF, final_RMI)[,c("Target_AF", "Target_CN_HF", "Target_Relapse_MI")]
sampledCount = sapply(X = sampled_target_df,
                      FUN = table)

sampledCount = data.frame(Variable = rep(colnames(sampledCount), each = nrow(sampledCount)), 
                          Category = rep(rownames(sampledCount), ncol(sampledCount)), 
                          Count = as.vector(sampledCount))
plot6 = ggplot(sampledCount, aes(x=Variable, y=Count,  fill = Category)) + 
  geom_bar(position='stack', stat='identity') +
  geom_text(aes(x=Variable, label=Count), position = 'stack', 
            vjust=0.4, angle=0,hjust=0.5, size=2.3) + 
  coord_flip() +
  scale_fill_brewer(palette="Pastel1", direction = 1) + 
  labs(title="Distribution of Sampled Target Variables")

plot6
ggsave("charts/Distribution_Sampled_Target_Variables.png", plot = plot6, width=10, height=5) # tjh



################################################################################
#############################   MODEL TRAINING    ##############################
################################################################################


##########################################################################
####################  Complication: Atrial Fibrillation  #################
##########################################################################


##############################
#####  TRAIN TEST SPLIT ######
##############################

set.seed(2004)
trainAF = sample.split(Y=final_AF$Target_AF,
                       SplitRatio = 0.7)
trainsetAF = subset(final_AF, trainAF == T)
testsetAF = subset(final_AF, trainAF == F)

summary(trainsetAF)
summary(testsetAF)


#############################
#########   CART   ##########
#############################

summary(final_AF)

#### Model Building #### 
## Phrase 1: Grow trees to max
cart_AF = rpart(Target_AF~., 
              data = final_AF, 
              method = 'class', 
              control = rpart.control(minsplit = 10, cp=0))

# # Visualising the Maximal Tree
# rpart.plot(cart_AF, nn=T, 
#            main="Maximal Tree for Atrial Fibrillation")

## Phrase 2: Prune trees to min 

# Visualize Pruning Sequence 
printcp(cart_AF)
plotcp(cart_AF)
title(main="Pruning Sequence of Atrial Fibrillation", line=2, cex.main = 1)

# Calculating CP for optimal tree
CVerror.cap = cart_AF$cptable[which.min(cart_AF$cptable[,"xerror"]), "xerror"] + cart_AF$cptable[which.min(cart_AF$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart.
i = 1; j = 4
while (cart_AF$cptable[i,j] > CVerror.cap) {
  i = i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp_optimal_AF = ifelse(i > 1, sqrt(cart_AF$cptable[i,1] * cart_AF$cptable[i-1,1]), 1)

# Prune to optimal Subtree
optimal_subtree_AF = prune(cart_AF, cp=cp_optimal_AF)
# rpart.plot(optimal_subtree_AF)
print(optimal_subtree_AF)
summary(optimal_subtree_AF)

#### Model Evaluation ####

pred_cart_AF = predict(optimal_subtree_AF, newdata = testsetAF, type = "class")

table_AF <- table(testsetAF$Target_AF, pred_cart_AF, deparse.level = 2)
table_AF
round(prop.table(table_AF)*100, 3)

# Overall Accuracy
sprintf("For Atrial Fibrillation Complication:")
AF_acc =  mean(pred_cart_AF == testsetAF$Target_AF)*100
sprintf("The Overall CART Accuracy (Atrial Fibrillation) is %0.2f%%", AF_acc)


#### Feature Importance #### 
round(optimal_subtree_AF$variable.importance[1:15]/sum(optimal_subtree_AF$variable.importance)*100, 3)
summary(optimal_subtree_AF)





#################################################################




##########################################################################
###################  Complication: Chronic Heart Failure #################
##########################################################################


##############################
#####  TRAIN TEST SPLIT ######
##############################

set.seed(2004)
trainCNHF = sample.split(Y=final_CNHF$Target_CN_HF,
                         SplitRatio = 0.7)
trainsetCNHF = subset(final_CNHF, trainCNHF == T)
testsetCNHF = subset(final_CNHF, trainCNHF == F)

summary(trainsetCNHF)
summary(testsetCNHF)


#############################
#########   CART   ##########
#############################

summary(final_CNHF)

#### Model Building #### 
## Phrase 1: Grow trees to max
cart_CNHF = rpart(Target_CN_HF~., 
              data = final_CNHF, 
              method = 'class', 
              control = rpart.control(minsplit = 10, cp=0))

# # Visualising the Maximal Tree
# rpart.plot(cart_CNHF, nn=T, 
#            main="Maximal Tree for Chronic Heart Failure")

## Phrase 2: Prune trees to min 

# Visualize Pruning Sequence 
printcp(cart_CNHF)
plotcp(cart_CNHF)
title(main="Pruning Sequence of Chronic Heart Failure", line=2, cex.main = 1)


# Calculating CP for optimal tree
CVerror.cap = cart_CNHF$cptable[which.min(cart_CNHF$cptable[,"xerror"]), "xerror"] + cart_CNHF$cptable[which.min(cart_CNHF$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart.
i = 1; j = 4
while (cart_CNHF$cptable[i,j] > CVerror.cap) {
  i = i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp_optimal_CNHF = ifelse(i > 1, sqrt(cart_CNHF$cptable[i,1] * cart_CNHF$cptable[i-1,1]), 1)

# Prune to optimal Subtree
optimal_subtree_CNHF = prune(cart_CNHF, cp=cp_optimal_CNHF)
# rpart.plot(optimal_subtree_CNHF)
print(optimal_subtree_CNHF)


#### Model Evaluation ####

pred_cart_CNHF = predict(optimal_subtree_CNHF, newdata = testsetCNHF, type = "class")

table_CNHF <- table(testsetCNHF$Target_CN_HF, pred_cart_CNHF, deparse.level = 2)
table_CNHF
round(prop.table(table_CNHF)*100, 3)

# Overall Accuracy
sprintf("For Chronic Heart Failure: ")
CNHF_acc = mean(pred_cart_CNHF == testsetCNHF$Target_CN_HF)*100
sprintf("The Overall CART Accuracy (Chronic Heart Failure) is %0.2f%%", CNHF_acc)


#### Feature Importance #### 
round(optimal_subtree_CNHF$variable.importance[1:15]/sum(optimal_subtree_CNHF$variable.importance)*100, 3)
summary(optimal_subtree_CNHF)





#################################################################




##########################################################################
####  Complication: Relapse in Myocardial Infarction (Heart Attack)  #####
##########################################################################

##############################
#####  TRAIN TEST SPLIT ######
##############################

set.seed(2004)
trainMI = sample.split(Y=final_RMI$Target_Relapse_MI,
                       SplitRatio = 0.7)
trainsetMI = subset(final_RMI, trainMI == T)
testsetMI = subset(final_RMI, trainMI == F)

summary(trainsetMI)
summary(testsetMI)

#############################
#########   CART   ##########
#############################

summary(final_RMI)

#### Model Building #### 
## Phrase 1: Grow trees to max
cart_RMI = rpart(Target_Relapse_MI~., 
              data = final_RMI, 
              method = 'class', 
              control = rpart.control(minsplit = 10, cp=0))

# rpart.plot(cart_RMI, nn=T, 
#            main="Maximal Tree for Relapse of MI")

## Phrase 2: Prune trees to min 

# Visualize Pruning Sequence 
printcp(cart_RMI)
plotcp(cart_RMI)
title(main="Pruning Sequence of Relapse of MI", line=2, cex.main = 1)

# Calculating CP for optimal tree
CVerror.cap = cart_RMI$cptable[which.min(cart_RMI$cptable[,"xerror"]), "xerror"] + cart_RMI$cptable[which.min(cart_RMI$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart.
i = 1; j = 4
while (cart_RMI$cptable[i,j] > CVerror.cap) {
  i = i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp_optimal_RMI = ifelse(i > 1, sqrt(cart_RMI$cptable[i,1] * cart_RMI$cptable[i-1,1]), 1)

# Prune to optimal Subtree
optimal_subtree_RMI = prune(cart_RMI, cp=cp_optimal_RMI)
# rpart.plot(optimal_subtree_RMI)
print(optimal_subtree_RMI)


#### Model Evaluation ####

pred_CART3 = predict(optimal_subtree_RMI, newdata = testsetMI, type = "class")

table_RMI <- table(testsetMI$Target_Relapse_MI, pred_CART3, deparse.level = 2)
table_RMI
round(prop.table(table_RMI)*100, 3)

# Overall Accuracy
RMI_acc = mean(pred_CART3 == testsetMI$Target_Relapse_MI)*100
sprintf("The Overall CART Accuracy (RMI) is %0.2f%%", RMI_acc)


### FEATURE IMPORTANCE

round(optimal_subtree_RMI$variable.importance[1:15]/sum(optimal_subtree_RMI$variable.importance)*100, 3)
summary(optimal_subtree_RMI)





#################################################################



###########################################
##########   CONFUSION MATRIX    ##########
###########################################



plot_confusion_matrix = function (act, pred, title="") {
  act_pred = cbind(data.table(act=act), data.table(pred=pred))
  conf_df = act_pred[,.N,by=c("act", "pred")]
  
  df <- data.frame(act=factor(c(0, 0, 1, 1)), 
                   pred=factor(c(0, 1, 0, 1)),
                   val=c(conf_df[act==0 & pred==0, N],
                         conf_df[act==0 & pred==1, N],
                         conf_df[act==1 & pred==0, N],
                         conf_df[act==1 & pred==1, N]))
  
  plt = ggplot(data = df, mapping = aes(x = act, y = pred)) +
    geom_tile(aes(fill = val), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", val)), vjust = 1) +
    scale_fill_gradient(low = "pink", high = "cyan") +
    theme_bw() + theme(legend.position = "none") + ggtitle(title)
  ggsave(file=sprintf("%s.png",title), limitsize = T, plot=plt)
}

plot_confusion_matrix(pred_cart_AF, testsetAF$Target_AF, "Target_AF Confusion Matrix")
plot_confusion_matrix(pred_cart_CNHF, testsetCNHF$Target_CN_HF, "Target_CN_HF Confusion Matrix")
plot_confusion_matrix(pred_CART3, testsetMI$Target_Relapse_MI, "Target_Relapse_MI Confusion Matrix")



#################################################################

##############################################
#########   CONSOLIDATING RESULTS   ##########
##############################################

## For each of our complication, we ran variable importance and set the baseline 
## for significance to be 2%. 



 
# Atrial Fibrillation (Complication #1)
AF_result = round(optimal_subtree_AF$variable.importance/sum(optimal_subtree_AF$variable.importance)*100, 2)
AF_result = data.frame("Weightage"=AF_result[AF_result>2])

ggplot(AF_result, aes(y = reorder(rownames(AF_result), Weightage)
                        , x=Weightage, fill=Weightage, label = Weightage)) + 
  geom_col()+
  geom_text(aes(label=Weightage), 
            vjust=0.4, angle=0,hjust=1.5, size=3, color="white", fontface="bold") +
  labs(x="% Weightage", y="Feature Variables"
       , title="Variable Importance for Atrial Fibrillation")




# Chronic Heart Failure (Complication #2)
CNHF_result = round(optimal_subtree_CNHF$variable.importance/sum(optimal_subtree_CNHF$variable.importance)*100, 2)
CNHF_result = data.frame("Weightage"=CNHF_result[CNHF_result>2])

ggplot(CNHF_result, aes(y = reorder(rownames(CNHF_result), Weightage)
                       , x=Weightage, fill=Weightage, label = Weightage)) + 
  geom_col()+
  geom_text(aes(label=Weightage), 
            vjust=0.4, angle=0,hjust=1.5, size=3, color="white", fontface="bold") +
  labs(x="% Weightage", y="Feature Variables"
       , title="Variable Importance for Chronic Heart Failure")




# Relapse of MI (Complication #3)
RMI_result = round(optimal_subtree_RMI$variable.importance/sum(optimal_subtree_RMI$variable.importance)*100, 2)
RMI_result = data.frame("Weightage"=RMI_result[RMI_result>2])

ggplot(RMI_result, aes(y = reorder(rownames(RMI_result), Weightage)
                       , x=Weightage, fill=Weightage, label = Weightage)) + 
  geom_col()+
  geom_text(aes(label=Weightage), 
            vjust=0.4, angle=0,hjust=1.5, size=3, color="white", fontface="bold") +
  labs(x="% Weightage", y="Feature Variables"
       , title="Variable Importance for Relapse of MI")
  





##############################################
########      MODEL COMPARISON      ##########
##############################################


# Comparison of Model Accuracy
models_acc = transpose(data.frame("Accuracy" = c(AF_acc, CNHF_acc, RMI_acc)))
colnames(models_acc) = c("Atrial Fibrillation", "Chronic Heart Failure", "Relapse of MI")
rownames(models_acc) = "Accuracy (%)"
round(models_acc,2)


# Comparison of Complication Feature Variables
AF_result
CNHF_result
RMI_result




##########################################
#######   LOGISTIC REGRESSION    #########
##########################################

## We then extracted our important variables and conducted logistics regression to understand the
#  significance of each level for each variable.  


# Avoid Scientific Notation
options(scipen=999)


### Atrial Fibrillation ### 

# Extracting only the important features 
final_AF_VI = final_AF[, colnames(final_AF) %in% c(rownames(AF_result), "Target_AF"), with=FALSE]
summary(final_AF_VI)

# Logistic Regression
AF_log = glm(Target_AF~., family = binomial, data = final_AF_VI)
summary(AF_log)

# Summary of AF Logistics Regression
AF_logstats = tidy(AF_log)
AF_Sig = data.table(AF_logstats[AF_logstats$p.value<0.05,], key='term') # p-value < 0.05


# Evaluating Odds Ratio 
OR_AF = exp(coef(AF_log))
OR_AF_Sig = data.frame(OR_AF[OR_AF>1])  # Extracting Odds Ratio > 1
OR_AF_Sig = data.table(cbind("term" = rownames(OR_AF_Sig), OR_AF_Sig), key='term')
colnames(OR_AF_Sig) = c('term', "Odds Ratio")

# Confidence Intervals
OR_AF_CI = data.frame(exp(confint(AF_log))) 
OR_AF_CI = data.table(cbind("term" = rownames(OR_AF_CI), OR_AF_CI), key='term')
colnames(OR_AF_CI) = c('term', "CI_2.5%", "CI_97.5%")

# Consolidating z-test, odds ratio & odds ratio CI into single table
AF_summary = merge(AF_Sig, OR_AF_Sig, by='term')
AF_summary = merge(AF_summary, OR_AF_CI, by='term')
AF_summary





### Chronic Heart Failure ### 

# Extracting only the important features 
final_CNHF_VI = final_CNHF[, colnames(final_CNHF) %in% c(rownames(CNHF_result), "Target_CN_HF"), with=FALSE]
summary(final_CNHF_VI)

# Logistic Regression
CNHF_log = glm(Target_CN_HF~., family = binomial, data = final_CNHF_VI)
summary(CNHF_log)

# Summary of CNHF Logistics Regression
CNHF_logstats = tidy(CNHF_log)
CNHF_Sig = data.table(CNHF_logstats[CNHF_logstats$p.value<0.05,], key='term') # p-value < 0.05


# Evaluating Odds Ratio 
OR_CNHF = exp(coef(CNHF_log))
OR_CNHF_Sig = data.frame(OR_CNHF[OR_CNHF>1])  # Extracting Odds Ratio > 1
OR_CNHF_Sig = data.table(cbind("term" = rownames(OR_CNHF_Sig), OR_CNHF_Sig), key='term')
colnames(OR_CNHF_Sig) = c('term', "Odds Ratio")

# Confidence Intervals
OR_CNHF_CI = data.frame(exp(confint(CNHF_log))) 
OR_CNHF_CI = data.table(cbind("term" = rownames(OR_CNHF_CI), OR_CNHF_CI), key='term')
colnames(OR_CNHF_CI) = c('term', "CI_2.5%", "CI_97.5%")

# Consolidating z-test, odds ratio & odds ratio CI into single table
CNHF_summary = merge(CNHF_Sig, OR_CNHF_Sig, by='term')
CNHF_summary = merge(CNHF_summary, OR_CNHF_CI, by='term')
CNHF_summary




### RELAPSE OF MI ### 

# Extracting only the important features 
final_RMI_VI = final_RMI[, colnames(final_RMI) %in% c(rownames(RMI_result), "Target_Relapse_MI"), with=FALSE]
summary(final_RMI_VI)

# Logistics Regression
RMI_log = glm(Target_Relapse_MI~., family = binomial, data = final_RMI_VI)
summary(RMI_log)

# Summary of RMI Logistics Regression
RMI_logstats = tidy(RMI_log)
RMI_Sig = data.table(RMI_logstats[RMI_logstats$p.value<0.05,], key='term') # p-value < 0.05


# Evaluating Odds Ratio 
OR_RMI = exp(coef(RMI_log))
OR_RMI_Sig = data.frame(OR_RMI[OR_RMI>1])  # Extracting Odds Ratio > 1
OR_RMI_Sig = data.table(cbind("term" = rownames(OR_RMI_Sig), OR_RMI_Sig), key='term')
colnames(OR_RMI_Sig) = c('term', "Odds Ratio")

# Confidence Intervals
OR_RMI_CI = data.frame(exp(confint(RMI_log))) 
OR_RMI_CI = data.table(cbind("term" = rownames(OR_RMI_CI), OR_RMI_CI), key='term')
colnames(OR_RMI_CI) = c('term', "CI_2.5%", "CI_97.5%")

# Consolidating z-test, odds ratio & odds ratio CI into single table
RMI_summary = merge(RMI_Sig, OR_RMI_Sig, by='term')
RMI_summary = merge(RMI_summary, OR_RMI_CI, by='term')
RMI_summary



options(scipen = 0, digits=7)

# Results
AF_summary
CNHF_summary
RMI_summary



#########################################
#########   EXPORTING CHARTS   ##########
#########################################


### FINAL TABLE TO CONSOLIDATE RESULTS ###
# export_summs(AF_log, CNHF_log, RMI_log, scale=TRUE, 
#              to.file = "docx", file.name = "Log_Summary_Results.docx", 
#              model.names = c("Atrial Fibrillation", "Chronic Heart Failure", "Relapse of MI"))



### Pruning Sequence ###

# Saving as jpeg file
jpeg(file="charts/Pruning_Sequence_AF.jpeg")
plotcp(cart_AF)
title(main="Pruning Sequence of Relapse of MI", line=2, cex.main = 1)
dev.off()

jpeg(file="charts/Pruning_Sequence_CNHF.jpeg")
plotcp(cart_CNHF)
title(main="Pruning Sequence of Relapse of MI", line=2, cex.main = 1)
dev.off()

jpeg(file="charts/Pruning_Sequence_RMI.jpeg")
plotcp(cart_RMI)
title(main="Pruning Sequence of Relapse of MI", line=2, cex.main = 1)
dev.off()



### Variable Importance ###

plot_AF = ggplot(AF_result, aes(y = reorder(rownames(AF_result), Weightage)
                                , x=Weightage, fill=Weightage, label = Weightage)) + 
            geom_col()+
            geom_text(aes(label=Weightage), 
                      vjust=0.4, angle=0,hjust=1.5, size=3, color="white", fontface="bold") +
            labs(x="% Weightage", y="Feature Variables"
                 , title="Variable Importance for Atrial Fibrillation")
ggsave(file="charts/Variable_Importance_AF.png", limitsize = T, plot=plot_AF)



plot_CNHF = ggplot(CNHF_result, aes(y = reorder(rownames(CNHF_result), Weightage)
                                    , x=Weightage, fill=Weightage, label = Weightage)) + 
              geom_col()+
              geom_text(aes(label=Weightage), 
                        vjust=0.4, angle=0,hjust=1.5, size=3, color="white", fontface="bold") +
              labs(x="% Weightage", y="Feature Variables"
                   , title="Variable Importance for Chronic Heart Failure")
ggsave(file="charts/Variable_Importance_CNHF.png", limitsize = T, plot=plot_CNHF)


plot_RMI = ggplot(RMI_result, aes(y = reorder(rownames(RMI_result), Weightage)
                                 , x=Weightage, fill=Weightage, label = Weightage)) + 
                      geom_col()+
                      geom_text(aes(label=Weightage), 
                                vjust=0.4, angle=0,hjust=1.5, size=3, color="white", fontface="bold") +
                      labs(x="% Weightage", y="Feature Variables"
                         , title="Variable Importance for Relapse of MI")
ggsave(file="charts/Variable_Importance_RMI.png", limitsize = T, plot=plot_RMI)


################################################################################
#########################       END OF ANALYSIS       ##########################
################################################################################


