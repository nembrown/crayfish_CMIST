devtools::install_github("https://github.com/remi-daigle/CMISTR")
library(CMISTR)
set.seed(11)
risks <- sample(x = c(1:3),size = 17,replace = TRUE)
mat_risks<- matrix(risks, nrow=2, ncol=17, byrow=TRUE)
uncertainties <- sample(x = c(1:3),size = 17,replace = TRUE)
mat_uncertainties<- matrix(uncertainties , nrow=2, ncol=17, byrow=TRUE)



score <- CMISTScore(risks,uncertainties)
score
colnames(df <- data.frame(matrix(ncol = 3, nrow = 0)))
str(score)
#runs

risks
length(risks)
length()

score_mat <- CMISTScore(mat_risks,mat_uncertainties)
score_mat
#does not work with a matrix. Only 17 values. 


# Create the loop with r and c to iterate over the matrix and add scores 
    
for (r in 1:nrow(mat_risks))   
    new<-CMISTScore(mat_risks[r,],mat_uncertainties[r,])
    score[nrow(score)+1, ]<-new


colnames(score)

library(ggplot2)
theme_set(theme_bw())

ggplot(score, aes(y=CMIST_Score, x=rownames(score))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()


marble_risk<-read.csv("C:Inputs//marble_score.csv")
marble_risk
rownames(marble_risk)<-marble_risk$region
marble_risk_simple<-marble_risk[,-c(1:3)]
marble_risk_simple
marble_risk_simple_1_3<-marble_risk_simple+1
marble_risk_simple_1_3
colnames(marble_risk_simple_1_3)<-NULL
rownames(marble_risk_simple_1_3)<-NULL
marble_risk_simple_1_3
dim(marble_risk_simple_1_3)

marble_uncertainties<-read.csv("C:Inputs//marble_uncertainty.csv")
rownames(marble_uncertainties)<-marble_uncertainties$ID
marble_uncertainties_simple<-marble_uncertainties[,-c(1:3)]
marble_uncertainties_simple_1_3<-marble_uncertainties_simple+1
colnames(marble_uncertainties_simple_1_3)<-NULL
rownames(marble_uncertainties_simple_1_3)<-NULL
marble_uncertainties_simple_1_3


score_marble <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:21) {
    new<-CMISTScore(as.numeric(marble_risk_simple_1_3[r,]),as.numeric(marble_uncertainties_simple_1_3[r,]))
    score_marble[nrow(score_marble)+1, ]<-new
    }

    
score_marble    
rownames(score_marble)<-rownames(marble_risk_simple)
colnames(score_marble)<-colnames(score)



ggplot(score_marble, aes(y=CMIST_Score, x=rownames(score_marble))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()
