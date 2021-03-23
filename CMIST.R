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


marble_risk<-read.csv("C:Inputs//marble_risk.csv")
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

marble_uncertainties<-read.csv("C:Inputs//marble_certainty_321.csv")
rownames(marble_uncertainties)<-marble_uncertainties$ID
marble_uncertainties_simple<-marble_uncertainties[,-c(1:3)]
marble_uncertainties_simple_1_3<-marble_uncertainties_simple
colnames(marble_uncertainties_simple_1_3)<-NULL
rownames(marble_uncertainties_simple_1_3)<-NULL
marble_uncertainties_simple_1_3


score_marble <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:21) {
    new_marble<-CMISTScore(as.numeric(marble_risk_simple_1_3[r,]),as.numeric(marble_uncertainties_simple_1_3[r,]))
    score_marble[nrow(score_marble)+1, ]<-new_marble
    }

    
score_marble    
rownames(score_marble)<-rownames(marble_risk_simple)
colnames(score_marble)<-colnames(score)



ggplot(score_marble, aes(y=CMIST_Score, x=rownames(score_marble))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()


############### redswamp
redswamp_risk<-read.csv("C:Inputs//redswamp_risk.csv")
redswamp_risk
rownames(redswamp_risk)<-redswamp_risk$region
redswamp_risk_simple<-redswamp_risk[,-c(1:3)]
redswamp_risk_simple
redswamp_risk_simple_1_3<-redswamp_risk_simple+1
redswamp_risk_simple_1_3
colnames(redswamp_risk_simple_1_3)<-NULL
rownames(redswamp_risk_simple_1_3)<-NULL
redswamp_risk_simple_1_3
dim(redswamp_risk_simple_1_3)

redswamp_uncertainties<-read.csv("C:Inputs//redswamp_certainty_321.csv")
rownames(redswamp_uncertainties)<-redswamp_uncertainties$ID
redswamp_uncertainties_simple<-redswamp_uncertainties[,-c(1:3)]
redswamp_uncertainties_simple_1_3<-redswamp_uncertainties_simple
colnames(redswamp_uncertainties_simple_1_3)<-NULL
rownames(redswamp_uncertainties_simple_1_3)<-NULL
redswamp_uncertainties_simple_1_3


score_redswamp <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:21) {
  new_redswamp<-CMISTScore(as.numeric(redswamp_risk_simple_1_3[r,]),as.numeric(redswamp_uncertainties_simple_1_3[r,]))
  score_redswamp[nrow(score_redswamp)+1, ]<-new_redswamp
}


score_redswamp    
rownames(score_redswamp)<-rownames(redswamp_risk_simple)
colnames(score_redswamp)<-colnames(score)



ggplot(score_redswamp, aes(y=CMIST_Score, x=rownames(score_redswamp))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()

score_redswamp_id<-score_redswamp
score_redswamp_id$region<-rownames(score_redswamp_id)
score_redswamp_id$species<-"redswamp"
score_redswamp_id

score_marble_id<-score_marble
score_marble_id$region<-rownames(score_marble_id)
score_marble_id$species<-"marble"
score_marble_id

scores_combined<-rbind(score_marble_id, score_redswamp_id)
head(scores_combined)

ggplot(scores_combined, aes(y=CMIST_Score, x=region, colour=species)) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1, position=position_dodge(width=0.5)) +
  geom_point(size=4, position=position_dodge(width=0.5))


#Likelihood
ggplot(scores_combined, aes(y=Likelihood_Score, x=region, colour=species)) + 
  geom_errorbar(aes(ymin=Likelihood_Lower, ymax=Likelihood_Upper), width=.1, position=position_dodge(width=0.5)) +
  geom_point(size=4, position=position_dodge(width=0.5))
#Impact
ggplot(scores_combined, aes(y=Impact_Score, x=region, colour=species)) + 
  geom_errorbar(aes(ymin=Impact_Lower, ymax=Impact_Upper), width=.1, position=position_dodge(width=0.5)) +
  geom_point(size=4, position=position_dodge(width=0.5))


#' scores <- simScore(risk=2,certainty=1)
simScore <- function(risk,certainty,n=1000){
  p <- probs[probs$Risk==risk&probs$Certainty==certainty,]
  sample(x=p$Score, size=n, prob=p$Probability,replace=TRUE)
}


simScore(risk=2,certainty=1)
