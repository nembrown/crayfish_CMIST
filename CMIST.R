devtools::install_github("https://github.com/remi-daigle/CMISTR")



library(CMISTR)
set.seed(11)
risks <- sample(x = c(1:3),size = 17,replace = TRUE)
mat_risks<- matrix(risks, nrow=2, ncol=17, byrow=TRUE)
uncertainties <- sample(x = c(1:3),size = 17,replace = TRUE)
mat_uncertainties<- matrix(uncertainties , nrow=2, ncol=17, byrow=TRUE)



score <- CMISTScore(risks,uncertainties)
score
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


score
