###
#Link to description of Bayesian R2 calculation
#http://www.stat.columbia.edu/~gelman/research/unpublished/bayes_R2.pdf
##

#Run this code after fitting each MCMC chain to obtain ICC and R2 estimates
#Average results over 5 chains to account for plausible values

#Get the mathematics fitted values
fv_maths<-rowMeans(my_mod$predictions[,1,] +
  my_mod$predictions_tau1[,1,]*Z1[,1] +
  my_mod$predictions_tau2[,1,]*Z2[,1] +
  my_mod$predictions_tau3[,1,]*Z3[,1] +
  my_mod$predictions_tau4[,1,]*Z4[,1])

#Get the science fitted values
fv_science<-rowMeans(my_mod$predictions[,2,] +
  my_mod$predictions_tau1[,2,]*Z1[,2] +
  my_mod$predictions_tau2[,2,]*Z2[,2] +
  my_mod$predictions_tau3[,2,]*Z3[,2] +
  my_mod$predictions_tau4[,2,]*Z4[,2])

#Get the variance of the fitted values
var_fv_maths<-var(fv_maths)
var_fv_science<-var(fv_science)

#Get variance of mathematics and science residuals from sigmas output
var_res_maths<-mean(my_mod$sigmas[1,1,])
var_res_science<-mean(my_mod$sigmas[2,2,])

#Calculate R2 in mathematics and science
R2_maths<-var_fv_maths/(var_fv_maths+var_res_maths)
R2_science<-var_fv_science/(var_fv_science+var_res_science)

#Calculate the variance of the mathematics and science random effects
#Note the multiplication by 73^2 and 83^2 to scale random effects back up to the TIMSS scale
var_ranef_maths<-mean(my_mod$ts[1,1,]*73^2)
var_ranef_science<-mean(my_mod$ts[2,2,]*83^2)

#Calculate ICC in maths and science
ICC_maths<-var_ranef_maths/(var_ranef_maths+var_res_maths)
ICC_science<-var_ranef_science/(var_ranef_science+var_res_science)

#Print results
var_res_maths
var_res_science

var_fv_maths
var_fv_science

var_ranef_maths
var_ranef_science

R2_maths
R2_science

ICC_maths
ICC_science





