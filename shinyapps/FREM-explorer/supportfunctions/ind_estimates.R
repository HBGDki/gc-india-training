#Calculate EBEs for a specific individual given a omega + sigma matrix estimates, theta estimates and observations
#Written by Joakim Nyberg, Pharmetheus for the Gates project, 2016-07

#Input: 
# cData = vector of data points for this individual
# cTime = vector of independent variable (e.g. time points) for this individual
# om = Omega matrix (IIV)
# sig = Sigma matrix (RUV)
# start_eta start point for the EBE-search, default = typical values, i.e. vector of 0
# eta_fixed is a vector of the same length as start_eta with the number of fixed/constant etas, constant implies that they are fixed to 0, i.e. not the same as OMEGA fixed
# modelfunction is a pointer to the model that should be used

# Output
# List of individual estimates (EBEs) and the observed Fisher information matrix for the EBEs (uncertainty for the EBEs)

ind_estimates <-function(cData,cTime,theta,om,sig,start_eta=rep(0,nrow(om)),eta_fixed=rep(FALSE,length(start_eta)),modelfunction){

start_eta<-start_eta[which(eta_fixed==FALSE)] #Removed fix parameters

om<-om[which(eta_fixed==FALSE),which(eta_fixed==FALSE)] #Removed fix parameters

c1 = length(start_eta)/2*log(2*pi)
c2 = 1/2*log(det(om))
c3 = solve(om)

#Assume no interaction model and additive (homeoscedastic) residual error
h<-rep(1,length(cTime))
if (length(h)!=1) {
  res_var <- diag(as.numeric(h*sig*t(h)))
} else {
  res_var<-as.matrix(h*sig*t(h))
}
lC<-solve(chol(res_var)) #Calulcate inverse of cholesky factorized residual variance
det_res_var = det(res_var) #Calculate determinant of residual variance

#This could be changed to any optimization method in R or user written
ebe = nlm(min_function,start_eta,cData,cTime,theta,om,sig,c1,c2,c3,lC,det_res_var,eta_fixed,modelfunction,hessian=TRUE)
return(list(ebe$estimate,ebe$hessian))
}


#This is the function that should be minimized, w.r.t eta
min_function<- function(eta_ind,cData,cTime,theta,om,sig,c1,c2,c3,lC,det_res_var,eta_fixed,modelfunction) {
    li <- ind_likelihood(cData,cTime,theta,eta_ind,det_res_var,lC,eta_fixed,modelfunction) #Individual log likelihood
    ret =c1+c2+1/2*t(eta_ind)%*%c3%*%eta_ind-li
}
                    