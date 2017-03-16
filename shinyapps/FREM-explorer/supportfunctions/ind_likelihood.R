#Calculate the individual log likelihood
#Written by Joakim Nyberg, Pharmetheus 2016-07 for Shiny App estimation in Gates project

#Input: cData, a vector of data points and times cTime, the theta estimates, the current eta estimates.
ind_likelihood <-function(cData,cTime,theta,eta,det_res_var,lC,eta_fixed,modelfunction) {
  cIpred<-modelfunction(cTime,theta,etas=ifelse(eta_fixed==FALSE,eta,0))
  res = cData-cIpred #Individual residuals
  R=(t(res)%*%lC)
  li = -1/2*log(det_res_var)-1/2*R%*%t(R)
  return(li)
}