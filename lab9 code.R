x1=c(1, 0 ,0 ,0 ,1, 1 ,0 ,1 ,0, 1)
x2=c(1, 1 ,1 ,1 ,0, 1 ,1 ,1 ,1, 1)
x3=c(1, 0 ,1 ,1 ,1, 1 ,1 ,0 ,1, 1)
x4=c(1, 0 ,1 ,0 ,0, 0 ,1 ,1 ,0, 0)
x5=c(0, 1 ,1 ,1 ,0, 1 ,1 ,1 ,0, 1)
obs=rbind(x1,x2,x3,x4,x5)
obs
propa_initial=c(.6,.5)
errr=10
propb=0
propa=propa_initial


# E-step
mstep <- function (obs,propa) {
  dat <- matrix (0,ncol = 4, nrow = 5)
  for (i in 1:5) {
    headnum=sum(obs[i,]==1)
    tailnum=sum(obs[i,]==0)
    lik_A=(propa[1])^headnum*(1-propa[1])^tailnum
    lik_B=(propa[2])^headnum*(1-propa[2])^tailnum
    pro_A=lik_A/(lik_A+lik_B)
    pro_B=lik_B/(lik_A+lik_B)
    dat[i,1]=pro_A*headnum
    dat[i,2]=pro_A*tailnum
    dat[i,3]=pro_B*headnum
    dat[i,4]=pro_B*tailnum
  }
  colSums(dat)
}
mstep(obs,propa)


# M-step
while(errr > 10^-6)
{
  propb=propa
  pro_vec=mstep(obs,propa)
  proA_est=pro_vec[1]/(pro_vec[1]+pro_vec[2])
  proB_est=pro_vec[3]/(pro_vec[3]+pro_vec[4])
  propa=c(proA_est,proB_est)
  errr=sum(abs(propa-propb))
}
propa

