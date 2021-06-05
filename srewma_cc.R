rm(list = ls())
graphics.off()

library(mvtnorm)
library(MNM)

source('spa_rank.R')
source('spa_rank_xt.R')

set.seed(123)

m = 20
nic = 30
noc = 50
p = 2
lam = 0.025

D = read.csv("winequality_white.csv", sep = ";")
D.ic = D[which(D[,ncol(D)] == 7),(1:(ncol(D)-1))] # In control data

r.ic = sample(1:nrow(D.ic), (m+nic))

Xr = D.ic[r.ic[1:m], ]

Xic = D.ic[r.ic[(m+1):(m+nic)], ]
Xoc = D[which(D[,ncol(D)] == 6),(1:(ncol(D)-1))]
Xoc = Xoc[sample(1:nrow(Xoc), noc),]

Xi = rbind(Xic, Xoc)

ni = nic + noc

S0 = cov(Xr)

M = chol(solve(S0))

m = nrow(Xr)
p = ncol(Xr)

sr_m0 = matrix(0, m ,p)

for (i in 1:m) {
  
  s = rep(0, p)
  
  for (j in 1:m) {
    
    sps_j = spatial.sign(t(M)%*%t(Xr[i,]-Xr[j,]), center = F, shape = F)
    
    s = s+sps_j
    
  }
  
  sr_m0[i,] = s/m
  
} 

sr_m0_2 = sr_m0^2
RE0 = apply(sr_m0_2, 1, sum)
RE0 = sum(RE0)

sr_t = c()

REt = matrix(0, ni, p)

vt = matrix(0, ni+1, p)

Qt = c()

for (t in 1:ni) {
  
  S = cov(Xr)
  
  M = chol(solve(S))
  
  m = nrow(Xr)
  p = ncol(Xr)
  
  s = rep(0, p)
  
  for (j in 1:m) {
    
    sps_j = spatial.sign(t(M)%*%t(Xi[t,]-Xr[j,]), center = F, shape = F)
    
    s = s+sps_j
    
  }
  
  sr_t = s/m
  
  REt[t,] = sr_t^2
  sREt = apply(REt, 1, sum)
  sREt = sum(sREt)
  
  eps_t = (RE0 + sREt)/nrow(Xr)
  
  Xr = rbind(Xr, Xi[t,])
  
  vt[(t+1),] = ((1-lam)*vt[t,])+((lam)*sr_t)
  
  Qt = c(Qt, (((2-lam)*p)/(lam*eps_t))*sum(vt[(t+1),]^2))
  
  print(t)
  
}

x11()
par(cex.axis = 1.5)
plot(1:ni, Qt, pch = 20, type = 'o', main = "SREWMA Control Chart", xlab = "Time",
     cex.lab = 1.5, cex.main = 2)
abline(h = 21.397)