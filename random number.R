Nsim=10^4; lambda=100
spread=3*sqrt(lambda)
t=round(seq(lambda-spread,lambda+spread,1))
prob=ppois(t, lambda)

prob1<- as.data.frame(prob)

X=rep(0,Nsim)

for (i in 1){
  u=runif(1) 
   X[i]=t[1]+sum(prob<u) }

xx <- as.data.frame(X)

md <- runif(1)
t[1]

