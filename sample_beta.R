beta=function(a,b,x){
  if(x>=0 & x<=1){
    (x**(a-1))*((1-x)**(b-1))/(gamma(a)*gamma(b)/gamma(a+b))  
  }else{
    0
  }
}

N=500000
x=vector(length=N)
x[1]=runif(1)

u=runif(N)
a=2
b=5
for(i in 2:N){
  #sample from p(y|x[i-1])
  y=rnorm(1,mean=x[i-1])
  
  p_accept=beta(a,b,y)/beta(a,b,x[i-1])
  
  if(u[i]<p_accept){
    x[i]=y
  }else{
    x[i]=x[i-1]
  }
}
hist(x,breaks = 50,freq = F,main = "beta(2,5)")
lines(density(rbeta(N,2,5)))
mean(x)
a/(a+b)