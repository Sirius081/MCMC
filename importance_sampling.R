# integrate 3x^2  [-10,10] =  
f=function(x){
  3*x*x
}
high=10
low=-10
N=100000
x=vector(length=N)

for(i in 1:N){
  z=runif(1,low,high)
  x[i]=f(z)
}
print(mean(x)*(high-low))