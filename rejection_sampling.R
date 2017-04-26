#p is gaussian(0,1)
#q is uniform(min,max)
rejection_sampling_gau=function(mean,variance){
  N=100000
  x=vector(length=N)
  
  min_v=mean-3*variance
  max_v=mean+3*variance
  k=max-min
  print(min_v)
  print(max_v)
  i=1
  count=0
  
  while(i<=N){
    z=runif(1,min_v,max_v)  #sample from q
    p_z=dnorm(z,mean,variance)          #p(z)
    q_z=dunif(z,min_v,max_v)      #q(z)
    u=runif(1,min=0,max=k*q_z)#sample from uniform(0,k*q(z))
    if(p_z>=u){               #accept
      x[i]=z
      i=i+1
    }
  }
  hist(x,breaks = 50,freq = F)
  lines(density(rnorm(N*10,0,1)))
}
rejection_sampling_gau(0,1)