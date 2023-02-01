tournament = function(nAll=100,nSelect=1,sampleSize=10000,p=1)
{
   x = c()
   if(p==1 | nSelect<2)
   {
      for(i in 1:sampleSize)
      {
         x = c(x, min(sample(nAll,nSelect)))
      }
   }
   else
   {
       for(i in 1:sampleSize)
       {
           xi   = sort(sample(nAll,nSelect))
           prob = c(p*(1-p)^(0:(nSelect-2)),(1-p)^(nSelect-1))
           xii  = sample(xi,1,prob=prob)
           x    = c(x, xii)
       }
   }
   hist(x,seq(.5,nAll+.5,1),col="yellow",border="blue",
   main=paste("Tournament:", nSelect, "out of" ,nAll, " with p=",p ),
   ylab=paste("Counts (sample size:", sampleSize,")"),
   xlab="Ordered individual by its fitness")
}
