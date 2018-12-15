poissonish.dates.fun = function( length.in , rate.in ){
  
  #  Generate random exponential interarrival times.
  #  Sum interarrival times to generate event times.
  #  Round event times to integer (row indexes) 
  #  Apply unique() to eliminate duplicates.  
  
  index.full = unique( round( cumsum( rexp( n = length.in ,
                                         rate =   rate.in ) ) ) )
  
  index.trim = index.full[index.full <= length.in]
  
  dates.out = rep(0,length.in)
  
  dates.out[index.trim] = 1
  
  return(dates.out)
  
}




gen.test = FALSE

if(gen.test){


test.days  = 90
test.rates = seq(from = 0    , 
                   to = 5    ,
                  by  = 0.05 )

test.sim = 200000

test.case.fun = function( length.test , rate.test , sim.test){
  
  case.list = list()
  for (i in 1:sim.test){
    
    case.list[[i]] = poissonish.dates.fun( length.in = length.test,
                                             rate.in =   rate.test )
    }
  
   case.df = sapply( case.list , sum )
   
   result.df = data.frame(length = length.test     , 
                            rate =   rate.test     ,
                             sim =    sim.test     ,
                            mean = mean( case.df ) ,
                             var =  var( case.df ) ) 
   
   return( result.df )
  
}

test.summary = t(sapply( test.rates , test.case.fun , length.test = test.days , 
                                                        sim.test = test.sim  ))

}