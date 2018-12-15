attrib.weight.fun =  function( days , attrib.window.length , decay.terminal.value ){
  
  fill = rep( 0 , times = days^2 )
  
  attrib.weight.df = as.data.frame( matrix( fill , nrow=days , ncol=days ) )
  
  decay.weight = exp( log(decay.terminal.value) / (attrib.window.length-1) )
  
  full.decay = decay.weight^seq( from = attrib.window.length - 1 ,
                                   to =  0                       , 
                                   by = -1                       )
  
  for (day in 1:days){
    
    lower.array.idx = pmax( 1 , day-(attrib.window.length-1       ) )
    lower.decay.idx = pmax( 1 ,     (attrib.window.length-(day-1) ) )
    
    attrib.weight.df[ lower.array.idx :        day           , day ] = 
          full.decay[ lower.decay.idx : attrib.window.length ]
    
  }
  
  return(attrib.weight.df)
  
}
  
  
 