create.users.fun = function(dummy.arg = NULL){


options(stringsAsFactors=FALSE)

N.exposed   = 100
N.unexposed = 100
N.total     = N.exposed +
              N.unexposed

campaign.length = 90

ad.rates = c( 0.10 ,    # ad1 
              0.15 ,    # ad2
              0.20 ,    # ad3
              0.25 ,    # ad4
              0.40 )    # ad5

vr.baseline = 0.15
vr.adeffect = 0.05

vr.exposed = vr.baseline + vr.adeffect
vr.control = vr.baseline


user.df = data.frame(user = rep( 1:N.total , campaign.length ))

user.df = user.df %>% group_by(user) %>% mutate(day = 1:campaign.length) %>% ungroup()

user.df = user.df %>% mutate( group = ifelse( user <= N.exposed , "exposed" , "unexposed" ) )


source("PoissonishDatesFun.R")

user.df = user.df %>% group_by(user) %>%
mutate( ad1.exp = ifelse( group == "exposed" ,
                  poissonish.dates.fun( campaign.length , ad.rates[1] ) , 0 ) ,
        ad2.exp = ifelse( group == "exposed" ,
                  poissonish.dates.fun( campaign.length , ad.rates[2] ) , 0 ) ,
        ad3.exp = ifelse( group == "exposed" ,
                  poissonish.dates.fun( campaign.length , ad.rates[3] ) , 0 ) ,
        ad4.exp = ifelse( group == "exposed" ,
                  poissonish.dates.fun( campaign.length , ad.rates[4] ) , 0 ) ,
        ad5.exp = ifelse( group == "exposed" ,
                  poissonish.dates.fun( campaign.length , ad.rates[5] ) , 0 ) ) %>% ungroup()


user.df = user.df %>%
mutate( overall.exp = rowSums( select( . , ends_with(".exp") ) ) )




user.df = user.df %>% group_by(user) %>%
  mutate_at(vars(ends_with(".exp")),funs(cum = cumsum)) %>% ungroup()

user.df = user.df %>% rename_at( vars( ends_with(".exp_cum") ) , 
                                 funs( str_replace( . , "exp_cum" , "cum" ) ) ) 




user.df = user.df %>% group_by(user) %>% 
  mutate( dofX = ifelse( day < min( which(overall.exp==1) ) , "preX" , "pstX")) %>% ungroup()




user.df = user.df %>% 
  select(user,group,day,dofX,ends_with(".exp"),ends_with(".cum"))




user.df = user.df %>% group_by(user) %>% 
  mutate(visit = ifelse( group == "exposed" ,
                         poissonish.dates.fun( campaign.length , vr.exposed ) , 
                         poissonish.dates.fun( campaign.length , vr.control ) ) ) %>% ungroup()
  


user.df = user.df %>% group_by(user) %>% 
  mutate(vis.cum = cumsum(visit)) %>% ungroup()




user.df  = user.df %>% group_by(user) %>% 
  mutate( vis.preX.vis = ifelse( dofX=="preX" , visit , 0) ,
          vis.pstX.vis = ifelse( dofX=="pstX" , visit , 0) ) %>%  ungroup()

user.df = user.df %>% group_by(user) %>% 
  mutate( vis.preX.cum = cumsum(vis.preX.vis) ,
          vis.pstX.cum = cumsum(vis.pstX.vis) ) %>% ungroup()

user.df = user.df %>% group_by(user) %>% 
  mutate( vis.adjX.cum = ifelse( dofX=="preX" , vis.preX.cum , vis.pstX.cum ) )


#
#  Add scaling factors to produce unscaled results (visits per reporting period),
#  as well as visits per day, visits per week, and visits per month
#

user.df = user.df %>%  group_by(user) %>%
  mutate( factor.unscaled =  1     ,
          factor.day      =  1/day ,
          factor.week     =  7/day ,
          factor.mnth     = 30/day )

user.df = user.df %>% gather( key, value , starts_with("factor")) %>%
  extract( key , c( "factor" , "scale" ) , "(factor)\\.(.*)" ) %>%
  spread(factor,value)





return(user.df)
 
}





