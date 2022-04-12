#install and run:
library('reshape2')
library('ggplot2')
library('shiny')
#Finite difference method function
#dt-size of interval for time grid
#dS-size of interval for S (underlying price) grid
#r-free risk rate of return (f.e. return from annual goverment bonds)
#sigma-volatility (annual one- can be a interval, worst scenario is then picked at every point)
#strike-strike price of an option
#barrier-barrier of an option
#call-type of an option (TRUE-call, FALSE-put)
#upperlimit-upperlimit of S for put option
#div-dividend rate (always exactly in the middle of option's period)
#Time-time to expiration date (in years)
#american-type of an option (TRUE-european,FALSE-american)
#percentagediv-type of dividend (TRUE-percentage, FALSE-PLN)
Finite_Difference <- function(dt=1/50000, dS=10, r=0.03, sigma=c(0.15,0.25), strike=2150, barrier=2400, call=TRUE,upperlimit=6600, div=0.1,Time=0.75, american=FALSE, percentageDiv=TRUE){
  #defining the X axis size (time intervals)
  cols <- round(Time/dt + 1)
  div_day <- round((Time/dt)/2)
  sigma_vec <- sigma
  
  if (call==TRUE){ # For call option
    if(percentageDiv==TRUE){
      rows <- floor(barrier/(dS*(1-div))+1) #defining the Y axis size (S price intervals)
    }
    else{
      rows <- floor(barrier/dS +1)
    }
    M <- matrix(0, rows, cols) # Matrix for option values (representation of the grid rectangle)
    if(percentageDiv==TRUE){
      S <- seq(barrier,0,-dS*(1-div)) # Defining S from 0 to a barrier (we assume that for call option the barrier is above strike price K)
    }
    else{
      S <- seq(barrier-div,0-div,-dS)
      for(i in 1:length(S)){
        if(S[i]<0){
          S[i] <- 0
        }
      }
    }
    
    S2 <- S  #saving S after the dividend happens
    M[2:rows,cols] <- pmax((S-strike),0)[2:rows] # Prices on the right border (payoffs of an option)
    
    DM <- matrix(0, rows, cols) #Delta (first derivative of an option)
    DM[,cols] <- (as.numeric(M[,cols] > 0))
    GM <- matrix(0, rows, cols) #Gamma (second derivative of an option)
    if(american){
      AM <- matrix(0, rows, cols)
      AM[,cols] <- DM[,cols]
    }
    
    for(k in cols:(div_day+1)){ #mo?e div_day
      
      #Formulas for derivatives approximations
      Delta <- (M[1:(rows-2),k]-M[3:rows,k])/(2*dS)
      Gamma <- (M[1:(rows-2),k]-2*M[2:(rows-1),k]+M[3:rows,k])/(dS)^2
      
      DM[2:(rows-1),k-1] <- Delta
      GM[2:(rows-1),k-1] <- Gamma
      
      #Calculatiing where second derivatives are over 0 (used for picking worse volatility if it's an interval)
      vt <- (Gamma >= 0) 
      sigma <- sigma_vec[2]
      if(american){
        M[2:(rows-1),k-1][vt] <- pmax((S-strike)[2:(rows-1)][vt],M[2:(rows-1),k][vt]+dt*(0.5*sigma^2*S[2:(rows-1)][vt]^2*Gamma[vt]+r*S[2:(rows-1)][vt]*Delta[vt]-M[2:(rows-1),k][vt]*r))    
      } else{
        M[2:(rows-1),k-1][vt] <- M[2:(rows-1),k][vt]+dt*(0.5*sigma^2*S[2:(rows-1)][vt]^2*Gamma[vt]+r*S[2:(rows-1)][vt]*Delta[vt]-M[2:(rows-1),k][vt]*r)    
      }
      

      vf <- (Gamma < 0)
      sigma <- sigma_vec[1]
      if(american){
        M[2:(rows-1),k-1][vf] <- pmax((S-strike)[2:(rows-1)][vf],M[2:(rows-1),k][vf]+dt*(0.5*sigma^2*S[2:(rows-1)][vf]^2*Gamma[vf]+r*S[2:(rows-1)][vf]*Delta[vf]-M[2:(rows-1),k][vf]*r))    
      } else{  
        M[2:(rows-1),k-1][vf] <- M[2:(rows-1),k][vf]+dt*(0.5*sigma^2*S[2:(rows-1)][vf]^2*Gamma[vf]+r*S[2:(rows-1)][vf]*Delta[vf]-M[2:(rows-1),k][vf]*r)    
      }
    }
    
    #Checking early exercises of american option
    if(american){
      AM[2:(rows-1),div_day:(cols-1)] <- as.numeric(M[2:(rows-1),div_day:(cols-1)] == (S[2:(rows-1)]-strike))
    }
    

    if(percentageDiv==TRUE){
      S <- seq(barrier*(1/(1-div)),0,-dS)
    }
    else{
      S <- seq(barrier,0,-dS)
    }
    
    
    S1 <- S
    #divi day, grid has to be rescaled
    NewTop <- tail(which(S>=barrier),1)
    M[1:NewTop,div_day] <- 0
    
    for(k in div_day:2){ #mo?e div_day+1
      Delta <- (M[NewTop:(rows-2),k]-M[(NewTop+2):rows,k])/(2*dS)
      Gamma <- (M[NewTop:(rows-2),k]-2*M[(NewTop+1):(rows-1),k]+M[(NewTop+2):rows,k])/(dS)^2
      
      DM[(NewTop+1):(rows-1),k-1] <- Delta
      GM[(NewTop+1):(rows-1),k-1] <- Gamma
      
      vt <- (Gamma >= 0)
      sigma <- sigma_vec[2]
      if(american){
        M[(NewTop+1):(rows-1),k-1][vt] <- pmax((S-strike)[(NewTop+1):(rows-1)][vt],M[(NewTop+1):(rows-1),k][vt]+dt*(0.5*sigma^2*S[(NewTop+1):(rows-1)][vt]^2*Gamma[vt]+r*S[(NewTop+1):(rows-1)][vt]*Delta[vt]-M[(NewTop+1):(rows-1),k][vt]*r))
      } else{
        M[(NewTop+1):(rows-1),k-1][vt] <- M[(NewTop+1):(rows-1),k][vt]+dt*(0.5*sigma^2*S[(NewTop+1):(rows-1)][vt]^2*Gamma[vt]+r*S[(NewTop+1):(rows-1)][vt]*Delta[vt]-M[(NewTop+1):(rows-1),k][vt]*r)
      }
      
      vf <- (Gamma < 0) 
      sigma <- sigma_vec[1]
      if(american){
        M[(NewTop+1):(rows-1),k-1][vf] <- pmax((S-strike)[(NewTop+1):(rows-1)][vf],M[(NewTop+1):(rows-1),k][vf]+dt*(0.5*sigma^2*S[(NewTop+1):(rows-1)][vf]^2*Gamma[vf]+r*S[(NewTop+1):(rows-1)][vf]*Delta[vf]-M[(NewTop+1):(rows-1),k][vf]*r))
      } else{
        M[(NewTop+1):(rows-1),k-1][vf] <- M[(NewTop+1):(rows-1),k][vf]+dt*(0.5*sigma^2*S[(NewTop+1):(rows-1)][vf]^2*Gamma[vf]+r*S[(NewTop+1):(rows-1)][vf]*Delta[vf]-M[(NewTop+1):(rows-1),k][vf]*r)          
      }
    }
    if(american){
      AM[(NewTop+1):(rows-1),1:(div_day-1)] <- as.numeric(M[(NewTop+1):(rows-1),1:(div_day-1)] == (S[(NewTop+1):(rows-1)]-strike))
    }
  }
  
  else{ #same for put option
    if(percentageDiv==TRUE){
      add_rows <- floor((barrier/(1-div)-barrier)/dS)+1
      rows <- floor((upperlimit-barrier)/(dS*(1-div))) +1 + add_rows 
    }
    else{
      add_rows <- floor(div/dS)+1
      rows <- floor((upperlimit-barrier)/dS) +1 + add_rows
    }
    M <- matrix(0, rows, cols)
    if(percentageDiv==TRUE){
      S <- seq(barrier,upperlimit,dS*(1-div))
    }
    else{
      S <- seq(barrier,upperlimit,dS)
    }
    S <- rev(S)
    M[1:(rows-add_rows-1),cols] <- pmax((strike-S),0)[1:(rows-add_rows-1)]
    
    if(percentageDiv==TRUE){
      S2 <- c(S,seq(from=barrier-dS*(1-div),by=-dS*(1-div), length.out = add_rows))
    }
    else{
      S2 <- c(S,seq(from=barrier-dS,by=-dS, length.out = add_rows))
    }
    
    
    DM <- matrix(0, rows, cols)
    DM[,cols] <- (as.numeric(M[,cols] > 0))
    GM <- matrix(0, rows, cols)
    if(american){
      AM <- matrix(0, rows, cols)
      AM[,cols] <- DM[,cols]
    }
    
    for(k in cols:(div_day+1)){
      Delta <- (M[1:(rows-add_rows-2),k]-M[3:(rows-add_rows),k])/(2*dS)
      Gamma <- (M[1:(rows-add_rows-2),k]-2*M[2:(rows-add_rows-1),k]+M[3:(rows-add_rows),k])/(dS)^2
      
      DM[2:(rows-add_rows-1),k-1] <- Delta
      GM[2:(rows-add_rows-1),k-1] <- Gamma
      
      vt <- (Gamma >= 0) 
      sigma <- sigma_vec[2]
      if(american){
        M[2:(rows-add_rows-1),k-1][vt] <- pmax((strike-S)[2:(rows-add_rows-1)][vt],M[2:(rows-add_rows-1),k][vt]+dt*(0.5*sigma^2*S[2:(rows-add_rows-1)][vt]^2*Gamma[vt]+r*S[2:(rows-add_rows-1)][vt]*Delta[vt]-M[2:(rows-add_rows-1),k][vt]*r))
      } else{
        M[2:(rows-add_rows-1),k-1][vt] <- M[2:(rows-add_rows-1),k][vt]+dt*(0.5*sigma^2*S[2:(rows-add_rows-1)][vt]^2*Gamma[vt]+r*S[2:(rows-add_rows-1)][vt]*Delta[vt]-M[2:(rows-add_rows-1),k][vt]*r)    
      }
      
      
      vf <- (Gamma < 0) 
      sigma <- sigma_vec[1]
      if(american){
        M[2:(rows-add_rows-1),k-1][vf] <- pmax((strike-S)[2:(rows-add_rows-1)][vf],M[2:(rows-add_rows-1),k][vf]+dt*(0.5*sigma^2*S[2:(rows-add_rows-1)][vf]^2*Gamma[vf]+r*S[2:(rows-add_rows-1)][vf]*Delta[vf]-M[2:(rows-add_rows-1),k][vf]*r))
      } else{
        M[2:(rows-add_rows-1),k-1][vf] <- M[2:(rows-add_rows-1),k][vf]+dt*(0.5*sigma^2*S[2:(rows-add_rows-1)][vf]^2*Gamma[vf]+r*S[2:(rows-add_rows-1)][vf]*Delta[vf]-M[2:(rows-add_rows-1),k][vf]*r)
      }
    }
    
    if(american){
      AM[2:(rows-add_rows-1),div_day:(cols-1)] <- as.numeric(M[2:(rows-add_rows-1),div_day:(cols-1)] == (strike-S[2:(rows-add_rows-1)]))
    }
    
    if(percentageDiv==TRUE){
      S <- seq(barrier*(1/(1-div)),upperlimit*(1/(1-div)),dS) 
    }
    else{
      S <- seq(barrier+div,upperlimit+div,dS) 
    }
    S <- c(S[1:add_rows]-add_rows*dS, S)
    S <- rev(S)
    
    S1 <- S
    
    for(k in div_day:2){ 
      Delta <- (M[1:(rows-2),k]-M[3:rows,k])/(2*dS)
      Gamma <- (M[1:(rows-2),k]-2*M[2:(rows-1),k]+M[3:rows,k])/(dS)^2
      
      DM[2:(rows-1),k-1] <- Delta
      GM[2:(rows-1),k-1] <- Gamma
      
      vt <- (Gamma >= 0) 
      sigma <- sigma_vec[2]
      if(american){
        M[2:(rows-1),k-1][vt] <- pmax((strike-S)[2:(rows-1)][vt],M[2:(rows-1),k][vt]+dt*(0.5*sigma^2*S[2:(rows-1)][vt]^2*Gamma[vt]+r*S[2:(rows-1)][vt]*Delta[vt]-M[2:(rows-1),k][vt]*r))
      } else {
        M[2:(rows-1),k-1][vt] <- M[2:(rows-1),k][vt]+dt*(0.5*sigma^2*S[2:(rows-1)][vt]^2*Gamma[vt]+r*S[2:(rows-1)][vt]*Delta[vt]-M[2:(rows-1),k][vt]*r)    
      }
      vf <- (Gamma < 0) 
      sigma <- sigma_vec[1]
      if(american){
        M[2:(rows-1),k-1][vf] <- pmax((strike-S)[2:(rows-1)][vf],M[2:(rows-1),k][vf]+dt*(0.5*sigma^2*S[2:(rows-1)][vf]^2*Gamma[vf]+r*S[2:(rows-1)][vf]*Delta[vf]-M[2:(rows-1),k][vf]*r))
      } else {
        M[2:(rows-1),k-1][vf] <- M[2:(rows-1),k][vf]+dt*(0.5*sigma^2*S[2:(rows-1)][vf]^2*Gamma[vf]+r*S[2:(rows-1)][vf]*Delta[vf]-M[2:(rows-1),k][vf]*r)
      }
      
    }
    
    if(american){
      AM[2:(rows-1),1:(div_day-1)] <- as.numeric(M[2:(rows-1),1:(div_day-1)] == (strike-S[2:(rows-1)]))
    }
  }
  
  #Preparing the returns
  if(american){
    L<- list(M,M[,1:div_day],M[,(div_day+1):cols],S1,S2,seq(0,Time,dt),DM[,1:div_day],DM[,(div_day+1):cols],GM[,1:div_day],GM[,(div_day+1):cols], AM[,1:div_day],AM[,(div_day+1):cols])
    names(L) <- c('Krata','Krata_Przed','Krata_Po','S_Przed','S_Po','dt_Wektor','Delta_Przed','Delta_Po','Gamma_Przed','Gamma_Po', 'American_Wykonanie_Przed','American_Wykonanie_Po')  
    
    colnames(L$American_Wykonanie_Przed) <- L$dt_Wektor[1:div_day]
    rownames(L$American_Wykonanie_Przed) <- L$S_Przed
    
    colnames(L$American_Wykonanie_Po) <- L$dt_Wektor[(div_day+1):cols]
    rownames(L$American_Wykonanie_Po) <- L$S_Po
  } else {
    L<- list(M,M[,1:div_day],M[,(div_day+1):cols],S1,S2,seq(0,Time,dt),DM[,1:div_day],DM[,(div_day+1):cols],GM[,1:div_day],GM[,(div_day+1):cols])
    names(L) <- c('Krata','Krata_Przed','Krata_Po','S_Przed','S_Po','dt_Wektor','Delta_Przed','Delta_Po','Gamma_Przed','Gamma_Po')
  }
  colnames(L$Krata_Przed) <- L$dt_Wektor[1:div_day]
  rownames(L$Krata_Przed) <- L$S_Przed
  
  colnames(L$Krata_Po) <- L$dt_Wektor[(div_day+1):cols]
  rownames(L$Krata_Po) <- L$S_Po
  
  colnames(L$Delta_Przed) <- L$dt_Wektor[1:div_day]
  rownames(L$Delta_Przed) <- L$S_Przed
  
  colnames(L$Delta_Po) <- L$dt_Wektor[(div_day+1):cols]
  rownames(L$Delta_Po) <- L$S_Po
  
  colnames(L$Gamma_Przed) <- L$dt_Wektor[1:div_day]
  rownames(L$Gamma_Przed) <- L$S_Przed
  
  colnames(L$Gamma_Po) <- L$dt_Wektor[(div_day+1):cols]
  rownames(L$Gamma_Po) <- L$S_Po
  
  return(L) #returning all values, values before div and after separately, vector of all underlying prices before and after div, same for time stamps, first derivatives (Deltas) and second ones (Gammas)
}

