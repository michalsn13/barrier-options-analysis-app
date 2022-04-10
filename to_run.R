#install and run:
library('reshape2')
library('ggplot2')
library('shiny')
#Finite difference method function
Finite_Difference <- function(dt=1/50000, dS=10, r=0.03, sigma=c(0.15,0.25), strike=2150, barrier=2400, call=TRUE,upperlimit=6600, div=0.1,Time=0.75, american=FALSE, percentageDiv=TRUE){
  #wymiary po t
  cols <- round(Time/dt + 1)
  div_day <- round((Time/dt)/2)
  sigma_vec <- sigma
  
  if (call==TRUE){ # Opcja Call
    if(percentageDiv==TRUE){
      rows <- floor(barrier/(dS*(1-div))+1) # Wymiary po S
    }
    else{
      rows <- floor(barrier/dS +1)
    }
    M <- matrix(0, rows, cols) # Macierz
    if(percentageDiv==TRUE){
      S <- seq(barrier,0,-dS*(1-div)) # Zdefiniowanie S od bariery do zera
    }
    else{
      S <- seq(barrier-div,0-div,-dS) # Zdefiniowanie S od bariery do zera
      for(i in 1:length(S)){
        if(S[i]<0){
          S[i] <- 0
        }
      }
    }
    
    S2 <- S  # Dla zapami?tania jaki by? S po dywidendzie
    M[2:rows,cols] <- pmax((S-strike),0)[2:rows] # Wpisanie Payoff?w w ostatniej kolumnie
    
    #Macierze do pami?tania pod analizy
    DM <- matrix(0, rows, cols) #Delta
    DM[,cols] <- (as.numeric(M[,cols] > 0)) #Ostatnia kolumna Delty
    GM <- matrix(0, rows, cols) #Gamma
    if(american){
      AM <- matrix(0, rows, cols)
      AM[,cols] <- DM[,cols]
    }
    
    # For od ko?ca do dnia dywidendy
    for(k in cols:(div_day+1)){ #mo?e div_day
      
      #Liczenie Delty i Gammy dla wszystkich wierszy
      Delta <- (M[1:(rows-2),k]-M[3:rows,k])/(2*dS)
      Gamma <- (M[1:(rows-2),k]-2*M[2:(rows-1),k]+M[3:rows,k])/(dS)^2
      
      DM[2:(rows-1),k-1] <- Delta
      GM[2:(rows-1),k-1] <- Gamma
      
      #Liczymy ca?? kolumn? naraz bez najni?szej (Poziom zera) i najwy?szej (Bariera) warto?ci S
      
      #Rozr??nienie na worst case scenario sigma:
      #1. vt = vector warto?ci true, gdzie Gamma >= 0, czyli we?niemy najwi?ksz? sigm?
      vt <- (Gamma >= 0) 
      sigma <- sigma_vec[2]
      if(american){
        M[2:(rows-1),k-1][vt] <- pmax((S-strike)[2:(rows-1)][vt],M[2:(rows-1),k][vt]+dt*(0.5*sigma^2*S[2:(rows-1)][vt]^2*Gamma[vt]+r*S[2:(rows-1)][vt]*Delta[vt]-M[2:(rows-1),k][vt]*r))    
      } else{
        M[2:(rows-1),k-1][vt] <- M[2:(rows-1),k][vt]+dt*(0.5*sigma^2*S[2:(rows-1)][vt]^2*Gamma[vt]+r*S[2:(rows-1)][vt]*Delta[vt]-M[2:(rows-1),k][vt]*r)    
      }
      
      #2. vf = vector warto?ci true, gdzie Gamma < 0, czyli najmniejsza sigma
      vf <- (Gamma < 0)
      sigma <- sigma_vec[1]
      if(american){
        M[2:(rows-1),k-1][vf] <- pmax((S-strike)[2:(rows-1)][vf],M[2:(rows-1),k][vf]+dt*(0.5*sigma^2*S[2:(rows-1)][vf]^2*Gamma[vf]+r*S[2:(rows-1)][vf]*Delta[vf]-M[2:(rows-1),k][vf]*r))    
      } else{  
        M[2:(rows-1),k-1][vf] <- M[2:(rows-1),k][vf]+dt*(0.5*sigma^2*S[2:(rows-1)][vf]^2*Gamma[vf]+r*S[2:(rows-1)][vf]*Delta[vf]-M[2:(rows-1),k][vf]*r)    
      }
    }
    
    #Sprawdzamy, gdzie warto by?o wykona? ameryka?sk?
    if(american){
      AM[2:(rows-1),div_day:(cols-1)] <- as.numeric(M[2:(rows-1),div_day:(cols-1)] == (S[2:(rows-1)]-strike))
    }
    
    # Nast?puje dzie? dywidendy
    
    # Zmieniamy wi?c wektor S (mamy tylko procentowo, je?eli kwotowo to jaki? if i siema, ech nie takie siema)
    if(percentageDiv==TRUE){
      S <- seq(barrier*(1/(1-div)),0,-dS)
    }
    else{
      S <- seq(barrier,0,-dS)
    }
    
    # Teraz b?dzie od przeskalowanej bariery, dS'ami w d?? ile si? da
    
    # Dla zapami?tania jaki by? S przed dywidend?
    S1 <- S
    
    # Znajdujemy od kt?rego wiersza, wiersze przeskoczy?y nad barier?. NewTop = wiersz odgrywaj?cy teraz rol? bariery (pierwszy nad barier?)
    NewTop <- tail(which(S>=barrier),1)
    # Zerujemy wszystkie nad barier?
    M[1:NewTop,div_day] <- 0
    
    # For od dnia dywidendy do pocz?tku
    for(k in div_day:2){ #mo?e div_day+1
      Delta <- (M[NewTop:(rows-2),k]-M[(NewTop+2):rows,k])/(2*dS)
      Gamma <- (M[NewTop:(rows-2),k]-2*M[(NewTop+1):(rows-1),k]+M[(NewTop+2):rows,k])/(dS)^2
      
      #Zapamietanie dla analiz
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
  
  else{ # Opcja Put
    # Wiemy, ?e przed dywidend?, po lewej (je?li dS jest ma?e) pojawi? si? niezerowe wiersze.
    # add_rows liczy ile ich ma si? pojawi? + 1, aby doda? jeszcze pierwszy wiersz pod barier?
    if(percentageDiv==TRUE){
      add_rows <- floor((barrier/(1-div)-barrier)/dS)+1
      rows <- floor((upperlimit-barrier)/(dS*(1-div))) +1 + add_rows # Wymiary po S
    }
    else{
      add_rows <- floor(div/dS)+1
      rows <- floor((upperlimit-barrier)/dS) +1 + add_rows # Wymiary po S
    }
    M <- matrix(0, rows, cols) # Macierz 
    if(percentageDiv==TRUE){
      S <- seq(barrier,upperlimit,dS*(1-div)) # Zdefiniowanie S od bariery do limitu g?rnego (jest 6600, bo to trzykrotno?? ceny spot z 1.1.2020)
    }
    else{
      S <- seq(barrier,upperlimit,dS)
    }
    S <- rev(S)
    M[1:(rows-add_rows-1),cols] <- pmax((strike-S),0)[1:(rows-add_rows-1)] # Przypisanie Payoff?w do ostatniej kolumny
    
    # Dla zapami?tania jaki by? S po dywidendzie
    if(percentageDiv==TRUE){
      S2 <- c(S,seq(from=barrier-dS*(1-div),by=-dS*(1-div), length.out = add_rows))
    }
    else{
      S2 <- c(S,seq(from=barrier-dS,by=-dS, length.out = add_rows))
    }
    
    
    #Macierze do pamietania Delt i Gamm'm i r?cznie przypisanie Delt do ostatniej kolumny (Gammy tam s? zero i tak)
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
      S <- seq(barrier*(1/(1-div)),upperlimit*(1/(1-div)),dS) #je?eli kwotowo to jakis if i siema
    }
    else{
      S <- seq(barrier+div,upperlimit+div,dS) #je?eli kwotowo to jakis if i siema
    }
    S <- c(S[1:add_rows]-add_rows*dS, S)
    S <- rev(S)
    
    # Dla zapami?tania jaki by? S przed dywidend?
    S1 <- S
    
    for(k in div_day:2){ #mo?e div_day+1
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
  
  #Przygotowanie odpowiedzi
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
  #Nazwy Wierszy i Kolumn w odpowiedziach
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
  
  return(L)
}

