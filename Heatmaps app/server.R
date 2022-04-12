shinyServer(function(input,output){
  observeEvent(input$divtype,  {
    updateSliderInput(session = getDefaultReactiveDomain(),inputId = "div",max=(if (input$divtype=="PLN"){200}else{30}),value=(if (input$divtype=="PLN"){50}else{10}),step=(if (input$divtype=="PLN"){10}else{5}),label=(if (input$divtype=="PLN"){"Dividend (PLN):"}else{"Dividend yield (%):"}))
  })
  observeEvent(input$type,  {
    updateSliderInput(session = getDefaultReactiveDomain(),inputId = "barrier", value=(if (input$type=="put"){1900}else{2400}))
  })
  observeEvent(input$type,  {
    updateSliderInput(session = getDefaultReactiveDomain(), inputId = "limit", max=(if (input$type=="call"){input$barrier}),min=(if (input$type=="call"){0}),value=(if (input$type=="call"){0}))
  })
  observeEvent(input$type,  {
    updateSliderInput(session = getDefaultReactiveDomain(), inputId = "limit", min=(if (input$type=="put"){input$barrier}),max=(if (input$type=="put"){3500}),value=(if (input$type=="put"){3500}))
  })
  observeEvent(input$sigma_l,  {
    updateSliderInput(session = getDefaultReactiveDomain(), inputId = "sigma_p", min=input$sigma_l)
  })
  observeEvent(input$choices, {
    choice = input$choices
    if(choice == "Early exercises")
    {
      updateSliderInput(session = getDefaultReactiveDomain(), inputId = "continent", value="A")
    }
  })
  output$plot <- renderPlot({
    if (input$divtype=="PLN"){
      divtype=F
    }
    else{
      divtype=T
    }
    if (input$continent=="A"){
      american=T
    }
    else{
      american=F
    }
    if (input$type=="call"){
      call=T
      dt=1/36000
    }
    else{
      call=F
      dt=1/84000
    }
    A <- Finite_Difference(call=call,strike=input$strike,Time=input$Time,barrier=input$barrier,r=input$r, div=input$div/(if (divtype){100}else{1}),sigma=c(input$sigma_l,input$sigma_p),american=american,dt=dt,percentageDiv=divtype)
    melted_A_before =melt(as.matrix(A$Krata_Przed),varnames = c("S","t"))
    melted_A_before=melted_A_before[(if (call){melted_A_before$S<input$barrier & melted_A_before$S>input$limit}else{melted_A_before$S<input$limit & melted_A_before$S>input$barrier}),]
    melted_A_after =melt(as.matrix(A$Krata_Po),varnames = c("S","t"))
    melted_A_after=melted_A_after[(if (call){melted_A_after$S<input$barrier & melted_A_after$S>input$limit}else{melted_A_after$S<input$limit & melted_A_after$S>input$barrier}),]
    melted_A_before=melted_A_before[melted_A_before$t %in% unique(melted_A_before$t)[seq(1,length(unique(melted_A_before$t)),if (call){18}else{84})],]
    melted_A_after=melted_A_after[melted_A_after$t %in% unique(melted_A_after$t)[seq(1,length(unique(melted_A_after$t)),if (call){18}else{84})],]
    melted_A=rbind(melted_A_after,melted_A_before)
    line=data.frame(x=c(0,input$Time),y=c(input$strike,input$strike))
    ggplot(melted_A,aes(x=t,y=S))+
      geom_tile(data=melted_A_before,aes(x=t,y=S,fill=value))+
      geom_tile(data=melted_A_after,aes(x=t,y=S,fill=value))+
      theme_light()+
      geom_line(data=line,aes(x=x,y=y),linetype="dotted", color="red", size=1)+
      labs(x="Time (years)",y="Underlying price (PLN)",fill="Option price (PLN)")+
      scale_x_continuous(breaks=seq(0,input$Time,0.25))+
      scale_y_continuous(breaks=if (call){seq(input$limit,input$barrier,(input$barrier-input$limit)/10)}else{seq(input$barrier,input$limit,(input$limit-input$barrier)/10)})+
      theme(plot.title=element_text(hjust=0.5,size=20),text = element_text(size=20))+
      scale_fill_viridis_c(breaks=round(seq(0,max(melted_A_after$value,melted_A_after$value),max(melted_A_after$value,melted_A_after$value)/5),2),option = "inferno")+
      ggtitle(paste("Values of",if (american){"american"}else{"european"}," knock-and-out ",if (call){"call"}else{"put"}," with K=",sprintf("%.2f",input$strike),"PLN,B=",sprintf("%.2f",input$barrier),"PLN and d=",input$div,(if (divtype){"%"}else{"PLN"})))
  })
  output$plot2 <- renderPlot({
    if (input$divtype=="PLN"){
      divtype=F
    }
    else{
      divtype=T
    }
    if (input$continent=="A"){
      american=T
    }
    else{
      american=F
    }
    if (input$type=="call"){
      call=T
      dt=1/36000
    }
    else{
      call=F
      dt=1/84000
    }
    A <- Finite_Difference(call=call,strike=input$strike,Time=input$Time,barrier=input$barrier,r=input$r, div=input$div/(if (divtype){100}else{1}),sigma=c(input$sigma_l,input$sigma_p),american=american,dt=dt,percentageDiv=divtype)
    melted_A_before =melt(as.matrix(A$Delta_Przed),varnames = c("S","t"))
    melted_A_before=melted_A_before[(if (call){melted_A_before$S<input$barrier & melted_A_before$S>input$limit}else{melted_A_before$S<input$limit & melted_A_before$S>input$barrier}),]
    melted_A_after =melt(as.matrix(A$Delta_Po),varnames = c("S","t"))
    melted_A_after=melted_A_after[(if (call){melted_A_after$S<input$barrier & melted_A_after$S>input$limit}else{melted_A_after$S<input$limit & melted_A_after$S>input$barrier}),]
    melted_A_before=melted_A_before[melted_A_before$t %in% unique(melted_A_before$t)[seq(1,length(unique(melted_A_before$t)),if (call){18}else{84})],]
    melted_A_after=melted_A_after[melted_A_after$t %in% unique(melted_A_after$t)[seq(1,length(unique(melted_A_after$t)),if (call){18}else{84})],]
    melted_A=rbind(melted_A_after,melted_A_before)
    line=data.frame(x=c(0,input$Time),y=c(input$strike,input$strike))
    ggplot(melted_A,aes(x=t,y=S))+
      geom_tile(data=melted_A_before,aes(x=t,y=S,fill=value))+
      geom_tile(data=melted_A_after,aes(x=t,y=S,fill=value))+
      theme_light()+
      geom_line(data=line,aes(x=x,y=y),linetype="dotted", color="red", size=1)+
      labs(x="Time (years)",y="Underlying price (PLN)",fill="Delta")+
      scale_x_continuous(breaks=seq(0,input$Time,0.25))+
      scale_y_continuous(breaks=if (call){seq(input$limit,input$barrier,(input$barrier-input$limit)/10)}else{seq(input$barrier,input$limit,(input$limit-input$barrier)/10)})+
      theme(plot.title=element_text(hjust=0.5,size=20),text = element_text(size=20),legend.text=element_text(size=9))+
      scale_fill_viridis_c(breaks=c(round(seq(min(melted_A_after$value,melted_A_after$value),max(melted_A_after$value,melted_A_after$value),(max(melted_A_after$value,melted_A_after$value)-min(melted_A_after$value,melted_A_after$value))/5),2)))+
      ggtitle(paste("Delta of ",if (american){"american"}else{"european"}," knock-and-out ",if (call){"call"}else{"put"}," with K=",sprintf("%.2f",input$strike),"PLN,B=",sprintf("%.2f",input$barrier),"PLN and d=",input$div,(if (divtype){"%"}else{"PLN"})))
  })
  output$plot3 <- renderPlot({
    if (input$divtype=="PLN"){
      divtype=F
    }
    else{
      divtype=T
    }
    if (input$continent=="A"){
      american=T
    }
    else{
      american=F
    }
    if (input$type=="call"){
      call=T
      dt=1/36000
    }
    else{
      call=F
      dt=1/84000
    }
    A <- Finite_Difference(call=call,strike=input$strike,Time=input$Time,barrier=input$barrier,r=input$r, div=input$div/(if (divtype){100}else{1}),sigma=c(input$sigma_l,input$sigma_p),american=american,dt=dt,percentageDiv=divtype)
    melted_A_before =melt(as.matrix(A$Gamma_Przed),varnames = c("S","t"))
    melted_A_before=melted_A_before[(if (call){melted_A_before$S<input$barrier & melted_A_before$S>input$limit}else{melted_A_before$S<input$limit & melted_A_before$S>input$barrier}),]
    melted_A_after =melt(as.matrix(A$Gamma_Po),varnames = c("S","t"))
    melted_A_after=melted_A_after[(if (call){melted_A_after$S<input$barrier & melted_A_after$S>input$limit}else{melted_A_after$S<input$limit & melted_A_after$S>input$barrier}),]
    melted_A_before=melted_A_before[melted_A_before$t %in% unique(melted_A_before$t)[seq(1,length(unique(melted_A_before$t)),if (call){18}else{84})],]
    melted_A_after=melted_A_after[melted_A_after$t %in% unique(melted_A_after$t)[seq(1,length(unique(melted_A_after$t)),if (call){18}else{84})],]
    melted_A_before$value=input$sigma_p*(melted_A_before$value>=0)+input$sigma_l*(melted_A_before$value<0)
    melted_A_after$value=input$sigma_p*(melted_A_after$value>=0)+input$sigma_l*(melted_A_after$value<0)
    melted_A=rbind(melted_A_after,melted_A_before)
    line=data.frame(x=c(0,input$Time),y=c(input$strike,input$strike))
    ggplot(melted_A,aes(x=t,y=S))+
      geom_tile(data=melted_A_before,aes(x=t,y=S,fill=factor(value)))+
      geom_tile(data=melted_A_after,aes(x=t,y=S,fill=factor(value)))+
      theme_light()+
      geom_line(data=line,aes(x=x,y=y),linetype="dotted", color="red", size=1)+
      labs(x="Time (years)",y="Underlying price (PLN)",fill="Assumed volatility")+
      scale_x_continuous(breaks=seq(0,input$Time,0.25))+
      scale_y_continuous(breaks=if (call){seq(input$limit,input$barrier,(input$barrier-input$limit)/10)}else{seq(input$barrier,input$limit,(input$limit-input$barrier)/10)})+
      theme(plot.title=element_text(hjust=0.5,size=15),text = element_text(size=20),legend.text=element_text(size=12))+
      ggtitle(paste("Worst Case Scenario volatility of ",if (american){"american"}else{"european"}," knock-and-out ",if (call){"call"}else{"put"}," with K=",sprintf("%.2f",input$strike),"PLN,B=",sprintf("%.2f",input$barrier),"PLN and d=",input$div,(if (divtype){"%"}else{"PLN"})))
  })
  output$plot4 <- renderPlot({
    if (input$divtype=="PLN"){
      divtype=F
    }
    else{
      divtype=T
    }
    if (input$continent=="A"){
      american=T
    }
    else{
      american=F
    }
    if (input$type=="call"){
      call=T
      dt=1/36000
    }
    else{
      call=F
      dt=1/84000
    }
    A <- Finite_Difference(call=call,strike=input$strike,Time=input$Time,barrier=input$barrier,r=input$r, div=input$div/(if (divtype){100}else{1}),sigma=c(input$sigma_l,input$sigma_p),american=american,dt=dt,percentageDiv=divtype)
    melted_A_before =melt(as.matrix(A$American_Wykonanie_Przed),varnames = c("S","t"))
    melted_A_before=melted_A_before[(if (call){melted_A_before$S<input$barrier & melted_A_before$S>input$limit}else{melted_A_before$S<input$limit & melted_A_before$S>input$barrier}),]
    melted_A_after =melt(as.matrix(A$American_Wykonanie_Po),varnames = c("S","t"))
    melted_A_after=melted_A_after[(if (call){melted_A_after$S<input$barrier & melted_A_after$S>input$limit}else{melted_A_after$S<input$limit & melted_A_after$S>input$barrier}),]
    melted_A_before=melted_A_before[melted_A_before$t %in% unique(melted_A_before$t)[seq(1,length(unique(melted_A_before$t)),if (call){18}else{84})],]
    melted_A_after=melted_A_after[melted_A_after$t %in% unique(melted_A_after$t)[seq(1,length(unique(melted_A_after$t)),if (call){18}else{84})],]
    melted_A_before$value=as.logical(melted_A_before$value)
    melted_A_after$value=as.logical(melted_A_after$value)
    melted_A=rbind(melted_A_after,melted_A_before)
    line=data.frame(x=c(0,input$Time),y=c(input$strike,input$strike))
    ggplot(melted_A,aes(x=t,y=S))+
      geom_tile(data=melted_A_before,aes(x=t,y=S,fill=factor(value)))+
      geom_tile(data=melted_A_after,aes(x=t,y=S,fill=factor(value)))+
      theme_light()+
      geom_line(data=line,aes(x=x,y=y),linetype="dotted", color="red", size=1)+
      labs(x="Time (years)",y="Underlying price (PLN)",fill="Exercise early?")+
      scale_x_continuous(breaks=seq(0,input$Time,0.25))+
      scale_y_continuous(breaks=if (call){seq(input$limit,input$barrier,(input$barrier-input$limit)/10)}else{seq(input$barrier,input$limit,(input$limit-input$barrier)/10)})+
      scale_fill_manual(values  =c("#1f08b0", "#ff9400"))+
      theme(plot.title=element_text(hjust=0.5,size=20),text = element_text(size=20),legend.text=element_text(size=9))+
      ggtitle(paste("Possible early exercises of ",if (american){"american"}else{"european"}," knock-and-out ",if (call){"call"}else{"put"}," with K=",sprintf("%.2f",input$strike),"PLN,B=",sprintf("%.2f",input$barrier),"PLN and d=",input$div,(if (divtype){"%"}else{"PLN"})))
  })
})

