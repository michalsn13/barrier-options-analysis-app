shinyServer(function(input,output){
  observeEvent(input$divtype,  {
    updateSliderInput(session = getDefaultReactiveDomain(),inputId = "div",max=(if (input$divtype=="zł"){200}else{30}),value=(if (input$divtype=="zł"){50}else{10}),step=(if (input$divtype=="zł"){10}else{5}),label=(if (input$divtype=="zł"){"Dywidenda (zł):"}else{"Dywidenda (%):"}))
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
laduj<-reactive({
    
    return(melted_A)
  })
  output$plot <- renderPlot({
    if (input$divtype=="zł"){
      divtype=F
    }
    else{
      divtype=T
    }
    if (input$type=="call"){
      call=T
      dt=1/9000
    }
    else{
      call=F
      dt=1/42000
    }
    if (input$freeze=="Go"){
      A <- Finite_Difference(call=call,strike=input$strike,Time=input$Time,barrier=input$barrier,r=input$r, div=input$div/(if (divtype){100}else{1}),sigma=c(input$sigma_l,input$sigma_p),american=F,dt=dt,percentageDiv=divtype)
      melted_A_before =melt(as.matrix(A$Krata_Przed),varnames = c("S","t"))
      melted_A_before=melted_A_before[(if (call){melted_A_before$S<input$barrier & melted_A_before$S>input$limit}else{melted_A_before$S<input$limit & melted_A_before$S>input$barrier}),]
      melted_A_after =melt(as.matrix(A$Krata_Po),varnames = c("S","t"))
      melted_A_after=melted_A_after[(if (call){melted_A_after$S<input$barrier & melted_A_after$S>input$limit}else{melted_A_after$S<input$limit & melted_A_after$S>input$barrier}),]
      melted_A_before=melted_A_before[melted_A_before$t %in% unique(melted_A_before$t)[seq(1,length(unique(melted_A_before$t)),if (call){18}else{84})],]
      melted_A_after=melted_A_after[melted_A_after$t %in% unique(melted_A_after$t)[seq(1,length(unique(melted_A_after$t)),if (call){18}else{84})],]
      melted_A_e=rbind(melted_A_after,melted_A_before)
      rm(A,melted_A_before,melted_A_after)
      melted_A_e$type=paste('europejski ',(if (call){"call"}else{"put"}))
      A <- Finite_Difference(call=call,strike=input$strike,Time=input$Time,barrier=input$barrier,r=input$r, div=input$div/(if (divtype){100}else{1}),sigma=c(input$sigma_l,input$sigma_p),american=T,dt=dt,percentageDiv=divtype)
      melted_A_before =melt(as.matrix(A$Krata_Przed),varnames = c("S","t"))
      melted_A_before=melted_A_before[(if (call){melted_A_before$S<input$barrier & melted_A_before$S>input$limit}else{melted_A_before$S<input$limit & melted_A_before$S>input$barrier}),]
      melted_A_after =melt(as.matrix(A$Krata_Po),varnames = c("S","t"))
      melted_A_after=melted_A_after[(if (call){melted_A_after$S<input$barrier & melted_A_after$S>input$limit}else{melted_A_after$S<input$limit & melted_A_after$S>input$barrier}),]
      melted_A_before=melted_A_before[melted_A_before$t %in% unique(melted_A_before$t)[seq(1,length(unique(melted_A_before$t)),if (call){18}else{84})],]
      melted_A_after=melted_A_after[melted_A_after$t %in% unique(melted_A_after$t)[seq(1,length(unique(melted_A_after$t)),if (call){18}else{84})],]
      melted_A_a=rbind(melted_A_after,melted_A_before)
      rm(A,melted_A_before,melted_A_after)
      melted_A_a$type=paste('amerykański ',(if (call){"call"}else{"put"}))
      melted_A<<-do.call("rbind", list(melted_A_e,melted_A_a))
    }
      maks=ceiling(max(melted_A$value) / 10) * 10
      moment=unique(melted_A$t)[which.min(abs(unique(melted_A$t)-input$moment))]
      melted_A=melted_A[melted_A$t==moment,]
      ggplot(data=melted_A,aes(x=S,y=value,color=type))+
        geom_line(size=1.2)+
        geom_vline(xintercept =input$strike,linetype='dotted',size=1.1,color="red")+
        theme_light()+
        scale_x_continuous(breaks=if (call){seq(input$limit,input$barrier,(input$barrier-input$limit)/10)}else{seq(input$barrier,input$limit,(input$limit-input$barrier)/10)},expand = expansion(add = c(0,10)))+
        scale_y_continuous(breaks=round(seq(0,maks,maks/5),2),expand = expansion(add = c(10,10)))+
        labs(x="Wartość aktywa bazowego (zł)",y="Wartość opcji (zł)",color="Rodzaj opcji:")+
        scale_color_manual(values  =c("#1f08b0", "#ff9400"))+
        theme(plot.title=element_text(hjust=0.5,size=20),text = element_text(size=15),legend.text=element_text(size=12))+
        ggtitle(paste("Wartości knock-and-out ",if (call){"call"}else{"put"}," o K=",sprintf("%.2f",input$strike),"zł,B=",sprintf("%.2f",input$barrier),"zł i d=",input$div,(if (divtype){"%"}else{"zł"})," w momencie t=",round(moment,3)))
    })
})

