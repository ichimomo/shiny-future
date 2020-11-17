source("shiny/app.R")
a <- load("shiny/pms_files.rda")

Fmsy <- round(1/as.numeric(as.character(MSY_res$summary$Fref2Fcurrent[1])),2)
#        input.tmp <- future.default$input
input.tmp$N <- 1000 # ceiling(input$simu.number)+3
input.tmp$det.run <- FALSE
input.tmp$nyear <- 30
input.tmp$is.plot <- FALSE

input <- list()
input$beta1 <- 0.8
input$beta2 <- 1.3
input$is.HCR1 <- TRUE
input$is.HCR2 <- TRUE
input$simu.number <- 1
input$simu.number <- 1
input$is.CI <- TRUE

input.tmp1 <- input.tmp
input.tmp1$HCR$beta <- input$beta1
input.tmp1$HCR$Blim <- refs.plot$SSB[2]
input.tmp1$HCR$Bban <- refs.plot$SSB[3]
refs.plot <- dplyr::filter(MSY_res$summary_tb,RP.definition%in%c("Btarget0","Blimit0","Bban0"))        

input.tmp2 <- input.tmp
input.tmp2$HCR$beta <- input$beta2
if(!isTRUE(input$is.HCR2)) input.tmp2$HCR$Blim <- -Inf
input.tmp2$HCR$Bban <- 0
input.tmp2$HCR$Blim <- 0
        
future1 <- do.call(future.vpa,input.tmp1)
future2 <- do.call(future.vpa,input.tmp2)

theme_SH2 <- function(){
    theme_bw(base_size=24) +
    theme(panel.grid = element_blank(),
          axis.text.x=element_text(size=22,color="black"),
          axis.text.y=element_text(size=22,color="black"),
          axis.line.x=element_line(size= 0.8),
          axis.line.y=element_line(size= 0.8),
          legend.position="none")
}

g3_future <- plot_futures(future1$input$res0,list(future1,future2),
                          future.name=c(str_c("(1) beta=",input$beta1),
                                        str_c("(2) beta=",input$beta2)),
                          biomass.unit=1000,n_example=0,
                          future.replicate=input$simu.number,
                          is.plot.CIrange=input$is.CI,
                          exclude.japanese.font=TRUE,                                      
                          Btarget=refs.plot$SSB[1],
                          Blimit=refs.plot$SSB[2],
                          Bban=refs.plot$SSB[3],
                          MSY=refs.plot$Catch[1],
                          what.plot = c("SSB", "catch"),ncol=1)+
    coord_cartesian(xlim=c(2000,2040)) + 
    scale_color_manual(values = c("#f8766D","#00bfc4", "black")) +
    theme_SH2() 


ggsave(plot=g3_future, filename="g3_future.png", dpi=1000)

