#=================================================================
                                             #Rafael Tieppo
                                             #tiepporc@unemat.br
                                             #02-12-2014
#=================================================================



#Week 1- Developing Data Products
#SERVER FILE

#Getting stared
#install.packages("shiny")
#install.packages("UsingR")

library(shiny)
#library(UsingR)
library(ggplot2)
library(ggthemes)
#data(galton)



shinyServer(
 function(input, output) {
 #   output$newHist <- renderPlot({
 #     hist(galton$child, xlab='child height', col='lightblue',main='Histogram')
 #     mu <- input$mu
 #     lines(c(mu, mu), c(0, 200),col="red",lwd=5)
 #     mse <- mean((galton$child - mu)^2)
 #     text(63, 150, paste("mu = ", mu))
 #     text(63, 140, paste("MSE = ", round(mse, 2)))
 #     })

 
IndexT_HARVEST <-   (function (PRIC,SAVAGPRICE, ULMAXY, ULHOPEY, IT, TF,
                            CCO, TIME, FC, FUELPRIC, AREATOWORK,
                            SALARYMONTH, TIMELABORMONTH, OUT_SOURC_COST)
{ #begin Function

#DEPRECIATION CALC    
#if def
    
############Creating MAtrix#########################
  AREATOT <- 10000


################################################
#######Depreciation Matrix######################

  DEPRECMATRIX <- matrix(1, ULHOPEY+1,3)
  DEPRECMATRIX [1,1] <- (0)
  DEPRECMATRIX [1,2] <- (0)
  DEPRECMATRIX [1,3] <- (PRIC)

  TAXDEPRECDB <- 1-(SAVAGPRICE/PRIC)^(1/ULMAXY)
  # CVDBY= contabil value decl balance year
  CVDBY <- PRIC

  for (ydb in 2:(ULHOPEY+1))
  {
    DEPRECMATRIX [ydb,1] <- (ydb-1)
    DEPRCDBY <- (CVDBY*TAXDEPRECDB)
    DEPRECMATRIX [ydb,2] <- DEPRCDBY
    CVDBY <- CVDBY - DEPRCDBY
    DEPRECMATRIX [ydb,3] <- CVDBY
  }

  Depreciat <- data.frame(Year=c(DEPRECMATRIX[,1]), Deprec=c(DEPRECMATRIX[,2]), Contab_Value=c(DEPRECMATRIX[,3]))
  DEPRECDBPERIOD <- sum(Depreciat$Deprec)

#endif
    

#COST TO PLOT GRAPH
#if def

#################################################################
#####Calculating Costs TO PLOT THE GRAPHICS #####################

#CRETING MATRIX
  DIM_1_MATRIX <- matrix(0,2*AREATOT,22)

#MACHINE
  FCITAXY <-((PRIC+SAVAGPRICE)/2)*(IT/100) # Custo anual com juros
  FCTAXY <- PRIC * ((TF/100)) #Custo anual com outras taxas


  NC <- 1
  AREAA <- 0

  for (ar in 1:(2*AREATOT))
  {

   AREAA <- AREAA + 0.5
   DIM_1_MATRIX[ar,2] <- AREAA
   AREAMAXMACH <- TIME * CCO

   if (AREAA < AREAMAXMACH*NC)
       {
        #---Machine

        DIM_1_MATRIX[ar,1] <- NC
        DEPREC_MACH_Y<-  ((DEPRECDBPERIOD)/(ULHOPEY)) * NC  #Depreciation Machine anual cost for NC machine
        DIM_1_MATRIX[ar,3] <- DEPREC_MACH_Y

        FIX_COST_INT_MACH_Y <-  FCITAXY * NC #Interest Cost Machine anual cost for NC machine
        DIM_1_MATRIX[ar,4] <- FIX_COST_INT_MACH_Y

        FIX_COST_TAX_MACH_Y <- FCTAXY * NC #Insurance Home anual Cost for NC machine
        DIM_1_MATRIX[ar,5] <- FIX_COST_TAX_MACH_Y

        FIX_COST_TOT_MACH_Y <- DEPREC_MACH_Y + FIX_COST_INT_MACH_Y + FIX_COST_TAX_MACH_Y # Fix cost total mach for NC machine
        DIM_1_MATRIX[ar,6] <- FIX_COST_TOT_MACH_Y

        FIX_COST_TOT_MACH_H <- (FIX_COST_TOT_MACH_Y) /  (AREAA/(CCO*NC)) #Total cost machi hour for NC machine
        DIM_1_MATRIX[ar,7] <- FIX_COST_TOT_MACH_H

        RM_MACH <-  (((0.05)*PRIC*0.5 * (ULHOPEY*(AREAA/(CCO*NC))/1000)^2.1)/(ULHOPEY*(AREAA/(CCO*NC)))) #repair maint cost mach for ONE mach
        DIM_1_MATRIX[ar,8] <-  RM_MACH

        #RM_MACH <-  (((0.007)*PRIC * (ULHOPEY*(TIME*CCO/(CCO))/1000)^2)/(ULHOPEY*TIME*CCO/(CCO))) #repair maint cost mach for ONE mach
        #DIM_1_MATRIX[ar,8] <-  RM_MACH

        FUEL_COST_H <-  FC * FUELPRIC # fuel cost hour for ONE mach
        DIM_1_MATRIX[ar,9] <- FUEL_COST_H

        LABOR_COST_H <-  ((SALARYMONTH*(1+0)) / TIMELABORMONTH) # labor cost H for ONE mach
        DIM_1_MATRIX[ar,10] <-  LABOR_COST_H

        TOT_COST_MACH_H <- FIX_COST_TOT_MACH_H + NC* (RM_MACH + FUEL_COST_H + LABOR_COST_H) #Total cost Mach Hour for NC machine
        DIM_1_MATRIX[ar,11] <-  TOT_COST_MACH_H


        #--- Set

        FIX_COST_SET_H <- ( FIX_COST_TOT_MACH_H + 0)
        DIM_1_MATRIX[ar,19] <- FIX_COST_SET_H

        VAR_COST_SET_H <- NC * (RM_MACH + FUEL_COST_H + LABOR_COST_H) + NC *(0)
        DIM_1_MATRIX[ar,20] <-  VAR_COST_SET_H

        TOT_COST_SET_H <-  FIX_COST_SET_H + VAR_COST_SET_H
        DIM_1_MATRIX[ar,21] <-  TOT_COST_SET_H

        TOT_COST_SET_OPER <-  TOT_COST_SET_H / (NC*CCO)
        DIM_1_MATRIX[ar,22] <- TOT_COST_SET_OPER
        }
    else
        {
        NC <- NC + 1

        #---Machine

        DIM_1_MATRIX[ar,1] <- NC


        DEPREC_MACH_Y<-  ((DEPRECDBPERIOD)/(ULHOPEY)) * NC  #Depreciation Machine anual cost for NC machine
        DIM_1_MATRIX[ar,3] <- DEPREC_MACH_Y

        FIX_COST_INT_MACH_Y <-  FCITAXY * NC #Interest Cost Machine anual cost for NC machine
        DIM_1_MATRIX[ar,4] <- FIX_COST_INT_MACH_Y

        FIX_COST_TAX_MACH_Y <- FCTAXY * NC #Insurance Home anual Cost for NC machine
        DIM_1_MATRIX[ar,5] <- FIX_COST_TAX_MACH_Y

        FIX_COST_TOT_MACH_Y <- DEPREC_MACH_Y + FIX_COST_INT_MACH_Y + FIX_COST_TAX_MACH_Y # Fix cost total mach for NC machine
        DIM_1_MATRIX[ar,6] <- FIX_COST_TOT_MACH_Y

        FIX_COST_TOT_MACH_H <- (FIX_COST_TOT_MACH_Y) /  (AREAA/(CCO*NC)) #Total cost machi hour for NC machine
        DIM_1_MATRIX[ar,7] <- FIX_COST_TOT_MACH_H

        RM_MACH <-  (((0.05)*PRIC*0.5 * (ULHOPEY*(AREAA/(CCO*NC))/1000)^2.1)/(ULHOPEY*(AREAA/(CCO*NC)))) #repair maint cost mach for ONE mach
        DIM_1_MATRIX[ar,8] <-  RM_MACH

        #RM_MACH <-  (((0.007)*PRIC * (ULHOPEY*(TIME*CCO/(CCO))/1000)^2)/(ULHOPEY*TIME*CCO/(CCO))) #repair maint cost mach for ONE mach
        #DIM_1_MATRIX[ar,8] <-  RM_MACH


        FUEL_COST_H <-  FC * FUELPRIC # fuel cost hour for ONE mach
        DIM_1_MATRIX[ar,9] <- FUEL_COST_H

        LABOR_COST_H <-  ((SALARYMONTH*(1+0)) / TIMELABORMONTH) # labor cost H for ONE mach
        DIM_1_MATRIX[ar,10] <-  LABOR_COST_H

        TOT_COST_MACH_H <- FIX_COST_TOT_MACH_H + NC*(RM_MACH + FUEL_COST_H + LABOR_COST_H) #Total cost Mach Hour for NC machine
        DIM_1_MATRIX[ar,11] <-  TOT_COST_MACH_H

        #--- Set

        FIX_COST_SET_H <- ( FIX_COST_TOT_MACH_H + 0)
        DIM_1_MATRIX[ar,19] <- FIX_COST_SET_H

        VAR_COST_SET_H <- NC * (RM_MACH + FUEL_COST_H + LABOR_COST_H) + NC *(0)
        DIM_1_MATRIX[ar,20] <-  VAR_COST_SET_H

        TOT_COST_SET_H <-  FIX_COST_SET_H + VAR_COST_SET_H
        DIM_1_MATRIX[ar,21] <-  TOT_COST_SET_H

        TOT_COST_SET_OPER <-  TOT_COST_SET_H / (NC*CCO)
        DIM_1_MATRIX[ar,22] <- TOT_COST_SET_OPER
       }
  }
#endif


#DATA FRAME WITH COSTS
#if def

###Data Frame with Costs
  DATAS <- data.frame(AREA = DIM_1_MATRIX[,2], NC=DIM_1_MATRIX[,1], MACH_DEPR_H=DIM_1_MATRIX[,3], MACH_TOT_FIX_H=DIM_1_MATRIX[,7], MACH_RM=DIM_1_MATRIX[,8], MACH_FUEL=DIM_1_MATRIX[,9], MACH_TOT_H=DIM_1_MATRIX[,11], IMPL_DEPR_Y = DIM_1_MATRIX[,12], IMPL_I_Y=DIM_1_MATRIX[,13], IMPL_TAX_Y=DIM_1_MATRIX[,14], IMPL_RM=DIM_1_MATRIX[,17], IMPL_TOT_H=DIM_1_MATRIX[ar,18], SET_TOT_FIX_H=DIM_1_MATRIX[ar,19], SET_TOT_VAR_H=DIM_1_MATRIX[,20], SET_TOT_H=DIM_1_MATRIX[,21], SET_TOT_OPER=DIM_1_MATRIX[,22])

#endif


#EFFICIENCE INDEX
#if def

###################################################################################
#####Efficiency Index###############

  NCNEED <- (AREATOWORK/TIME)/CCO #Number necssaty sets
  NCNEEDARRED <- ceiling(NCNEED) #round to up NC

  AREAWORKED <- NCNEED * TIME * CCO

  RM_MACH_NCNEED <-  (((0.05)*PRIC*0.5 * (ULHOPEY*(AREATOWORK/(CCO*NCNEEDARRED))/1000)^2.1)/(ULHOPEY*(AREATOWORK/(CCO*NCNEEDARRED)))) #RM for NCNEED


  OPERCOSTNCNEED <- (((DEPRECDBPERIOD/ULHOPEY + FCITAXY + FCTAXY) + (0)) / (AREATOWORK/(CCO*NCNEEDARRED)) + (RM_MACH_NCNEED+FUEL_COST_H+LABOR_COST_H+0))/CCO

  FILTERNCNEEDARRED <- subset(DATAS, NC == NCNEEDARRED) #Filtrando intervalo com numero de conj necessários arred para cima
  FILTERNCNEEDARRED_ <- subset(DATAS, NC == NCNEEDARRED & AREA == AREATOWORK) #Filtrando intervalo com numero de conj necessários arred para cima

  DIM_1_FRAME <- data.frame(NC=FILTERNCNEEDARRED_$NC, MACH_CFT_H=FILTERNCNEEDARRED_$MACH_TOT_FIX_H/NCNEEDARRED, MACH_RM_H=FILTERNCNEEDARRED_$MACH_RM, MACH_FUEL=FILTERNCNEEDARRED_$MACH_FUEL, MACH_TOT_H=FILTERNCNEEDARRED_$MACH_TOT_H/NCNEEDARRED,   CONJ_TOT_H=FILTERNCNEEDARRED_$SET_TOT_H/NCNEEDARRED, CONJ_TOT_OPER=FILTERNCNEEDARRED_$SET_TOT_OPER,  CONJ_TIME= AREATOWORK/(NCNEEDARRED*CCO))

  # IMPL_IIT_H=(FILTERNCNEEDARRED_$IMPL_I_Y/NCNEEDARRED + FILTERNCNEEDARRED_$IMPL_TAX_Y/NCNEEDARRED)/(AREATOWORK/(NCNEEDARRED*CCO)),

  MINAREANCNEEDARRED <- min(FILTERNCNEEDARRED$AREA)
  MAXCOSTNCNEEDARRED <- max(FILTERNCNEEDARRED$SET_TOT_OPER)

  MAXAREANCNEEDARRED <- max(FILTERNCNEEDARRED$AREA)
  MINCOSTNCNEEDARRED <- min(FILTERNCNEEDARRED$SET_TOT_OPER)

  INDEXT <- (MAXCOSTNCNEEDARRED-OPERCOSTNCNEED)/(MAXCOSTNCNEEDARRED-MINCOSTNCNEEDARRED)



###############################################################  

#endif

#DATA FRAME TO PLOT
#if def

INDEXDATAFRAME <- data.frame(DESCRIP=c("Predict Cost", "Outsourcing"),XCOORD=c(AREATOWORK,  AREATOWORK), YCOORD=c(OPERCOSTNCNEED, OUT_SOURC_COST))
 
#  xIndex<-c(INDEXDATAFRAME$XCOORD)
#  yIndex<-c(INDEXDATAFRAME$YCOORD)

# tatu <- data.frame(x=c(MINAREANCNEEDARRED,FILTERNCNEEDARRED$AREA,MAXAREANCNEEDARRED), y=c(MINCOSTNCNEEDARRED,FILTERNCNEEDARRED$SET_TOT_OPER,MINCOSTNCNEEDARRED))


###############################################################
#####DATA FRAME FOR INDEX #############################

#INDEXT_FRAME <-  data.frame( DIM=c("Dim 1", "Dim 2", "Dim 3","Terceirizado" ), CT_OPER = c(OPERCOSTNCNEED, TOTAL_COST_DIM_2_OPER, TOTAL_COST_DIM_3_OPER, OUT_SOURC_COST), INDEXT=c(INDEXT, INDEXT_DIM_2, INDEXT_DIM_3, INDEXT_DIM_4))

#endif

##################################################################
#####Outout data #################################################

  return(list(DATAS, MINCOSTNCNEEDARRED, INDEXDATAFRAME))

     
}) #end Function

     

output$Plot_Cost <- renderPlot({

     PRIC           <- input$PP_
     SAVAGPRICE     <- PRIC * 0.35
     ULMAXY         <- 10  
     ULHOPEY        <- 10
     IT             <- 4.5/100 
     TF             <- 2.5/100
     SPEED          <- input$SPEED_
     WIDTH          <- input$WIDTH_
     CCO            <- (SPEED * WIDTH /10)*0.7  
     TIME           <- input$TIME_
     FC             <- input$POWER_ * 0.16
     FUELPRIC       <- input$FP_
     AREATOWORK     <- input$AREA_
     SALARYMONTH    <- 1
     TIMELABORMONTH <- 100
     OUT_SOURC_COST <- input$OUTSOURC_


    INDEX_CASE <- IndexT_HARVEST(PRIC,SAVAGPRICE, ULMAXY, ULHOPEY, IT, TF, CCO, TIME, FC, FUELPRIC, AREATOWORK, SALARYMONTH, TIMELABORMONTH, OUT_SOURC_COST)
    
    mydata <- data.frame (AREA = (INDEX_CASE[[1]]$AREA), SET_TOT_OPER = (INDEX_CASE[[1]]$SET_TOT_OPER) )
    LIM    <- (INDEX_CASE[[2]]) 

    mypoints <- data.frame(DESCRIP = INDEX_CASE[[3]]$DESCRIP,
                           XCOORD  = INDEX_CASE[[3]]$XCOORD,
                           YCOORD  = INDEX_CASE[[3]]$YCOORD)
                                 
     
    #xIndex<-c(INDEXDATAFRAME$XCOORD)
    #yIndex<-c(INDEXDATAFRAME$YCOORD)
    #tatu <- data.frame(x=c(MINAREANCNEEDARRED,FILTERNCNEEDARRED$AREA,MAXAREANCNEEDARRED), y=c(MINCOSTNCNEEDARRED,FILTERNCNEEDARRED$SET_TOT_OPER,MINCOSTNCNEEDARRED))

     
    OPERCOSTPLOT <- ggplot() +
      geom_line (data=mydata, aes(x=AREA, y=SET_TOT_OPER)) + theme_bw() +
          theme(text = element_text(size=14))+
          theme(legend.position = "left", legend.text = (element_text(size = 14))) +
          xlab("Area (ha)") + ylab(expression(paste("Operational Cost"~("$"~~ha^-1)))) +
          scale_x_continuous(breaks=seq(0,10000, floor(TIME*CCO))) +
          scale_y_continuous(limits=c(LIM - 1, LIM*2.55 ))+
          theme(legend.position="bottom", legend.direction = "horizontal",  legend.key=element_rect(colour="transparent", fill="transparent"), legend.text = element_text(size=9.5)) +
          geom_point(data=mypoints, aes(x=XCOORD, y=YCOORD, shape=DESCRIP), colour="black")+
          scale_shape_manual("",values = c(8, 17)) +
          guides(shape= guide_legend(title=NULL ))    
          #geom_polygon(data=tatu, aes(x=x, y=y), fill="gray",alpha=0.3)  #rachura no grafico
          ##geom_text(aes(x=7000, y=60, label="Área para o tempo
  ##disponível"), size=3) + geom_path(aes(x=c(5216,5400), y=c(60,60)))
      print(OPERCOSTPLOT)
     
                                    })
     
                           }
                           )


