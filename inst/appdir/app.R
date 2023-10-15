################################################################################
##                                                                            ##
## Shiny app for visualization of crossbreed classification of pigs based on  ##
## PLS-QDA.                                                                   ##
##                                                                            ## 
## See Vinje. et.al. (2023) for details.                                      ##
##                                                                            ##
## App authors: Lars Erik Gangsei, Hilde Vinje.                               ##

## Calculations done only once (at startup) --------------------------------- ##

# Load necessary libraries.

library(dplyr)
library(shiny)
library(rgl)
library(mvtnorm)
library(devtools)
library(CrossBreedPrediction)



# List with length 35 containing expectation (mu's) and covariance parameters 
# for the multivariate normal assoisted with the likelihood function for the 
# 35 possible breed combinations.
T_likelihood <- LikelihoodParameters_func(Train = "TrainP-", alpha0 = 73.58105)


# Make Score Vectors for purbreeds as averages of all PB's 
PB_scores <- dplyr::group_by(data.frame(
  Scores = I(as.matrix(Mod_pls$`TrainP-`$scores)),
  Breed = Mod_pls$`TrainP-`$Breed),by = Breed)%>%
  dplyr::summarise(mean(Scores[,1]),
                   mean(Scores[,2]),
                   mean(Scores[,3]))


# Viewdata: Data frame with elements for n individual pigs: 
# - "Scores" (n x 3) (PLS scores)
# - Breed:   (n x 1) Breed as four letter abbreviations (simulated data or PB's)
#            or "unknown" for real data.
# - ID :     (n x 1) Character vector, levels "Real", "Simulated" or "PBs"
#            where PBs area average scores for the PBs (real data).
# - LogLike: (n x 36) Likelihoods for all individuals for all 36 possible 
#           vcategories (35 combinations + unknown).
ViewData <- data.frame(
  Scores = I(rbind(as.matrix(Real_Scores$`TrainP-`),
                   as.matrix(Sim_Scores$`TrainP-`),
                   as.matrix(PB_scores[,-1]))),
  Breed = c(rep('Unknown',dim(Real_Scores$`TrainP-`)[1]),
            rownames(Sim_Scores$`TrainP-`),
            PB_scores$by),
  ID = c(rep('Real',dim(Real_Scores$`TrainP-`)[1]),
         rep('Simulated',dim(Sim_Scores$`TrainP-`)[1]),
         rep('PBs',dim(PB_scores)[1])))


ViewData$LogLike <- I(matrix(NA,dim(ViewData)[1],length(T_likelihood)+1,
                      dimnames=list(NULL,c(names(T_likelihood),'Unknown')))) 

for(jj in 1:(dim(ViewData$LogLike)[2]-1)){ViewData$LogLike[,jj]<-
  dmvnorm(ViewData$Scores,mean = T_likelihood[[jj]]$mu,
         sigma = T_likelihood[[jj]]$cov,log=TRUE)}

ViewData$LogLike[,'Unknown'] <- -sum(log(apply(apply(Mod_pls$`TrainP-`$scores,
                                                     2,range),2,diff)))

# Dimensions for simplified coding
nn <- dim(ViewData)[1]
mm <- dim(ViewData$LogLike)[2]


## Input panels ------------------------------------------------------------- ##

ui <- fluidPage(
  titlePanel('Crossbreed Classification of Pigs'),
  fluidRow(
# Radiobuttons for choosing predictions or True breed (known for simulated data)
# as colorbase
    column(3,wellPanel(radioButtons('ColBase', 'Colors from',
                                    c('True breed' = 0,
                                      'Classification' = 1),selected = 1,
                                    inline = TRUE),
                       
# Slider for setting point sizes in 3D panel
                       verticalLayout(sliderInput('SizePB', 'Point sizes PB', 
                                                  min = 0, max = 15,value = 4,step = 0.05),
                                      sliderInput('SizeSim', 'Point sizes Simulated/ Real',
                                                  min = 0, max = 4,value = 1,step = 0.05))),

# Checkboxes for showing real data or not and which (if any) Simulated crossbreeds
# to show.
           wellPanel(checkboxInput('ShowReal','Show Real Data',TRUE),
                     checkboxGroupInput(inputId = 'CBs',label = 'Simulated CBs to show',
                                        choices = Prior$`P+`$BreedComb[-which(Prior$`P+`$BreedComb=='Unknown')],
                                        selected = 'None',
                                        inline = TRUE,
                                        width = NULL,
                                        choiceNames = NULL,
                                        choiceValues = NULL
                     ))),

# Middle of panel is used for 3D panel.

  column(5,plotOutput('ColorCode',width = '500px',height = '50px'),
         rglwidgetOutput("rglPlot",width = 500, height = 500)),

# Sliderinput for transparency of ellipses showing regions of classifications
  column(3,wellPanel(sliderInput('EllipseTransp','Transparancy for ellipses',
                               min = 0, max = 1,value = 0.5,step = 0.011),
                     
# Checkboxinput for which breedcombinations ellises should be shown for 
         checkboxGroupInput(inputId = 'CBsEllipse',label = 'Ellipses to show',
           choices = Prior$`P-`$BreedComb[-which(Prior$`P-`$BreedComb=='Unknown')],
           selected = c('DDLW','HHLW'),
           inline = TRUE,
           width = NULL,
           choiceNames = NULL,
           choiceValues = NULL
         )),

# Sliderinput for varying degree of informative prior and support for unknown 
# breed
         wellPanel(verticalLayout(sliderInput('Prior', 'Degree of informative prior', 
                                              min = 0, max = 1,value = 0,step = 0.01),
                                  sliderInput('deltaUK', 'Support for unknown breed', 
                                              min = -1, max = 1,value = 0,step = 0.02),
                                  plotOutput('priorPlot',height = '300px')))))
)


## Server functions, i.e. how to react on changes in input ------------------ ##
server <- function(input, output) {

# Show the prior support for the 36 possible categories as a pie chart
  output$priorPlot <- renderPlot({
    par(mar= c(0,0,0,0))
    pie(Prior_func(input$Prior,input$deltaUK,'P-'),
        labels = Prior$`P-`$BreedComb,cex=0.75)})

#   
  output$ColorCode <- renderPlot({
    par(mar=c(0,0,0,0))
    plot(1,axes=FALSE,pch='',xlab = '',ylab = '')
    legend('center',c('Duroc','Hampshire','Landrace','Pietrain','Large white'),
           fill=c('red','green','blue','black','white'),
           ncol = 5)})
  
  ###############################################################
  output$rglPlot <- renderRglwidget({
    rgls <- rgl.dev.list()
    try(for(rr in rgls){close3d(rr)})
    open3d(useNULL=TRUE)
    cex_vec <- rep(input$SizePB,nn)
    cex_vec[ViewData$ID=='Real'] <- input$SizeSim
    cex_vec[ViewData$ID=='Simulated'] <- input$SizeSim
    
    PB_idx <- which(ViewData$ID =='PBs')
    sim_idx <- which((ViewData$ID =='Simulated')&is.element(ViewData$Breed,input$CBs))
    if(input$ShowReal==TRUE){real_idx <- which(ViewData$ID=='Real')}else{real_idx <- NULL}
    r_idx <- sort(c(PB_idx,sim_idx,real_idx))
    
    Prior_vec <- Prior_func(input$Prior,input$deltaUK,'P-')
    Pred_breed <- Pred_func(Prior_vec,ViewData$LogLike)
    
    
    if(input$ColBase==0){
      col_vec <- BreedToColor(ViewData$Breed,
                      rgbw = gsub('P','','DHLPW'),
                      blackBreed = 'P')
      #r_idx <- r_idx[-grep(input$BreedExclude,ViewData$Breed)] 
    }else{
      col_vec <- BreedToColor(Pred_breed,
                 rgbw = gsub('P','','DHLPW'),
                 blackBreed = 'P')
      #r_idx <- r_idx[-grep(input$BreedExclude,Pred_breed)] 
      }
    
    plot3d(ViewData$Scores[r_idx,1],ViewData$Scores[r_idx,2], 
           ViewData$Scores[r_idx,3],xlab = 'PLS score 1',ylab = 'PLS score 2',
           zlab = 'PLS score 3',col = col_vec[r_idx],radius = cex_vec[r_idx],
           type = 's')
    
    if(input$EllipseTransp!=0)
    {
      for(cb in input$CBsEllipse)
      {
        ellips <- Ellipse_func(log_unknown = (ViewData$LogLike[1,'Unknown']  +
                                              log(Prior_vec['Unknown'])),
                               mu = T_likelihood[[cb]]$mu,
                               Sigma = T_likelihood[[cb]]$cov,
                               log_prior = log(Prior_vec[cb]))
        
        shade3d(ellips,
                color = BreedToColor(cb,rgbw = gsub('P','','DHLPW'),
                                     blackBreed = 'P'),
                alpha = input$EllipseTransp)
        
      }
    }
    rglwidget(width = 1000, height = 1000 )
  })
  ###############################################################
  
}

shinyApp(ui, server)