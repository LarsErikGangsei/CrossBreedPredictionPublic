################################################################################
##                                                                            ##
## R-script reproducing results presented in manuscript:                      ##
##      "Classification of breed combinations for slaughterpigs based on      ##
##       DNA sequencing - Viewing DNA samples of crossbreeds as fuzzy         ##
##       samples from purebreed founders".                                    ##
##                                                                            ##
## Authors: - Hilde Vinje (NMBU), hilde.vinje@nmbu.no                         ##
##          - Hilde Brustad (UiO),                                            ##
##          - Andrew Heggli (Animalia, NMBU), andrew.heggli@animalia.no       ##
##          - Claudia A. Sevillano (Topigs Norsvin),                          ##
##          - Maren van Son (Norsvin), maren.van.son@norsvin.no               ##
##          - Lars Erik Gangsei (Animalia, NMBU),                             ##
##                                lars.erik.gangsei@animalia.no               ## 
##                                                                            ##
##                                                                            ##
## Adresses: Norwegian University of Life Sciences (NMBU),                    ##
##           Faculty of Chemistry, Biotechnology and Food Science,            ##
##           P.O. Box 5003,                                                   ##
##           NO-1432 Ås, Norway                                               ##
##                                                                            ##
##           Animalia AS,                                                     ##
##           P.O. Box 396 - Økern,                                            ##
##           NO-0513 Oslo, Norway                                             ##
##                                                                            ##
##           UiO,                                                             ##
##           Oslo Center of Biostatistics and Epidemiology,                   ##               
##           Oslo University Hospital,                                        ##
##           Norway                                                           ##
##                                                                            ##
##           Topigs Norsvin                                                   ##
##           Topigs Norsvin Research Center,                                  ##
##           Beuningen,                                                       ##
##           The Netherland                                                   ##
##                                                                            ##
##           Norsvin SA,                                                      ##
##           Hamar,                                                           ##
##           Norway                                                           ##
##                                                                            ##
##                                                                            ##
## Manuscript submitted to "Frontiers in Genetics, section Livestock Genomics"##
## in September 2023                                                          ##

## 1) Clean workspace, load necessary packages ----------------------------   ##

#  Clean workspace              
rm(list = ls())

# Load packages from Cran
packages <- c('xtable','pls','randomForest','devtools','tidyverse')

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

for(pp in packages){library(pp,character.only=TRUE)}

# Load the CrossbreedPrediction pacgage from Github
# As long as package is private, special token
if(!is.element('CrossBreedPrediction',rownames(installed.packages())))
{
devtools::install_github("LarsErikGangsei/CrossBreedPredictionPublic")
}
library('CrossBreedPrediction')



## 2) Figure with comparison of PLS and PCA analysis ------------------------ ##
Scoreplot_func()#Scoreplot_func('Figures/Fig_PCAvsPLS.png')

## 3) Predict breedcombinations for test sets ------------------------------- ##
## List with different Test sets
Test_ResultsSim <- vector('list',2)
names(Test_ResultsSim) <- c('TrainP+','TrainP-')

for(nnB in c('TrainP+','TrainP-'))
  {
  Test_ResultsSim[[nnB]] <- PredictCB_func(DNA = list(
    Scores = Sim_Scores[[nnB]],RF_pred = RF_Sim_predictions[[nnB]],
    PLSR_pred = PLSR_Sim_predictions[[nnB]]),Train = nnB,alpha0 = 73.58105,
    Unkn_LogLike = NULL,PriorDist = NULL)}

Test_ResultsSim$`TrainP+`$Admixture <- dplyr::arrange(
                Admixture_Sim_predictions$`TrainP+`$Supervised$Predictions,
                as.numeric(ID)<=500,Breed)%>%
                dplyr::select(c('DDDD','HHHH','LLLL','PPPP','WWWW'))%>%
  as.matrix()

Test_ResultsSim$`TrainP-`$Admixture <- dplyr::arrange(
  Admixture_Sim_predictions$`TrainP-`$Supervised$Predictions,
  as.numeric(ID)<=500,Breed)%>%
  dplyr::select(c('DDDD','HHHH','LLLL','WWWW'))%>%
  as.matrix()

colnames(Test_ResultsSim$`TrainP+`$PLSQDA) <- strrep(colnames(Test_ResultsSim$`TrainP+`$PLSQDA),4)
colnames(Test_ResultsSim$`TrainP-`$PLSQDA) <- strrep(colnames(Test_ResultsSim$`TrainP-`$PLSQDA),4)
  
## 4) Make plot showing distributions for soft predictions ------------------ ##
TrueBreeds <- substr(rownames(Test_ResultsSim$`TrainP+`),1,4)
nn_test <- length(TrueBreeds)

Distributionplot_func(image_path = 'Figures/Fig_PLS_vsSim_Real.png',
                      TrueBreed = TrueBreeds,
                      BreedNames = list(
                        `All others` = c("DDDD","LLLL","PPPP","WWWW"),
                        Hampshire = "HHHH"),
                      SoftPreds = Test_ResultsSim$`TrainP+`[c("RF","Admixture","PLSR","PLSQDA")],
                      MethodNames = c("Random Forest","Admixture","PLSR","PLS-QDA"),
                      dens_adj = c(1,1,1,500))

## 5) Table with results for test sets -------------------------------------- ##
Res_tab <- dplyr::bind_rows(
              lapply(as.list(Test_ResultsSim$`TrainP+`[c('RF','Admixture','PLSR',
                            'PLSQDA')]),Dist_eval_func,
                   TrueBreed = substr(rownames(Test_ResultsSim$`TrainP+`),1,4)),
              lapply(as.list(Test_ResultsSim$`TrainP-`[c('RF','Admixture','PLSR',
                            'PLSQDA')]),Dist_eval_func,
                   TrueBreed=substr(rownames(Test_ResultsSim$`TrainP-`),1,4)))%>%
           dplyr::bind_cols(
             dplyr::mutate(
               data.frame(Comb = paste(
                        rep(c('TrainP+','TrainP-'),each=nn_test),
                        grepl('P',rep(TrueBreeds,2)),sep='')),
                    Comb = gsub('TRUE','TestP+',gsub('FALSE','TestP-',Comb))))%>%
           dplyr::group_by(Comb)%>%
           dplyr::reframe(RF = sum_func_df(RF),ADMIXTURE = sum_func_df(Admixture),
                          PLSR = sum_func_df(PLSR),PLSQDA = sum_func_df(PLSQDA))%>%
           dplyr::mutate(Method = rep(c('KL-dist (soft)','KL-dist (hard)',
                               'Sq-loss (soft)','Sq-loss (hard)'),4),.after=Comb)


Res_tab$Comb[-seq(2,16,by=4)] <- ''

print.xtable(
  xtable(Res_tab,align = c('c','c','l|','r','|r','|r','|r'),label = 'tab:Loss',
        caption = 'Evaluation of prediction errors for soft predictions as mean
            $\\pm$ standard deviations and hard predictions given as number of 
            correct classifications as total number and \\%, (n = 3500 for 
            TestP+ and TestP-) based on Kullback Leibler divergences, 
            quadratic errors, crossed over the two training (TrainP+ and TrainP-) 
            sets, and test sets (TestP+ and TestP-)'), 
    sanitize.text.function = function(x){x},include.rownames = FALSE,
    include.colnames = TRUE,comment = FALSE,hline.after = c(-1,0,4,8,8,12,16),
  caption.placement = 'top') 


## 6) Results for posterior max as hard predictions ------------------------- ##
PosteriorMax <- data.frame(
                 Pred = c(colnames(Test_ResultsSim$`TrainP+`$PLSQDA_Posteriors)[
                            apply(Test_ResultsSim$`TrainP+`$PLSQDA_Posteriors,
                            1,which.max)],
                         colnames(Test_ResultsSim$`TrainP-`$PLSQDA_Posteriors)[
                            apply(Test_ResultsSim$`TrainP-`$PLSQDA_Posteriors,
                            1,which.max)]),
                 TrueBreeds = rep(TrueBreeds,2),
                 Comb = paste(rep(c('TrainP+','TrainP-'),each=nn_test),
                             grepl('P',rep(TrueBreeds,2)),sep=''))

# Show results for test set 
dplyr::group_by(PosteriorMax,Comb)%>%dplyr::summarise(sum(Pred==TrueBreeds),
                                                      sum(Pred=='Unknown'))

# Table with results when Pietrain is viewed as "unknown"
PosteriorMax$Gen_breed_true <- sapply(split(PosteriorMax$TrueBreeds,
                                  f=1:(2*nn_test)),GenBreed_func,KeepBreed = 'P')
PosteriorMax$Gen_breed_pred <- sapply(split(PosteriorMax$Pred,
                                  f=1:(2*nn_test)),GenBreed_func,KeepBreed = 'P')

PosteriorMax$Gen_breed_pred[PosteriorMax$Pred=='Unknown'] <- 'Unknown'

Res_tab <- dplyr::filter(PosteriorMax,Comb=='TrainP-TRUE')%>%
           dplyr::select(Gen_breed_true,Gen_breed_pred)
Res_tab <- as.matrix(table(Res_tab$Gen_breed_true,Res_tab$Gen_breed_pred))
Res_tab <- cbind(round(100*Res_tab/matrix(rowSums(Res_tab),
                                 dim(Res_tab)[1],dim(Res_tab)[2],byrow=FALSE)),
            rowSums(Res_tab))
rownames(Res_tab) <- paste('&',rownames(Res_tab))
colnames(Res_tab) <- gsub('unknown','Unknown',colnames(Res_tab))
colnames(Res_tab)[dim(Res_tab)[]] <- 'n'

cat(gsub('rrrrrrrr','|rr|rrrrrr|r|',
    print.xtable(
     xtable(Res_tab,
       caption = 'Proportions (in \\%) of predicted  breed combination, 
        i.e.maximum posteriori probabilities with PLS-QDA,for model trained 
        without Pietrain (TrainP-),vs. true breedcombinations for the simulated 
        test set with Pietrain (TestP+). Breedcombinations are generalized, i.e. 
        "XXXX" = $\\lbrace "DDDD","HHHH","LLLL","PPPP","WWWW"\\rbrace$ (PB), "XXXY" = 
        $\\lbrace "DDDH",\\ldots,"ZZZL"\\rbrace$ i.e. three grandparents of same 
        breed, etc."',label = 'tab:class_TestP-',digits=0),
     caption.placement = 'top',comment = FALSE,rotate.rownames=FALSE,
     add.to.row = 
       list(pos = list(-1,0),
          command = c('\\hline \n & &\\multicolumn{6}{|c|}{Predicted 
                      Combination(\\%)}&n\\\\ \n &','\\hline \n 
                      \\multirow{7}{4}{\\begin{sideways}
                      True combinations\\end{sideways}}')),
     sanitize.rownames.function = function(x){x},
     hline.after = c(dim(Res_tab)[1]),print.results = FALSE)))


## 7) Table with generalized variances -------------------------------------- ##
T_likePpluss <- LikelihoodParameters_func(Train = 'TrainP+',alpha0 = 73.58105)
nn_pluss <- length(T_likePpluss)

T_likePminus <- LikelihoodParameters_func(Train = 'TrainP-',alpha0 = 73.58105)
nn_minus <- length(T_likePminus)


Res_tab <- data.frame(
             log_det_Mixed = c(sapply(T_likePpluss,function(x) log(det(x$cov))),
                               sapply(T_likePminus,function(x) log(det(x$cov)))),
             log_det_Emp = c(sapply(split(Sim_Scores$`TrainP+`,
                                f=rownames(Sim_Scores$`TrainP+`)),
                                function(x) log(det(var(matrix(x,length(x)/4,4))))),
                                    sapply(split(Sim_Scores$`TrainP-`[-grep('P',
                                          rownames(Sim_Scores$`TrainP-`)),],
                                        f=rownames(Sim_Scores$`TrainP-`)[
                                          -grep('P',rownames(Sim_Scores$`TrainP-`))]),
                                    function(x) log(det(var(matrix(x,
                                                  length(x)/3,3)))))))%>%
           dplyr::mutate(Gen_breed = sapply(split(c(names(T_likePpluss),
                    names(T_likePminus)),f = 1:(nn_pluss+nn_minus)),
                    GenBreed_func,KeepBreed = 'A'),
                    Training = c(rep('TrainP+',nn_pluss),rep('TrainP-',nn_minus)))%>%
            dplyr::group_by(Training,Gen_breed)%>%
            dplyr::summarise(n = n(),Mixednorm = sum_func_soft(log_det_Mixed),
                             Empirical = sum_func_soft(log_det_Emp))%>%
            dplyr::mutate(Gen_breed = paste(Gen_breed,' (n = ',n,')',sep=''))%>%
            dplyr::select(-n)%>%as.data.frame()

Res_tab$Training <- ''
Res_tab$Training[c(3,8)] <- c('P+','P-')
names(Res_tab) <- c('','Generalized Breed', 'Mixed Normals','Simulated Data')

print.xtable(
  xtable(Res_tab,align = c('c','l','|l||','c|','|c'),
      label = 'tab:DeterminantSizes',
      caption ='Mean $\\pm$ standard deviation for covariance matrix 
      determinants at logaritmic scale, crossed over generalized breed 
      combinations i.e. "XXXX" = $\\lbrace "DDDD","HHHH","LLLL","PPPP","WWWW"\\rbrace$ 
      (PB), "XXXY" = $\\lbrace "DDDH",\\ldots,"ZZZL"\\rbrace$ i.e. three 
      grandparents of same breed, etc." and if Pietrain is included in 
      analyzis or not. Column "Mixed Normals" are beased on $\\Sigma$-s in 
      the likelihood function and column "Simulated data" is based on
      empiric covariances from simulated data.'), 
  sanitize.text.function = function(x){x},include.rownames = FALSE,
  include.colnames = FALSE,comment = FALSE,hline.after = c(-1,0,0,5,5,10),
  caption.placement = 'top') 

  

## 8) Table with results for real data -------------------------------------- ##
Test_ResultsReal <- list(
  Flat = PredictCB_func(DNA = list(Scores = Real_Scores$`TrainP+`,
                        RF_pred = RF_Real_predictions$`TrainP+`,
                        PLSR_pred = PLSR_Real_predictions$`TrainP+`),
                        Train = 'TrainP+',alpha0 = 73.58105,Unkn_LogLike = NULL,
                        PriorDist = NULL)%>%dplyr::select(PLSQDA_Posteriors),
  Informative = PredictCB_func(DNA = list(Scores = Real_Scores$`TrainP+`,
                        RF_pred = RF_Real_predictions$`TrainP+`,
                        PLSR_pred = PLSR_Real_predictions$`TrainP+`),
                        Train = 'TrainP+',alpha0 = 73.58105,Unkn_LogLike = NULL,
                        PriorDist = 'informative')%>%dplyr::select(PLSQDA_Posteriors))

Test_ResultsReal <- dplyr::full_join(
                data.frame(
                  ID = rownames(Test_ResultsReal$Flat),
                  Informative = colnames(Test_ResultsReal$Informative$PLSQDA_Posteriors)[
                    apply(Test_ResultsReal$Informative$PLSQDA_Posteriors,1,which.max)],
                  Flat = colnames(Test_ResultsReal$Flat$PLSQDA_Posteriors)[
                    apply(Test_ResultsReal$Flat$PLSQDA_Posteriors,1,which.max)]),
                  data.frame(
                    ID = Admixture_Real_predictions$`TrainP+`$Supervised$Predictions$PIG_ID,
                Admixture = SoftToHard_func(
                  as.matrix(Admixture_Real_predictions$`TrainP+`$Supervised$Predictions[,-1]),
                  method = 'Sq_loss')),
                by = 'ID')

Res_tab <- lapply(lapply(lapply(dplyr::select(Test_ResultsReal,-ID),table),
                  function(x) c(x[x>=10],sum(x[x<10]))),
            function(x) data.frame(BreedComb = names(x),Freq = x))

Res_tab <- dplyr::full_join(Res_tab$Flat,Res_tab$Informative,by= 'BreedComb')%>%
            dplyr::rename(Informative = Freq.y, Flat = Freq.x)%>%
            dplyr::full_join(Res_tab$Admixture,by = 'BreedComb')%>%
            dplyr::rename(Admixture = Freq)%>%
            replace(is.na(.), 0)%>%
            dplyr::mutate(`Inf. vs. Adm.` = Informative - Admixture,
                          `Adm. vs. Flat` = Admixture - Flat,
                          `Inf. vs. Flat` = Informative - Flat)%>%
            dplyr::arrange(desc(Informative))
            
Res_tab$BreedComb[Res_tab$BreedComb==''] <- 'Other combinations'
rownames(Res_tab) <- Res_tab$BreedComb
Res_tab <- dplyr::select(Res_tab,-BreedComb)

print.xtable(
  xtable(Res_tab,digits=0,
    caption = 'Predicted breed combinations for the test set containing real 
    data. Predictions as posterior maximums applying the informative, and flat 
    prior to the PLS-DA method trained with Pietrain (TrainP+), and hard 
    predictions based on square loss for Admixture trained with all 5 
    breeds as reference. The last collumns show differences in total number 
    classified to different breedgroups for the three methods. Group 
    "Other combinations" contain sums for breed combinations with fewer than 10 
    predictions for any of the methods.',label = 'tab:TestR_FlatPriors',
    align = c('l|','c','c','c|','c','c','c|')),
  caption.placement = 'top',comment = FALSE,rotate.rownames=FALSE)





