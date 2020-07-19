############################################################

# adult data
# aim:  to determine whether a person makes over 50K(salary) a year.
############################################################
rm(list=ls(all=TRUE))

rstudio  =  TRUE;
terminal = FALSE;
ifelse( rstudio == TRUE, dirname(rstudioapi::getSourceEditorContext()$path), print("object rstudio is not TRUE") )

########################################################################################################################
#                                         function boruta_rf                                                           #
########################################################################################################################
run_boruta_rf <-function( data, object_col, train_test_rate, seed, RF_num_tree,
                          boruta_need = FALSE, train_print = FALSE, test_print = FALSE ){
   #===================================
   # Load required packages
   #===================================
   if( rstudio  == TRUE ){ library(needs); needs( skimr, doParallel, foreach, randomForest, ggplot2, Boruta ); }
   if( terminal == TRUE ){ library(skimr); library(doParallel); library(foreach); library(randomForest);  library(Boruta); }
   
   #===================================
   # settinng 
   #===================================
   train_test_rate = 0.8; 
   seed = 123;
   RF_num_tree = 500;
   maxruns = 1000;
   dotrace = 0; 
   
   # if you wish to remove any column, then do it in the original file in advance.
   
   #===================================
   # summarize data
   #===================================
   if(!is.data.frame(data)) data = data.frame( data );
   cat( "\nNow, summarize data\n\n"); 
   print( skim( data ) )
   
   #===================================
   # arrange data
   #===================================
   cat("\nNow, arrange data\n\n");
   
   set.seed(seed); 
   train_row = sample( nrow(data), nrow(data)*train_test_rate   );
   
   train_data = data[train_row,]
   test_data  = data[-train_row,]
   
   train_feature = train_data[,-object_col]
   test_feature  = test_data [,-object_col]
   
   train_label   = as.factor( train_data[,object_col] )
   test_label    = as.factor( test_data[,object_col] )
   
   #===================================
   # Boruta-> select more important attributes
   #===================================
   if( boruta_need == TRUE ){
      st <- proc.time()
      
      boruta = Boruta( x= train_feature, y = train_label, maxRuns=maxruns, doTrace = dotrace, ntree = RF_num_tree)
      boruta_col = which( names(data) %in% getSelectedAttributes(boruta) == TRUE ) 
      
      et <- proc.time();
      boruta_cal_time = et - st;
      
      pdf("plot_boruta.pdf",height=10,width=10)
      plot(boruta)
      dev.off()
      
      #### case: Boruta makes sense! ####
      if( length(boruta_col) < length(train_feature) ){
         
         train_feature = train_data[,boruta_col]
         test_feature  = test_data [,boruta_col]
      }
   }
   
   #===================================
   # run RF without parallel
   #===================================
   ### RF setting ####
   cat("Now, run RF\n\n");
   
   st <- proc.time()
   
   set.seed(seed); 
   rf = randomForest( x=train_feature, y=train_label, ntree=RF_num_tree) #train_data[,object_col]
   
   et <- proc.time();
   rf_cal_time = et - st;
   
   #### reserve rf result ####
   if( boruta_need   == FALSE ){ save(rf, file="rf.rds") } else{ save(rf, file="rf+boruta.rds") }
   
   #===================================
   # prediction 
   #===================================
   cat("Now, predict test_data\n\n");
   
   st = proc.time();
   
   prdct = predict( rf, test_data )
   prd_smmry = table( prdct, test_label )
   accuracy = sum( diag( prd_smmry ) ) / sum( prd_smmry )
   
   et <- proc.time();
   prediction_time = et - st;
   
   #===================================
   # Plot
   #===================================
   importance(rf)
   
   #===================================
   # output
   #===================================
   summary = list(
      boruta_effect   = ifelse( boruta_need == FALSE, "FALSE", "TRUE" ),
      if(boruta_need == FALSE ) {boruta_cal_time = "FALSE"} else{ boruta_cal_time = boruta_cal_time },
      train_data      = ifelse( train_print == FALSE, "FALSE", train_data),
      test_data       = ifelse( test_print  == FALSE, "FALSE", test_data),
      train_feature   = ifelse( train_print == FALSE, "FALSE", train_feature),
      test_feature    = ifelse( test_print  == FALSE, "FALSE", test_feature),
      train_label     = ifelse( train_print == FALSE, "FALSE", train_label),
      test_label      = ifelse( test_print  == FALSE, "FALSE", test_label),
      predict         = prdct,
      rf_cal_time     = rf_cal_time,
      prediction_time = prediction_time,
      accuracy        = accuracy,
      importance      = importance( rf )
   )
   return( summary );
}

########################################################################################################################
#                                              run boruta_rf                                                           #
########################################################################################################################
data = readRDS("adult.rds")

res = list();
#===================================
# RFのみ (borutaなし)
#===================================
res[[1]] = run_boruta_rf( data, object_col=15 )

#===================================
# Boruta + RF
#===================================
res[[2]] = run_boruta_rf( data, object_col=15, boruta_need = TRUE )

#===================================
# save res
#===================================
save( res, file = "adult_res.rds")
#load( "res.Rdata" )