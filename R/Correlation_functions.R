#' MULTIVARIATE PERMUTATION TESTING OF CORRELATIONS
#'
#' "This R program performs multivariate permutation tests of the correlation between a single criterion variable and multiple predictor variables. Data must be imported by user with subjects/cases as rows and variables as columns.  Missing data must be specified."
#' This script was Written by Jennifer Urbano Blackford, Ph.D. of The Kennedy Center, Vanderbilt University, Nashville, TN.
#' The original can be found at http://users.cla.umn.edu/~nwaller/downloads/mpt/mptcorr.r
#'
#' @export

#----------------------------------------------------------------------------
#MULTIVARIATE PERMUTATION TESTING OF CORRELATIONS
#Description:  This R program performs multivariate permutation tests of
#              the correlation between a single criterion variable and
#              multiple predictor variables.
#              Data must be imported by user with subjects/cases as rows
#              and variables as columns.  Missing data must be specified.
#
#Written by:   Jennifer Urbano Blackford, Ph.D.
#
#Owned by:     The Kennedy Center, Vanderbilt University, Nashville, TN
#--------------------------------------------------------------------------


#define function & specify arguments
#defaults are specified with an '='
MPT.Corr<-function(observed.data,criterion,predictor.start,predictor.end,
                   output.file="Results.txt",test=2,tail=2,alpha=.05,output=0,permutations=10000){


  #--------------------------------------------------------------------------
  #CREATE NECESSARY DATASETS AND VARIABLES
  #set user specifications
  #set text to print to screen or print to specified file
  if (output.file != '') (sink(output.file))

  #create datasets using the user specifications above
  predict.data<-as.matrix(observed.data[, predictor.start:predictor.end,drop=FALSE])
  if (output==1) (print (predict.data))
  crit.data<-as.matrix(observed.data[,criterion,drop=FALSE])
  if (output==1) (print (crit.data))

  #Assign variable numbers from predictor and criterion datasets
  #set number of subjects as number of rows in Data
  nsubj<-nrow(crit.data)
  #set number of predictors to number of columns in predict.data
  npredict<-ncol(predict.data)
  #set number of criterion variables to number of columns in crit.data
  ncrit<-ncol(crit.data)
  #save the variable names for later use
  colnames <- dimnames(predict.data)[[2]]
  #set tail information for the cor.test function
  if (test==2) (alt<-"t")
  if (test==1 && tail==1) (alt<-"g")
  if (test==1 && tail==-1) (alt<-"l")
  #------------------------------------------------------------------------------


  #------------------------------------------------------------------------------
  #CREATE OBSERVED AND SIMULATED CORRELATION DATASETS
  #calculate correlation for actual data--crit.data and predict.data
  #save correlations in one matrix and p values in another

  observed.r<-matrix(0,1,npredict,dimnames=list(c("r"),c(colnames)))
  observed.p<-matrix(0,1,npredict,dimnames=list(c("p"),c(colnames)))

  #create a matrix to save the simulated r values
  simulated.r<-matrix(0, permutations,npredict)

  #create a vector OriginalCorr to save correlation results
  OriginalCorr<-apply(predict.data,2,cor.test,y=crit.data,alternative=alt)

  for(i in 1:npredict){
    observed.r[i]<-as.matrix(OriginalCorr[[i]]$estimate) #get the r value
    observed.p[i]<-as.matrix(OriginalCorr[[i]]$p.value)} #get the p value

  #print correlation results
  writeLines("Multivariate Permutation Test for Correlations")
  writeLines("   Yoder, Blackford, Waller, and Kim (2003)")
  writeLines("")
  writeLines("Observed Correlations & P-values")
  writeLines("")
  print(cbind(t(round(observed.r,digits=3)),t(round(observed.p,digits=3))))
  writeLines("")

  #create simulated r by shuffling the original criterion data and computing correlations
  for(loop in 1:permutations){
    #randomly sampling nsubj times without replacement=shuffling
    ShuffleData<-as.matrix(crit.data[sample(1:nsubj),])

    #save the post-shuffle corr for each variable and each loop into a matrix called simulated.r
    #note: when using the corr function you must specify to use pairwise deletion
    SimulatedCorr<-cor(predict.data,ShuffleData,use="pairwise.complete.obs")
    for(i in 1:npredict){
      simulated.r[loop,i]<-SimulatedCorr[i]}
  } #end bracket for(loop in 1:permutations)

  #------------------------------------------------------------------------------


  #------------------------------------------------------------------------------
  #PERFORM MULTIVARIATE PERMUATION TESTS

  #create a matrix to store the MPT results
  Results<-matrix(,1,4,dimnames=list(NULL,c("Variable","r","p","MPT exact p")))[-1,]

  #create matrices of maximum corrs and observed corrs in each permutation
  #loop to test significance for max r (i.e., step down procedure).
  while (npredict > 0) {

    #select largest of the absolute values--save column position
    #using column as placeholder provides a way to track the sign of the r
    max.col<-matrix(0,permutations)
    for (loop in 1:permutations){
      max.col[loop,]<-(1:npredict)[abs(simulated.r[loop,])== max(abs(simulated.r[loop,]))]}

    #save the largest value--maintain original sign (using max.col)
    max.simulated.r<-matrix(0,permutations)
    for (loop in 1:permutations){
      max.simulated.r[loop,1]<-simulated.r[loop,max.col[loop,]]}

    #print histogram
    if (output==1) (hist(max.simulated.r))


    #select the largest correlation from the observed correlation for testing
    #use test specification information to determine which correlation to use
    #set max.col.r to the appropriate column number
    #two tailed test: get largest absolute value
    if (test==2) (max.col.r <-(order(abs(observed.r[1,]))[npredict]))
    #one-tailed positive test: get largest positive value
    if (test==1 && tail==1) (max.col.r<-(order(observed.r[1,])[npredict]))
    #one-tailed negative test: get largest negative value
    if (test==1 && tail==-1) (max.col.r<-(order(observed.r[1,])[1]))

    #use the max.col.r variable to get the name, r, and p-value
    #associated with the largest correlation to be tested
    largest.observed.r<-observed.r[max.col.r]
    smallest.observed.p<-observed.p[max.col.r]
    variable<-colnames[max.col.r]

    #apply breaks to stop the program if necessary
    #if testing the positive tail only then stop the program when the observed r is less than 0
    if (test==1 && tail==1 && largest.observed.r <0) break
    #if testing the negative tail only then stop the program when the observed r is greater than 0
    if (test==1 && tail==-1 && largest.observed.r >0) break

    #compare largest correlation to the max r distribution
    #determine significance by creating an array of 0s and 1s representing
    #whether the simulated corr is larger than the observed corr.

    p.dist<-rep(0,permutations)

    #for two tailed test
    #positive observed correlation--check positive tail & negative tail
    if (test==2 && (largest.observed.r >0)) (p.dist[max.simulated.r>= largest.observed.r]<-1)
    if (test==2 && (largest.observed.r >0)) (p.dist[max.simulated.r<= largest.observed.r*-1]<-1)
    #negative observed correlation--check positive tail & negative tail
    if (test==2 && (largest.observed.r <0))(p.dist[max.simulated.r<= largest.observed.r]<-1)
    if (test==2 && (largest.observed.r <0))(p.dist[max.simulated.r>= largest.observed.r*-1]<-1)

    #for one tailed positive test check only the positive tail
    if (test==1 && tail==1)  (p.dist[max.simulated.r>= largest.observed.r]<-1)

    #for one tailed negative test check only the negative tail
    if (test==1 && tail==-1) (p.dist[max.simulated.r<= largest.observed.r]<-1)


    #get p-value as number of permutations above/below observed r and divided by number of permutations
    p.value<-0
    if (sum(p.dist) > 0) (p.value<-(sum(p.dist)/permutations))

    #stop the program when p.value is greater than the value specified
    if (p.value > alpha)  break

    #if the p-value was significant then delete the column for max r and continue

    #delete the column associated with the maximum observed corr
    simulated.r<-simulated.r[,-max.col.r,drop=FALSE]
    colnames<-colnames[-max.col.r,drop=FALSE]
    observed.r<-observed.r[,-max.col.r,drop=FALSE]
    observed.p<-observed.p[,-max.col.r,drop=FALSE]
    #reset number of variables
    npredict<-npredict-1


    #create a matrix to store the results
    ResultsTemp<-cbind(variable,round(largest.observed.r,digits=3),
                       round(smallest.observed.p,digits=3),round(p.value, digits=3))
    Results<-rbind(Results,ResultsTemp)
    rm(ResultsTemp)

    #end testing
  }

  #add row labels to Results
  if (nrow(Results)>=1) (dimnames(Results)[[1]]<-c(1:nrow(Results)))

  #print final results if there are any
  writeLines("Results of MPT for Correlations")
  writeLines("")
  if (nrow(Results)>=1) print(Results,quote=FALSE,rowlab=c(1:nrow(Results)))
  if (nrow(Results)<1)  writeLines("No significant Results")
  writeLines("")
  sink()  #close sink

}

