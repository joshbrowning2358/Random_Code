#Function to optimize some fitness function via genetic algorithms.  The input is a list of
#arbitrary parameters, and these input lists are bred and mutated until an optimal value is
#reached.  GA is very flexible, but at a cost: you must supply many of the functions for it
#to work.
#creater: A function whose return value is a new parameter list.
#fitness: A function that computes the "fitness" or objective function for a particular input.
#  It should always be positive (as selection is done by random sampling according to fitness).
#crossover: A function that takes two parameters and generates a new parameter.
#breedProp: The proportion of population that should be breed at each step.
#mutateProb: All individuals that survive have a chance for mutating, specified by this.  The
#  only exception are the top few who survive without a chance of mutation, given by nKeepTop.
#check: A function to evaluate on the parameters generated to ensure they are all valid.  More
#  built for safety than anything else, but should be implemented!
#mutation: A function specifying how to mutate individuals.  Defaults to creater(), which is
#  not a very good default.
#selection: Function that selects n new parameters from a list of possible parameters (listList)
#  and their corresponding fitness values.  Default is to do weighted sampling based on fitness.
#popSize: Size of the population to maintain for each generation.
#nGen: Number of generations to create in the algorithm.  More generations means longer
#  computation time but a higher chance of converging to the global optimum.
#verbose: Should the best parameter be output after each run?
GA = function( creater, fitness, crossover, breedProp=1/2, mutateProb=.05, nKeepTop=3
  ,check=function(listEl){return(TRUE)}
  ,mutation=function(listEl){return(creater())}
  ,selection = function(listList, fitnessList, n){
    if(min(fitnessList)<0){
      warning("fitnessList has values <0!")
      fitnessList[fitnessList<0] = 0
    }
    pSel = fitnessList/sum(fitnessList)
    selectPop = sample(listList, size=n, prob=pSel, replace=TRUE)
  }
  ,popSize=10, nGen=100, verbose=T, ...){
  
  if(popSize<1)
    stop("popSize must be >=1!")
  popSize = round(popSize)
  if(nGen<1)
    stop("nGen must be >=1!")
  nGen = round(nGen)
  if(breedProp<0 | breedProp > 1)
    stop("breedProp must be in [0,1]!")
  if(mutateProb<0 | mutateProb>1)
    stop("breedProp must be in [0,1]!")
  if(nKeepTop<0 | nKeepTop > popSize)
    stop("nKeepTop must be in [0,popSize]!")
  
  #Simple function check
  fitness(creater())
  print("Fitness and creater function evaluated successfully!")
  crossover(creater(),creater())
  print("Crossover function evaluated successfully!")
  mutation(creater())
  print("Mutation function evaluated successfully!")
  check(creater())
  print("Check function evaluated successfully!")

  #Initialize population
  listList = lapply(1:popSize, function(i){creater()})
  if( !all(do.call("c", lapply(listList, check))) )
    stop("A list created in initialization did not pass check()")
  
  nCrossover = round(breedProp*popSize)
  nSurvive   = popSize-nCrossover-nKeepTop
  nSelect    = 2*nCrossover + nSurvive
  print("Initial Population Generated")
  for(i in 1:nGen){
    #Select new individuals from population.  Note that selection() is called once 
    #per loop so that we only evaluate fitness functions once per loop
    fitnessList = do.call("c",lapply(listList, fitness))
    if(verbose){
      cat("Best fitness is",max(fitnessList),"occuring for element:\n")
      print(listList[which.max(fitnessList)])
    }

    selList = selection(listList, fitnessList, nSelect)
    if(!all(do.call("c", lapply(selList, check)) ) )
      stop("Selection produced invalid parameters!")
    
    #Crossover parents
    newList = list()
    for(j in 1:nCrossover) #for loop ok since this computation should be minimal
      newList[[j]] = crossover(selList[[2*j-1]],selList[[2*j]])
    if(!all(do.call("c", lapply(newList, check)) ) )
      stop("Crossover produced invalid parameters!")
        
    #Keep survivors
    for(j in 1:nSurvive)
      newList[[j+nCrossover]] = selList[[2*nCrossover+j]]
    if(!all(do.call("c", lapply(newList[(nCrossover+1):nSurvive], check)) ) )
      stop("Survivors are invalid!")
    
    #Mutate randomly selected obs
    toMutate = rbinom(popSize, size=1, prob=mutateProb)
    newList[toMutate] = lapply(newList[toMutate], mutation)
    if(!all(do.call("c", lapply(newList[toMutate], check)) ) )
      stop("Mutations produced invalid parameters!")
    
    #Keep some of the best
    if(nKeepTop>0){
      thresh = sort(fitnessList, decr=T)[nKeepTop]
      newList = c(newList, listList[fitnessList>=thresh])
    }
    
    listList = newList
    cat("Population",i,"simulated\n")
  }
  
  fitness = do.call("c", lapply(listList, fitness) )
  #First maximum found is returned in the case of ties
  return( listList[which.max(fitness)] )
}

#Loads some example functions into R's workspace for use with GA
example = function(){
  creater <<- function(){
    as.list(round(runif(3,.5,10.5)))
  }
  
  fitness <<- function(listEl){
    sum(listEl[1]==1) + sum(listEl[2]==3) + sum(listEl[3]==7)
  }
  
  crossover <<- function(listEl1, listEl2){
    newEl = list()
    for(i in 1:3)
      newEl[i] = sample(c(listEl1[[i]],listEl2[[i]]),size=1)
    return(newEl)
  }
  
  mutation <<- function(listEl){
    listEl[[sample(1:3,size=1)]] = round(runif(1,.5,10.5))
    return(listEl)
  }
  #GA(creater = creater,fitness = fitness, crossover = crossover, mutation=mutation, nKeepTop=1 )
}
