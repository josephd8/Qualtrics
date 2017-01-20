setwd("~/Documents/Conjoint/Projects/MD_Design_Scrub")

design <- read.csv("design_correct.csv",header=TRUE)
head(design)

final.design <- matrix(0,nrow(design),7)
row<-1
for(d in 12:48){
  
  use.design <- design[design$NumberAttribute ==d,2:ncol(design)]
  
  print(d)
  
  try(new.design <- MD_clean(use.design,1))
  
  for(r in 1:nrow(new.design)){
    for(c in 1:ncol(new.design)){
      final.design[row,c] <- new.design[r,c]
    }	
    row<-row+1
  }
  
  
}

write.csv(new.design,"cleaned_design.csv")


MD_clean <- function(design,threshold=2){
  
  numItems <- ncol(design) - 2
  
  vers.list <- list()   
  for (q in 1:max(design$version)){
    tester <- design[design$version==q,]
    vec.test <- c()
    cntr <- 1
    for (i in 1:max(tester$task)){
      cntr <- cntr
      for (j in (ncol(tester) - numItems + 1):ncol(tester)){
        vec.test[cntr] <- c(tester[i,j])
        cntr <- cntr + 1
      }
    }
    vers.list[[q]] <- vec.test
  }
  
  for (b in 0:(threshold - 1)){ 
    
    occ.list <- list()
    for (j in 1:length(vers.list)){
      occurences <- c()
      for (i in  1:max(design[,(ncol(design)-numItems + 1):ncol(design)])){
        occurences[i] <- sum(vers.list[[j]] == i)
      }
      occ.list[[j]] <- occurences
    }
    
    temp.list <- vers.list
    no.mins.list <- list()
    for (j in 1:length(occ.list)){
      ind.mins <- c()
      ind.maxes <- c()
      sampl.maxes <- c()
      max.places <- c()
      maxes <- c()
      ind.max.places <- c()
      before.sampl.places <- c()
      if (is.element(b,occ.list[[j]]) == TRUE){
        ind.mins <- which(occ.list[[j]] == b)
        ind.maxes <- which(occ.list[[j]] == max(occ.list[[j]]))
        
        cntr <- 1
        while (length(ind.maxes) < length(ind.mins)){
          cntr <- cntr + 1
          ind.next.max <- c()
          add.nexts <- c()
          maxes.to.add <- c()
          ind.next.max <- which(occ.list[[j]] == max(occ.list[[j]])-cntr)
          add.nexts <- sample(length(ind.next.max),size=(length(ind.mins)-length(ind.maxes)))
          maxes.to.add <- ind.next.max[c(add.nexts)]
          ind.maxes <- c(ind.maxes,maxes.to.add)
        }
        
        sampl.maxes <- sample(length(ind.maxes),size=length(ind.mins))
        maxes <- ind.maxes[c(sampl.maxes)]
        cntr <- 1
        for (t in 1:length(maxes)){
          cntr <- cntr
          for (q in 1:length(which(vers.list[[j]] == maxes[t]))){
            before.sampl.places[cntr] <- which(vers.list[[j]] == maxes[t])[q]
            cntr <- cntr + 1
          }
        }
        ind.max.places <- sample(x=length(before.sampl.places),size=length(ind.mins))
        max.places <- before.sampl.places[c(ind.max.places)]
        splits <- split(vers.list[[j]], ceiling(seq_along(vers.list[[j]])/numItems))
        max.in.split <- ceiling(max.places/numItems)
        for (v in 1:length(ind.mins)){
          if (is.element(ind.mins[v],splits[max.in.split][v])==TRUE){
            before.sampl.places <- before.sampl.places[-ind.max.places]
            ind.max.places <- sample(x=length(before.sampl.places),size=length(ind.mins))
            max.places <- before.sampl.places[c(ind.max.places)]
          } else {
            max.places <- max.places
          }
        }
        
        for (w in 1:length(ind.mins)){
          temp.list[[j]][max.places[w]] <- ind.mins[w]
        }
        no.mins.list[[j]] <- temp.list[[j]]
      } else{
        no.mins.list[[j]] <- temp.list[[j]]
      }
    }
    
    vers.list <- no.mins.list
    
  }
  
  
  undone.nomins <- unlist(vers.list)
  tasks.df <- data.frame(matrix(undone.nomins,nrow=length(undone.nomins)/numItems,byrow=T))
  temp.df <- design
  temp.df[,c((ncol(design)-numItems + 1):ncol(design))] <- tasks.df
  
  return(temp.df)
} 

MD_clean(design)

# Threshold is automatically set to two, so you don't have to worry about it.
# The design is the only input and this will work AS LONG AS the design is in the same format:
#     version task Item_1 Item_2 ..... Item_N
#

## Run Time ##
# user  system elapsed 
# 0.083   0.003   0.087 
