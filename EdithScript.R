## Edith Plants-Paris' Mark Recapture Project ##

# Requires both Program MARK and RMark #
# Requires the .inp mark-recapture files #

#Load RMark
library(RMark)

#Import .inp files into environment
neches1=convert.inp("C:/Users/Michael/Desktop/Edith RMark/INP Files/neches1.inp")
neches2=convert.inp("C:/Users/Michael/Desktop/Edith RMark/INP Files/neches2.inp")
neches3=convert.inp("C:/Users/Michael/Desktop/Edith RMark/INP Files/neches3.inp")
sabine1=convert.inp("C:/Users/Michael/Desktop/Edith RMark/INP Files/sabine1.inp")
sabine3=convert.inp("C:/Users/Michael/Desktop/Edith RMark/INP Files/sabine3.inp")

neches1.model=mark(neches1)
neches1.ddl=make.design.data(neches1)
neches1.ddl

#
neches1.processed<-process.data(neches1,model="POPAN") #preprocessing data
run.neches1.popan=function() # setting up function
{
  neches1.ddl=design.data(neches1.processed) # set up matrix for all parameters
  Phidot=list(formula=~1) # default value?
  Phitime=list(formula=~1) #default value?
  pdot=list(formula=~1)
  
  
}


## example montana.edu ##
# http://www.montana.edu/rotella/502/lab08RMark.html #

neches1=convert.inp("C:/Users/Michael/Desktop/Edith RMark/INP Files/neches1.inp", group.df = NULL)
head(neches1)

# Process data NOTE: for this data set, we need to provide the time
# intervals because they are not all the same length
neches1.pr = process.data(neches1, begin.time = 1, model = "POPAN", time.intervals = c(26, 1))

# Create default design data
neches1.ddl = make.design.data(neches1.pr)

# examine the design data
neches1.ddl

run.neches1.popan=function() {
  # Define range of models for each parameter type. Here, we only have a few
  # models as the labs focused more on how how work with the output than on
  # setting up a wide variety of competing models
  
  # Define model for Phi
  Phi.time=list(formula=~time)
  
  # Define range of models for p
  p.dot=list(formula=~1)
  p.time=list(formula=~time)
  
  # Define model for pent (probability of entry)
  pent.time=list(formula=~time)
  
  # Define model for N
  N.dot=list(formula=~1)
  
  # Run assortment of models
  Phi.time.p.dot.pent.time.N=mark(neches1.pr, neches1.ddl, model.parameters=list(Phi=Phi.time, p=p.dot, pent=pent.time, N=N.dot))
  
  # Note this initial run will penalize for too many parameters because we are
  # not adjusting the parameter count for confounded parameters
  Phi.time.p.time.pent.time.N=mark(neches1.pr,neches1.ddl,model.parameters=list(Phi=Phi.time, p=p.time, N=N.dot))
  
  # Here, we adjust the parameter count to the correct value
  Phi.time.p.time.pent.time.N=adjust.parameter.count(Phi.time.p.time.pent.time.N,2) #the parameter count is less than the columns of the design matrix
  
  # Return model table and list of models
  return(collect.models())
  
}

# Execute
neches1.popan.results=run.neches1.popan()

# Examine model-selection table
neches1.popan.results

# look at output from top model
# View the model names
names(neches1.popan.results)

# examine the output from top-ranked model (#1) and store top model in
# object with short name
top = neches1.popan.results$Phi.time.p.dot.pent.time.N

# Check what link functions were used for the parameters

# Notice that RMark used parameter-specific link functions that make sense
# here, i.e., the logit lin for Phi & p, the mlogit for pent, and a log link
# for N.
top$links

# look at summary of top model's output
summary(top, showall = FALSE)

# store and examine estimates of Phi, p, pent, and N

# First examine estimates of the real parameters to see how results are
# stored. We drop 'fixed' & 'note' columns, so we can use the 'round'
# command
round(top$results$real[, 1:4], 5)

# for Phi in top model there are 2 estimates
top.Phi = top$results$real[1:2, ]
top.Phi

# for p in top model there is 1 estimate and it's in the 3rd row of output
top.p = top$results$real[3, ]
top.p

# Store and examine the estimates of pent
top.pent = top$results$real[5:6, ]
round(top.pent[, 1:4], 5)

# Store estimate for N
top.N = top$results$real[4, ]
top.N