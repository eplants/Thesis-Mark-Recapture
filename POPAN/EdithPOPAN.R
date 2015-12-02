## Edith Plants-Paris' Mark Recapture Project ##

# This is a practice run that follows a guide provided by Dr. Jay Rotella #
# http://www.montana.edu/rotella/502/lab08RMark.html #

# Requires both Program MARK and RMark #
# Requires the .inp mark-recapture files #

# Load RMark
library(RMark)

# Converts the .inp file for use with RMARK and shows the head
neches1=convert.inp("C:/Users/Michael/Desktop/Edith RMark/INP Files/neches1.inp", group.df = NULL)
head(neches1)

# Process data
# Time interval needs to be set, as there is a 1 year time difference between visits 1 & 2, and a 2 week interval between visits 2 & 3
# This also allows us to set the model type, in this case POPAN. Other desired models include LinkBarker and CJS
neches1.pr = process.data(neches1, begin.time = 1, model = "POPAN", time.intervals = c(26, 1))

# Create default design data from the processed data
neches1.ddl = make.design.data(neches1.pr)

# This allows us to examine the design data we just created
neches1.ddl

# This line sets up the function
run.neches1.popan=function() {
  
  # Define range of models for each parameter type
  
  # Define model for Phi, survival
  Phi.time=list(formula=~time)
  Phi.dot=list(formula=~1)
  
  # Define range of models for p, detection
  p.dot=list(formula=~1)
  p.time=list(formula=~time)
  
  # Define model for pent, probability of entry
  pent.time=list(formula=~time)
  pent.dot=list(formula=~1)
  
  # Define model for N, the "superpopulation"
  N.dot=list(formula=~1)
  N.time=list(formula=~time)
  
  # Run assortment of models
  Phi.time.p.dot.pent.time.N.time=mark(neches1.pr, neches1.ddl, model.parameters=list(Phi=Phi.time, p=p.dot, pent=pent.time, N=N.time))

  
  # Note this initial run will penalize for too many parameters because we are
  # not adjusting the parameter count for confounded parameters
  
  # Here, we adjust the parameter count to the correct value
  # The parameter count is less than the columns of the design matrix
 
  
  # Return model table and list of models
  return(collect.models())
  
}

# Execute
neches1.popan.results=run.neches1.popan()

# Examine model-selection table
neches1.popan.results

# Look at output from top model and view the model names
names(neches1.popan.results)

# examine the output from top-ranked model (#1) and store the top model in object with a short name
top = neches1.popan.results$Phi.time.p.dot.pent.time.N

# Check what link functions were used for the parameters
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