## Edith Plants-Paris' Mark Recapture Project ##

# This is a practice run that follows a guide provided by Dr. Jay Rotella #
# http://www.montana.edu/rotella/502/lab08RMark.html #

# Requires both Program MARK and RMark #
# Requires the .inp mark-recapture files #

# Create a file directory ?#

# Load RMark
library(RMark)

# Converts the .inp file for use with RMARK
sabine3=convert.inp("C:/Users/Michael/Desktop/Edith RMark/INP Files/sabine3.inp", group.df = NULL) 

# Shows the head
head(sabine3)

# Process data
# Time interval needs to be set, as there is a 1 year time difference between visits 1 & 2, and a 2 week interval between visits 2 & 3
# This also allows us to set the model type, in this case LinkBarker. Other desired models include POPAN and CJS
sabine3.pr = process.data(sabine3, begin.time = 1, model = "LinkBarker", time.intervals = c(26, 1))

# Create default design data from the processed data
sabine3.ddl = make.design.data(sabine3.pr)

# Examine the design data we just created
sabine3.ddl

# Sets up the function
run.sabine3.LinkBarker=function() {
  
  #
  sabine3.processed=process.data(sabine3,model="LinkBarker")
  # Define range of models for each parameter type
  # .time extension = time dependent
  # .dot extension = time independent
  
  # # Define model for Phi, survival
  Phi.time=list(formula=~time)
  Phi.dot=list(formula=~1)
  
  # Define range of models for p, detection
  p.dot=list(formula=~1)
  p.time=list(formula=~time)
  
  # Define model for f, recruitment: births and immigration
  # Additional choices for f?
  # Cannot use Pradel, as it assumes no loss (deaths) on capture
  f.dot=list(formula=~1)
  f.time=list(formula=~time)

  # Run assortment of models
  Phi.time.p.time.f.time=mark(sabine3.pr, sabine3.ddl, model.parameters=list(Phi=Phi.time, p=p.time, f=f.time))
  
  Phi.time.p.time.f.dot=mark(sabine3.pr, sabine3.ddl, model.parameters=list(Phi=Phi.time, p=p.time, f=f.dot))
  
  Phi.time.p.dot.f.dot=mark(sabine3.pr, sabine3.ddl, model.parameters=list(Phi=Phi.time, p=p.dot, f=f.dot))
  
  Phi.dot.p.dot.f.dot=mark(sabine3.pr, sabine3.ddl, model.parameters=list(Phi=Phi.dot, p=p.dot, f=f.dot))
  
  Phi.dot.p.dot.f.time=mark(sabine3.pr, sabine3.ddl, model.parameters=list(Phi=Phi.dot, p=p.dot, f=f.time))
  
  Phi.dot.p.time.f.time=mark(sabine3.pr, sabine3.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, f=f.time))
  
  Phi.dot.p.time.f.dot=mark(sabine3.pr, sabine3.ddl, model.parameters=list(Phi=Phi.dot, p=p.time, f=f.dot))
  
  Phi.time.p.dot.f.time=mark(sabine3.pr, sabine3.ddl, model.parameters=list(Phi=Phi.time, p=p.dot, f=f.time))
  
  # Note this initial run will penalize for too many parameters because we are
  # not adjusting the parameter count for confounded parameters
  
  
  # Here, we adjust the parameter count to the correct value
  # The parameter count is less than the columns of the design matrix
  
  
  # Return model table and list of models
  return(collect.models())
  
}

# Execute
sabine3.LinkBarker.results=run.sabine3.LinkBarker()

# Examine model-selection table
sabine3.LinkBarker.results

# Look at output from top model and view the model names
names(sabine3.LinkBarker.results)

# examine the output from top-ranked model (#1) and store the top model in object with a short name
top = sabine3.LinkBarker.results$Phi.time.p.dot.f.time

# Check what link functions were used for the parameters
top$links

# look at summary of top model's output
summary(top, showall = FALSE)


# Export the data files and models for use in Program MARK
# Desired output in console is NULL
sabine3.processed=process.data(sabine3)
export.MARK(sabine3.processed, "sabine3project", sabine3.LinkBarker.results)



