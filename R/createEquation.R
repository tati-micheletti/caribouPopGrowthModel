createEquation <- function(model = modelCoeff){
  howManyCovars <- length(model$Coefficient) - 1
  if (!"Intercept" %in% model$Coefficient[1])
    stop("The model intercept ('Intercept') couldn't be found in the table. Review your data.")
  coeffs <- model[Coefficient != "Intercept", Coefficient]
  eq <- model[Coefficient == "Intercept", Value]
  for (coef in coeffs){
    eq <- paste0(eq, paste0( " + ", coef, " * ", model[Coefficient == coef, Value]))
  }
  return(eq)
}
