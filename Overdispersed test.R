#Stoppa in modellen

overdisp_fun <- function(global.modell)
  ## number of variance parameters in an n-by-n variance-covariance matrix
  vpars <- function(m) 
    nrow(m) * (nrow(m) + 1)/2
 
  # The next two lines calculate the residual degrees of freedom
  model.df <- sum(sapply(VarCorr(global.modell), vpars)) + length(fixef(global.modell))
  rdf <- nrow(model.frame(global.modell)) - model.df
  # extracts the Pearson residuals
  rp <- residuals(global.modell, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  # Generates a p-value. If less than 0.05, the data are overdispersed.
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)


