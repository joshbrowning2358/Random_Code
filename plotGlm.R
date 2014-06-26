plotGlm = function(fit){
  if( !is(fit, "glm") )
    stop("fit must be a glm object!")
  if( !fit$family[1] %in% c("gaussian", "binomial") )
    warning("Error dist. isn't gaussian or binomial, raw coeff CI's returned.")
  
  library(ggplot2)
  library(reshape)
  
  coeff = data.frame( summary(fit)$coeff)
  colnames(coeff) = c("Estimate", "Std.Error", "t.value", "p.value")
  coeff$variable = rownames(coeff)
  coeff$groupVariable = coeff$variable #changed later for factor variables
  coeff$groupEst = coeff$Estimate #changed later for factor variables
  av = anova(fit)
  vars = rownames(av)[-1] #use anova() to extract the variables used
  factorVars = rownames(av)[av$Df>1][-1] #actually this is factor variables with >1 level
  for(fact in factorVars){
    matchFact = grepl( paste0("^",fact), coeff$variable )
    coeff$variable = gsub(paste0("^",fact), "", coeff$variable)
    coeff$groupVariable[matchFact] = fact
    coeff$groupEst[matchFact] = max(coeff$Estimate[matchFact])
  }
  coeff = coeff[order(coeff$groupEst),]
  filt = coeff$variable!=coeff$groupVariable
  coeff$variable[filt] = paste(coeff$groupVariable, ":", coeff$variable)[filt]
  coeff$variable = factor(coeff$variable, levels=coeff$variable)
  coeff$lab = formatC(coeff$Estimate, digits=2, format="e")
  coeff$lab = paste0(coeff$lab, ifelse(coeff$p.value<0.001,"***"
                               ,ifelse(coeff$p.value<0.01, "**"
                               ,ifelse(coeff$p.value<0.05, "*"
                               ,ifelse(coeff$p.value<.1,".","")))))
  coeff$Hi = coeff$Estimate + 2*coeff$Std.Error
  coeff$Lo = coeff$Estimate - 2*coeff$Std.Error
  if(fit$family[1]=="binomial"){
    coeff$Hi = exp(coeff$Hi)
    coeff$Lo = exp(coeff$Lo)
  }
  yLab = ifelse(fit$family[1]=="binomial", "Odds Ratio", "Coefficient")

  p = ggplot(coeff, aes(x=variable) ) +
    geom_bar(aes(y=Estimate, fill=Estimate>0), stat="identity", width=.4) +
    geom_errorbar(aes(ymax=Hi, ymin=Lo), width=.2) +
    geom_text(aes(x=variable, y=-.25, label=lab ), vjust=-2, hjust=0 ) +
    coord_flip() + guides(fill=F) + labs(x="", y=yLab)
  
  return(p)
}
