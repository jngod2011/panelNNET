
calc_hlayers <- function(parlist, X = X, param = param, fe_var = fe_var, nlayers = nlayers, convolutional, activation, biasVars){
# parlist = obj$parlist
# X = D
# param = P
# fe_var = obj$fe_var
# nlayers = length(obj$hidden_layers)-!is.null(obj$convolutional)# subtract off 1 when convolutional because "nlayers" doesn't include conv layer
# convolutional = obj$convolutional
# activation = obj$activation
# biasVars = B
  if (activation == 'tanh'){
    activ <- tanh
  }
  if (activation == 'logistic'){
    activ <- logistic
  }
  if (activation == 'relu'){
    activ <- relu
  }
  if (activation == 'lrelu'){
    activ <- lrelu
  }
  hlayers <- vector('list', nlayers)
  for (i in 1:(nlayers + !is.null(convolutional))){
    if (i == 1){D <- X} else {D <- hlayers[[i-1]]}
    D <- as.matrix(cbind(biasVars, D)) #add bias, including vars to interact each layer
    # make sure that the time-invariant variables pass through the convolutional layer without being activated
    if (is.null(convolutional) | i > 1){
      hlayers[[i]] <- activ(MatMult(D, parlist[[i]]))        
    } else {
      HL <- MatMult(D, parlist[[i]])
      HL[,1:(convolutional$N_TV_layers * convolutional$Nconv)] <- activ(HL[,1:(convolutional$N_TV_layers * convolutional$Nconv)])
      hlayers[[i]] <- HL
    }
  }
  colnames(hlayers[[i]]) <- paste0('nodes',1:ncol(hlayers[[i]]))
  if (!is.null(param)){#Add parametric terms to top layer
    hlayers[[i]] <- cbind(param, hlayers[[i]])
    colnames(hlayers[[i]])[1:ncol(param)] <- paste0('param',1:ncol(param))
  }
  if (is.null(fe_var)){#add intercept if no FEs
    hlayers[[i]] <- cbind(1, hlayers[[i]])
  }
  return(hlayers)
}
