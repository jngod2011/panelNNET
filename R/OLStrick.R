
OLStrick_function <- function(parlist, hidden_layers, y, fe_var, lam, parapen){
  # parlist <- pnn$parlist
  # hidden_layers <- pnn$hidden_layers
  # y = pnn$y
  # fe_var <- pnn$fe_var
  # lam <- pnn$lam
  # parapen <- pnn$parapen
  # hidden_layers <- hlayers
  constraint <- sum(c(parlist$beta_param*parapen, parlist$beta)^2)
  #getting implicit regressors depending on whether regression is panel
  if (!is.null(fe_var)){
    Zdm <- demeanlist(as.matrix(hidden_layers[[length(hidden_layers)]]), list(fe_var))
    targ <- demeanlist(y, list(fe_var))
  } else {
    Zdm <- hidden_layers[[length(hidden_layers)]]
    targ <- y
  }
  #set up the penalty vector
  D <- rep(1, ncol(Zdm))
  if (is.null(fe_var)){
    pp <- c(0, parapen) #never penalize the intercept
  } else {
    pp <- parapen #parapen
  }
  D[1:length(pp)] <- D[1:length(pp)]*pp #incorporate parapen into diagonal of covmat
  # find implicit lambda
  b <- c(parlist$beta_param, parlist$beta)
  Zty <- MatMult(t(Zdm), targ)
  ZtZ <- MatMult(t(Zdm), Zdm)
#   newlam <-   1/constraint * MatMult(t(b), (Zty - MatMult(ZtZ,b)))
# # if (newlam<lam){stop("newlam")}
#   newlam1 <- max(lam, newlam) #dealing with the case where you're not constrained
  f <- function(lam){
    bi <- as.numeric(MatMult(solve(ZtZ + diag(D)*as.numeric(lam)), Zty))
    (crossprod(bi*D) - constraint)^2
  }
  o <- optim(par = lam, f = f, method = 'Brent', lower = lam, upper = 1e9)
  newlam2 <- o$par
  #New top-level params
  b <- tryCatch(as.numeric(MatMult(solve(ZtZ + diag(D)*as.numeric(newlam2)), Zty)),
                error = function(e)e)
  if (inherits(b, "error")){
    stop("ginv?")
    return(parlist)
    # b <- as.numeric(MatMult(ginv(ZtZ + diag(D)*as.numeric(newlam)), Zty))
  }
  parlist$beta_param <- b[1:length(parlist$beta_param)]
  parlist$beta <- b[(length(parlist$beta_param)+1):length(b)]
  return(parlist)
}


deactiv <- function(x){
  xx <- x
  xx[xx<0] <- xx[xx<0]*100
  return(xx)
}
constraint <- sum(parlist[[1]]^2)
targ <- deactiv(hlayers[[1]])
b <- parlist[[1]]
Z <- X
Zty <- MatMult(t(Z), targ)
ZtZ <- MatMult(t(Z), Z)
bn <- MatMult(solve(ZtZ), Zty)
sum(bn^2)
f <- function(lam){
  bi <- as.numeric(MatMult(solve(ZtZ + as.numeric(lam)), Zty))
  (crossprod(bi) - constraint)^2
}


y = a(XG)B



