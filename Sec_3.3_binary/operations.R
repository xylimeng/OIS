################# Internal Functions #################
multi <- function(X){
  n = nrow(X)
  p = ncol(X)
  X_tmp = matrix(0, nrow = n, ncol = choose(p,2))
  count = 1
  names = c()
  for(i in 1:(p-1)){
    for(j in (i+1):p){
      X_tmp[,count] = X[,i] * X[,j]
      names[count] = paste("(", colnames(X)[i], "*", colnames(X)[j], ")", sep = "")
      count = count + 1
    }
  }
  colnames(X_tmp) = names
  return(X_tmp)
}

divd <- function(X){
  n = nrow(X)
  p = ncol(X)
  X_tmp = matrix(0, nrow = n, ncol = choose(p,2))
  count = 1
  names = c()
  for(i in 1:(p-1)){
    for(j in (i+1):p){
      X_tmp[,count] = X[,i] / X[,j]
      names[count] = paste("(", colnames(X)[i], "/", colnames(X)[j], ")", sep = "")
      count = count + 1
    }
  }
  colnames(X_tmp) = names
  return(X_tmp)
}

add <- function(X){
  n = nrow(X)
  p = ncol(X)
  X_tmp = matrix(0, nrow = n, ncol = choose(p,2))
  count = 1
  names = c()
  for(i in 1:(p-1)){
    for(j in (i+1):p){
      X_tmp[,count] = X[,i] + X[,j]
      names[count] = paste("(", colnames(X)[i], "+", colnames(X)[j], ")", sep = "")
      count = count + 1
    }
  }
  colnames(X_tmp) = names
  return(X_tmp)
}

minus <- function(X){
  n = nrow(X)
  p = ncol(X)
  X_tmp = matrix(0, nrow = n, ncol = choose(p,2))
  count = 1
  names = c()
  for(i in 1:(p-1)){
    for(j in (i+1):p){
      X_tmp[,count] = X[,i] - X[,j]
      names[count] = paste("(", colnames(X)[i], "-", colnames(X)[j], ")", sep = "")
      count = count + 1
    }
  }
  colnames(X_tmp) = names
  return(X_tmp)
}

ABS_minus <- function(X){
  n = nrow(X)
  p = ncol(X)
  X_tmp = matrix(0, nrow = n, ncol = choose(p,2))
  count = 1
  names = c()
  for(i in 1:(p-1)){
    for(j in (i+1):p){
      X_tmp[,count] = abs(X[,i] - X[,j])
      names[count] = paste("|", colnames(X)[i], "-", colnames(X)[j], "|", sep = "")
      count = count + 1
    }
  }
  colnames(X_tmp) = names
  return(X_tmp)
}

ABS <- function(X){
  X_tmp = abs(X)
  p = ncol(X)
  names = c()
  for(i in 1:p){
    names[i] = paste("|", colnames(X)[i], "|", sep = "")
  }
  colnames(X_tmp) = names
  return(X_tmp)
}

SQRT <- function(X){
  X_tmp = sqrt(abs(X))
  p = ncol(X)
  names = c()
  for(i in 1:p){
    names[i] = paste(colnames(X)[i], "^0.5", sep = "")
  }
  colnames(X_tmp) = names
  return(X_tmp)
}

SQRE <- function(X){
  X_tmp = X^2
  p = ncol(X)
  names = c()
  for(i in 1:p){
    names[i] = paste(colnames(X)[i], "^2", sep = "")
  }
  colnames(X_tmp) = names
  return(X_tmp)
}

INV <- function(X){
  X_tmp = 1/X
  p = ncol(X)
  names = c()
  for(i in 1:p){
    names[i] = paste(colnames(X)[i], "^-1", sep = "")
  }
  colnames(X_tmp) = names
  return(X_tmp)
}

CUBE <- function(X){
  X_tmp = X^3
  p = ncol(X)
  names = c()
  for(i in 1:p){
    names[i] = paste(colnames(X)[i], "^3", sep = "")
  }
  colnames(X_tmp) = names
  return(X_tmp)
}

LOG <- function(X){
  X_tmp = log(abs(X))
  p = ncol(X)
  names = c()
  for(i in 1:p){
    names[i] = paste("log(", colnames(X)[i], ")", sep = "")
  }
  colnames(X_tmp) = names
  return(X_tmp)
}

EXP <- function(X){
  X_tmp = exp(X)
  p = ncol(X)
  names = c()
  for(i in 1:p){
    names[i] = paste("exp(", colnames(X)[i], ")", sep = "")
  }
  colnames(X_tmp) = names
  return(X_tmp)
}

SIN <- function(X){
  X_tmp = sin(pi*X)
  p = ncol(X)
  names = c()
  for(i in 1:p){
    names[i] = paste("sin(pi*", colnames(X)[i], ")", sep = "")
  }
  colnames(X_tmp) = names
  return(X_tmp)
}

COS <- function(X){
  X_tmp = cos(pi*X)
  p = ncol(X)
  names = c()
  for(i in 1:p){
    names[i] = paste("cos(pi*", colnames(X)[i], ")", sep = "")
  }
  colnames(X_tmp) = names
  return(X_tmp)
}

unary <- function(X){
  X = as.data.frame(X)
  X_SQRT = SQRT(X) # Square root
  X_SQRE = SQRE(X) # Square
  X_LOG = LOG(X)   # Log
  X_EXP = EXP(X)   # Exponential
  X_SIN = SIN(X)   # Sine
  X_COS = COS(X)   # Cosine
  X_ABS = ABS(X)   # Absolute value
  X_INV = INV(X)   # Inverse
  
  X_uni = cbind(X, X_SQRT, X_SQRE,
                X_LOG, X_EXP, X_ABS,
                X_SIN, X_COS, X_INV)
  X_uni = as.data.frame(t(na.omit(t(X_uni)))) # Remove NAs
  X_uni = remove_inf(X_uni)                   # Remove +-Inf
  X_uni = remove_dup(X_uni)                   # Remove duplicated columns
  return(X_uni)
}

binary <- function(X){
  X = as.data.frame(X)
  X_ADD = add(X)
  X_MINUS = minus(X)
  X_MULTI = multi(X)
  X_DIVD = divd(X)
  X_ABS_MINUS = ABS_minus(X)
  
  X_bi = cbind(X, X_ADD, X_MINUS, X_MULTI, X_DIVD, X_ABS_MINUS)
  X_bi = as.data.frame(t(na.omit(t(X_bi)))) # Remove NAs
  X_bi = remove_inf(X_bi)                   # Remove +-Inf
  X_bi = remove_dup(X_bi)                   # Remove duplicated columns
  return(X_bi)
}

remove_inf <- function(X){
  X = as.matrix(X)
  ind_inf = apply(X, 2, function(x) any(abs(x) == Inf))
  rm_ind = which(ind_inf == TRUE)
  if(length(rm_ind) > 0){
    X = X[,-rm_ind]
  }
  X = as.data.frame(X)
  return(X)
}

remove_corr <- function(X, y){
  X = as.matrix(X)
  corr = 0
  idx_remove = c()
  if(ncol(X) > 1){
    for(i in 1:(ncol(X)-1)){
      if(is.null(idx_remove) == FALSE){
        if(i %in% idx_remove){
          next
        }
      }
      for(j in (i+1):ncol(X)){
        corr = abs(cor(X[,i], X[,j]))
        if(corr > 0.9){
          mar_corr = apply(X[,c(i,j)], 2, function(x) abs(cor(x,y)))
          idx_tmp = which.min(mar_corr)
          if(idx_tmp == 1){
            idx_remove = c(idx_remove, i)
            break
          } else{
            idx_remove = c(idx_remove, j)
          }
        }
      }
    }
  }
  if(is.null(idx_remove) == FALSE){
    names = colnames(X)
    X = as.matrix(X[,-idx_remove])
    colnames(X) = names[-idx_remove]
  }
  return(X)
}

remove_dup <- function(X){
  # Remove X cols with duplicated data
  X = as.matrix(X)
  temp = round(X, digits = 8)
  dup_index = duplicated(temp, MARGIN = 2)
  X = X[,!dup_index]
  X = as.data.frame(X)
  return(X)
}