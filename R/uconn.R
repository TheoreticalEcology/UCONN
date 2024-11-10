#' UCONN - Unconstrained/Constrained Ordination Neural Network
#'
#' Fits a conditional variational autoencoder (cVAE) model to the given community matrix, optionally constrained/conditionally on covariates
#' to obtain latent representations that can be interpreted as an ordination. This model can be used for binary (presence-absence) or regression responses.
#'
#' @param Y Community matrix, columns correspond to the species
#' @param X A matrix or data frame of env variables. If `NULL`, only `Y` will be used as input. Factors will be one-hot encoded.
#' @param iterations An integer specifying the number of training iterations. Default is 200.
#' @param learning_rate A numeric value specifying the learning rate for model optimization. Default is 0.001.
#' @param l2 A numeric value specifying the L2 regularization penalty. Default is 0.0 (no regularization).
#' @param architecture A numeric vector specifying the number of units in each hidden layer of the model. Default is `c(50, 20, 10)`.
#' @param latent_dim An integer specifying the dimension of the latent space. Default is 2.
#' @param response A character string specifying the response type. Possible values are `"binary"` or `"regression"` for continuous outcomes. Default is `"binary"`.
#' @param KLD_weight A numeric value specifying the weight of the Kullback-Leibler divergence term in the loss function. Default is 5.0.
#'
#' @return An object of class `uconn` containing:
#'   \item{latent}{A matrix of latent representations of the data in the specified latent dimension.}
#'   \item{importance}{A matrix or data frame indicating the importance of the covariates (if `X` is provided).}
#'   \item{model}{A serialized model object for reuse or further analysis.}
#'
#' @examples
#' n = 100
#' sp = 10
#' e = 2
#' X = mvtnorm::rmvnorm(n, sigma = diag(1.0, e))
#' w = mvtnorm::rmvnorm(sp, sigma = diag(1.0, e))
#' sigma = cov2cor(rWishart(1, sp, Sigma = diag(1.0, sp))[,,1])
#' Y = 1*(X %*% t(w) + mvtnorm::rmvnorm(n, sigma = sigma) < 0 )
#'
#'
#' model = uconn(Y, X)
#' model
#' plot(model)
#'
#'
#' @export
uconn = function(Y,
                 X = NULL,
                 iterations = 200,
                 learning_rate = 0.001,
                 l2 = 0.0,
                 architecture = c(50, 20, 10),
                 latent_dim = 2L,
                 response = c("binary", "regression"),
                 KLD_weight = 2.0
) {

  out = list()

  if(!is.null(X)) X = stats::model.matrix(~0+., data = data.frame(X)) # onehot encode factors

  model = cVAE(Y = ncol(Y), Yc = ncol(X), hidden = architecture, latent_dim = latent_dim)
  model$fit(Y = Y, Yc = X, epochs = iterations, l2 = l2, KLD_weight = KLD_weight, lr = learning_rate, response = response)

  out$latent = model$predict(Y, X, type = "latent")
  if(!is.null(X)) out$importance = get_importance(model, Y, X, out$latent, )
  out$model = model
  out$model_serialized = torch::torch_serialize(model)
  class(out) = "uconn"
  return(out)
}



get_importance = function(model, Y, X, latent, n = 10) {

  result = matrix(NA, nrow = ncol(X), ncol = ncol(latent))
  for(l in 1:ncol(latent)) {
    for(i in 1:ncol(X)) {
      res = replicate(n, {
        Xtmp = X
        Xtmp[,i] = sample(Xtmp[,i])
        mean((model$predict(Y, Xtmp, type = "latent")[,l] - latent[,l])**2)
      })
      result[i, l]= mean(res)
    }
  }
  return(result)
}

#' Plot UCONN method
#'
#' @param x object of class "uconn"
#' @export
plot.uconn = function(x, ...) {
  pred = x$latent
  pred = apply(pred, 2, function(x) scale(x, scale = FALSE))
  plot(pred, cex = 0.0001, xlab = "Lat dim 1", ylab = "Lat dim 2")
  text(pred, labels = 1:nrow(x$latent), col = "darkgrey")
  # if(!is.null(x$importance)) {
  #   scale_factor = 0.5*(max(abs(pred)) / max(abs(x$importance)))
  #   for(i in 1:nrow(x$importance)) segments(x0 = 0, y0= 0, x=x$importance[i,1]*scale_factor, y1 =x$importance[i,2]*scale_factor, col = "red", lwd = 2)
  #   importances = x$importance*scale_factor
  #   importances[,1] = importances[,1] + 0.001
  #   text(importances, pos = 4, labels = 1:nrow(x$importance), col = "red")
  # }
}


#' print UCONN method
#'
#' @param x object of class "uconn"
#' @export
print.uconn = function(x, ...) {

  print_table = x$importance
  colnames(print_table) = paste0("latent_", 1:ncol(x$importance))
  rownames(print_table) = paste0("env_", 1:nrow(x$importance))
  print(print_table)
  return(invisible(print_table))
}


