require(torch)


cVAE = torch::nn_module(
  "cVAE",
  initialize = function(Y,
                        Yc = NULL,
                        hidden = c(50, 20, 10),
                        activation = "selu",
                        latent_dim = 2L,
                        dropout = 0.0
  ) {

    input = Y
    output = Y
    conditional = FALSE
    if(!is.null(Yc)) {
      input = Y + Yc
      conditional = TRUE
    }
    hidden = c(input, hidden)

    self$conditional = conditional
    self$latent_dim = latent_dim
    self$mu_indices = 1:latent_dim
    self$var_indices = (latent_dim+1) : (2*latent_dim)

    ### Encoder
    layers = list()
    counter = 1
    for(i in 2:length(hidden)) {
      if(i == 1) {
        layers[[counter]] = torch::nn_linear(hidden[1], hidden[i])
        counter = counter + 1
      } else {
        layers[[counter]] = torch::nn_linear(hidden[i-1], hidden[i])
        counter = counter + 1
      }
        layers[[counter]] = torch::nn_selu()
        counter = counter + 1
        layers[[counter]] = torch::nn_dropout(p = dropout)
        counter = counter + 1
    }
    layers[[length(layers)]] = torch::nn_linear(hidden[length(hidden)], 2*latent_dim)

    self$encoder = do.call(torch::nn_sequential, layers)

    ### Decoder
    if(conditional) hidden = c(latent_dim+Yc,rev(hidden[-1]))
    else hidden = c(latent_dim,rev(hidden[-1]))

    layers = list()
    counter = 1
    for(i in 2:length(hidden)) {
      if(i == 1) {
        layers[[counter]] = torch::nn_linear(hidden[1], hidden[i])
        counter = counter + 1
      } else {
        layers[[counter]] = torch::nn_linear(hidden[i-1], hidden[i])
        counter = counter + 1
      }
      layers[[counter]] = torch::nn_selu()
      counter = counter + 1
      layers[[counter]] = torch::nn_dropout(p = dropout)
      counter = counter + 1
    }
    layers[[length(layers)]] = torch::nn_linear(hidden[length(hidden)], output)
    self$decoder = do.call(torch::nn_sequential, layers)

  },

  reparameterize = function(latent) {
    mu = latent[,self$mu_indices]
    logvar = latent[,self$var_indices]$exp()
    std = torch::torch_exp(0.5 *logvar)
    eps = torch::torch_randn_like(std)
    self$KLD = -0.5 * torch::torch_sum(1 + logvar - mu$pow(2.0) - logvar$exp())
    return(eps$mul(std)$add_(mu))
  },


  forward = function(Y, Yc = NULL) {
    self$train()
    if(!is.null(Yc)) input = torch::torch_cat(list(Y, Yc), dim = -1)
    else input = Y
    latent = self$encoder(input)
    z = self$reparameterize(latent)

    if(self$conditional) {
      z = torch::torch_cat(list(z, Yc), dim = -1)
    }
    decode = self$decoder(z)
    return(decode)
  },
  fit = function(Y, Yc = NULL, lr = 0.001, epochs = 100, batch_size = 0.5, l2 = 0.0, KLD_weight = 1.0, response = c("binary", "regression")) {
    if(self$conditional && is.null(Yc)) stop("Yc must be provided...")

    if(self$conditional) dataset = torch::tensor_dataset(torch::torch_tensor(Y, dtype=torch::torch_float32()),
                                                         torch::torch_tensor(Yc, dtype=torch::torch_float32()))
    else dataset = torch::tensor_dataset(torch::torch_tensor(Y, dtype=torch_float32()))

    batch_size = ceiling(batch_size*nrow(Y))

    DataLoader = torch::dataloader(dataset, batch_size = batch_size, shuffle = TRUE)

    optim = torch::optim_adam(self$parameters, lr = lr, weight_decay = l2)

    cli::cli_progress_bar(format = "Epoch: {cli::pb_current}/{cli::pb_total} {cli::pb_bar} ETA: {cli::pb_eta} Train: {mean_epoch_loss}",
                          total = epochs, clear = FALSE)

    loss_epoch = NULL

    response = match.arg(response)
    self$response = response

    if(response == "binary") loss_function = function(pred, true) torch::nnf_binary_cross_entropy(pred$sigmoid(), true, reduction = "sum")
    if(response == "regression") loss_function = function(pred, true) torch::nnf_mse_loss(pred, true, reduction = "sum")


    for(e in 1:epochs) {
      loss_epoch = NULL
      coro::loop(for ( b in DataLoader) {
        y = b[[1]]
        if(self$conditional) yc = b[[2]]
        else yc = NULL

        optim$zero_grad()
        predictions = self$forward(y, yc)
        loss = (loss_function(predictions, y) + KLD_weight*self$KLD) / nrow(y)

        loss$backward()
        optim$step()
        loss_epoch = c(loss_epoch, loss$item())
      })
      mean_epoch_loss = mean(loss_epoch)
      mean_epoch_loss = round(mean_epoch_loss, 4)
      cli::cli_progress_update()
    }
  },

  predict = function(Y, Yc=NULL, type = c("latent", "response")) {
    self$eval()
    type = match.arg(type)
    Y = torch::torch_tensor(Y, dtype=torch::torch_float32())
    if(self$conditional) Yc = torch::torch_tensor(Yc, dtype=torch::torch_float32())

    if(type=="latent") {
      predictions = self$encoder(torch::torch_cat(list(Y, Yc), dim = -1))[,self$mu_indices] |> as.matrix()
    } else {
      predictions = self$forward(Y, Yc)|> as.matrix()
      if(self$response == "binary") predictions = predictions$sigmoid()
    }
    return(predictions)
  }
  )
