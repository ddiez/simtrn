#' Simulation of transcriptional regulatory networks (TRN)
#'
#' @param n number of samples (data points) to generate.
#' @param nreg number of regulators.
#' @param ngene number of genes.
#' @param seed seed for random number generator.
#' @param reg_info data.frame or matrix with information about regulator's mean
#' and variance expression levels.
#' @param mod_info matrix with information about TRN
#'
#' @return a list with network information.
#' @export
#'
sim_trn <- function(n = 10, nreg = 3, ngene = 3, seed = NULL, reg_info = NULL, mod_info = NULL) {
  if (!is.null(seed))
    set.seed(seed)

  regname <- paste0("R", seq_len(nreg))
  genename <- paste0("G", seq_len(ngene))

  # generate info for regulators if not found.
  if (is.null(reg_info)) {
    reg_info <- data.frame(
      regulator = regname,
      mean = 1,
      sd = 1,
      stringsAsFactors = FALSE
    )
  }

  # generate info for model if not found.
  if (is.null(mod_info)) {
    mod_info <- matrix(0, nrow = nreg, ncol = ngene)
    colnames(mod_info) <- genename
    rownames(mod_info) <- regname

    mod_info[] <- sample(0:5, length(mod_info), replace = TRUE)
  }

  # generate values for regulators.
  reg_value <- lapply(seq_len(nreg), function(k) {
    sim_reg(n, mean = reg_info[k, "mean"], sd = reg_info[k, "sd"])
  })
  names(reg_value) <- regname
  reg_value <- do.call(cbind, reg_value)

  # compute values for genes based on regulators and model.
  gene_value <- lapply(seq_len(ngene), function(k) {
    colSums(mod_info[, k] * t(reg_value))
  })
  names(gene_value) <- genename
  gene_value <- do.call(cbind, gene_value)

  list(mod_info = mod_info, reg_info = reg_info, reg_value = reg_value, gene_value = gene_value)
}

sim_reg <- function(n = 10, mean = 1, sd = 1) {
 rnorm(n, mean, sd)
}
