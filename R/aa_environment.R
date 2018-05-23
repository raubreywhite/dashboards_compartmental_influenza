#' CONFIG_DIR
#' @export CONFIG_DIR
CONFIG_DIR <- new.env(parent = emptyenv())
CONFIG_DIR$DIR_TMP <- "/tmp/sykdomspuls_compartmental_influenza"
CONFIG_DIR$DIR_SRC <- system.file("src", package = "sykdomspulscompartmentalinfluenza")
CONFIG_DIR$DIR_DATA <- system.file("extdata", package = "sykdomspulscompartmentalinfluenza")

#' CONFIG_PAR
#' @export CONFIG_PAR
CONFIG_PAR <- new.env(parent = emptyenv())
CONFIG_PAR$beta <- 0.75
CONFIG_PAR$gamma <- 3
CONFIG_PAR$a <- 1.9
