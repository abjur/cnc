#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Vetorizando parsers
#'
#' Vetoriza um parser (função) para um vetor de arquivos.
#'
#' @param fun função a ser aplicada em cada arquivo.
#' @param arqs character vector dos caminhos de arquivos a serem transformados.
#' @param verbose se \code{TRUE} (default), mostra o arquivo que está sendo lido com probabilidade de 5\%.
#'
#' @export
dvec <- function(fun, arqs, verbose = TRUE) {
  f <- dplyr::failwith(data_frame(result = 'erro'), fun)
  tibble::data_frame(arq = arqs) %>%
    dplyr::distinct(arq) %>%
    dplyr::group_by(arq) %>%
    dplyr::do({
      if (runif(1) < .05 && verbose) print(.$arq)
      d <- f(.$arq)
      if (tibble::has_name(d, 'result')) d$result <- 'OK'
      d
    }) %>%
    dplyr::ungroup()
}
