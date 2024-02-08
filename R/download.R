#' Download CNC
#'
#' [cnc_npag()] acessa a pagina do CNC com uma consulta genérica e coleta o
#' número de páginas disponível para consulta.
#'
#' @return [cnc_npag()] retorna o número de páginas que podem ser
#' consultadas no CNC.
#'
#' @rdname download
#'
#' @export
cnc_npag <- function() {
  httr::GET("http://www.cnj.jus.br/improbidade_adm/consultar_requerido.php?validar=form&rs=pesquisarRequeridoGetTabela&rst=&rsrnd=0&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=I&rsargs[]=0&rsargs[]=POSICAO_INICIAL_PAGINACAO_PHP0&rsargs[]=QUANTIDADE_REGISTROS_PAGINACAO15") |>
    httr::content() |>
    xml2::xml_find_first(xpath = ".//body/table[2]//b[2]") |>
    xml2::xml_text() |>
    base::as.numeric()
}

#' Baixa página do CNC
#'
#' [cnc_download_pag()] acessa uma página do CNC indicada e salva em arquivos HTML.
#' Acessa páginas do tipo
#' <http://www.cnj.jus.br/improbidade_adm/consultar_requerido.php?validar=form&rs=pesquisarRequeridoGetTabela&rst=&rsrnd=0&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=I&rsargs[]=0&rsargs[]=POSICAO_INICIAL_PAGINACAO_PHP1&rsargs[]=QUANTIDADE_REGISTROS_PAGINACAO15>
#'
#' @param pag página a ser baixada.
#' @param path caminho da pasta onde os arquivos HTML serão salvos. Se a pasta não existir, será criada.
#'
#' @return As demais funções retornam o caminho do arquivo baixado.
#'
#' @name download
#'
#' @export
cnc_download_pag <- function(pag, path) {
  x <- "http://www.cnj.jus.br/improbidade_adm/consultar_requerido.php?validar=form&rs=pesquisarRequeridoGetTabela&rst=&rsrnd=0&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=I&rsargs[]=0&rsargs[]=POSICAO_INICIAL_PAGINACAO_PHP%s&rsargs[]=QUANTIDADE_REGISTROS_PAGINACAO15"
  u <- sprintf(x, as.character((pag - 1) * 15))
  fs::dir_create(path)
  arq <- sprintf("%s/%s.html", path, pag)
  if (!file.exists(arq)) {
    httr::GET(u) %>%
      httr::content() %>%
      readr::write_lines(arq)
  }
  arq
}

#' Baixa página de pessoa
#'
#' [cnc_download_pessoa()] baixa HTML de pesquisas do tipo
#' <http://www.cnj.jus.br/improbidade_adm/visualizar_condenacao.php?seq_condenacao=1101>
#'
#' @param link retornado pela função [cnc_parse_pag()].
#'
#' @rdname download
#'
#' @export
cnc_download_condenacao <- function(id_condenacao, path) {
  fs::dir_create(path)
  # num_link <- gsub('[^0-9]', '', link)
  f <- sprintf("%s/%s.html", path, id_condenacao)
  if (!file.exists(f)) {
    link <- paste0("http://www.cnj.jus.br/improbidade_adm/visualizar_condenacao.php?seq_condenacao=", id_condenacao)
    httr::GET(link) %>%
      httr::content() |>
      readr::write_lines(f)
  }
  f
}

#' Baixa página de processo
#'
#' [cnc_download_processo()] baixa HTML de pesquisas do tipo
#' <http://www.cnj.jus.br/improbidade_adm/visualizar_processo.php?seq_processo=9421>
#'
#' @rdname download
#'
#' @export
cnc_download_processo <- function(id_processo, path) {
  fs::dir_create(path)
  # num_link <- gsub('[^0-9]', '', link)
  f <- sprintf("%s/%s.html", path, id_processo)
  if (!file.exists(f)) {
    link <- paste0("http://www.cnj.jus.br/improbidade_adm/visualizar_processo.php?seq_processo=", id_processo)
    httr::GET(link) %>%
      httr::content() |>
      readr::write_lines(f)
  }
  f
}

#' Baixa infos de pessoa
#'
#' [cnc_download_pessoa_infos()] baixa HTML de pesquisas do tipo
#' <http://www.cnj.jus.br/improbidade_adm/visualizar_condenacao.php?seq_condenacao=1&rs=getDadosParte&rst=&rsrnd=0&rsargs[]=1>
#'
#' @rdname download
#'
#' @export
cnc_download_pessoa_infos <- function(id, path) {
  fs::dir_create(path)
  # num_link <- gsub("[^0-9]", "", link)
  f <- sprintf("%s/%s.html", path, id)
  if (!file.exists(f)) {
    link <- paste0(
      "http://www.cnj.jus.br/improbidade_adm/visualizar_condenacao.php?",
      "seq_condenacao=1&rs=getDadosParte&rst=&rsrnd=0&rsargs[]=",
      id
    )
    httr::GET(link) %>%
      httr::content() %>%
      readr::write_lines(f)
  }
  f
}
