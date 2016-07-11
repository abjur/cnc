#' Baixa uma página web qualquer usando PhantomJS
#'
#' Acessa uma URL, roda javascripts e baixa página usando PhantomJS.
#' Para funcionar, é necessário instalar o PhamtomJS
#' (\url{http://phantomjs.org/download.html}).
#'
#' @param url endereço web que se deseja baixar.
#' @param arq caminho do arquivo para baixar.
#'
#' @return retorna \code{TRUE} se der tudo certo e ele conseguir criar e deletar todos os arquivos temporários.
#'
#' @export
GET_pjs <- function(url, arq = 'arq.html') {
  tmp <- tempfile('scrape', fileext = '.js')
  writeLines(sprintf("var page = require('webpage').create();
    page.open('%s', function () {
        console.log(page.content); //page source
        phantom.exit();
    });", url), con = tmp)
  system(sprintf("phantomjs %s > %s", tmp, arq))
  file.remove(tmp)
}

cnc_pag <- function(p, path, parse = TRUE) {
  x <- 'http://www.cnj.jus.br/improbidade_adm/consultar_requerido.php?validar=form&rs=pesquisarRequeridoGetTabela&rst=&rsrnd=0&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=I&rsargs[]=0&rsargs[]=POSICAO_INICIAL_PAGINACAO_PHP%s&rsargs[]=QUANTIDADE_REGISTROS_PAGINACAO15'
  u <- sprintf(x, as.character((p - 1) * 15))
  arq <- sprintf('%s/pag_%05d.html', path, p)
  if (!file.exists(arq)) {
    r <- httr::GET(u, httr::write_disk(arq, overwrite = TRUE))
    tibble::data_frame(result = 'OK')
  } else {
    tibble::data_frame(result = 'arquivo existe')
  }
}

#' Baixa páginas do CNC
#'
#' Acessa todas as páginas do CNC indicadas e salva em arquivos HTML. Acessa páginas do tipo
#' \url{http://www.cnj.jus.br/improbidade_adm/consultar_requerido.php?validar=form&rs=pesquisarRequeridoGetTabela&rst=&rsrnd=0&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=I&rsargs[]=0&rsargs[]=POSICAO_INICIAL_PAGINACAO_PHP1&rsargs[]=QUANTIDADE_REGISTROS_PAGINACAO15}
#'
#' @param path caminho da pasta onde os arquivos HTML serão salvos. Se a pasta não existir, será criada.
#' @param pags páginas a serem baixadas. Default 1:2399 (não atualiza com muita frequênica)
#'
#' @return \code{data.frame} indicando "OK" se baixou corretamente e "arquivo existe" se o arquivo já existe.
#'
#' @export
cnc_pags <- function(path = 'data-raw/pags', pags = 1:2399) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  tibble::data_frame(pag = pags) %>%
    dplyr::group_by(pag) %>%
    dplyr::do(cnc_pag(.$pag, path)) %>%
    dplyr::ungroup()
}

#' Baixa pessoas
#'
#' Baixa HTMLs de pesquisas do tipo
#' \url{http://www.cnj.jus.br/improbidade_adm/visualizar_condenacao.php?seq_condenacao=1101}
#'
#' @param dados retornados pela função \code{\link{parse_pags}}.
#' @inheritParams cnc_pags
#'
#' @return \code{data.frame} indicando "OK" se baixou corretamente e "arquivo existe" se o arquivo já existe.
#'
#' @export
cnc_pessoas <- function(dados, path = 'data-raw/pessoas') {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  dados %>%
    dplyr::filter(key == 'nm_pessoa') %>%
    dplyr::distinct(link) %>%
    dplyr::mutate(num_link = gsub('[^0-9]', '', link)) %>%
    dplyr::group_by(link, num_link) %>%
    dplyr::do({
      l <- paste0('http://www.cnj.jus.br/improbidade_adm/', .$link)
      a <- sprintf('%s/%s.html', path, .$num_link)
      if (!file.exists(a)) {
        # httr::GET(l, httr::write_disk(a, overwrite = TRUE))
        GET_pjs(l, a)
        tibble::data_frame(result = 'OK')
      } else {
        tibble::data_frame(result = 'ja existe')
      }
    }) %>%
    dplyr::ungroup()
}

#' Baixa processos
#'
#' Baixa HTMLs de pesquisas do tipo
#' \url{http://www.cnj.jus.br/improbidade_adm/visualizar_processo.php?seq_processo=9421}
#'
#' @param dados retornados pela função \code{\link{parse_pags}}.
#' @inheritParams cnc_pags
#'
#' @return \code{data.frame} indicando "OK" se baixou corretamente e "arquivo existe" se o arquivo já existe.
#'
#' @export
cnc_processos <- function(dados, path = 'data-raw/processos') {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  dados %>%
    dplyr::filter(key == 'num_processo') %>%
    dplyr::distinct(link) %>%
    dplyr::mutate(num_link = gsub('[^0-9]', '', link)) %>%
    dplyr::group_by(link, num_link) %>%
    dplyr::do({
      l <- paste0('http://www.cnj.jus.br/improbidade_adm/', .$link)
      a <- sprintf('%s/%s.html', path, .$num_link)
      if (!file.exists(a)) {
        GET_pjs(l, a)
        # httr::GET(l, httr::write_disk(a, overwrite = TRUE))
        tibble::data_frame(result = 'OK')
      } else {
        tibble::data_frame(result = 'ja existe')
      }
    }) %>%
    dplyr::ungroup()
}

#' Baixa infos de pessoas
#'
#' Baixa HTMLs de pesquisas do tipo
#' \url{http://www.cnj.jus.br/improbidade_adm/visualizar_condenacao.php?seq_condenacao=1&rs=getDadosParte&rst=&rsrnd=0&rsargs[]=1}
#'
#' @param dados retornados pela função \code{\link{parse_pessoas}}.
#' @inheritParams cnc_pags
#'
#' @return \code{data.frame} indicando "OK" se baixou corretamente e "arquivo existe" se o arquivo já existe.
#'
#' @export
cnc_pessoas_infos <- function(d_pessoas, path = 'data-raw/pessoas_infos') {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  id_pessoa <- d_pessoas %>%
    dplyr::filter(stringr::str_detect(value, 'recuperarDados')) %>%
    with(gsub('[^0-9]', '', value))
  l <- 'http://www.cnj.jus.br/improbidade_adm/visualizar_condenacao.php?seq_condenacao=1&rs=getDadosParte&rst=&rsrnd=0&rsargs[]=%s'
  l <- sprintf(l, id_pessoa)
  tibble::data_frame(id = id_pessoa, link = l) %>%
    dplyr::group_by(id, link) %>%
    dplyr::do({
      a <- sprintf('%s/%s.html', path, .$id)
      if (!file.exists(a)) {
        httr::GET('http://www.cnj.jus.br/improbidade_adm/visualizar_condenacao.php?seq_condenacao=1')
        httr::GET(.$link, httr::write_disk(a, overwrite = TRUE))
        tibble::data_frame(result = 'OK')
      } else {
        tibble::data_frame(result = 'ja existe')
      }
    }) %>%
    dplyr::ungroup()
}

