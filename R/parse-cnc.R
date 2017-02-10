parse_pag_um <- function(arq) {
  arq %>%
    xml2::read_html() %>%
    rvest::html_nodes('table') %>%
    dplyr::first() %>% {
      h <- .
      tb <- h %>%
        rvest::html_table(header = TRUE) %>%
        setNames(c('nm_pessoa', 'num_processo')) %>%
        dplyr::mutate(id = 1:n()) %>%
        tidyr::gather(key, value, -id) %>%
        dplyr::mutate(value = stringr::str_replace_all(value, stringr::fixed('\\'), '@'),
                      value = stringr::str_replace_all(value, "\n|\t|@.", ''),
                      value = stringr::str_trim(value))
      l <- h %>%
        rvest::html_nodes('a') %>%
        rvest::html_attr('href') %>%
        stringr::str_replace_all("'|\\\\", '') %>% {
          c(.[stringr::str_detect(., '_condenacao')],
            .[stringr::str_detect(., '_processo')])
        }
      tb %>%
        dplyr::mutate(link = l)
    }
}

#' Página do CNC para dados.
#'
#' Transforma arquivo HTML de uma página do CNC em um data.frame.
#'
#' @inheritParams dvec
#'
#' @return \code{data.frame} com as colunas
#' \itemize{
#'   \item \code{arq} nome do arquivo lido.
#'   \item \code{id} id (1 a 15) do indivíduo obtido na página.
#'   \item \code{key} \code{nm_pessoa} ou \code{num_processo}, indicando se é o nome da pessoa ou número do processo.
#'   \item \code{value} nome da pessoa ou número do processo.
#'   \item \code{link} para acessar informações da pessoa ou do processo (info utilizada nos outros scrapers).
#'   \item \code{result} indica se o algoritmo funcionou ('OK') ou deu erro ('erro')
#' }
#'
#' @export
parse_pags <- function(arqs, verbose = TRUE) {
  dvec(parse_pag_um, arqs, verbose)
}

parse_pessoa_um <- function(arq) {
  # data de cadastro
  h <- xml2::read_html(arq)
  dt_cad <- h %>%
    rvest::html_nodes(xpath = '//td[@class="td_form" and @width="20%"]//following-sibling::td[1]') %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    lubridate::dmy_hms()
  # dados do processo
  n_processo <- h %>%
    rvest::html_nodes(xpath = '//td[@width="85%"]') %>%
    rvest::html_text() %>%
    stringr::str_trim()
  n_link <- h %>%
    rvest::html_nodes(xpath = '//td[@width="85%"]//a') %>%
    rvest::html_attr('href') %>%
    stringr::str_trim()
  tr <- stringr::str_trim
  tab_processo <- h %>%
    rvest::html_nodes('#hierarquia > div') %>%
    rvest::html_text() %>%
    stringr::str_replace_all("[[:space:]]+", " ") %>%
    stringr::str_trim() %>%
    stringr::str_split_fixed(':', 2) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    tibble::as_data_frame() %>%
    setNames(c('key', 'value')) %>%
    dplyr::mutate_all(dplyr::funs(tr(.))) %>%
    dplyr::add_row(key = 'n_processo', value = n_processo) %>%
    dplyr::add_row(key = 'link', value = n_link)
  # dados da pessoa
  tab_pessoa <- h %>%
    rvest::html_nodes(xpath = '//table[@width="700px" and @align="center"]//tr//table') %>%
    dplyr::last() %>% {
      tb <- rvest::html_table(., fill = TRUE) %>%
        setNames(c('nome', 'situacao', 'na')) %>%
        dplyr::select(nome, situacao) %>%
        dplyr::slice(2)
      link <- rvest::html_node(., 'a') %>% rvest::html_attr('onclick')
      tb %>%
        dplyr::mutate(link = link) %>%
        tidyr::gather()
    }
  # dados da condenacao
  assunto <- h %>%
    rvest::html_node('#listaAssuntos') %>% {
      x <- .
      nm_assunto <- rvest::html_nodes(x, '.textoAssunto') %>% rvest::html_text() %>% paste(collapse = '\n')
      cod_assunto <- rvest::html_nodes(x, 'input') %>% rvest::html_attr('value') %>% paste(collapse = '\n')
      tibble::data_frame(key = c('nm_assunto', 'cod_assunto'),
                         value = c(nm_assunto, cod_assunto))
    }
  tipo_pena <- h %>%
    rvest::html_node(xpath = '//input[@type="radio" and @checked="checked"]') %>%
    rvest::html_attr('value')
  tipo_pena <- ifelse(tipo_pena == 'J',
                      'Tr\032nsito em julgado', '\032rg\032o colegiado')
  tab_condenacao <- h %>%
    rvest::html_nodes(xpath = '//table[@width="700px" and @align="center"]//tr[not(@style="display: none;")]') %>% {
      x <- .
      i <- x %>% rvest::html_text() %>%
        stringr::str_detect("INFORMA\032\032ES SOBRE A CONDENA\032\032O") %>%
        which()
      x[(1+i):length(x)]
    } %>%
    lapply(function(x) {
      x %>%
        rvest::html_nodes('td') %>%
        rvest::html_text() %>%
        stringr::str_replace_all("[[:space:]]+|:", " ") %>%
        stringr::str_trim() %>%
        matrix(ncol = 2) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        tibble::as_data_frame() %>%
        setNames(c('key', 'value'))
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate_all(funs(str_trim(.))) %>%
    dplyr::filter(key != '', key != 'Tipo Julgamento') %>%
    dplyr::add_row(key = 'tipo_pena', value = tipo_pena) %>%
    dplyr::bind_rows(assunto)

  dplyr::bind_rows(tab_processo, tab_pessoa, tab_condenacao)
}

#' Página de resultados de pessoas para dados.
#'
#' Transforma HTMLs de pesquisas do tipo
#' \url{http://www.cnj.jus.br/improbidade_adm/visualizar_condenacao.php?seq_condenacao=1101}
#' em um data.frame com dados estruturados.
#'
#' @inheritParams dvec
#'
#' @return \code{data_frame} com as colunas
#' \itemize{
#'   \item \code{arq} nome do arquivo.
#'   \item \code{key} nome (label) da informação. Geralmente contém infos da data de cadastro, dados do processo, dados da pessoa e dados da condenação.
#'   \item \code{value} conteúdo da informação.
#'   \item \code{result} indica se o algoritmo funcionou ('OK') ou deu erro ('erro')
#' }
#'
#' @export
parse_pessoas <- function(arqs, verbose = TRUE) {
  dvec(parse_pessoa_um, arqs, verbose)
}

parse_processo_um <- function(arq) {
  h <- xml2::read_html(arq)
  tab_processo <- h %>%
    rvest::html_nodes('#hierarquia > div') %>%
    rvest::html_text() %>%
    stringr::str_replace_all("[[:space:]]+", " ") %>%
    stringr::str_trim() %>%
    stringr::str_split_fixed(':', 2) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    tibble::as_data_frame() %>%
    setNames(c('key', 'value')) %>%
    dplyr::mutate_all(funs(str_trim(.)))

  tab_condenacao <- h %>%
    rvest::html_node(xpath = '//table[@width="700px" and @align="center"]') %>%
    rvest::html_table(fill = TRUE) %>%
    dplyr::filter(str_detect(X1, 'Num. do|Data da')) %>%
    dplyr::select(key = X1, value = X2) %>%
    dplyr::mutate(key = gsub(':', '', key))

  dplyr::bind_rows(tab_processo, tab_condenacao)
}

#' Página de resultados de processos para dados.
#'
#' Transforma HTMLs de pesquisas do tipo
#' \url{http://www.cnj.jus.br/improbidade_adm/visualizar_processo.php?seq_processo=9421}
#' em um data.frame com dados estruturados.
#'
#' @inheritParams dvec
#'
#' @return \code{data_frame} com as colunas
#' \itemize{
#'   \item \code{arq} nome do arquivo.
#'   \item \code{key} nome (label) da informação. Geralmente contém localização do processo, data da propositura e número do processo.
#'   \item \code{value} conteúdo da informação.
#'   \item \code{result} indica se o algoritmo funcionou ('OK') ou deu erro ('erro')
#' }
#'
#' @export
parse_processos <- function(arqs, verbose = TRUE) {
  dvec(parse_processo_um, arqs, verbose)
}

parse_infos_pessoa_um <- function(arq) {
  h <- xml2::read_html(arq)
  s <- h %>%
    rvest::html_text() %>%
    stringr::str_match('res = \'(.*?)\';') %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    with(V2) %>%
    stringr::str_split(',') %>%
    unlist()
  tibble::data_frame(
    id = s[1],
    tipo_pessoa = s[2],
    nm_pessoa = s[3],
    sexo = s[4],
    publico = s[5],
    esfera = s[6],
    orgao = s[7],
    cargo = s[8],
    uf = s[9],
    cod = s[12]
  ) %>%
    dplyr::mutate(uf = ifelse(publico == 'N', '', uf),
                  sexo = ifelse(tipo_pessoa == 'J', '', tipo_pessoa)) %>%
    tidyr::gather()
}

#' Página de resultados de infos de pessoa para dados.
#'
#' Transforma HTMLs de pesquisas do tipo
#' \url{http://www.cnj.jus.br/improbidade_adm/visualizar_condenacao.php?seq_condenacao=1&rs=getDadosParte&rst=&rsrnd=0&rsargs[]=1}
#' em um data.frame com dados estruturados.
#'
#' @inheritParams dvec
#'
#' @return \code{data_frame} com as colunas
#' \itemize{
#'   \item \code{arq} nome do arquivo.
#'   \item \code{key} nome (label) da informação. Geralmente contém id da pessoa, tipo de pessoa, nome, sexo, funcionário público (S ou N) e código. Se for público, informa também esfera, órgão, cargo, uf.
#'   \item \code{value} conteúdo da informação.
#'   \item \code{result} indica se o algoritmo funcionou ('OK') ou deu erro ('erro')
#' }
#'
#' @import stringr
#' @export
parse_infos_pessoas <- function(arqs, verbose = TRUE) {
  dvec(parse_infos_pessoa_um, arqs, verbose)
}
