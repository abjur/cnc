#' @import dplyr
#' @export
parse_cnc_pag <- function(arq) {
  arq %>%
    xml2::read_html() %>%
    rvest::html_nodes('table') %>%
    dplyr::first() %>% {
      h <- .
      tb <- h %>%
        rvest::html_table(header = TRUE) %>%
        setNames(c('nm_pessoa', 'num_processo')) %>%
        mutate(id = 1:n()) %>%
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

cnc_pag <- function(p, path, parse = TRUE) {
  x <- 'http://www.cnj.jus.br/improbidade_adm/consultar_requerido.php?validar=form&rs=pesquisarRequeridoGetTabela&rst=&rsrnd=0&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=&rsargs[]=I&rsargs[]=0&rsargs[]=POSICAO_INICIAL_PAGINACAO_PHP%s&rsargs[]=QUANTIDADE_REGISTROS_PAGINACAO15'
  u <- sprintf(x, as.character((p - 1) * 15))
  arq <- sprintf('%s/pag_%05d.html', path, p)
  if (!file.exists(arq)) {
    r <- httr::GET(u, httr::write_disk(arq, overwrite = TRUE))
    if (parse) {
      r %>% httr::content('text') %>% parse_cnc_pag()
    } else {
      r
    }
  } else {
    dplyr::data_frame(result = 'arquivo existe')
  }
}

cnc_pags <- function(path = 'data-raw/pags', parse = TRUE) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  dplyr::data_frame(pag = 1:2399) %>%
    dplyr::group_by(pag) %>%
    dplyr::do(cnc_pag(.$pag, path)) %>%
    dplyr::ungroup()
}

cnc_condenacao <- function(dados, path = 'data-raw/pessoas') {
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
        dplyr::data_frame(result = 'OK')
      } else {
        dplyr::data_frame(result = 'ja existe')
      }
    }) %>%
    dplyr::ungroup()
}

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

#' @import xml2
#' @import rvest
#' @import stringr
#' @import dplyr
#' @export
parse_condenacao_um <- function(arq) {
  # data de cadastro
  h <- read_html(arq)
  dt_cad <- h %>%
    html_nodes(xpath = '//td[@class="td_form" and @width="20%"]//following-sibling::td[1]') %>%
    html_text() %>%
    str_trim() %>%
    lubridate::dmy_hms()
  # dados do processo
  n_processo <- h %>%
    html_nodes(xpath = '//td[@width="85%"]') %>%
    html_text() %>%
    str_trim()
  n_link <- h %>%
    html_nodes(xpath = '//td[@width="85%"]//a') %>%
    html_attr('href') %>%
    str_trim()
  tab_processo <- h %>%
    html_nodes('#hierarquia > div') %>%
    html_text() %>%
    str_replace_all("[[:space:]]+", " ") %>%
    str_trim() %>%
    str_split_fixed(':', 2) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    tbl_df() %>%
    setNames(c('key', 'value')) %>%
    mutate_all(funs(str_trim(.))) %>%
    add_row(key = 'n_processo', value = n_processo) %>%
    add_row(key = 'link', value = n_link)
  # dados da pessoa
  tab_pessoa <- h %>%
    html_nodes(xpath = '//table[@width="700px" and @align="center"]//tr//table') %>%
    last() %>% {
      tb <- html_table(., fill = TRUE) %>%
        setNames(c('nome', 'situacao', 'na')) %>%
        select(nome, situacao) %>%
        slice(2)
      link <- html_node(., 'a') %>% html_attr('onclick')
      tb %>%
        mutate(link = link) %>%
        tidyr::gather()
    }
  # dados da condenacao
  assunto <- h %>%
    html_node('#listaAssuntos') %>% {
      x <- .
      nm_assunto <- html_nodes(x, '.textoAssunto') %>% html_text() %>% paste(collapse = '\n')
      cod_assunto <- html_nodes(x, 'input') %>% html_attr('value') %>% paste(collapse = '\n')
      data_frame(key = c('nm_assunto', 'cod_assunto'),
                 value = c(nm_assunto, cod_assunto))
    }
  tipo_pena <- h %>%
    html_node(xpath = '//input[@type="radio" and @checked="checked"]') %>%
    html_attr('value')
  tipo_pena <- ifelse(tipo_pena == 'J', 'Trânsito em julgado', 'Órgão colegiado')
  tab_condenacao <- h %>%
    html_nodes(xpath = '//table[@width="700px" and @align="center"]//tr[not(@style="display: none;")]') %>% {
      x <- .
      i <- x %>% html_text() %>% str_detect('INFORMAÇÕES SOBRE A CONDENAÇÃO') %>% which()
      x[(1+i):length(x)]
    } %>%
    lapply(function(x) {
      x %>%
        html_nodes('td') %>%
        html_text() %>%
        str_replace_all("[[:space:]]+|:", " ") %>%
        str_trim() %>%
        matrix(ncol = 2) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        tbl_df() %>%
        setNames(c('key', 'value'))
    }) %>%
    bind_rows() %>%
    mutate_all(funs(str_trim(.))) %>%
    filter(key != '', key != 'Tipo Julgamento') %>%
    add_row(key = 'tipo_pena', value = tipo_pena) %>%
    bind_rows(assunto)

  bind_rows(tab_processo, tab_pessoa, tab_condenacao)

}

cnc_processo <- function(dados, path = 'data-raw/processos') {
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
        dplyr::data_frame(result = 'OK')
      } else {
        dplyr::data_frame(result = 'ja existe')
      }
    }) %>%
    dplyr::ungroup()
}

parse_processo <- function() {

}

