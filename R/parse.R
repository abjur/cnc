#' Parse dos arquivos HTML baixados
#'
#' [cnc_parse_pag()] transforma arquivo HTML de uma página do CNC em um data.frame.
#'
#' @param arq caminho do arquivo que deve ser processado.
#'
#' @return Para [cnc_parse_pag()], uma `tibble` com as colunas
#'
#' \itemize{
#'   \item `id` id (1 a 15) do indivíduo obtido na página.
#'   \item `key` `"nm_pessoa"` ou `"num_processo"`, indicando se é o nome da pessoa ou número do processo.
#'   \item `value` nome da pessoa ou número do processo.
#'   \item `link` para acessar informações da pessoa ou do processo (info utilizada nos outros scrapers).
#' }
#'
#' @name parse
#'
#' @export
cnc_parse_pag <- function(arq) {

  aux_tab <- arq %>%
    xml2::read_html() %>%
    rvest::html_nodes('table') %>%
    dplyr::first()

  tb <- aux_tab %>%
    rvest::html_table(header = TRUE) %>%
    purrr::set_names(c('nm_pessoa', 'num_processo')) %>%
    dplyr::mutate(id = 1:n()) %>%
    tidyr::pivot_longer(-id, names_to = "key") %>%
    dplyr::mutate(
      value = stringr::str_replace_all(value, stringr::fixed('\\'), '@'),
      value = stringr::str_replace_all(value, "\n|\t|@.", ''),
      value = stringr::str_trim(value)
    )

  l0 <- aux_tab %>%
    xml2::xml_find_all('//a') %>%
    rvest::html_attr('href') %>%
    stringr::str_replace_all("'|\\\\", '')

  # links <- c(
  #   l0[stringr::str_detect(l0, '_condenacao')],
  #   l0[stringr::str_detect(l0, '_processo')]
  # )

  tb %>%
    tibble::as_tibble() %>%
    # dplyr::mutate(link = links)
    dplyr::mutate(link = l0)

}

#' Página de pessoa de processos para dados.
#'
#' [cnc_parse_pessoa()] transforma HTMLs de pesquisas do tipo
#' <http://www.cnj.jus.br/improbidade_adm/visualizar_processo.php?seq_condenacao=1101>
#' em um data.frame com dados estruturados.
#'
#'
#' @return Para [cnc_parse_pessoa()], uma `tibble` com as colunas
#' \itemize{
#'   \item `key` nome (label) da informação. Geralmente contém localização do processo, data da propositura e número do processo.
#'   \item `value` conteúdo da informação.
#' }
#'
#' @rdname parse
#'
#' @export
cnc_parse_pessoa <- function(arq) {
  # data de cadastro
  h <- xml2::read_html(arq)
  dt_cad <- h %>%
    xml2::xml_find_all(xpath = '//td[@class="td_form" and @width="20%"]//following-sibling::td[1]') %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    lubridate::dmy_hms()

  # dados do processo ----
  n_processo <- h %>%
    xml2::xml_find_all(xpath = '//td[@width="85%"]') %>%
    rvest::html_text() %>%
    stringr::str_trim()
  n_link <- h %>%
    xml2::xml_find_all(xpath = '//td[@width="85%"]//a') %>%
    xml2::xml_attr('href') %>%
    stringr::str_trim()
  tab_processo <- h %>%
    xml2::xml_find_all("//*[@id='hierarquia']//div") %>%
    xml2::xml_text() %>%
    stringr::str_replace_all("[[:space:]]+", " ") %>%
    stringr::str_trim() %>%
    stringr::str_split_fixed(':', 2) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    tibble::as_tibble() %>%
    purrr::set_names(c('key', 'value')) %>%
    dplyr::mutate(dplyr::across(.fns = stringr::str_squish)) %>%
    dplyr::add_row(key = 'n_processo', value = n_processo) %>%
    dplyr::add_row(key = 'link', value = n_link) %>%
    dplyr::filter(value != "")

  # dados da pessoa ----
  aux_tab_pessoa <- h %>%
    xml2::xml_find_all('//table[@width="700px" and @align="center"]//tr//table') %>%
    dplyr::last()
  tb <- rvest::html_table(aux_tab_pessoa, fill = TRUE) %>%
    setNames(c('nome', 'situacao', 'na')) %>%
    dplyr::select(nome, situacao) %>%
    dplyr::slice(2)
  link <- aux_tab_pessoa %>%
    xml2::xml_find_first('.//a') %>%
    xml2::xml_attr('onclick')
  tab_pessoa <- tb %>%
    dplyr::mutate(link = link) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "key")

  # dados da condenacao ----
  assunto <- h %>%
    xml2::xml_find_all("//script") %>%
    dplyr::last() %>%
    xml2::xml_text() %>%
    stringr::str_extract_all("(?<=addAssunto\\(')[^)]+") %>%
    purrr::flatten_chr() %>%
    stringr::str_split_fixed("','?", 3) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::transmute(assunto_cod = V1, assunto_nm = stringr::str_squish(V2)) %>%
    tibble::rowid_to_column() %>%
    tidyr::pivot_longer(-rowid) %>%
    tidyr::unite(key, name, rowid, sep = "_")

  tipo_pena <- h %>%
    xml2::xml_find_first('//input[@type="radio" and @checked="checked"]') %>%
    xml2::xml_attr('value')

  tipo_pena <- ifelse(
    tipo_pena == 'J',
    'Tr\u00e2nsito em julgado',
    '\u00d3rg\u00e3o colegiado'
  )

  aux_infos <- h %>%
    xml2::xml_find_all('//table[@width="700px" and @align="center"]//tr[not(@style="display: none;")]')
  indice <- aux_infos %>%
    xml2::xml_text() %>%
    stringr::str_which("INFORMA\u00c7\u00d5ES SOBRE A CONDENA\u00c7\u00c3O")
  aux_infos <- aux_infos[-seq_len(indice)]
  tab_condenacao <- purrr::map_dfr(aux_infos, montar_info) %>%
    dplyr::mutate(dplyr::across(.fns = stringr::str_squish)) %>%
    dplyr::filter(key != 'na', key != 'Tipo Julgamento') %>%
    dplyr::add_row(key = 'tipo_pena', value = tipo_pena) %>%
    dplyr::bind_rows(assunto)

  dplyr::bind_rows(tab_processo, tab_pessoa, tab_condenacao)
}

montar_info <- function(info) {

  key_span <- info %>%
    xml2::xml_find_first(".//span")

  key <- xml2::xml_attr(key_span, "id")
  if (is.na(key)) key <- xml2::xml_text(key_span)
  key <- janitor::make_clean_names(key)

  value <- info %>%
    xml2::xml_text() %>%
    stringr::str_squish()

  tibble::tibble(key, value)

}

#' Página de resultados de processos para dados.
#'
#' [cnc_parse_processo()] transforma HTMLs de pesquisas do tipo
#' <http://www.cnj.jus.br/improbidade_adm/visualizar_processo.php?seq_processo=9421>
#' em um data.frame com dados estruturados.
#'
#' @return Para [cnc_parse_processo()], uma `tibble` com as colunas
#' \itemize{
#'   \item `key` nome (label) da informação. Geralmente contém localização do processo, data da propositura e número do processo.
#'   \item `value` conteúdo da informação.
#' }
#'
#' @rdname parse
#'
#' @export
cnc_parse_processo <- function(arq) {
  h <- xml2::read_html(arq)

  tab_processo <- h %>%
    xml2::xml_find_all("//*[@id='hierarquia']//div") %>%
    xml2::xml_text() %>%
    stringr::str_replace_all("[[:space:]]+", " ") %>%
    stringr::str_trim() %>%
    stringr::str_split_fixed(':', 2) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    tibble::as_tibble() %>%
    purrr::set_names(c('key', 'value')) %>%
    dplyr::mutate(dplyr::across(.fns = stringr::str_squish))

  tab_condenacao <- h %>%
    xml2::xml_find_first('//table[@width="700px" and @align="center"]') %>%
    rvest::html_table(fill = TRUE) %>%
    dplyr::filter(stringr::str_detect(X1, 'Num. do|Data da')) %>%
    dplyr::select(key = X1, value = X2) %>%
    dplyr::mutate(key = gsub(':', '', key)) %>%
    tibble::as_tibble()

  dplyr::bind_rows(tab_processo, tab_condenacao) %>%
    dplyr::filter(value != "")
}

#' Página de resultados de infos de pessoa para dados.
#'
#' [cnc_parse_pessoa_infos()] transforma HTMLs de pesquisas do tipo
#' <http://www.cnj.jus.br/improbidade_adm/visualizar_condenacao.php?seq_condenacao=1&rs=getDadosParte&rst=&rsrnd=0&rsargs[]=1>
#' em um data.frame com dados estruturados.
#'
#' @return Para [cnc_parse_pessoa_infos()], uma `tibble` com as colunas
#' \itemize{
#'   \item `key` nome (label) da informação. Geralmente contém id da pessoa, tipo de pessoa, nome, sexo, funcionário público (S ou N) e código. Se for público, informa também esfera, órgão, cargo, uf.
#'   \item `value` conteúdo da informação.
#' }
#'
#' @rdname parse
#'
#' @export
cnc_parse_pessoa_infos <- function(arq) {

  h <- xml2::read_html(arq)

  s <- h %>%
    xml2::xml_text() %>%
    stringr::str_match('res = \'(.*?)\';') %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    with(V2) %>%
    stringr::str_split(',') %>%
    unlist()

  tibble::tibble(
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
    dplyr::mutate(
      uf = ifelse(publico == 'N', '', uf),
      sexo = ifelse(tipo_pessoa == 'J', '', tipo_pessoa)
    ) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "key")
}
