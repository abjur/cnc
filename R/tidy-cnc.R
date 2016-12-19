tidy_nm <- function(x) {
  x %>%
    tolower() %>%
    abjutils::rm_accent() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all('[^0-9a-z]+', '_') %>%
    stringr::str_replace_all('_+', '_') %>%
    stringr::str_replace_all('^_|_$', '')
}

#' Tidyfica base que vem de parse_cnc_pags
#'
#' @export
tidy_pags <- function(cnc_pags) {
  cnc_pags_tidy <- cnc_pags %>%
    tidyr::unite(v, value, link, sep = '@@@') %>%
    tidyr::spread(key, v) %>%
    tidyr::separate(nm_pessoa, c('lab_pessoa', 'link_condenacao'), sep = '@@@') %>%
    tidyr::separate(num_processo, c('lab_processo', 'link_processo'), sep = '@@@') %>%
    dplyr::rename(id_pag = id) %>%
    dplyr::mutate(id_processo = stringr::str_extract(link_processo, '[0-9]+$'),
                  id_condenacao = stringr::str_extract(link_condenacao, '[0-9]+$')) %>%
    dplyr::select(arq, id_pag, id_condenacao, id_processo, lab_pessoa,
                  lab_processo, link_condenacao, link_processo)
  cnc_pags_tidy
}

#' Tidyfica base que vem de parse_cnc_processos
#'
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @import tidyr
#' @export
tidy_processos <- function(cnc_processos) {
  cnc_processos_spr <- cnc_processos %>%
    mutate(key = tidy_nm(key)) %>%
    group_by(arq, key) %>%
    summarise(value = paste(value, collapse = '\n')) %>%
    ungroup() %>%
    spread(key, value) %>%
    setNames(c('arq', 'subsecao', names(.)[-c(1,2)]))
  cnc_processos_tidy <- cnc_processos_spr %>%
    unite(secao, secao_judiciaria, subsecao, sep = '\n') %>%
    mutate(secao = if_else(secao == 'NA\nNA', NA_character_, secao)) %>%
    mutate(secao = if_else(is.na(secao), comarca, secao)) %>%
    mutate(instancia = if_else(
      !is.na(`1_grau_justica_estadual`) | !is.na(`1_grau_justica_federal`), '1 grau',
      if_else(
        !is.na(`2_grau_justica_estadual`) | !is.na(`2_grau_justica_federal`), '2 grau',
        if_else(!is.na(auditoria_militar), 'militar', 'superior'))
    )) %>%
    unite(tribunal, tribunal_de_justica_estadual:tribunal_superior) %>%
    mutate(tribunal = str_replace_all(tribunal, '_NA|NA_', ''),
           tribunal = if_else(tribunal == 'NA', NA_character_, tribunal)) %>%
    unite(vara_camara, starts_with('gabinete'),
          starts_with('varas'), auditoria_militar) %>%
    mutate(vara_camara = str_replace_all(vara_camara, '_NA|NA_', ''),
           vara_camara = if_else(vara_camara == 'NA', NA_character_, vara_camara)) %>%
    mutate(dt_propositura = dmy(data_da_propositura_da_acao),
           dt_cadastro = dmy_hms(data_da_informacao)) %>%
    mutate(id_processo = str_match(arq, "([0-9]+)\\.html$")[, 2]) %>%
    select(arq_processo = arq, id_processo, dt_cadastro,
           n_processo = num_do_processo,
           esfera_processo = esfera, tribunal, instancia, comarca_secao = secao,
           vara_camara, dt_propositura)
  cnc_processos_tidy
}

#' Tidyfica base que vem de parse_cnc_pessoas, parse_cnc_pags e parse_cnc_processos
#'
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @import tidyr
#' @import janitor
#' @export
tidy_condenacoes <- function(cnc_condenacoes, cnc_pags, cnc_processos) {
  loc <- readr::locale(decimal_mark = ',', grouping_mark = '.')
  re_pena <- sprintf('Anos%s([0-9]+)%sMeses%s([0-9]+)%sDias%s([0-9]+)',
                     '[[:space:]]+', '[[:space:]]+', '[[:space:]]+',
                     '[[:space:]]+', '[[:space:]]+', '[[:space:]]+')
  re_pena_de <- 'De[[:space:]]+([0-9]{2}/[0-9]{2}/[0-9]{4})'
  re_pena_ate <- 'Até[[:space:]]+([0-9]{2}/[0-9]{2}/[0-9]{4})'
  calcula_pena <- function(pena_txt) {
    conta <- function(x) {
      x <- as.numeric(x)
      x[1] * 365 + x[2] * 30 + x[3]
    }
    apply(str_match(pena_txt, re_pena)[, c(2:4)], 1, conta)
  }
  cnc_condenacoes_spr <- cnc_condenacoes %>%
    mutate(key = tidy_nm(key),
           key = if_else(key == 'link' & str_detect(value, 'recuperarDados'),
                         'link_pessoa',
                         if_else(key == 'link' & str_detect(value, 'visualizar_pr'),
                                 'link_processo', key))) %>%
    filter(!key %in% unique(tidy_nm(cnc_processos$key))) %>%
    group_by(arq, key) %>%
    summarise(value = paste(value, collapse = '\n')) %>%
    ungroup() %>%
    spread(key, value) %>%
    mutate_all(funs(suppressWarnings(convert_to_NA(., c('', 'NA'))))) %>%
    remove_empty_cols()

  aux_pags <- cnc_pags %>%
    tidy_pags() %>%
    select(arq_pag = arq, id_pag, id_condenacao, id_processo)

  cnc_condenacoes_tidy <- cnc_condenacoes_spr %>%
    mutate(id_pessoa = str_match(link_pessoa, "'([0-9]+)'")[, 2],
           id_condenacao = str_match(arq, "([0-9]+)\\.html$")[, 2]) %>%
    mutate(dt_decisao = dmy(data_da_decisao_do_orgao_colegiado),
           dt_transito = dmy(data_do_transito_em_julgado),
           dt_pena = if_else(is.na(dt_decisao), dt_transito, dt_decisao)) %>%
    mutate(teve_inelegivel = tolower(inelegibilidade)) %>%
    mutate(teve_multa = if_else(str_detect(pagamento_de_multa, 'SIM'), 'sim',
                                pagamento_de_multa),
           vl_multa = if_else(str_detect(pagamento_de_multa, 'SIM'),
                              readr::parse_number(pagamento_de_multa, locale = loc),
                              NA_real_)) %>%
    mutate(pena_txt = if_else(is.na(pena_privativa_de_liberdade),
                              pena_privativa_de_liberdade_aplicada,
                              pena_privativa_de_liberdade),
           teve_pena = if_else(str_detect(pena_txt, 'SIM'), 'sim', pena_txt),
           duracao_pena = calcula_pena(pena_txt),
           de_pena = dmy(str_match(pena_txt, re_pena_de)[, 2]),
           ate_pena = dmy(str_match(pena_txt, re_pena_ate)[, 2])) %>%
    mutate(perda_bens = perda_de_bens_ou_valores_acrescidos_ilicitamente_ao_patrimonio,
           teve_perda_bens = if_else(str_detect(perda_bens, 'SIM'), 'sim', perda_bens),
           vl_perda_bens = if_else(str_detect(perda_bens, 'SIM'),
                                   readr::parse_number(perda_bens, locale = loc),
                                   NA_real_)) %>%
    mutate(teve_perda_cargo = tolower(perda_de_emprego_cargo_funcao_publica)) %>%
    mutate(proibicao_txt = proibicao_de_contratar_com_o_poder_publico_ou_receber_incentivos_fiscais_ou_crediticios_direta_ou_indiretamente_ainda_que_por_intermedio_de_pessoa_juridica_da_qual_seja_socio_majoritario,
           teve_proibicao = if_else(str_detect(proibicao_txt, 'SIM'), 'sim',
                                    proibicao_txt),
           duracao_proibicao = calcula_pena(proibicao_txt),
           de_proibicao = dmy(str_match(proibicao_txt, re_pena_de)[, 2]),
           ate_proibicao = dmy(str_match(proibicao_txt, re_pena_ate)[, 2])) %>%
    mutate(ressarcimento = ressarcimento_integral_do_dano,
           teve_ressarcimento = if_else(str_detect(ressarcimento, 'SIM'), 'sim',
                                        ressarcimento),
           vl_ressarcimento = if_else(str_detect(ressarcimento, 'SIM'),
                                      readr::parse_number(ressarcimento, locale = loc),
                                      NA_real_)) %>%
    mutate(suspensao_txt = suspensao_dos_direitos_politicos,
           teve_suspensao = if_else(str_detect(suspensao_txt, 'SIM'), 'sim',
                                    suspensao_txt),
           duracao_suspensao = calcula_pena(suspensao_txt),
           de_suspensao = dmy(str_match(suspensao_txt, re_pena_de)[, 2]),
           ate_suspensao = dmy(str_match(suspensao_txt, re_pena_ate)[, 2]),
           comunicacao_tse = if_else(str_detect(suspensao_txt, 'Comunica.+SIM'),
                                     'sim', NA_character_)) %>%
    separate(cod_assunto, paste('assunto_cod', 1:5, sep = '_'),
             sep = '\n', fill = 'right') %>%
    separate(nm_assunto, paste('assunto_nm', 1:5, sep = '_'),
             sep = '\n', fill = 'right') %>%
    inner_join(aux_pags, 'id_condenacao') %>%
    select(arq_pag, id_pag, arq,
           id_condenacao, id_processo, id_pessoa,
           # infos condenacao
           tipo_pena, dt_pena, starts_with('assunto'),
           # teve tal coisa?
           starts_with('teve_'),
           # qual o valor?
           starts_with('vl_'),
           # duracao, de, até
           starts_with('duracao_'), starts_with('de_'), starts_with('ate_'))
  cnc_condenacoes_tidy
}

#' Tidyfica base que vem de parse_cnc_pessoas_infos
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#' @export
tidy_pessoas <- function(cnc_pessoa_infos) {
  data(cadmun, package = 'abjutils')
  cnc_pessoa_tidy <- cnc_pessoa_infos %>%
    spread(key, value) %>%
    rename(arq_pessoa = arq, id_pessoa = id) %>%
    mutate_all(funs(suppressWarnings(convert_to_NA(., c('', 'NA'))))) %>%
    select(arq_pessoa, id_pessoa, tipo_pessoa, nm_pessoa,
           sexo, publico, esfera, orgao, cargo, uf, cod) %>%
    mutate(sexo = if_else(!sexo %in% c('F', 'M'), NA_character_, sexo)) %>%
    mutate(publico = if_else(!publico %in% c('S', 'N'), NA_character_, publico)) %>%
    mutate(esfera = if_else(!esfera %in% c('F', 'D', 'E', 'M'),
                            NA_character_, esfera)) %>%
    mutate(uf = if_else(!uf %in% levels(cadmun$uf), NA_character_, uf))
  cnc_pessoa_tidy
}

#' Tidyfica base que vem de todas as bases
#'
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @import tidyr
#' @import janitor
#' @export
tidy_cnc <- function(cnc_condenacoes, cnc_pags, cnc_processos, cnc_pessoa_infos) {
  cnc1 <- tidy_condenacoes(cnc_condenacoes, cnc_pags, cnc_processos)
  cnc2 <- tidy_pessoas(cnc_pessoa_infos)
  cnc3 <- tidy_processos(cnc_processos)
  tidy_cnc <- cnc1 %>%
    inner_join(cnc2, 'id_pessoa') %>%
    inner_join(cnc3, 'id_processo')
  tidy_cnc
}


#' Base tidy do CNC
#'
#' Banco de dados contendo 57 infos de uma base de 35,977 variaveis de interesse.
#'
#' @format A data frame with 35977 rows and 57 variables:
#' \describe{
#'   \item{a}{a}
#'   \item{b}{b}
#'   ...
#' }
#' @source \url{https://www.cnj.jus.br/improbidade_adm/consultar_requerido.php}
"tidy_cnc"
