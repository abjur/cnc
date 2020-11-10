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
#' Tidyfica base que vem de parse_cnc_pags.
#'
#' @param cnc_pags base de dados raw das paginas do CNC.
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
    dplyr::select(arq_pag, id_pag, id_condenacao, id_processo, lab_pessoa,
                  lab_processo, link_condenacao, link_processo)
  cnc_pags_tidy
}

#' Tidyfica base que vem de parse_cnc_processos
#'
#' Tidyfica base que vem de parse_cnc_processos.
#'
#' @param cnc_processos base de dados raw dos processos.
#'
#' @import dplyr
#' @importFrom lubridate dmy
#' @importFrom lubridate dmy_hms
#' @import stringr
#' @import tidyr
#' @export
tidy_processos <- function(cnc_processos) {
  cnc_processos_spr <- cnc_processos %>%
    mutate(key = tidy_nm(key)) %>%
    group_by(arq_processo, key) %>%
    summarise(value = paste(value, collapse = '\n')) %>%
    ungroup() %>%
    filter(key != "") %>%
    spread(key, value) %>%
    select(-`4ra`)
  # setNames(c('arq', 'subsecao', names(.)[-c(1, 2)]))
  cnc_processos_tidy <- cnc_processos_spr %>%
    unite(secao, secao_judiciaria, subsecao, sep = '\n') %>%
    mutate(secao = if_else(secao == 'NA\nNA', NA_character_, secao)) %>%
    mutate(secao = if_else(is.na(secao), comarca, secao))

  cnc_processos_tidy %<>%
    mutate(instancia = if_else(
      !is.na(`1_grau_justica_estadual`) |
        !is.na(`1_grau_justica_federal`),
      '1 grau',
      if_else(
        !is.na(`2_grau_justica_estadual`) |
          !is.na(`2_grau_justica_federal`),
        '2 grau',
        if_else(!is.na(auditoria_militar), 'militar', 'superior')
      )
    )) %>%
    unite(tribunal, tribunal_de_justica_estadual:tribunal_superior) %>%
    mutate(
      tribunal = str_replace_all(tribunal, '_NA|NA_', ''),
      tribunal = if_else(tribunal == 'NA', NA_character_, tribunal)
    ) %>%
    unite(vara_camara,
          starts_with('gabinete'),
          starts_with('varas'),
          auditoria_militar) %>%
    mutate(
      vara_camara = str_replace_all(vara_camara, '_NA|NA_', ''),
      vara_camara = if_else(vara_camara == 'NA', NA_character_, vara_camara)
    ) %>%
    mutate(
      dt_propositura = dmy(data_da_propositura_da_acao),
      dt_cadastro = dmy_hms(data_da_informacao)
    ) %>%
    mutate(id_processo = str_match(arq_processo, "([0-9]+)\\.html$")[, 2]) %>%
    select(
      arq_processo,
      id_processo,
      dt_cadastro,
      n_processo = num_do_processo,
      esfera_processo = esfera,
      tribunal,
      instancia,
      comarca_secao = secao,
      vara_camara,
      dt_propositura
    )

  cnc_processos_tidy
}

#' Tidyfica base que vem de parse_cnc_pessoas, parse_cnc_pags e parse_cnc_processos
#'
#' Tidyfica base que vem de parse_cnc_pessoas, parse_cnc_pags e parse_cnc_processos.
#'
#' @param cnc_condenacoes base raw de condenações.
#' @param cnc_pags base raw das paginas.
#' @param cnc_processos base raw dos processos.
#'
#' @import dplyr
#' @importFrom lubridate dmy
#' @import stringr
#' @import tidyr
#' @import janitor
#'
#' @export
tidy_condenacoes <- function(cnc_condenacoes, cnc_pags, cnc_processos) {
  loc <- readr::locale(decimal_mark = ',', grouping_mark = '.')
  re_pena <-
    sprintf(
      'Anos%s([0-9]+)%sMeses%s([0-9]+)%sDias%s([0-9]+)',
      '[[:space:]]+',
      '[[:space:]]+',
      '[[:space:]]+',
      '[[:space:]]+',
      '[[:space:]]+',
      '[[:space:]]+'
    )
  re_pena_de <- 'De:?[[:space:]]+([0-9]{2}/[0-9]{2}/[0-9]{4})'
  re_pena_ate <- 'At\u00e9:?[[:space:]]+([0-9]{2}/[0-9]{2}/[0-9]{4})'
  calcula_pena <- function(pena_txt) {
    conta <- function(x) {
      x <- as.numeric(x)
      x[1] * 365 + x[2] * 30 + x[3]
    }
    apply(str_match(pena_txt, re_pena)[, c(2:4)], 1, conta)
  }
  cnc_condenacoes_spr <- cnc_condenacoes %>%
    mutate(
      key = tidy_nm(key),
      key = if_else(
        key == 'link' & str_detect(value, 'recuperarDados'),
        'link_pessoa',
        if_else(
          key == 'link' & str_detect(value, 'visualizar_pr'),
          'link_processo',
          key
        )
      )
    ) %>%
    filter(!key %in% unique(tidy_nm(cnc_processos$key))) %>%
    group_by(arq_pessoa, key) %>%
    summarise(value = paste(value, collapse = '\n')) %>%
    ungroup() %>%
    spread(key, value) %>%
    mutate_all(funs(suppressWarnings(na_if(., '')))) %>%
    mutate_all(funs(suppressWarnings(na_if(., 'NA')))) %>%
    remove_empty() %>%
    rename(arq = arq_pessoa)

  aux_pags <- cnc_pags %>%
    tidy_pags() %>%
    select(arq_pag, id_pag, id_condenacao, id_processo)

  cnc_condenacoes_tidy <- cnc_condenacoes_spr %>%
    mutate(
      id_pessoa = str_match(link_pessoa, "'([0-9]+)'")[, 2],
      id_condenacao = str_match(arq, "([0-9]+)\\.html$")[, 2]
    ) %>%
    mutate(
      dt_pena = dmy(label_data_julg_coleg)
      # dt_transito = dmy(data_do_transito_em_julgado),
      # dt_pena = if_else(is.na(dt_decisao), dt_transito, dt_decisao)
    ) %>%
    mutate(teve_inelegivel = tolower(inelegibilidade)) %>%
    mutate(
      teve_multa = if_else(
        str_detect(pagamento_de_multa, 'SIM'),
        'sim',
        pagamento_de_multa
      ),
      vl_multa = if_else(
        str_detect(pagamento_de_multa, 'SIM'),
        readr::parse_number(pagamento_de_multa, locale = loc),
        NA_real_
      )
    ) %>%
    rename(pena_txt = label_pena_privativa) %>%
    mutate(
      # pena_txt = if_else(
      #   is.na(pena_privativa_de_liberdade),
      #   pena_privativa_de_liberdade_aplicada,
      #   pena_privativa_de_liberdade
      # ),
      teve_pena = if_else(str_detect(pena_txt, 'SIM'), 'sim', pena_txt),
      duracao_pena_regex = calcula_pena(pena_txt),
      de_pena = dmy(str_match(pena_txt, re_pena_de)[, 2]),
      ate_pena = dmy(str_match(pena_txt, re_pena_ate)[, 2]),
      duracao_pena = as.numeric(ate_pena - de_pena)
    ) %>%
    mutate(
      perda_bens = perda_de_bens_ou_valores_acrescidos_ilicitamente_ao_patrimonio,
      teve_perda_bens = if_else(str_detect(perda_bens, 'SIM'), 'sim', perda_bens),
      vl_perda_bens = if_else(
        str_detect(perda_bens, 'SIM'),
        readr::parse_number(perda_bens, locale = loc),
        NA_real_
      )
    ) %>%
    mutate(teve_perda_cargo = tolower(perda_de_emprego_cargo_funcao_publica)) %>%
    rename(
      proibicao1 = proibicao_de_contratar_com_o_poder_publico_ou_receber_incentivos_fiscais_ou_crediticios_direta_ou_indiretamente_ainda_que_por_intermedio_de_pessoa_juridica_da_qual_seja_socio_majoritario,
      proibicao2 = proibicao_de_contratar_com_o_poder_publico_direta_ou_indiretamente_ainda_que_por_intermedio_de_pessoa_juridica_da_qual_seja_socio_majoritario,
      proibicao3 = proibicao_de_receber_incentivos_crediticios_direta_ou_indiretamente_ainda_que_por_intermedio_de_pessoa_juridica_da_qual_seja_socio_majoritario,
      proibicao4 = proibicao_de_receber_incentivos_fiscais_direta_ou_indiretamente_ainda_que_por_intermedio_de_pessoa_juridica_da_qual_seja_socio_majoritario
    ) %>%
    mutate(
      proibicao_txt = str_c(proibicao1, proibicao2, proibicao3, proibicao4, sep = " | "),
      teve_proibicao = if_else(str_detect(proibicao_txt, 'SIM'), 'sim',
                               proibicao_txt),
      duracao_proibicao_regex = calcula_pena(proibicao_txt),
      de_proibicao = dmy(str_match(proibicao_txt, re_pena_de)[, 2]),
      ate_proibicao = dmy(str_match(proibicao_txt, re_pena_ate)[, 2]),
      duracao_proibicao = as.numeric(ate_proibicao - de_proibicao)
    ) %>%
    mutate(
      ressarcimento = ressarcimento_integral_do_dano,
      teve_ressarcimento = if_else(str_detect(ressarcimento, 'SIM'), 'sim',
                                   ressarcimento),
      vl_ressarcimento = if_else(
        str_detect(ressarcimento, 'SIM'),
        readr::parse_number(ressarcimento, locale = loc),
        NA_real_
      )
    ) %>%
    mutate(
      suspensao_txt = suspensao_dos_direitos_politicos,
      teve_suspensao = if_else(str_detect(suspensao_txt, 'SIM'), 'sim',
                               suspensao_txt),
      duracao_suspensao_regex = calcula_pena(suspensao_txt),
      de_suspensao = dmy(str_match(suspensao_txt, re_pena_de)[, 2]),
      ate_suspensao = dmy(str_match(suspensao_txt, re_pena_ate)[, 2]),
      duracao_suspensao = as.numeric(ate_suspensao - de_suspensao),
      comunicacao_tse = if_else(
        str_detect(suspensao_txt, 'Comunica.+SIM'),
        'sim',
        NA_character_
      )
    ) %>%
    # separate(cod_assunto,
    #          paste('assunto_cod', 1:5, sep = '_'),
    #          sep = '\n',
    #          fill = 'right') %>%
    # separate(nm_assunto,
    #          paste('assunto_nm', 1:5, sep = '_'),
    #          sep = '\n',
    #          fill = 'right') %>%
    inner_join(aux_pags, 'id_condenacao') %>%
    select(
      arq_pag,
      id_pag,
      arq,
      id_condenacao,
      id_processo,
      id_pessoa,
      # infos condenacao
      tipo_pena,
      dt_pena,
      starts_with('assunto'),
      # teve tal coisa?
      starts_with('teve_'),
      # qual o valor?
      starts_with('vl_'),
      # duracao, de, at\032
      starts_with('duracao_'),
      starts_with('de_'),
      starts_with('ate_')
    )
  cnc_condenacoes_tidy
}

#' Tidyfica base que vem de parse_cnc_pessoas_infos
#'
#' @param cnc_pessoa_infos base raw cnc_pessoa_infos
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#' @export
tidy_pessoas <- function(cnc_pessoa_infos) {
  data(cadmun, package = 'abjData',  envir = environment())
  cnc_pessoa_tidy <- cnc_pessoa_infos %>%
    spread(key, value) %>%
    rename(id_pessoa = id) %>%
    mutate(across(.fns = ~na_if(.x, '')),
           across(.fns = ~na_if(.x, 'NA'))) %>%
    # mutate_all(funs(suppressWarnings(na_if(., '', 'NA')))) %>%
    select(arq_pessoa_infos, id_pessoa, tipo_pessoa, nm_pessoa,
           sexo, publico, esfera, orgao, cargo, uf, cod) %>%
    mutate(sexo = if_else(!sexo %in% c('F', 'M'), NA_character_, sexo)) %>%
    mutate(publico = if_else(!publico %in% c('S', 'N'), NA_character_, publico)) %>%
    mutate(esfera = if_else(!esfera %in% c('F', 'D', 'E', 'M'),
                            NA_character_, esfera)) %>%
    mutate(uf = if_else(!uf %in% unique(cadmun$uf), NA_character_, uf))
  cnc_pessoa_tidy
}

#' Tidyfica base que vem de todas as bases
#'
#'
#' @param cnc_condenacoes base raw cnc_condenacoes
#' @param cnc_pags base raw cnc_pags
#' @param cnc_processos base raw cnc_processos
#' @param cnc_pessoa_infos base raw cnc_pessoa_infos
#'
#' @import dplyr
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

  data(cadmun, package = 'abjData', envir = environment())
  data(pnud_uf, package = 'abjData', envir = environment())
  data(br_uf_map, package = 'abjData', envir = environment())

  cadmun %<>% distinct(cod, uf) %>% mutate_all(as.character)
  pnud_uf %<>% filter(ano == 2010) %>%
    select(uf, ufn, popt) %>%
    mutate(uf = as.character(uf)) %>%
    inner_join(cadmun, c('uf' = 'cod')) %>%
    select(id = uf.y, ufn, popt)

  regex_uf_estadual <- '\u00e7a d[eo] (Estado d[oae] )?(.+)$'
  regex_uf_federal <- pnud_uf %>%
    with(ufn) %>%
    {
      sprintf('(%s)|(%s)', ., abjutils::rm_accent(.))
    } %>%
    paste(collapse = '|') %>%
    regex(ignore_case = TRUE)

  ufs_estadual <- tidy_cnc %>%
    filter(esfera_processo == 'Estadual') %>%
    mutate(
      ufn_processo = str_match(tribunal, regex_uf_estadual)[, 3],
      ufn_processo = str_replace_all(ufn_processo, ' e dos T.+', '')
    ) %>%
    inner_join(pnud_uf, c('ufn_processo' = 'ufn')) %>%
    select(id_condenacao, uf_processo = id)

  ufs_federal_1inst <- tidy_cnc %>%
    filter(esfera_processo == 'Federal', instancia == '1 grau') %>%
    mutate(ufn_processo = str_match_all(comarca_secao, regex_uf_federal) %>%
             purrr::map_chr( ~ {
               x <- as.character(.x[, -1])
               x[x != ''][1]
             })) %>%
    mutate(ufn_processo = toupper(abjutils::rm_accent(ufn_processo))) %>%
    inner_join(mutate(pnud_uf, ufn_processo = toupper(abjutils::rm_accent(ufn))),
               'ufn_processo') %>%
    select(id_condenacao, uf_processo = id)

  tidy_cnc <- tidy_cnc %>%
    left_join(bind_rows(ufs_estadual, ufs_federal_1inst), 'id_condenacao') %>%
    ungroup()

  control_table <- tpur::control_table

  tabela_assuntos <-
    tpur::download_table("assunto", "estadual", "primeiro grau") %>%
    tpur::build_table()
  # monta a tabela de assuntos separado pra debuggar mais facil

  assuntos_penais <- tabela_assuntos %>%
    filter(str_detect(n1, regex("penal", ignore_case = T))) %>%
    select(dplyr::contains("n")) %>%
    gather(nivel, assunto) %>%
    filter(assunto != '') %>%
    distinct(assunto, .keep_all = T) %>%
    with(assunto) %>%
    abjutils::rm_accent() %>%
    str_to_lower() %>%
    str_trim()

  assuntos_penais_regex <- paste0("^", assuntos_penais, "$")
  assuntos_penais_regex[which(assuntos_penais_regex == "^roubo$")] <-
    "^roubo"
  assuntos_penais_regex[which(assuntos_penais_regex == "^furto$")] <-
    "^furto"
  assuntos_penais_regex[which(assuntos_penais_regex == "^estelionato$")] <-
    "^estelionato"
  assuntos_penais_regex[which(assuntos_penais_regex == "^peculato$")] <-
    "^peculato"

  assuntos_penais_regex <-
    c(
      assuntos_penais_regex,
      "crime",
      "trafico",
      "contra a flora",
      "contra a vida",
      "dano \\(art\\. 163\\)",
      "contra a dignidade sexual",
      "contra o meio ambiente",
      "contra a fauna",
      "extorsao",
      "concussao \\(art\\. 316, caput\\)",
      "contra as financas publicas",
      "desobediencia \\(art\\. 330\\)",
      "contra a ordem tributaria",
      "contra o patrimonio",
      "corrupcao",
      "contrabando",
      "direito penal militar",
      "apropriacao indebita \\(art\\. 168, caput\\)",
      "concussao \\(art\\. 316, caput\\)",
      "contra o ordenamento urbano e o patrimonio cultural",
      "homicidio"
    )

  assuntos_penais_regex <-
    paste0(assuntos_penais_regex, collapse = "|")

  tidy_cnc2 <- tidy_cnc %>%
    gather(rank, assunto, dplyr::contains("assunto_nm")) %>%
    filter(!is.na(assunto)) %>%
    mutate(
      assunto2 = assunto %>%
        str_trim() %>%
        str_to_lower() %>%
        abjutils::rm_accent(),
      penal_lgl = str_detect(assunto2, assuntos_penais_regex)
    ) %>%
    select(-assunto2) %>%
    group_by(arq) %>%
    mutate(
      assunto_penal_any = any(penal_lgl),
      assunto_penal_all = all(penal_lgl)
    ) %>%
    select(-penal_lgl) %>%
    ungroup() %>%
    distinct() %>%
    spread(rank, assunto) %>%
    # escape_unicode_df() %>%
    mutate_at(
      .funs = funs(ifelse(!is.na(.), TRUE, FALSE)),
      .vars = vars(
        teve_inelegivel,
        teve_multa,
        teve_pena,
        teve_perda_bens,
        teve_perda_cargo,
        teve_proibicao,
        teve_ressarcimento,
        teve_suspensao
      )
    )
}


#' Base tidy do CNC
#'
#' Banco de dados contendo 57 infos de uma base de 35,977 variaveis de interesse.
#'
#' @format A data frame with 35977 rows and 57 variables:
#' \describe{
#'    \item{arq_pag}{arq_pag}
#'    \item{id_pag}{id_pag}
#'    \item{arq}{arq}
#'    \item{id_condenacao}{id_condenacao}
#'    \item{id_processo}{id_processo}
#'    \item{id_pessoa}{id_pessoa}
#'    \item{tipo_pena}{tipo_pena}
#'    \item{dt_pena}{dt_pena}
#'    \item{assunto_cod_1}{assunto_cod_1}
#'    \item{assunto_cod_2}{assunto_cod_2}
#'    \item{assunto_cod_3}{assunto_cod_3}
#'    \item{assunto_cod_4}{assunto_cod_4}
#'    \item{assunto_cod_5}{assunto_cod_5}
#'    \item{teve_inelegivel}{teve_inelegivel}
#'    \item{teve_multa}{teve_multa}
#'    \item{teve_pena}{teve_pena}
#'    \item{teve_perda_bens}{teve_perda_bens}
#'    \item{teve_perda_cargo}{teve_perda_cargo}
#'    \item{teve_proibicao}{teve_proibicao}
#'    \item{teve_ressarcimento}{teve_ressarcimento}
#'    \item{teve_suspensao}{teve_suspensao}
#'    \item{vl_multa}{vl_multa}
#'    \item{vl_perda_bens}{vl_perda_bens}
#'    \item{vl_ressarcimento}{vl_ressarcimento}
#'    \item{duracao_pena_regex}{duracao_pena_regex}
#'    \item{duracao_proibicao_regex}{duracao_proibicao_regex}
#'    \item{duracao_suspensao_regex}{duracao_suspensao_regex}
#'    \item{duracao_pena}{duracao_pena}
#'    \item{duracao_proibicao}{duracao_proibicao}
#'    \item{duracao_suspensao}{duracao_suspensao}
#'    \item{de_pena}{de_pena}
#'    \item{de_proibicao}{de_proibicao}
#'    \item{de_suspensao}{de_suspensao}
#'    \item{ate_pena}{ate_pena}
#'    \item{ate_proibicao}{ate_proibicao}
#'    \item{ate_suspensao}{ate_suspensao}
#'    \item{arq_pessoa_infos}{arq_pessoa_infos}
#'    \item{tipo_pessoa}{tipo_pessoa}
#'    \item{nm_pessoa}{nm_pessoa}
#'    \item{sexo}{sexo}
#'    \item{publico}{publico}
#'    \item{esfera}{esfera}
#'    \item{orgao}{orgao}
#'    \item{cargo}{cargo}
#'    \item{uf}{uf}
#'    \item{cod}{cod}
#'    \item{arq_processo}{arq_processo}
#'    \item{dt_cadastro}{dt_cadastro}
#'    \item{n_processo}{n_processo}
#'    \item{esfera_processo}{esfera_processo}
#'    \item{tribunal}{tribunal}
#'    \item{instancia}{instancia}
#'    \item{comarca_secao}{comarca_secao}
#'    \item{vara_camara}{vara_camara}
#'    \item{dt_propositura}{dt_propositura}
#'    \item{uf_processo}{uf_processo}
#'    \item{assunto_penal_any}{assunto_penal_any}
#'    \item{assunto_penal_all}{assunto_penal_all}
#'    \item{assunto_nm_1}{assunto_nm_1}
#'    \item{assunto_nm_2}{assunto_nm_2}
#'    \item{assunto_nm_3}{assunto_nm_3}
#'    \item{assunto_nm_4}{assunto_nm_4}
#'    \item{assunto_nm_5}{assunto_nm_5}
#' }
#' @source <https://www.cnj.jus.br/improbidade_adm/consultar_requerido.php>
"tidy_cnc"



