tidy_nm <- function(x) {
  x %>%
    tolower() %>%
    abjutils::rm_accent() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("[^0-9a-z]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
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
    tidyr::unite(v, value, link, sep = "@@@") %>%
    tidyr::spread(key, v) %>%
    tidyr::separate(nm_pessoa, c("lab_pessoa", "link_condenacao"), sep = "@@@") %>%
    tidyr::separate(num_processo, c("lab_processo", "link_processo"), sep = "@@@") %>%
    dplyr::rename(id_pag = id) %>%
    dplyr::mutate(
      id_processo = stringr::str_extract(link_processo, "[0-9]+$"),
      id_condenacao = stringr::str_extract(link_condenacao, "[0-9]+$")
    ) %>%
    dplyr::select(
      arq_pag, id_pag, id_condenacao, id_processo, lab_pessoa,
      lab_processo, link_condenacao, link_processo
    )
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
  sanitize <- cnc_processos |>
    purrr::map_df(\(c) {
      df <- c |>
        purrr::pluck("df")

      processo_id <- c |> purrr::pluck("processo_id")

      if (base::nrow(df) == 0) {
        return(tibble::tibble(processo_id = processo_id))
      }

      df |>
        dplyr::filter(stringr::str_length(key) > 0) |>
        dplyr::mutate(key = tidy_nm(key)) |>
        dplyr::group_by(key) |>
        dplyr::group_split() |>
        purrr::map_df(\(df_by_key) {
          if (base::nrow(df_by_key) > 1) {
            return(df_by_key |>
              dplyr::mutate(key = stringr::str_c(key, dplyr::row_number(), sep = "_")))
          }
          df_by_key
        }) |>
        tidyr::pivot_wider(names_from = key, values_from = value) |>
        tibble::add_column(processo_id = processo_id)
    })

  sanitize |>
    dplyr::transmute(
      instancia = dplyr::case_when(
        !is.na(`2_grau_justica_estadual`) | !is.na(`2_grau_justica_federal`) ~ "2 grau",
        !is.na(`1_grau_justica_estadual`) | !is.na(`1_grau_justica_federal`) ~ "1 grau",
        !is.na(`auditoria_militar`) ~ "militar",
        .default = "superior"
      ),
      tribunal = dplyr::case_when(
        !is.na(tribunal_de_justica_estadual) ~ tribunal_de_justica_estadual,
        !is.na(tribunal_regional_federal) ~ tribunal_regional_federal,
        !is.na(tribunal_militar_estadual) ~ tribunal_militar_estadual,
      ),
      esfera_processo = esfera,
      num_do_processo = num_do_processo,
      processo_id = processo_id,
      comarca = comarca,
      dt_propositura = lubridate::dmy(data_da_propositura_da_acao),
      dt_cadastro = lubridate::dmy_hms(data_da_informacao),
      esfera_gabinete_desembargador = dplyr::case_when(
        !is.na(gabinete_de_desembargador_estadual) ~ "Estadual",
        !is.na(gabinete_de_desembargador_federal) ~ "Federal"
      ),
      gabinete_desembargador = dplyr::case_when(
        !is.na(gabinete_de_desembargador_estadual) ~ gabinete_de_desembargador_estadual,
        !is.na(gabinete_de_desembargador_federal) ~ gabinete_de_desembargador_federal
      ),
      secao_judiciaria = secao_judiciaria,
      subsecao_1 = dplyr::case_when(
        !is.na(subsecao) ~ subsecao,
        !is.na(subsecao_1) ~ subsecao_1
      ),
      subsecao_2 = subsecao_2,
      esfera_vara_juizado = dplyr::case_when(
        !is.na(varas_e_juizados_federais) ~ "Federal",
        !is.na(varas_e_juizados_estaduais) ~ "Estadual"
      ),
      vara_juizados_1 = dplyr::case_when(
        !is.na(varas_e_juizados_federais) ~ varas_e_juizados_federais,
        !is.na(varas_e_juizados_estaduais) ~ varas_e_juizados_estaduais,
        !is.na(varas_e_juizados_federais_1) ~ varas_e_juizados_federais_1,
        !is.na(varas_e_juizados_estaduais_1) ~ varas_e_juizados_estaduais_1
      ),
      vara_juizados_2 = dplyr::case_when(
        !is.na(varas_e_juizados_federais_2) ~ varas_e_juizados_federais_2,
        !is.na(varas_e_juizados_estaduais_2) ~ varas_e_juizados_estaduais_2
      ),
      auditoria_militar = auditoria_militar
    )
}

#' Tidyfica base que vem de parse_cnc_condenacoes
#'
#' @param cnc_condenacoes base raw de condenações.
#'
#' @import dplyr
#' @importFrom lubridate dmy
#' @import stringr
#' @import purrr
#'
#' @export
tidy_condenacoes <- function(cnc_condenacoes) {
  sanitize <- cnc_condenacoes |>
    purrr::map_df(\(c) {
      condenacao_id <- c |> purrr::pluck("condenacao_id")
      df <- c |> purrr::pluck("df")

      if (base::nrow(df) == 0) {
        return(tibble::tibble(condenacao_id = condenacao_id))
      }

      df |>
        dplyr::filter(stringr::str_length(key) > 0) |>
        dplyr::filter(!(key == "link" & stringr::str_starts(value, "visualizar_processo.php?"))) |>
        dplyr::mutate(key = key |> tidy_nm()) |>
        dplyr::group_by(key) |>
        dplyr::group_split() |>
        purrr::map_df(\(df_by_key) {
          if (base::nrow(df_by_key) > 1) {
            return(df_by_key |>
              dplyr::mutate(key = stringr::str_c(key, dplyr::row_number(), sep = "_")))
          }
          df_by_key
        }) |>
        tidyr::pivot_wider(names_from = key, values_from = value) |>
        tibble::add_column(condenacao_id = c |> purrr::pluck("condenacao_id"))
    })

  loc <- readr::locale(decimal_mark = ",", grouping_mark = ".")

  re_pena <-
    sprintf(
      "Anos%s([0-9]+)%sMeses%s([0-9]+)%sDias%s([0-9]+)",
      "[[:space:]]+",
      "[[:space:]]+",
      "[[:space:]]+",
      "[[:space:]]+",
      "[[:space:]]+",
      "[[:space:]]+"
    )
  re_pena_de <- "De:?[[:space:]]+([0-9]{2}/[0-9]{2}/[0-9]{4})"
  re_pena_ate <- "At\u00e9:?[[:space:]]+([0-9]{2}/[0-9]{2}/[0-9]{4})"

  sanitize |>
    dplyr::transmute(
      condenacao_id = condenacao_id,
      dt_pena = lubridate::dmy(label_data_julg_coleg),
      teve_inelegivel = stringr::str_detect(inelegibilidade, "SIM"),
      teve_multa = base::is.character(pagamento_de_multa) & stringr::str_detect(pagamento_de_multa, "SIM"),
      vl_multa = base::ifelse(
        teve_multa,
        readr::parse_number(pagamento_de_multa, locale = loc),
        NA_real_
      ),
      teve_pena = base::is.character(label_pena_privativa) & stringr::str_detect(label_pena_privativa, "SIM"),
      de_pena = stringr::str_match(label_pena_privativa, re_pena_de)[, 2] |> lubridate::dmy(),
      ate_pena = stringr::str_match(label_pena_privativa, re_pena_ate)[, 2] |> lubridate::dmy(),
      durancao_pena = base::as.numeric(ate_pena - de_pena),
      teve_perda_bens = base::is.character(perda_de_bens_ou_valores_acrescidos_ilicitamente_ao_patrimonio) & stringr::str_detect(perda_de_bens_ou_valores_acrescidos_ilicitamente_ao_patrimonio, "SIM"),
      vl_perda_bens = base::ifelse(
        teve_perda_bens,
        readr::parse_number(perda_de_bens_ou_valores_acrescidos_ilicitamente_ao_patrimonio, locale = loc),
        NA
      ),
      teve_perda_cargo = base::is.character(perda_de_emprego_cargo_funcao_publica) & stringr::str_detect(perda_de_emprego_cargo_funcao_publica, "SIM"),
      proibicao1 = proibicao_de_contratar_com_o_poder_publico_ou_receber_incentivos_fiscais_ou_crediticios_direta_ou_indiretamente_ainda_que_por_intermedio_de_pessoa_juridica_da_qual_seja_socio_majoritario,
      proibicao2 = proibicao_de_contratar_com_o_poder_publico_direta_ou_indiretamente_ainda_que_por_intermedio_de_pessoa_juridica_da_qual_seja_socio_majoritario,
      proibicao3 = proibicao_de_receber_incentivos_crediticios_direta_ou_indiretamente_ainda_que_por_intermedio_de_pessoa_juridica_da_qual_seja_socio_majoritario,
      proibicao4 = proibicao_de_receber_incentivos_fiscais_direta_ou_indiretamente_ainda_que_por_intermedio_de_pessoa_juridica_da_qual_seja_socio_majoritario,
      proibicao_txt = stringr::str_c(proibicao1, proibicao2, proibicao3, proibicao4, sep = " | "),
      teve_proibicao = base::is.character(proibicao_txt) & stringr::str_detect(proibicao_txt, "SIM"),
      de_proibicao = lubridate::dmy(stringr::str_match(proibicao_txt, re_pena_de)[, 2]),
      ate_proibicao = lubridate::dmy(stringr::str_match(proibicao_txt, re_pena_ate)[, 2]),
      duracao_proibicao = base::as.numeric(ate_proibicao - de_proibicao),
      teve_ressarcimento = base::is.character(ressarcimento_integral_do_dano) & stringr::str_detect(ressarcimento_integral_do_dano, "SIM"),
      vl_ressarcimento = base::ifelse(
        teve_ressarcimento,
        readr::parse_number(ressarcimento_integral_do_dano, locale = loc),
        NA_real_
      ),
      teve_suspensao = base::is.character(suspensao_dos_direitos_politicos) & stringr::str_detect(suspensao_dos_direitos_politicos, "SIM"),
      de_suspensao = stringr::str_match(suspensao_dos_direitos_politicos, re_pena_de)[, 2] |> lubridate::dmy(),
      ate_suspensao = stringr::str_match(suspensao_dos_direitos_politicos, re_pena_ate)[, 2] |> lubridate::dmy(),
      duracao_suspensao = base::as.numeric(ate_suspensao - de_suspensao),
      comunicacao_tse = stringr::str_detect(suspensao_dos_direitos_politicos, "Comunica.+SIM"),
      situacao = situacao,
      tipo_pena = tipo_pena,
      visualizacao_pena = visualizacao_pena,
      assunto_cod_1 = assunto_cod_1,
      assunto_nm_1 = assunto_nm_1,
      assunto_cod_2 = assunto_cod_2,
      assunto_nm_2 = assunto_nm_2,
      assunto_cod_3 = assunto_cod_3,
      assunto_nm_3 = assunto_nm_3,
      assunto_cod_4 = assunto_cod_4,
      assunto_nm_4 = assunto_nm_4,
      assunto_cod_5 = assunto_cod_5,
      assunto_nm_5 = assunto_nm_5,
      link = link,
    ) |>
    dplyr::select(!c(proibicao_txt))
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
  cadmun <- abjData::cadmun
  cnc_pessoa_tidy <- cnc_pessoa_infos %>%
    spread(key, value) %>%
    rename(id_pessoa = id) %>%
    mutate(
      across(.fns = ~ na_if(.x, "")),
      across(.fns = ~ na_if(.x, "NA"))
    ) %>%
    # mutate_all(funs(suppressWarnings(na_if(., '', 'NA')))) %>%
    select(
      arq_pessoa_infos, id_pessoa, tipo_pessoa, nm_pessoa,
      sexo, publico, esfera, orgao, cargo, uf, cod
    ) %>%
    mutate(sexo = if_else(!sexo %in% c("F", "M"), NA_character_, sexo)) %>%
    mutate(publico = if_else(!publico %in% c("S", "N"), NA_character_, publico)) %>%
    mutate(esfera = if_else(!esfera %in% c("F", "D", "E", "M"),
      NA_character_, esfera
    )) %>%
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
    inner_join(cnc2, "id_pessoa") %>%
    inner_join(cnc3, "id_processo")

  cadmun <- abjData::cadmun
  pnud_uf <- abjData::pnud_uf

  cadmun %<>% distinct(cod, uf) %>% mutate_all(as.character)
  pnud_uf %<>% filter(ano == 2010) %>%
    select(uf, ufn, popt) %>%
    mutate(uf = as.character(uf)) %>%
    inner_join(cadmun, c("uf" = "cod")) %>%
    select(id = uf.y, ufn, popt)

  regex_uf_estadual <- "\u00e7a d[eo] (Estado d[oae] )?(.+)$"
  regex_uf_federal <- pnud_uf %>%
    with(ufn) %>%
    {
      sprintf("(%s)|(%s)", ., abjutils::rm_accent(.))
    } %>%
    paste(collapse = "|") %>%
    regex(ignore_case = TRUE)

  ufs_estadual <- tidy_cnc %>%
    filter(esfera_processo == "Estadual") %>%
    mutate(
      ufn_processo = str_match(tribunal, regex_uf_estadual)[, 3],
      ufn_processo = str_replace_all(ufn_processo, " e dos T.+", "")
    ) %>%
    inner_join(pnud_uf, c("ufn_processo" = "ufn")) %>%
    select(id_condenacao, uf_processo = id)

  ufs_federal_1inst <- tidy_cnc %>%
    filter(esfera_processo == "Federal", instancia == "1 grau") %>%
    mutate(ufn_processo = str_match_all(comarca_secao, regex_uf_federal) %>%
      purrr::map_chr(~ {
        x <- as.character(.x[, -1])
        x[x != ""][1]
      })) %>%
    mutate(ufn_processo = toupper(abjutils::rm_accent(ufn_processo))) %>%
    inner_join(
      mutate(pnud_uf, ufn_processo = toupper(abjutils::rm_accent(ufn))),
      "ufn_processo"
    ) %>%
    select(id_condenacao, uf_processo = id)

  tidy_cnc <- tidy_cnc %>%
    left_join(bind_rows(ufs_estadual, ufs_federal_1inst), "id_condenacao") %>%
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
    filter(assunto != "") %>%
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
