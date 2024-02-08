remove.packages("cnc")
devtools::load_all()

tmp_dir <- fs::path_join(c(base::getwd(), "tmp"))

consulta_dir <- fs::path_join(c(tmp_dir, "consulta"))
condenacao_dir <- fs::path_join(c(tmp_dir, "condenacao"))
processo_dir <- fs::path_join(c(tmp_dir, "processo"))
pessoa_info_dir <- fs::path_join(c(tmp_dir, "pessoa_info"))

if (!fs::dir_exists(tmp_dir)) {
  fs::dir_create(tmp_dir)
}

if (!fs::dir_exists(consulta_dir)) {
  fs::dir_create(consulta_dir)
}

if (!fs::dir_exists(condenacao_dir)) {
  fs::dir_create(condenacao_dir)
}

if (!fs::dir_exists(processo_dir)) {
  fs::dir_create(processo_dir)
}

if (!fs::dir_exists(pessoa_info_dir)) {
  fs::dir_create(pessoa_info_dir)
}

total_pages <- cnc_npag()

future::plan(future::multisession, workers = 6)

# Download pages in parallel
cnc_download_pag_async <- function(pag, path) {
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

page_range <- base::seq(from = 1, to = total_pages)

page_range |> furrr::future_walk(\(page_number) {
  cnc_download_pag_async(page_number, consulta_dir)
})

total_pages

pessoas_consulta_df <- page_range |> purrr::map_df(\(page_number) {
  fs::path_join(c(consulta_dir, base::paste0(page_number, ".html"))) |>
    readr::read_file() |>
    cnc_parse_pag()
})


cnc_download_condenacao_async <- function(id_condenacao, path) {
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

condenacao_ids <- pessoas_consulta_df |>
  dplyr::select(condenacao_url_id) |>
  dplyr::distinct() |>
  purrr::pluck(1)

condenacao_ids |> base::length()

condenacao_ids |>
  furrr::future_walk(\(id_condenacao) cnc_download_condenacao_async(id_condenacao, condenacao_dir))


cnc_download_processo_async <- function(id_processo, path) {
  fs::dir_create(path)
  # num_link <- gsub('[^0-9]', '', link)
  f <- sprintf("%s/%s.html", path, id_processo)
  # if (!file.exists(f)) {
  link <- paste0("http://www.cnj.jus.br/improbidade_adm/visualizar_processo.php?seq_processo=", id_processo)
  httr::GET(link) %>%
    httr::content() |>
    readr::write_lines(f)
  # }
  f
}

processo_ids <- pessoas_consulta_df |>
  dplyr::select(processo_url_id) |>
  dplyr::distinct() |>
  purrr::pluck(1)

processo_ids |>
  furrr::future_walk(\(id_processo) cnc_download_processo_async(id_processo, processo_dir))


condenacao_ids |> base::length()

condenacoes_parsed <- condenacao_ids |> purrr::map(\(id) {
  parsed <- fs::path_join(c(condenacao_dir, base::paste0(id, ".html"))) |>
    readr::read_file() |>
    cnc_parse_condenacao()

  base::list(condenacao_id = id, df = parsed)
})

condenacoes_parsed |> readr::write_rds(file = "condenacoes_parsed.rds")

processo_ids |> base::length()

processos_parsed <- processo_ids |> purrr::map(\(id) {
  parsed <- fs::path_join(c(processo_dir, base::paste0(id, ".html"))) |>
    readr::read_file() |>
    cnc_parse_processo()

  base::list(processo_id = id, df = parsed)
})

processos_parsed |> readr::write_rds(file = "processos_parsed.rds")


processos_parsed |>
  {
    \(x) x[purrr::map_int(x, \(p) p |>
      purrr::pluck("df") |>
      base::nrow()) |> base::order(decreasing = TRUE)]
  }() |>
  purrr::pluck(1)


example_condenacao <- condenacoes_parsed |>
  {
    \(x) x[purrr::map_int(x, \(p) p |>
      purrr::pluck("df") |>
      base::nrow()) |> base::order(decreasing = TRUE)]
  }() |>
  purrr::pluck(1)

example_condenacao$df |> print(n = 50)

condenacoes_parsed |>
  purrr::map_df(\(c) c |> purrr::pluck("df")) |>
  dplyr::filter(key == "label_pena_privativa")

condenacoes_parsed |>
  purrr::map_df(\(c) c |> purrr::pluck("df")) |>
  dplyr::distinct(key) |>
  dplyr::mutate(key = key |> tidy_nm())

sanitize_condenacao <- function(df) {
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
    tidyr::pivot_wider(names_from = key, values_from = value)
}

condenacoes_parsed

condenacoes_san <- condenacoes_parsed |>
  purrr::map_df(\(c) {
    c |>
      purrr::pluck("df") |>
      sanitize_condenacao() |>
      tibble::add_column(condenacao_id = c |> purrr::pluck("condenacao_id"))
  })

example_processo <- processos_parsed |>
  {
    \(x) x[purrr::map_int(x, \(p) p |>
      purrr::pluck("df") |>
      base::nrow()) |> base::order(decreasing = TRUE)]
  }() |>
  purrr::pluck(1)

sanitize_processo <- function(df) {
  if (base::is.null(df)) {
    return(df)
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
    tidyr::pivot_wider(names_from = key, values_from = value)
}

processos_san <- processos_parsed |>
  purrr::map_df(\(c) {
    c |>
      purrr::pluck("df") |>
      sanitize_condenacao() |>
      tibble::add_column(processo_id = c |> purrr::pluck("processo_id"))
  })

condenacoes_san |> dplyr::select(link)

users_info <- condenacoes_san |>
  dplyr::select(c(condenacao_id, link)) |>
  dplyr::mutate(
    id_user_info = link |>
      purrr::map_chr(\(str)
      str |>
        stringr::str_match("'([0-9]+)'") |>
        base::as.vector() |>
        tail(n = 1))
  )


cnc_download_pessoa_infos_async <- function(id, path) {
  # fs::dir_create(path)
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

# Vamos baixar as informacoes de cada pessoa
users_info |>
  purrr::pluck("id_user_info") |>
  base::unique() |>
  furrr::future_walk(\(id) {
    cnc_download_pessoa_infos_async(id, pessoa_info_dir)
  })


pessoas_info_san <- users_info |>
  purrr::pluck("id_user_info") |>
  base::unique() |>
  purrr::map_df(\(id) {
    fs::path_join(c(pessoa_info_dir, paste0(id, ".html"))) |>
      readr::read_file() |>
      cnc_parse_pessoa_infos() |>
      tidyr::pivot_wider(names_from = key, values_from = value)
  })


new_tidy_condenacoes <- function(new_df) {
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

  new_df |>
    dplyr::transmute(
      condenacao_id = condenacao_id,
      dt_pena = lubridate::dmy(label_data_julg_coleg),
      teve_inelegivel = stringr::str_detect(inelegibilidade, "SIM"),
      teve_multa = base::is.character(pagamento_de_multa) & stringr::str_detect(pagamento_de_multa, "SIM"),
      vl_multa = base::ifelse(
        base::is.character(pagamento_de_multa) & stringr::str_detect(pagamento_de_multa, "SIM"),
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
      de_suspensao = lubridate::dmy(stringr::str_match(suspensao_dos_direitos_politicos, re_pena_de)[, 2]),
      ate_suspensao = lubridate::dmy(stringr::str_match(suspensao_dos_direitos_politicos, re_pena_ate)[, 2]),
      duracao_suspensao = base::as.numeric(ate_suspensao - de_suspensao),
      comunicacao_tse = dplyr::if_else(
        stringr::str_detect(suspensao_dos_direitos_politicos, "Comunica.+SIM"),
        TRUE,
        NA
      ),
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
    ) |>
    dplyr::select(!c(proibicao_txt))
}

new_tidy_processos <- function(df) {
  df |>
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

tidy_condenacoes_df <- condenacoes_san |> new_tidy_condenacoes()
tidy_processos_df <- processos_san |> new_tidy_processos()
tidy_pessoas_info_df <- users_info |>
  dplyr::left_join(
    pessoas_info_san |> dplyr::rename(id_user_info = id),
    by = "id_user_info"
  ) |>
  dplyr::select(!c(link, nm_pessoa))

output <- pessoas_consulta_df |>
  dplyr::rename(condenacao_id = condenacao_url_id, processo_id = processo_url_id) |>
  dplyr::left_join(tidy_processos_df, by = "processo_id") |>
  dplyr::left_join(tidy_condenacoes_df, by = "condenacao_id") |>
  dplyr::left_join(tidy_pessoas_info_df, by = "condenacao_id")

output |> readr::write_csv(file = "df_merged.csv")
