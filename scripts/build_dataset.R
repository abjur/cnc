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

page_range <- base::seq(from = 1, to = total_pages)

page_range |> furrr::future_walk(\(page_number) {
  cnc_download_pag(page_number, consulta_dir)
})

# tibble com lista de todas as pessoas com nome numero processo
# condenacao_id e processo_id
pessoas_consulta_df <- page_range |> purrr::map_df(\(page_number) {
  fs::path_join(c(consulta_dir, base::paste0(page_number, ".html"))) |>
    readr::read_file() |>
    cnc_parse_pag()
})


# Vamos pega os ids de cada condenacao
condenacao_ids <- pessoas_consulta_df |>
  dplyr::select(condenacao_id) |>
  dplyr::distinct() |>
  purrr::pluck(1)

condenacao_ids |> base::length()

# Baixa cada condenacao e salva em condenacao_dir
condenacao_ids |>
  furrr::future_walk(\(id_condenacao) cnc_download_condenacao(id_condenacao, condenacao_dir))

# Vamos pegar os ids do processo
processo_ids <- pessoas_consulta_df |>
  dplyr::select(processo_id) |>
  dplyr::distinct() |>
  purrr::pluck(1)

processo_ids |> base::length()

# Baixa cada processo e salva em processo_dir
processo_ids |>
  furrr::future_walk(\(id_processo) cnc_download_processo(id_processo, processo_dir))

tidy_condenacoes_df <- condenacao_ids |>
  purrr::map(\(id) {
    df <- fs::path_join(c(condenacao_dir, base::paste0(id, ".html"))) |>
      readr::read_file() |>
      cnc_parse_condenacao()

    base::list(
      condenacao_id = id,
      df = df
    )
  }) |>
  tidy_condenacoes()

tidy_condenacoes_df |> readr::write_rds(file = "tmp/tidy_condenacoes.rds")


tidy_processos_df <- processo_ids |>
  purrr::map(\(id) {
    df <- fs::path_join(c(processo_dir, base::paste0(id, ".html"))) |>
      readr::read_file() |>
      cnc_parse_processo()

    base::list(processo_id = id, df = df)
  }) |>
  tidy_processos()

tidy_processos_df |> readr::write_rds(file = "tmp/tidy_processos.rds")

users_info <- tidy_condenacoes_df |>
  dplyr::select(c(condenacao_id, link)) |>
  dplyr::mutate(
    id_user_info = link |>
      purrr::map_chr(\(str)
      str |>
        stringr::str_match("'([0-9]+)'") |>
        base::as.vector() |>
        tail(n = 1))
  )

# Vamos baixar as informacoes de cada pessoa
users_info |>
  purrr::pluck("id_user_info") |>
  base::unique() |>
  furrr::future_walk(\(id) {
    cnc_download_pessoa_infos(id, pessoa_info_dir)
  })


pessoas_info_san <- users_info |>
  purrr::pluck("id_user_info") |>
  base::unique() |>
  purrr::map_df(\(id) {
    fs::path_join(c(pessoa_info_dir, paste0(id, ".html"))) |>
      readr::read_file() |>
      cnc_parse_pessoa_infos() |>
      tidyr::pivot_wider(names_from = key, values_from = value)
  }) |>
  dplyr::rename(id_user_info = id)


tidy_pessoas_info_df <- users_info |>
  dplyr::left_join(pessoas_info_san, by = "id_user_info") |>
  dplyr::select(!c(link, nm_pessoa))


output <- pessoas_consulta_df |>
  dplyr::left_join(tidy_processos_df, by = "processo_id") |>
  dplyr::left_join(tidy_condenacoes_df, by = "condenacao_id") |>
  dplyr::left_join(tidy_pessoas_info_df, by = "condenacao_id")

output |> readr::write_csv(file = "tmp/output.csv")
