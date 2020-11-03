library(magrittr)
devtools::load_all()

# pags ------------------------------------------------------------------------
# download
pags <- seq_len(cnc_npag())
progressr::with_progress({
  result <- lex::pvec(pags, cnc_download_pag, "data-raw/cnc_pag")
})

# parse
arqs <- fs::dir_ls("data-raw/cnc_pag")
progressr::with_progress({
  parsed <- lex::pvec(arqs, cnc_parse_pag)
})

# export
da_pag <- purrr::map_dfr(parsed, "result", .id = "arq_pag")
readr::write_rds(da_pag, "data-raw/da_pag.rds", compress = "xz")

# pessoa ----------------------------------------------------------------------

# import
da_pags <- readr::read_rds("data-raw/da_pags.rds")
future::plan(future::sequential)

links <- da_pags %>%
  dplyr::filter(name == "nm_pessoa") %>%
  dplyr::pull(link) %>%
  unique()

# download
progressr::with_progress({
  result <- lex::pvec(links, cnc_download_pessoa, "data-raw/cnc_pessoa")
})

# parse
arqs <- fs::dir_ls("data-raw/cnc_pessoa")
progressr::with_progress({
  parsed <- lex::pvec(arqs, cnc_parse_pessoa)
})

# export
da_pessoa <- purrr::map_dfr(parsed, "result", .id = "arq_pessoa")
readr::write_rds(da_pessoa, "data-raw/da_pessoa.rds", compress = "xz")


# processo --------------------------------------------------------------------

# import
da_pags <- readr::read_rds("data-raw/da_pags.rds")

links <- da_pags %>%
  dplyr::filter(name == "num_processo") %>%
  dplyr::pull(link) %>%
  unique()

# download
progressr::with_progress({
  result <- lex::pvec(links, cnc_download_processo, "data-raw/cnc_processo")
})

# parse
arqs <- fs::dir_ls("data-raw/cnc_processo")
progressr::with_progress({
  parsed <- lex::pvec(arqs, cnc_parse_processo)
})

# export
da_processo <- purrr::map_dfr(parsed, "result", .id = "arq_processo")
readr::write_rds(da_processo, "data-raw/da_processo.rds", compress = "xz")

# pessoa info -----------------------------------------------------------------

# import
da_pags <- readr::read_rds("data-raw/da_pags.rds")
links <- da_pags %>%
  dplyr::filter(name == "nm_pessoa") %>%
  dplyr::pull(link) %>%
  unique()

# download
future::plan(future::multisession, workers = 8)
progressr::with_progress({
  result <- lex::pvec(links, cnc_download_pessoa_infos)
})

# parse
arqs <- fs::dir_ls("data-raw/cnc_pessoa_infos")
progressr::with_progress({
  parsed <- lex::pvec(arqs, cnc_parse_pessoa_infos)
})

# export
da_pessoa_infos <- purrr::map_dfr(parsed, "result", .id = "arq_pessoa_infos")
readr::write_rds(da_pessoa_infos, "data-raw/da_pessoa_infos.rds", compress = "xz")


