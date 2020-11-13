library(magrittr)
devtools::load_all()

da_pag <- readr::read_rds("data-raw/da_pag.rds")
da_pessoa <- readr::read_rds("data-raw/da_pessoa.rds")
da_pessoa_infos <- readr::read_rds("data-raw/da_pessoa_infos.rds")
da_processo <- readr::read_rds("data-raw/da_processo.rds")

tidy_cnc <- tidy_cnc(da_pessoa, da_pag, da_processo, da_pessoa_infos)

usethis::use_data(tidy_cnc, overwrite = TRUE)
