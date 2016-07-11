# cnc

Scraper do Cadastro Nacional de Condenações Cíveis por Ato de Improbidade Administrativa e Inelegibilidade - CNJ.

## Instalação

```
if (!require(devtools)) install.packages(devtools)
devtools::install_github('abjur/cnc')
```

Também é necessário instalar o PhantomJS para o funcionamento de algumas funções. Ver ?GET_pjs para detalhes.

## Utilização

### Baixar páginas

- `cnc_pags` para baixar as páginas de resultados do CNC.
- `cnc_pessoas` para baixar as páginas de condenados do CNC.
- `cnc_processos` para baixar as páginas de processos do CNC.
- `cnc_infos_pessoas` para baixar as páginas de informações de pessoas do CNC.

### Parsers

- `parse_pags` transformas os arquivos baixados por `cnc_pags` em `data_frame`.
- `parse_pessoas` transformas os arquivos baixados por `cnc_pessoas` em `data_frame`.
- `parse_processos` transformas os arquivos baixados por `cnc_processos` em `data_frame`.
- `parse_infos_pessoas` transformas os arquivos baixados por `cnc_infos_pessoas` em `data_frame`.

### Workflow

Para obter o conjunto de dados completo, rodar nessa ordem:

```
library(magrittr)
library(cnc)

cnc_pags(path = 'data-raw/pags', pags = 1:2399)
d_pags <- 'data-raw/pags' %>% 
  dir(full.names = TRUE) %>% 
  parse_pags(arqs)

# baixa pessoas e processos
cnc_pessoas(d_pags, path = "data-raw/pessoas")
cnc_processos(d_pags, path = "data-raw/processos")

# parse pessoas e processos
d_pessoas <- "data-raw/pessoas" %>% 
  dir(full.names = TRUE) %>% 
  parse_pessoas(arqs)
d_processos <- "data-raw/processos" %>% 
  dir(full.names = TRUE) %>% 
  parse_pessoas(arqs)

# baixa infos das pessoas
cnc_pessoas_infos(d_pessoas, path = 'data-raw/pessoas_infos')
# parse infos das pessoas
d_pessoas_infos <- "data-raw/pessoas_infos" %>% 
  dir(full.names = TRUE) %>% 
  parse_pessoas_infos(arqs)
```

## TODO

- Melhorar parser: tempos de condenação.
- Parser++: montar base analítica de acordo com objetivos da pesquisa.
- Verificar forma/tempo de atualização da base no site

