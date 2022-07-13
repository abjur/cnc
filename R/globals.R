utils::globalVariables(c(
  '.','1_grau_justica_estadual','1_grau_justica_federal','2_grau_justica_estadual','2_grau_justica_federal','V2','X1','X2','arq','arq_pag',
  'arq_pessoa','auditoria_militar','cadmun','cargo','cod','cod_assunto','comarca',
  'data','data_da_decisao_do_orgao_colegiado','data_da_informacao',
  'data_da_propositura_da_acao','data_do_transito_em_julgado','dt_cadastro',
  'dt_decisao','dt_pena','dt_propositura','dt_transito','esfera','id_condenacao',
  'id_pag','id_pessoa','id_processo','inelegibilidade','instancia','key','lab_pessoa',
  'lab_processo','link','link_condenacao','link_pessoa','link_processo',
  'nm_assunto','nm_pessoa','nome','num_do_processo','num_link','num_processo','orgao',
  'pag','pagamento_de_multa','pena_privativa_de_liberdade',
  'pena_privativa_de_liberdade_aplicada','pena_txt','perda_bens',
  'perda_de_bens_ou_valores_acrescidos_ilicitamente_ao_patrimonio',
  'perda_de_emprego_cargo_funcao_publica',
  'proibicao_de_contratar_com_o_poder_publico_ou_receber_incentivos_fiscais_ou_crediticios_direta_ou_indiretamente_ainda_que_por_intermedio_de_pessoa_juridica_da_qual_seja_socio_majoritario',
  'proibicao_txt','publico','ressarcimento','ressarcimento_integral_do_dano',
  'runif','secao','secao_judiciaria','setNames','sexo','situacao','subsecao',
  'suspensao_dos_direitos_politicos','suspensao_txt','tipo_pena','tipo_pessoa',
  'tribunal','tribunal_de_justica_estadual','tribunal_superior','uf','v','value',
  'vara_camara','pnud_uf','%>%','ano','ufn','popt','uf.y','esfera_processo',
  'ufn_processo','comarca_secao','map_chr','n1','nivel','assunto','penal_lgl','ate_pena',
  'de_pena','%<>%','teve_inelegivel','teve_multa','teve_pena','teve_perda_bens',
  'teve_perda_cargo','teve_proibicao','teve_ressarcimento','teve_suspensao','ate_proibicao',
  'de_proibicao','ate_suspensao','de_suspensao','assunto2',
  "V1", "name", "rowid", '4ra', 'arq_pessoa_infos', 'arq_processo', 'label_data_julg_coleg',
  'label_pena_privativa', 'proibicao1', 'proibicao2', 'proibicao3', 'proibicao4',
  'proibicao_de_contratar_com_o_poder_publico_direta_ou_indiretamente_ainda_que_por_intermedio_de_pessoa_juridica_da_qual_seja_socio_majoritario',
  'proibicao_de_receber_incentivos_crediticios_direta_ou_indiretamente_ainda_que_por_intermedio_de_pessoa_juridica_da_qual_seja_socio_majoritario',
  'proibicao_de_receber_incentivos_fiscais_direta_ou_indiretamente_ainda_que_por_intermedio_de_pessoa_juridica_da_qual_seja_socio_majoritario'
))

# escape_unicode_df <- function(df){
#   df_res <- df
#   for(i in 1:ncol(df)){
#     if(is.character(df[[i]])){
#       df_res[[i]] <- iconv(df[[i]],to = 'UTF-8')
#     }
#   }
#   df_res
# }
