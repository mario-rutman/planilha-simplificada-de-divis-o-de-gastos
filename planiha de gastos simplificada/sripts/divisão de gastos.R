library(tidyverse)
library(googlesheets)
library(forcats)

# Primeiro pegar a planilha no googlesheets.
gastos_simplif <- gs_title("planilha simplificada de divisão de gastos")

gs_ws_ls(gastos_simplif) # Listando as abas da planilha. No caso tem "Página1" "Página2".

wsret <- gs_read(ss = gastos_simplif, ws = "Página1") # Criando o objeto para depois fazer o data frame.

df_gastos_simplif <- as.data.frame(wsret) # Transformando em data frame.

# Criando o df resultado onde será mostrado quanto um tem que pagar ao outro e quanto eu gastei no total.
result <- df_gastos_simplif %>% group_by(mes_ano, quem, despesa) %>% # Agrupando para somar por grupo
summarise(total = sum(valor)) %>%  # Somando
ungroup() %>% # Sem desagrupar as funções lag lead não funcionam
mutate(dif_a_pagar = (lead(total, 2) - total)/2) %>%  # Calculando a diferença a pagar
mutate(total_mario = total/2 + lead(total,2)/2 + lead(total,3)) # Fazendo a conta (comum + comum)/2 + próprio

saveRDS(df_gastos_simplif, "df_gastos_simplif.rds")
saveRDS(result, "result.rds")
