library(tidyverse)
library(dplyr)
library(googlesheets4)
library(forcats)
library(lubridate)
library(RColorBrewer)
library(wesanderson)

#Importando a planilha do googlesheets.
#Usando o URL.
   wsret <- read_sheet("https://docs.google.com/spreadsheets/d/1DFjssOOg7MPjfZfCksKlYKdGPtusgb7aQlEtvxdpBNo/edit#gid=0")#Importando a planilha do googlesheets.
 
# Usandoa  Sheet ID.
# fjfj <- read_sheet("1DFjssOOg7MPjfZfCksKlYKdGPtusgb7aQlEtvxdpBNo")

#Listando as abas da planilha. No caso tem "Página1" "Página2".


# Transformando em data frame.
df_gastos_simplif <- as.data.frame(wsret) %>% 
  mutate_if(is.character, as.factor) %>% # transformando em categoria todas colunas que são character.
  mutate(mes = fct_relevel(mes, "Janeiro", "Fevereiro", "Março", "Abril", 
                           "Maio", "Junho", "Julho", "Agosto", "Setembro",
                           "Outubro", "Novembro", "Dezembro")) # ordenando o factor mes


# Criando o df resultado onde será mostrado quanto um tem que pagar ao outro e quanto eu gastei no total.

result <- df_gastos_simplif %>% group_by(ano, mes, nome, tipo_desp) %>% # Agrupando para somar por grupo
  summarise(total = sum(valor)) %>%  # Somando
  ungroup() %>% # Sem desagrupar as funções lag lead não funcionam
  mutate(dif_a_pagar = (lead(total, 2) - total)/2) %>%  # Calculando a diferença a pagar
  mutate(total_mario = total/2 + lead(total,2)/2 + lead(total,3)) %>% #Fazendo a conta (comum + comum)/2 + próprio
  filter(nome == "a_Maria" & tipo_desp == "comum") %>% # Só estas linhas me interessam.
  mutate_if(is.character, as.factor) # transformando em categoria todas colunas que são character.


saveRDS(df_gastos_simplif, "df_gastos_simplif.rds")
saveRDS(result, "result.rds")


# GRÁFICO 1.
# Preparando para fazer o gráfico.
# Agrupando para somar. Saber o total de cada despesa por ano, mês, nome e tipo.
df_gastos_simplif %>% group_by(ano, mes, nome, tipo_desp, categ_desp) %>% 
  summarise(total = sum(valor)) %>% ungroup() %>% 
  # filtrando as linhas que me competem: todas as minhas e as comuns da Maria.
  filter((nome == 'a_Maria' & tipo_desp == 'comum') | (nome == 'o_Mário')) %>%
  # Não posso somar direto os valores das despesas comuns com as próprias,
  # pois nas comuns só arco com a metade,
  # assim tenho que dividí-las por 2 para depois agrupar.
  mutate(total = ifelse(tipo_desp == "comum", total/2, total)) %>%
  # agrupando por categoria despesa.
  group_by(mes, categ_desp) %>%
  # summarizando os totais por despesa.
  summarise(total = round (sum(total), 0)) %>% 
  # ordenando a categ_desp do maior para o menor.
  # mutate(categ_desp = fct_rev(fct_reorder(categ_desp, total))) %>%
  # criando a coluna total_percentual
  mutate(total_perc = round (total/sum(total),3)) %>% 
  # Agora vou criar a coluna label que será usada no facet wrap para indicar também o valor total dos gastos.
  mutate(label = paste0("Total  R$ ",(sum(total)))) %>% 
  # Agora fazendo o gráfico. 
  ggplot(aes(x = categ_desp, y = total)) +
  geom_col(aes(fill = categ_desp), color = 'black') +
  # Pode haver diversos geom_text, cada um com alguma coisa escrita
  geom_text(aes(label = paste(100*total_perc,"%")), vjust = -0.3, size = 3.5) +
  geom_text(aes(label = prettyNum(total, big.mark = ".")), vjust =  -1.9, size = 3.5) +
  # geom_text(data = result, aes(label = total_mario)) +
  # Aumentando o limite do eixo y para o label não ficar fora do gráfico.
  scale_y_continuous(limits=c(0, 8500)) +
  # scale_fill_manual(values = c("red", "grey", "seagreen3", "blue", "yellow", "orange", "violet", "gold", "aliceblue", "green")) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ mes + label, ncol = 3) +
  theme(legend.position = "none",
        axis.title = element_blank(), # tirando os títulos dos 2 eixos
        axis.text.x = element_text(face = "bold", color = "black", size = 8, angle = 0),
        axis.text.y = element_blank(), # tirando texto do eixo y
        axis.ticks.x = element_blank(), # tirando os traços do eixo x
        axis.ticks.y = element_blank(), # tirando os traços do eixo y
        panel.grid = element_blank(), # tirando todas linhas de grade
        # trabalhando elementos do fundo e contorno do gráfico.
        panel.background = element_rect(fill = "cornsilk",
                                        colour = "black",
                                        size = 1.5, linetype = "solid"),
        # modificando o quadro acima do gráfico
        strip.background = element_rect(colour = "black", size = 1.5, fill = alpha("cornsilk",0.5)),
        # modificando o texto
        strip.text.x = element_text(colour = "black", face = "bold", size=12, angle=0))




# GRÁFICO 2.
# Agora fazer outro gráfico de colunas, com os mesmos valores, porém ao invés
# de agrupar várias categorias em cada mês, vou agrupar a mesma categoria
# em diversos meses.

df_gastos_simplif %>% group_by(ano, mes, nome, tipo_desp, categ_desp) %>% 
  summarise(total = sum(valor)) %>% ungroup() %>% 
  # filtrando as linhas que me competem: todas as minhas e as comuns da Maria.
  filter((nome == 'a_Maria' & tipo_desp == 'comum') | (nome == 'o_Mário')) %>%
  # Não posso somar direto os valores das despesas comuns com as próprias,
  # pois nas comuns só arco com a metade,
  # assim tenho que dividí-las por 2 para depois agrupar.
  mutate(total = ifelse(tipo_desp == "comum", total/2, total)) %>%
  # agrupando por categoria despesa.
  group_by(mes, categ_desp) %>%
  # summarizando os totais por despesa.
  summarise(total = round (sum(total), 0)) %>%
  # ordenando a categ_desp do maior para o menor.
  # mutate(categ_desp = fct_rev(fct_reorder(categ_desp, total))) %>%
  # criando a coluna total_percentual
  mutate(total_perc = round (total/sum(total),3)) %>% 
  # Agora fazendo o gráfico propriamente dito.
  ggplot(aes(x = mes, y = total)) +
  geom_col(aes(fill = categ_desp), color = 'black') +
  # Pode haver diversos geom_text, cada um com alguma coisa escrita
  geom_text(aes(label = paste(100*total_perc,"%")), vjust = -0.3, size = 3.3) +
  geom_text(aes(label = prettyNum(total, big.mark = ".")), vjust = - 1.9, size = 3.3) +
  # Abreviando o nome dos meses.
  scale_x_discrete(labels = c("Janeiro" = "Jan","Fevereiro" = "Fev",
                              "Março" = "Mar", "Abril" = "Abr", "Maio" = "Mai", "Junho" = "Jun", "Julho" = "Jul",
                              "Agosto" = "Ago", "Setembro" = "Set", "Outubro" = "Out", "Novembro" = "Nov",
                              "Dezembro" = "Dez"), drop = TRUE) +
  # Aumentando o limite do eixo y para o label não ficar fora do gráfico.
  scale_y_continuous(limits=c(0, 8500)) +
  #scale_fill_manual(values = c("red", "grey", "seagreen3", "blue", "yellow", "orange", "violet", "gold", "aliceblue", "green")) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ categ_desp, ncol = 3) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank(), # tirando os títulos dos 2 eixos
        axis.text.x = element_text(face = "bold", color = "black", size = 8, angle = 0),
        axis.text.y = element_blank(), # tirando texto do eixo y
        axis.ticks.x = element_blank(), # tirando os traços do eixo x
        axis.ticks.y = element_blank(), # tirando os traços do eixo y
        panel.grid = element_blank(), # tirando todas linhas de grade
        # Aumentando espaço horizontal entre os gráficos
        panel.spacing.y=unit(1.5, "lines"),
        # trabalhando elementos do fundo e contorno do gráfico.
        panel.background = element_rect(fill = "cornsilk",
                                        colour = "black",
                                        size = 1.5, linetype = "solid"),
        # modificando o quadro acima do gráfico
        strip.background = element_rect(colour = "black", size = 1.5, fill = "cornsilk"),
        # modificando o texto
        strip.text.x = element_text(colour = "black", face = "bold", size=12, angle=0))


# Total por mes em 2020 das despesas comuns, isto é, da casa.
por_mes_2020 <- df_gastos_simplif %>% # Filtrando apenas as despesas comuns.
  filter(tipo_desp == 'comum') %>% group_by(mes) %>% summarise(total_mes = sum(valor))
