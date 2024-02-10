from shiny import App, Inputs, Outputs, Session, render, ui, req
import textgrid
import os
from unidecode import unidecode

# Lista de TextGrid
textgrids_lista = [arquivo for arquivo in os.listdir(".") if arquivo.endswith(".TextGrid")]

# Definir camada de falantes
FALANTE_1_TIER_1 = 0
FALANTE_2_TIER_1 = 6


app_ui = ui.page_fluid(
    ui.input_text("termo", "Busque um termo em nheengatu:", ""),
    ui.row(
        ui.column(
            6,
            ui.output_text_verbatim("saida", placeholder=True),
        ),
    ),
)


def server(input: Inputs, output: Outputs, session: Session):
     
    @render.text
    def saida():
        
        TERMO_BUSCADO = input.termo()
        req(input.termo())
        relatorio = []
        contador_total = 0
        
        
        for arquivo_textgrid in textgrids_lista:
        
            textgrid_alvo = arquivo_textgrid
            tg = textgrid.TextGrid.fromFile(textgrid_alvo)
            
            falante_1_ultimo_indice = len(tg[FALANTE_1_TIER_1])
            falante_2_ultimo_indice = len(tg[FALANTE_2_TIER_1])
            
            detalhes_ocorrencias = []
        
            corpus_falante_1 = []
            corpus_falante_2 = []         
        
            for indice in range(0, falante_1_ultimo_indice):
                intervalo = tg[FALANTE_1_TIER_1][indice]
                is_empty = intervalo.mark == "" 
                if not is_empty:
                    item = {
                        "text": intervalo.mark,
                        "indice": indice,
                        "camada": FALANTE_1_TIER_1
                        }
                    corpus_falante_1.append(item)
                          
            for indice in range(0, falante_2_ultimo_indice):
                if len(tg) > 10:
                    intervalo = tg[FALANTE_2_TIER_1][indice]
                    is_empty = intervalo.mark == "" 
                    if not is_empty:
                        item = {
                            "text": intervalo.mark,
                            "indice": indice,
                            "camada": FALANTE_2_TIER_1          
                            }
                        corpus_falante_2.append(item)
                
            contador = 0
            
            if len(tg) > 10:
                corpus_falantes_total = corpus_falante_1 + corpus_falante_2
            else:
                corpus_falantes_total = corpus_falante_1
            
            for item in corpus_falantes_total:
                        foi_encontrado = unidecode(TERMO_BUSCADO.upper()) in unidecode(item["text"].upper())
                        if foi_encontrado:
                            local = {"indice": item["indice"],
                                "camada": item["camada"]}
                            detalhes_ocorrencias.append(local)
                            contador += 1
            
            
            contador_total = contador_total + contador
            
            relatorio.append("\n")
            relatorio.append("\n")
            relatorio.append("---------------------------------------------")
            relatorio.append("\n")
            relatorio.append(textgrid_alvo)
            relatorio.append("\n")
            relatorio.append("n = ")
            relatorio.append(str(contador))
            relatorio.append("\n")
            relatorio.append("---------------------------------------------")
            relatorio.append("\n")
            relatorio.append("\n")
            
            
            for ocorrencia in detalhes_ocorrencias:
                indice = ocorrencia["indice"]
                camada = ocorrencia["camada"]
                relatorio.append(
                "Falante1_" if camada == 0 else "Falante2_")
                relatorio.append(
                round(tg[camada][indice].minTime, 2))
                relatorio.append("-")
                relatorio.append(round(tg[camada][indice].maxTime, 2))
                relatorio.append(":")
                relatorio.append("\n")
                for camada_indice in range(0, 6):
                    relatorio.append(tg[camada+camada_indice][indice].mark)
                    relatorio.append("\n")
                relatorio.append("\n")
                
            
            if arquivo_textgrid == textgrids_lista[len(textgrids_lista) - 1]:
                relatorio.insert(0, f"Termo buscado = {input.termo()}")
                relatorio.insert(1, "\n")
                relatorio.insert(2, "\n")
                relatorio.insert(3, f"{contador_total} ocorrências encontradas")
                relatorio.insert(4, "\n")
                relatorio.insert(5, "\n")
                relatorio.insert(6, "Arquivos")
                relatorio.insert(7, "\n")
                for i_textgrid in reversed(range(0, len(textgrids_lista))):
                    relatorio.insert(8,  textgrids_lista[i_textgrid])
                    relatorio.insert(9, "\n") 
                    
                
            
            
           
            
        
        
            
        return "".join(map(str, relatorio))


app = App(app_ui, server)


