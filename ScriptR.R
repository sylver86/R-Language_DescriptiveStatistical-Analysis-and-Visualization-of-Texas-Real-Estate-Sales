# city: città
# year: anno di riferimento
# month: mese di riferimento
# sales: numero totale di vendite
# volume: valore totale delle vendite in milioni di dollari
# median_price: prezzo mediano di vendita in dollari
# listings: numero totale di annunci attivi
# months_inventory: quantità di tempo necessaria per vendere tutte le inserzioni correnti al ritmo attuale delle vendite, espresso in mesi.


# Mi leggo la sorgente dati
  
  getwd()
  dati_immobiliari <- read.csv("./Scrivania/Progetti/2-Analisi Immobiliare/realestate_texas.csv")
  
# 2) Le variabili sono :
  
    # città (4 città:  Beaumont | Bryan-College Station | Tyler |Wichita Falls )
    # anno (dal 2010 al 2014)
    # mese (viene contato 20 per tutti quindi 20 rilevazioni)
  
# 3) Calcola Indici di posizione, variabilità e forma per tutte le variabili per le quali ha senso farlo, per le altre crea una distribuzione di frequenza. Commenta tutto brevemente.
  
    # Indici di posizione
      summary(dati_immobiliari)
      dati_immobiliari
      
    # Indici di variabilità
    
      #Range
        range(sales) # il numero totale di vendite nei 4 anni e nelle città di riferimento oscilla per mese tra "49" e "423" vendite 
        range(volume) # il valore totale delle vendite in milioni di dollari oscilla per mese tra "8.166" e "83.547" 
        range(median_price) # per un prezzo mediano di ciascuna singola vendita nei 4 anni e nelle città di riferimento che varia per mese tra "73,8k" e "180k"
        range(listings) # Gli annunci attivi variano nei 4 anni e nelle città di riferimento variano per mese tra "743" e "3296"
        range(months_inventory) #con una quantità mensile che varia tra i "3,4" e i "14,9" mesi per ciascuna osservazione degli annunci mensili.
        
      
      # Coefficiente di variazione 
      
      CV <- function(x) {
        return(sd(x)/mean(x)*100)        
      }
      
      CV(sales) #41% - il numero totale di vendite nei 4 anni e nelle città di riferimento oscilla per mese con una % del 41% mensilmente
      CV(volume) #53% - il valore totale delle vendite in milioni di dollari oscilla per mese con una % del 53% (quindi una variabilità maggiore rispetto al numero di vendite che sono del 41%)
      CV(median_price) #17% - il valore mediano del prezzo di ciascuna singola vendita nei 4 anni e nelle città di riferimento ha una % che oscilla mensilmente del 17% (piu ridotto sul volume totale)
      CV(listings)  #43% - il numero di annunci attivi nei 4 anni e nelle città di riferimento per mese ha una % che oscilla mensilmente del 43%
      CV(months_inventory) #25% - il numero di mesi medi per tempo necessasria alla vendita delle inserzioni mensili nei 4 anni e nelle città di riferimento ha una oscillazione del 25%
        
      #IQR
        
        IQR(sales) 
        IQR(volume)
        IQR(median_price)
        IQR(listings)
        IQR(months_inventory)
        
      #Varianza
        
        var(sales)
        var(volume)
        var(median_price)
        var(listings)
        var(months_inventory)
      
      #Deviazione standard
        
        sd(sales)
        sd(volume)
        sd(median_price)
        sd(listings)
        sd(months_inventory)
        
  # 4) Quale è la variabile con variabilità piu elevata? Come ci sei arrivato? e quale quella piu asimmetrica?
        
        # La variabile con una variabilità piu elevata è il "volume" ed ho utilizzato il coefficiente di variazione che consente di rilevare la variabilità in termini % consentendo
        # un confronto tra varie variabili in termini di variabilità.
        
        # Per il calcolo dell'asimmetria utilizziamo il calcolo dell'asimmetria di fisher

          indice_fisher <- function(x){
            mu <-mean(x)
            sigma <- sd(x)
            n <- length(x)
            m3 <- sum((x-mu)^3)/n
            Asim.index <- m3 / sigma^3 
            return (Asim.index)
          }    
          
          indice_fisher(sales)
          indice_fisher(volume) # Riporta la maggiore asimmetria positiva 
          indice_fisher(median_price)
          indice_fisher(listings)
          indice_fisher(months_inventory)
    
                
  # 5) Dividi una delle variabili quantitative in classi, scegli tu quale e come, costruisci la distribuzione di frequenze, il grafico a barre corrispondente e infine calcola l’indice di Gini. 
        
          # Mi creo una distribuzione di frequenze creando classi per la variabile volume
          
            delta <- max(volume)-min(volume)          
            segmentazione <- seq(0,max(volume),length.out=6)
            fasce_volumi <- cut(volume, breaks=segmentazione)
            dati_immobiliari$fasce_volumi <- fasce_volumi
            
          # Mi calcolo ora la distribuzione di frequenze assolute e relative
            
            c <- table(fasce_volumi)
            ni <- table(fasce_volumi)
            fi <- table(fasce_volumi)/sum(c)*100
            
          # Mi creo il grafico a barre
          
            install.packages("ggplot2")
            library(ggplot2)
            
            ggplot(data=dati_immobiliari)+
              geom_bar(aes(x=fasce_volumi),stat="count",col="yellow",fill="blue")+
              labs(title="Distribuzione delle classi il valore totale delle vendite",x="Classi in Mln $",y="Frequenze assolute")+
            theme_classic()
            
          # Mi calcolo l'indice di Gini
            
            gini.index <- function(x) {
              ni = table(x)
              fi= ni/length(x)
              fi2 = fi^2
              J = length(table(x))
              
              gini = 1-sum(fi2)
              gini_normalizzato = gini/((J-1)/J)
              return (gini_normalizzato)
            }
            
            gini.index(volume) # Ci indica che esiste un eterogeneità massima
            
            
#  6) Indovina l’indice di gini per la variabile city.
        
        attach(dati_immobiliari)
        gini.index(city)  # La variabile rileva una equidistribuzione negli anni e mesi quindi le rilevazioni sono equamente distribuite tra le 4 città nel tempo di rilevazione (5 anni per 12 mesi ciascuno). 
        
        
# 7) Qual è la probabilità che presa una riga a caso di questo dataset essa riporti la città “Beaumont”? 
 
        df <- table(city)
        pob_Beamount <- df['Beaumont'] / sum(df)  
        pob_Beamount <- pob_Beamount*100
        print(pob_Beamount)
        
      # E la probabilità che riporti il mese di Luglio? 
        
        df <- table(month)
        pob_Luglio <- df['7'] /sum(df)  
        pob_Luglio <- pob_Luglio*100
        print(pob_Luglio)
        
        
      # E la probabilità che riporti il mese di dicembre 2012?
        
        df <- table(month,year)
        prob_dic_2012 = df[12,"2012"] / sum(df) *100
        print(prob_dic_2012)  
        
        
# 8) Esiste una colonna col prezzo mediano, creane una che indica invece il prezzo medio, utilizzando le altre variabili che hai a disposizione.
        
        
        dati_immobiliari['prezzo_medio'] = dati_immobiliari['volume'] / dati_immobiliari['sales']
        dati_immobiliari['prezzo_medio'] = dati_immobiliari['prezzo_medio']*1000000
        
# 9) Prova a creare un’altra colonna che dia un’idea di “efficacia” degli annunci di vendita. Riesci a fare qualche considerazione?
        
        
        dati_immobiliari['kpi'] = dati_immobiliari['listings'] / dati_immobiliari['months_inventory']
        
        # Proviamo a creare un grafico con x le città , y anni e valore il kpi
        
        library(ggplot2)
        ggplot(data=dati_immobiliari,aes(x=city,y=kpi))+
          stat_summary(fun.y = "mean", geom = "bar") +
          ggtitle("Analisi efficacia vendite per città")+
          xlab("Città")+
          ylab("Kpi (Annunci su media mese di vendita)")
        
        # Nella Città di Tyler le vendite hanno maggiore efficacia seguito da Bryan-College Station
        
        df_Tyler <- dati_immobiliari[city=="Tyler",]
        
        library(ggplot2)
        ggplot(data=df_Tyler,aes(x=year,y=kpi))+
          stat_summary(fun.y = "mean", geom = "bar") +
          ggtitle("Analisi efficacia vendite per anno") +
          xlab("Anno")+
          ylab("Kpi (Annunci su media mese di vendita)")
        
        #Si rileva un tasso di crescita dell'efficacia delle vendite a partire dal 2012 fino al 2014 dopo un ribasso avuto tra il 2010 e il 2011
        

#10) Prova a creare dei summary(), o semplicemente media e deviazione standard, di alcune variabili a tua scelta, condizionatamente alla città, agli anni e ai mesi. 
#   Puoi utilizzare il linguaggio R di base oppure essere un vero Pro con il pacchetto dplyr. Ti lascio un suggerimento in pseudocodice, oltre al cheatsheet nel materiale:
 
        #   dati %>%
        #            group_by(una o più variabili di raggruppamento) %>%
        #  summarise(nomecolonna1=funzione1(variabile da sintetizzare),
        #            nomecolonna2=funzione2(variabile da sintetizzare))
        
        # Sfruttando questa notazione puoi creare anche dei grafici super!
        # Da qui in poi utilizza ggplot2 per creare grafici fantastici!
        # Ma non fermarti alla semplice soluzione del quesito, prova un po’ a personalizzare i grafici utilizzando temi, colori e annotazioni, e aggiustando i vari elementi come le etichette, gli assi e la legenda.
        # Consiglio: Fai attenzione quando specifichi le variabili month e year tra le estetiche, potrebbe essere necessario considerarle come fattori.  
            
        
        install.packages("dplyr")
        library(dplyr)
        
        df_elaborato <- dati_immobiliari %>% group_by(city) %>% summarise (
          sum_sales = sum(sales),
          mean_sales = mean(sales),
          dev_std_sales = sd(sales),
          sum_volume = sum(volume),
          mean_volume = mean(volume),
          dev_std_volume = sd(volume)
        )
        
        
        ggplot(df_elaborato,aes(x="",y=sum_sales,fill=city))+
          geom_bar(stat="identity", width=1)+
          coord_polar("y", start=0)+
          labs(title="Analisi vendite totali per città",y="Vendite dal 2010 al 2014")
        
        
        ggplot(df_elaborato,aes(x="",y=mean_volume,fill=city))+
          geom_bar(stat="identity", width=1)+
          coord_polar("y", start=0)+
          labs(title="Volumi medi mensili per città (Mln $)",y="Media volumi mese per città ")
        
        
# ESERCITAZIONE GRAFICI ---------------------------------------------------------------------------------------------------------------------------------------------------
        
      # 1) Utilizza i boxplot per confrontare la distribuzione del prezzo mediano delle case tra le varie città. Commenta il risultato
  
          ggplot(dati_immobiliari)+
          geom_boxplot(aes(x=city,y=median_price))+
          xlab("Citta")+
          ylab("Prezzo Mediano di vendita")
        
          # Nei 4 anni di osservazione, la città che riporta il prezzo piu basso in termini di vendita è "Wichita Falls" mentre quella piu altra è "Bryan-College Station"
          # Nel caso della prima la variabilità è inferiore della seconda seppur su una mediana di prezzo molto piu elevata
          # Inoltre nel primo caso la mediana tende ad assestarsi su valori piu vicini al secondo quartile mentre nel secondo la mediana si attesta quasi alla media anche se di poco verso il terzo quartile.  
        
      # 2) Utilizza i boxplot o qualche variante per confrontare la distribuzione del valore totale delle vendite tra le varie città ma anche tra i vari anni. 
          # Qualche considerazione da fare?
    
          # Converto l'anno in fattore in modo che possa usarlo come legenda.
          dati_immobiliari$year <- as.factor(dati_immobiliari$year)
          ggplot(dati_immobiliari)+
            geom_boxplot(aes(x=city,y=volume,fill=year))+
            xlab("Citta")+
            ylab("Volumi totali anno")
          
        
          # La città di Tyler mostra un andamento crescente negli anni generando un volume per anno superiore mediamente per tutte le altre città seppur con una varianza 
          # sicuramente inferiore rispeto a quello che si registra a Bryan-College Station (almeno per quest'ultima escludendo l'anno 2011).
          # La città invece che mostra i piu bassi volumi è "Wichita Falls" con una variabilità annuale molto piu piccola rispetto alle altre città 
          # Solo nella città di Tyler i volumi tra l'anno 2010 e 2011 risultano essere cresciuti mentre nelle altre città si è registrato un ribasso seppur poi recuperato 
          # negli annu successivi,
          # L'anno 2014 per la città di Tyler ha registrato il piu alto picco di volume realizzato mediamente negli anni tra le 4 città osservate nei 4 anni
          # L'anno 2011 per Wichita Falls invece rappresenta l'anno con il piu basso volume realizzato di vendita tra le 4 città osservate nei 4 anni
          
          
        
      # 3) Usa un grafico a barre sovrapposte per ogni anno, per confrontare il totale delle vendite nei vari mesi, sempre considerando le città. 
           # Prova a commentare ciò che viene fuori. 
           # Già che ci sei prova anche il grafico a barre normalizzato. 
           # Consiglio: Stai attento alla differenza tra geom_bar() e geom_col(). 
           # PRO LEVEL: cerca un modo intelligente per inserire ANCHE la variabile Year allo stesso blocco di codice, senza però creare accrocchi nel grafico.
  
          
          # Anno - mesi - citta - volume
            
            dati_immobiliari$month <- as.factor(dati_immobiliari$month)
            ggplot(data = dati_immobiliari)+
              geom_col(aes(x=year,fill=month,y=volume))+
              facet_wrap(~city, nrow = 2) +
              labs(title = "Ripartizione delle Vendite per Anno Città e Mese", x = "Anno", y = "Volume")
            
          # Si evince che nella città di Tyle le vendite hanno un tasso di crescita maggiore tendenzialmente concentrate nei mesi finali dell'anno.
          # Nell'ultimo anno nei mesi finali di ottobre novembre e dicembre a Tyler i valori sono tra i piu alti di vendite mentre a "Whichita Falls" i valori piu bassi si registrano 
          # nel 2011 
          
      # 4) Crea un line chart di una variabile a tua scelta per fare confronti commentati fra città e periodi storici. 
           # Ti avviso che probabilmente all’inizio ti verranno fuori linee storte e poco chiare, ma non demordere. 
           # Consigli: Prova inserendo una variabile per volta. Prova a usare variabili esterne al dataset, tipo vettori creati da te appositamente.
           # Se non riesci proprio a venirne a capo inizia lavorando su dataset ridotti, ad esempio prendendo in considerazione un solo anno o una sola città. 
           # Aiutati con il pacchetto dplyr:
               # dati2014 <- filter(dati, year==2014)
               # dati_Beaumont <- filter(dati, city==”Beaumont”
            
            df_elaborato <- dati_immobiliari %>% group_by(city,year) %>% summarise (
              sum_sales = sum(sales))
            
            df_elaborato$city <- as.factor(df_elaborato$city)
            df_elaborato$year <- as.numeric(as.character(df_elaborato$year))
            
            ggplot(data = df_elaborato,aes(x=year,y=sum_sales,color=city))+
              geom_line()+
              labs(title = "Andamento delle vendite per anno delle città ", x = "Anno", y = "Vendite")
            
            
          
        
        
        
        
        
        
        
        
        
        
        