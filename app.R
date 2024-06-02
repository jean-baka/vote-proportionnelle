#
# This is a Shiny web application. 
#
# Cette app calcule la répartition en sièges issue
# des résultats d'un scrutin à la proportionnelle.
# Pour le moment, seulement pour les scrutins à la plus forte moyenne.
#
# HACK: quand les gens rentrent les résultats en nb de voix exprimées, on simule
# un corps électoral de dix millions (10000000L) d'EXPRIMÉS.
# N.B. le taux d'abstention est absolument invisible dans ce cas.
#

library(shiny)
library(dplyr)
library(magrittr) # for %<>%
library(purrr) # to create the content for renderUI, from a map
library(DT)

# Define UI for application
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styling.css")
  ),
  
  # Application title
  titlePanel("Répartition des sièges à la proportionnelle – plus forte moyenne"),
  
  # Sidebar with a slider input for number of bins 
 sidebarLayout(
    sidebarPanel(
      
      numericInput("siegesAPourvoir",
                   "Nombre de sièges à pourvoir (81 pour les européennes FR de 2024)",
                   min = 1,
                   step = 1,
                   max = 5000,
                   value = 81),
      
      numericInput("numListes",
                   "Nombre de listes (pas nécessaire de les mettre toutes)",
                   min = 1,
                   step = 1,
                   max = 100,
                   value = 10),
      
      # radioButtons("typeProportionnelle",
      #              "Scrutin proportionnel",
      #              choices = c(`à la plus forte moyenne (e.g. sénatoriales, européennes, municipales)` = "moyenne", `au plus fort reste (e.g. conseils d'université)` = "reste"),
      #              selected = "moyenne"),
      
      numericInput("seuilMinimal",
                   "Seuil électoral (% des exprimé·e·s) pour l'admission aux sièges, par ex. 5% pour les européennes FR",
                   min = 0,
                   max = 100,
                   step = 1,
                   value = 5),
      
      radioButtons("typeEntree",
                   "Résultats entrés",
                   choices = c(`en % des exprimé·e·s` = "pourcent", `en nb de voix` = "voix"),
                   selected = "pourcent"), # défaut à changer pour les sénatoriales, par exemple
      
      
      uiOutput("suffrages"), # the series of numericInput() (one for each list)
      # this uiOutput is styled in the CSS
      
      # submitButton("Répartir les sièges") # instantly deactivates reactivity for the whole app
      br(), br(),
      
      actionButton("go", "(Re)répartir les sièges", class = "btn-success center")
      
    ),
    
    # Panneau principal : header et table des résultats
    mainPanel(
      htmlOutput("texteQuotient"),
      br(), br(),
      dataTableOutput("tableSieges")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  num_exprimes_fictif <- 10000000L # utilisé uniquement lorsque les résultats
  # sont donnés en pourcentages
  
  prettyNum <- function(x) { base::format(x, scientific = F, big.mark = " ") } # using U+202F
  
  # useful to interact with a modal:
  # when we need to break hard ties, we send "to the user"
  # a list of two vectors (listes, positions)
  # in which listes is the vector of list (ticket) indices
  # and positions is the vector of the corresponding positions,
  # in the respective lists, of the candidate entering the tie-break
  # hardTies <- reactiveVal(list())
  
  # Generate the list of numericInput fields for the number of votes
  fieldIDs <- reactive(paste0("suffrages_", seq_len(input$numListes)))
  fieldLabels <- reactive(paste("Résultat liste",  seq_len(input$numListes)))
  
  # Render the inputs corresponding to the different lists/electoral tickets
  output$suffrages <- renderUI({
    map2(fieldIDs(), fieldLabels(),
         ~ numericInput(inputId = .x, label = .y,
                        min = 0, step = 1,
                        value = isolate(input[[.x]] %||% 10)))
  })
  
  # compile all votes into one vector:
  voixObtenues <- reactive({
    req(input$numListes, input$suffrages_1)
    
    multiplying_factor <- ifelse(input$typeEntree == "pourcent", num_exprimes_fictif/100, 1L)

    # purrr::map_int() returns an integer vector:
    # but we prefer map_dbl() which is not limited to a few billion votes! :D
    map_dbl(1:input$numListes,
            function(x) {
              temp <- input[[paste0("suffrages_", x)]]
              ifelse(isTruthy(temp), multiplying_factor * temp, 0L)
              } # end function
    ) # end map_dbl()
    # the isTruthy() test in order not to get any error
    # when one score is erased (blank field)
  })
  
  suffragesExprimes <- reactive({
    # The max() here allows to specify only a few scores out of many running ballots,
    # and see the real number of seats (inferior to the number of available seats)
    # they would get.
    ifelse(input$typeEntree == "voix",
           sum(voixObtenues(), na.rm = T),
           max(sum(voixObtenues(), na.rm = T), num_exprimes_fictif)
           )
    })
  # And we have to yield an error in case input$typeEntree == "pourcent" AND
  # suffragesExprimes() > num_exprimes_fictif:
  # we write this in a validate() within the computation of output$tableSieges
  # as well as in siegesMoyenne()
  
  minimumVoixPourSiege <- reactive(ceiling(suffragesExprimes() * input$seuilMinimal / 100))
  
  quotient <- reactive({
    req(suffragesExprimes, input$siegesAPourvoir)
    suffragesExprimes() / input$siegesAPourvoir # will never be zero since siegesAPourvoir is >= 1
  })
  
  output$texteQuotient <- renderUI({
    sum_perc <- sum(voixObtenues() * 100 / num_exprimes_fictif)
    
    HTML(paste(
      
      strong("Récapitulatif des paramètres du scrutin"),
      
      paste("Suffrages exprimés (chiffre réel si résultats entrés en voix, virtuel sinon) :",
            prettyNum(suffragesExprimes())),
      
      ifelse(input$typeEntree == "voix",
             "Supposés correspondre à 100% des exprimés.",
             ifelse(sum_perc != 100,
                    paste0('<span style=\"color:red\">',
                           "Somme des pourcentages entrés : ",
                           sum_perc,
                           ifelse(sum_perc < 100,
                                  " (Attention ! Cette application attribue systématiquement tous les sièges à pourvoir.)",
                                  " (Attention ! La somme des poucentages entrés dépasse 100.)"
                           ),
                           '</span>'), # end of paste0()
                    paste("Somme des pourcentages entrés :", sum_perc))
             ),
 
      paste("Quotient électoral :",
            prettyNum(round(quotient(), 2))),
      
      paste("Suffrages minimum pour prétendre à un siège :",
            prettyNum(minimumVoixPourSiege())),
      sep = "<br/>"))
    
  })
  
  # on seuille avec minimumVoixPourSiege() pour exclure de la distribution
  # les listes ayant fait moins:
  voixPourSieges <- reactive({
    ifelse(voixObtenues() >= minimumVoixPourSiege(), voixObtenues(), 0)
  })
  
  siegesQuotient <- reactive({
    req(quotient, voixPourSieges)
    map_int(voixPourSieges(), ~ floor(.x / quotient()))
  })
  
  siegesRestantApresQuotient <- reactive({
    input$siegesAPourvoir - sum(siegesQuotient())
  })
  

  # problème des listes à départager lorsqu'elles ont la même moyenne ET
  # ont reçu le même nombre de suffrages (ce qui est le premier tie-breaker) :
  # l'utilisateur doit indiquer quelle est le candidat le plus vieux, à qui
  # le siège ira (ou le plus jeune, on laisse ça à gérer au niveau du modal).
  # Dès qu'on se trouve en présence d'une égalité à casser, on doit
  # arrêter les calculs pour laisser la place au modal.
  # On va donc utiliser une variable globale tieBreakers qui va accumuler les
  # réponses données aux modaux (il peut y avoir plusieurs cas d'égalité à
  # casser dans un même scrutin)
  tieBreakers <- reactiveVal(list()) # on commence avec une liste vide
  
  observeEvent(input$go, { tieBreakers(list()) }, priority = 3) # reinitialize the list of tiebreakers
  # each time one writes new poll results and hits "Go!"
  
  # does all the calculations for the seats "à la plus forte moyenne":
  siegesMoyenne <- eventReactive(
    {
      validate(
        need(input$typeEntree == "voix" | suffragesExprimes() <= num_exprimes_fictif,
             "Erreur : la somme des pourcentages que vous avez entrés est supérieure à 100.")
      )
      
      tieBreakers() #reacting to input$registerTieWinner instead double-triggers the modal because of
      # reacting to the *creation* of the variable when the modal is created.
      input$go
    }, {
    
    # just to make sure the dependency on the tieBreakers is enforced:
    toto <- tieBreakers()
    
    # local var that will be the result of the current function
    result <- numeric(input$numListes)
    # local var for the number of seats allocated so far to each ticket
    siegesObtenus <- isolate(siegesQuotient())
    # local var for the remaining number of seats to allocate
    siegesRestant <- isolate(siegesRestantApresQuotient())
    
    # une variable qui nous sert à voir où on en est dans la consommation
    # des tie breakers:
    hardTiesBroken <- 0 # variable locale à cette fonction
    

    # calcul des sièges à la plus forte moyenne (boucle)
    while(siegesRestant > 0) {
      
      # qui a la meilleure moyenne?
      meilleureMoyenne = 0
      vecteurDesMeilleurs = integer() # on va y mettre les indices des meilleures moyennes
      
      for (i in 1:input$numListes) {
        myMoyenne = voixPourSieges()[i]/(siegesObtenus[i] + 1)
        if(myMoyenne > meilleureMoyenne) {
          meilleureMoyenne <- myMoyenne
          vecteurDesMeilleurs <- i
        } else {
          if(myMoyenne == meilleureMoyenne)
            vecteurDesMeilleurs %<>% c(i)
        } # end ifelse
      } # end for
      
      # on sait maintenant qui a la meilleure moyenne, on va y attribuer un siège:
      if (length(vecteurDesMeilleurs) == 1) {
        # facile, on attribue le siège:
        result[vecteurDesMeilleurs] %<>% `+`(1)
        siegesObtenus[vecteurDesMeilleurs] %<>% `+`(1)
        siegesRestant %<>% `-`(1)
      } else { #break ties (because length(vecteurDesMeilleurs) >= 2)
        # by allocating the seat to the list having received most votes:
        tiers <- numeric() # we will build the list of contenders for this seat
        mostVotes <- 0
        for (i in vecteurDesMeilleurs) {
          if(voixPourSieges()[i] > mostVotes) {
            mostVotes = voixPourSieges()[i]
            tiers = i
            } else if(voixPourSieges()[i] == mostVotes) tiers %<>% c(i) 
        } # end for (i in vecteurDesMeilleurs)
        # we now have the tiers:
        if (length(tiers) == 1) {
          # facile, on attribue le siège:
          result[tiers] %<>% `+`(1)
          siegesObtenus[tiers] %<>% `+`(1)
          siegesRestant %<>% `-`(1)
        } else {
          # On se trouve dans le cas de figure où deux listes au moins
          # ont la même moyenne ET ont reçu le même nombre de suffrages.
          # On doit casser l'égalité en allant chercher le plus vieux des
          # candidats susceptibles d'être élus (pour les sénatoriales).
          
          # Si la liste tieBreakers contient au moins un élément de plus que
          # hardTiesBroken, on consomme cet élément et on poursuit le calcul.
          # Sinon, on affiche le modal et on sort de cette fonction.
          
          if(length(tieBreakers()) > hardTiesBroken) {
            # on va "consommer" un élément de cette liste de tieBreakers
            # pour affecter un siège à la liste en question:
            hardTiesBroken %<>% `+`(1)
            listeGagnante <- tieBreakers()[[hardTiesBroken]][1]
            result[listeGagnante] %<>% `+`(1)
            siegesObtenus[listeGagnante] %<>% `+`(1)
            siegesRestant %<>% `-`(1)
          } else {
            hardTies <- map2(.x = tiers, .y = siegesObtenus[tiers] + 1, .f = c)
            showModal(dataModal(hardTies))
            return(result) # break, break, break!
          } # end ifelse on length(tieBreakers())
        } # end ifelse on length(tiers)
      }  # end ifelse on length(vecteurDesMeilleurs)
    } # end while on siegesRestant
    
    return(result)
  })
  
  string_candidat <- function(pair) {
    # the input is a vector (a,b) where a is the list index
    # and b is the position in that list
    l <- pair[1]
    p <- pair[2]
    sprintf("candidat·e en position %d dans la liste %s", p, l)
  }
  
  #observeEvent(breakingHardTies(), { showModal(dataModal()) })
  
  dataModal <- function(tiers, failed = FALSE) {
    # remember tiers is of the form list(c(6,1), c(5,1), c(7,2))
    # to break ties between 1st candidate of list 6, 1st candidate of list 5
    # and 2nd candidate of list 7
    modalDialog(
      span("À ce stade, plusieurs listes ayant la même moyenne ET le même",
           "nombre de suffrages obtenus, il faut départager les candidat·e·s ci-dessous",
           "selon les règles propres au scrutin. Par exemple, pour les élections sénatoriales,",
           "le siège est attribué au candidat le plus âgé (Code électoral, article R169).",
           "Pour les élections européennes, le siège va à la liste dont la moyenne d'âge",
           "est la moins élevée (loi n° 77-729, article 3).", br(), br()),
      radioButtons("bestTier", "Le siège en balance va au/à la...",
                choices = map(tiers,`[`(1)) |> `names<-`(map(.x = tiers, .f = string_candidat))),
      
      title = "Départage des ex aequo",
      
      footer = tagList(
        modalButton("Annuler et redistribuer les suffrages"),
        actionButton("registerTieWinner", "Valider le départage", class = "btn-success")
      )
    )
  }
  
  observeEvent(input$registerTieWinner, {
    showNotification("Départage enregistré")
    removeModal()
    # careful: because of the way radioButtons() or selectInput() works,
    # the contents of input$bestTier is a string containing a number
    tieBreakers(c(tieBreakers(), as.numeric(input$bestTier))) # adding the solution
  })
  
  output$tableSieges <- renderDataTable({ # from DT if that library is loaded
    req(input$numListes, siegesQuotient(), siegesMoyenne())
    validate(
      need(input$typeEntree == "voix" | suffragesExprimes() <= num_exprimes_fictif,
           "Erreur : la somme des pourcentages que vous avez entrés est supérieure à 100."),
      need(length(siegesMoyenne()) == input$numListes, "Vous devez cliquer sur le bouton pour redemander le calcul de la répartition en sièges.")
    )
    tibble(Liste = 1:input$numListes,
           Suffrages = voixObtenues(),
           `Pourcentage des exprimés` = round(100 * Suffrages / suffragesExprimes(), 2),
           `Sièges au quotient` = siegesQuotient(),
           `Sièges à la plus forte moyenne` = siegesMoyenne(),
           `Total des sièges acquis` = `Sièges au quotient` + `Sièges à la plus forte moyenne`) %>%
      datatable(rownames = F, options = list(
        dom = 't',
        ordering = FALSE,
        paging = FALSE,
        searching = FALSE)) %>% 
      formatRound(columns = 2, digits = 0, mark = " ") %>% 
      formatStyle(columns = 4:6, fontWeight = styleInterval(0, c('normal', 'bold')))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

