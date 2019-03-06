library(shiny)
library(rvest)
library(jsonlite)

validURL<-function(x){
  if(!is.null(x)){
    isGH<-grepl("^(https?):\\/\\/(raw)*(\\.)*github(?:usercontent)?\\.com",x)
  }else{
    isGH<-FALSE
  }
  return(isGH)
}

githack_CDN<-function(url){
  ghack<-gsub("^(https?):\\/\\/(raw)*(\\.)*github(?:usercontent)?\\.com\\/([^\\/]+\\/[^\\/]+\\/[^\\/]+|[0-9A-Za-z-]+\\/[0-9a-f]+\\/raw)\\/(.+)",
              '\\1://raw.githack.com/\\4/\\5',url)
  
  apiToken<-gsub("^(\\w+:\\/\\/(raw).githack.com\\/([^\\/]+)\\/([^\\/]+))\\/([^\\/]+)\\/([^\\/]+)\\/(.*)",
                 'https://api.github.com/repos/\\3/\\4/git/refs/heads/\\6',
                 ghack)
  
  gh_sha<-jsonlite::fromJSON(apiToken)$object$sha
  
  newURL<-gsub("^(\\w+:\\/\\/(raw).githack.com\\/([^\\/]+)\\/([^\\/]+))\\/([^\\/]+)\\/([^\\/]+)\\/(.*)",
               paste0('https://rawcdn.githack.com/\\3/\\4/',gh_sha,'/\\7'),
               ghack)
  
  print(newURL)
  
  return(newURL)
}

shinyApp(
  ui = bootstrapPage(
    tags$html(style="width: 100%;height: 100%;position: fixed;outline: none;",
    tags$body(style="width: 100%;height: 100%;position: fixed;outline: none;",
    uiOutput("iframe",style="width: 100%;height: 100%;position: fixed;outline: none;"),
    title = "TwidgetServer",style='height: 100%;margin: 0;width:100%'))),
  server = function(input, output, session) {
    
    #getting URL
    url <- reactiveVal()
    observe({
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query)) {
        if("html" %in% names(query)){
          query<-query[["html"]]
        }else{
          query<-names(query)
        }
        if(validURL(query)){
          url(query)
        }else{
          url("invalid_URL")
          if(any(is.null(query),query!="")){
          updateTextInput(inputId = "ManualgithubURL",
            placeholder = "<span style='color:red'>InvalidURL</span>",
            session = session)
          }
        }
      }
    })
    
    #update URL
    observeEvent(input$submitGHURL,{
      if(input$ManualgithubURL!=""){
        updateQueryString(paste0("?",input$ManualgithubURL),mode = c("replace", "push"))
        url(input$ManualgithubURL)
      }
    })
    
    #render github url
    output$iframe<-renderUI({
      if( any(is.null(url()),
              length(url())==0,
              url()=="invalid_URL")){
        tags$div(style="gravity:center;height:100%",
                 tags$div(style="margin: 0 auto;text-align: center;",
                          tags$div(
          tags$p("Enter github URL"),
          textInput("ManualgithubURL",label = "Github URL",value = ""),
          actionButton("submitGHURL",label = "Submit")
        )))
      }else{
        cdn_url<-githack_CDN(url())
        
        
        
        tagList(
          tags$head(
            tags$meta(name="twitter:card",content="player"),
            tags$meta(name="twitter:title", content="Twidget: Served By Shiny"),
            tags$meta(name="twitter:description", content="Click start the Twidget. Served by github and Shiny"),
            tags$meta(name="twitter:player", content=cdn_url),
            tags$meta(name="twitter:player:width", content="600"),
            tags$meta(name="twitter:player:height", content="450"),
            tags$meta(name="twitter:image", content="http://www.edu.uwo.ca/img/click_to_play.png")
              
          ),
          tags$iframe(src=cdn_url,
                    style='height: 100%;margin: 0;width:100%'))
        
      }
    })
  }
)