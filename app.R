library(shiny)
library(rvest)
library(RCurl)

validURL<-function(x){
  isGH<-grepl("https[:]//(github[.]com)|(raw[.]githubusercontent[.]com)",x)
  isValid<-RCurl::url.exists(x)
  return(isGH && isValid)
}

raw_html<-function(x){
  if(!grepl("(https[:]//raw[.]githubusercontent[.]com)|([?]raw[=]true)",x)){
    x<-paste0(x,"?raw=true")
  }
  iframe_html<-xml2::read_html(paste(readLines(x),collapse=""))
  head<-as.character(html_children(html_node(iframe_html,"head")))
  body<-as.character(html_children(html_node(iframe_html,"body")))
  
  return(list(head=head,body=body))
}

shinyApp(
  ui = fluidPage(uiOutput("iframe"),title = "TwidgetServer",style='height: 100%;margin: 0;width:100%'),
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
        }
      }
    })
    
    observeEvent(input$submitGHURL,{
      if(input$ManualgithubURL!=""){
        updateQueryString(paste0("?",input$ManualgithubURL),mode = c("replace", "push"))
        url(input$ManualgithubURL)
      }
    })
    
    output$iframe<-renderUI({
      if(is.null(url()) | url()=="invalid_URL"){
        tags$div(
          tags$p("Enter github URL"),
          textInput("ManualgithubURL",label = "Github URL",value = ""),
          actionButton("submitGHURL",label = "Submit")
        )
      }else{
        url_html<-raw_html(url())
        tagList(tags$head(HTML(url_html$head)),HTML(url_html$body))
        # tags$link(rel='import',href=url())
      }
    })
  }
)