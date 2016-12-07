
ui <- fluidPage(
  theme='notebooks.css',
  withMathJax(),
  tags$script("
                    MathJax.Hub.Config({
                    tex2jax: {
                    inlineMath: [['$','$']],
                    processEscapes: true
                    }
                    });"
  ),
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Open+Sans:400,700|Cabin:400,700|Inconsolata:400,700|Roboto:400,700');

                    p {
                    font-family:  'Roboto', san-serif;
                    font-weight: 400;
                    line-height: 1.1;
                    color: gray;
                    }

                    "))),
  # Application title
  # titlePanel("Bayesian Data Analysis"),
  headerPanel(

    # tags$style("h5 {color:aliceblue;}
    #               label {color:dodgerblue;"),
    HTML("<span style=\"color:#ff6eb4; font-family:'Magneto'; \">Bayesian Demo</span>

    <br> <div><h5 style=\"color:gray; font-family:'Roboto'\">The following demo regards estimating a mean (ignoring estimating the variance) and how the prior distribution and likelihood combine to produce the posterior distribution. <br><br>You can set the following parameters: <br><br>

    <ul>
    <li> Sample size: 0-250
    <li> Observed and Prior Means: 1-10
    <li> Observed and Prior Variances: 1-5
    </ul>


    The tested $\\theta$ parameters are a sequence of 500 values from 0 to 10.
    </h5></div>"),
    windowTitle= 'Bayesian Demo'),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
                sidebarPanel(
      tags$style(".well {background-color:aliceblue;}
                  label {color:cornflowerblue; font-family:'Gill Sans MT'}"),
      numericInput("n",
                   "Data Sample Size:",
                   min = 0, max = 250, value = 10, step=10),
      numericInput("dataMean",
                   "Data Mean:",
                   min = 1, max = 10, value = 7, step=1),
      numericInput("dataVar",
                   "Data Variance:",
                   min = 1, max = 5, value = 3),
      numericInput("priorMean",
                   HTML("Prior Mean for $\\theta$:"),
                   min = 1, max = 10, value = 2, step=1),
      numericInput("priorVar",
                   HTML("Prior Variance for $\\theta$:"),
                   min = 1, max = 5, value = 1)
    , width=2),


    mainPanel(
      plotly::plotlyOutput("bayesPlot", width = '100%'),
      withMathJax(),
      htmlOutput('results'),
      br(),
      "$$p(\\theta|Data) \\propto p(Data|\\theta) \\cdot p(\\theta)$$
                      $$\\;\\;\\mathrm{posterior} \\;\\propto \\mathrm{likelihood} \\;\\!\\cdot \\mathrm{prior}$$",
      br(),
      HTML(
        "<p>All three distributions regard the <em>parameter</em> to be estimated, i.e. $\\theta$, the mean (we're assuming the variance is known for this demo).</p>

        <p>The <span style=\"color:#F8766D;font-weight:600\">prior</span> regards the initial distribution given for $\\theta$. This may be based on prior beliefs and/or research, or simply one known to work well within the modeling context. Here it is a normal distribution with the mean and variance you provide. More variance would mean a less informative prior.</p>

        <p>The <span style=\"color:#00BA38;font-weight:600\">likelihood</span> regards the data given a particular estimate for $\\theta$, and is the same that one is familiar with from standard maximum likelihood methods. The observed mean is the estimate we'd get using a maximum likelihood approach.  In this case we're assuming a normal distribution as the data generating process.</p>

        <p>Finally, the <span style=\"color:#619CFF;font-weight:600\">posterior</span> is the likelihood for the $\\theta$ values from the Bayesian estimation process, and can be seen as a weighted combination of the prior and the likelihood.</p>"
      ),
      tags$style("#results {color:cornflowerblue; font-variant:small-caps; font-family:'Inconsolata';}
                  #caption {color:gray;}")
    , width=5)
)
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  library(tidyverse); library(gridExtra); library(plotly)
  simN = 500
  theta = seq(0, 10, length.out = simN)

  obs  = reactive({ rnorm(input$n, input$dataMean, sqrt(input$dataVar))})
  prior = reactive({data.frame(Distribution='Prior', theta=theta,
                     density = dnorm(theta, input$priorMean, sqrt(input$priorVar))) %>%
      mutate(density=density/sum(density))
  })
  like = reactive({data.frame(Distribution='Likelihood', theta=theta,
                    density = sapply(theta, function(parm) exp(sum(dnorm(obs(), mean=parm, sd=sqrt(input$dataVar), log = T))))) %>%
      mutate(density=density/sum(density))
  })
  denom = reactive({sum(like()$density*prior()$density)})
  post = reactive({data.frame(Distribution='Posterior', theta=theta,
                    density = like()$density*prior()$density/denom()) %>%
      mutate(density=density/sum(density))
  })
  thetamean = reactive({sum(post()$density*theta)})
  plotdata = reactive({rbind(prior(), like(), post())})



  output$results = renderText({
    HTML(paste0("Observed Mean = ", format(mean(obs()), digits=3, nmsall=2), '<br>',
                "Mean SE = ",  format(sd(obs())/sqrt(input$n), digits=3, nmsall=2), '<br>',
                "Posterior Mean = ",  format(thetamean(), digits=3, nmsall=2))
         )
  })


  output$bayesPlot = renderPlotly({

    g = ggplot(aes(x=theta, y=density, group=Distribution, color=Distribution, fill=Distribution), data=plotdata()) +
      geom_ribbon(aes(ymin=0, ymax=density), alpha=.5 ) +
      geom_point(aes(x=value, y=0), data=data.frame(Distribution=c('Prior', 'Likelihood', 'Posterior'),
                                                        value=c(input$priorMean, mean(obs()), thetamean())),
                 color=alpha('#ff5503', .25)) +
      # facet_wrap(~Distribution, scales = 'free_y', ncol = 1) +
      xlab('') +# xlab(HTML('\\(\\theta\\)')) + # between shiny plotly and the web, it just don't work
      lims(x=c(0, 10)) +
      lazerhawk::theme_trueMinimal() +
      theme(axis.title.x=element_text(color=alpha('black',.6), vjust=.1, hjust=.5),
            axis.text.y=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            strip.text=element_text(color=alpha('black',.5), vjust=.01),
            legend.position='none',
            plot.background=element_rect(fill = "transparent",colour = NA))

    ggplotly(g, tooltip='none') %>%
      config(displayModeBar=F, sendData = F, displaylogo=F, collaborate=F,     # few of these work as advertiseed
             modeBarButtonsToRemove=list('lasso2d', 'hoverCompareCartesian', 'hoverClosestCartesian')) %>%
      layout(paper_bgcolor=rgb(0,0,0,0), plot_bgcolor=rgb(0,0,0,0))

  })

}

# Run the application
shinyApp(ui = ui, server = server)

# for testing
# input = data.frame(n=600, dataMean=5, dataVar=1, priorMean=5, priorVar=1)
# simN = 1000
# data  = rnorm(10, input$dataMean, input$dataVar)
# theta = seq(2, 8, length.out = input$n)

