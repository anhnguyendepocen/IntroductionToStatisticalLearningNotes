library(shiny)
shinyUI(fluidPage(
    
    titlePanel("Basic Model-Fitting Demo"),
    
    fluidRow(
        column(width=12,
               p("This shiny app demonstrates a few basic concepts of machine learning."),
               withMathJax(p("The data below have been simulated from the sine curve shown in black, 
                 \\(0 \\le x \\le \\pi \\).
                 We are pretending that we don't know it's a sine curve, and trying to discover
                 the relationship between the variables by fitting a spline
                 curve using R's ", code("smooth.spline"), " function.")),
               p("The data have been randomly partitioned into a ", strong("training set"), " (blue),
                 and a ", strong("test set"), " (red). The spline curve is
                 fit only to the training points."),
               p("The user can control the wiggliness of
                 the fitted curve by adjusting the ", strong("smoothing parameter"),
                 ". When the smoothing parameter is close to 1, the curve is almost a straight line,
                 resulting in large residuals for both the training and test set. On the other hand,
                 by bringing the smoothing parameter close to 0, the user can bring the wiggly
                 curve almost arbitrarily close to the blue points. The tradeoff is a
                 poorly-performing model that has large errors on the test points. This phenomenon
                 of small training errors and large test errors is known as ", strong("overfitting"),
                 ". A decent model is somewhere in between, near ",
                 strong("where the test error curve reaches its minimum"), ".")
               
               )
    ),
    
    fluidRow(
        column(width=3,sliderInput("sp", label=h3("smoothing parameter"), min=0, max=1, value=.5)),
        column(width=9,plotOutput("plot.function.with.fit"))
    ),
    
    fluidRow(column(width=6,plotOutput("plot.model.curves")),
             column(width=4,plotOutput("plot.resid.train")),
             column(width=2,plotOutput("plot.resid.test")))
))