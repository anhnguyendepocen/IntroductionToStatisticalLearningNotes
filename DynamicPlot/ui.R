library(shiny)
shinyUI(fluidPage(
    
    titlePanel("Basic Model-Fitting Demo"),
    
    fluidRow(p("This shiny app demonstrates a few basic concepts of machine learning."),
             withMathJax(p("The data in the top right graph have been simulated from
                 the sine curve shown in black, 
                 \\(0 \\le x \\le 2 \\pi \\).
                 We are pretending that we don't know it's a sine curve, and trying to discover
                 the relationship between the variables by fitting a spline
                 curve (shown in blue) using R's ", code("smooth.spline"), " function. ",
                           a(href='#explanation',"See below for more documentation.")))),
    
    fluidRow(
        column(width=4,
               selectInput("functionchoice", "Function",
                           list("sin(x/2)"=1,
                                "sin(x)"=2,
                                "sin(2x)"=3,
                                "sin(4x)"=4),selected=2),
               sliderInput("sp", label="Smoothing Parameter (S.P.)", min=0, max=1, value=.5),
               actionButton(inputId='resetSP',label='Reset to optimal S.P. (minimizing test RMSE)')),
        column(width=8,plotOutput("plot.function.with.fit", height="370px"))
    ),
    
    fluidRow(column(width=4,plotOutput("plot.model.curves", height="250px")),
             column(width=5,plotOutput("plot.resid.train", height="242px")),
             column(width=3,plotOutput("plot.resid.test", height="242px"))),
    
    fluidRow(
        column(width=12,a(name='explanation',h2("Explanation")),
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
    )
))