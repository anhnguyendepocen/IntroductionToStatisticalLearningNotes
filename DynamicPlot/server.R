library(shiny)
library(ggplot2)
theme_set(theme_minimal())
library(plyr)
n <- 150
from <- 0
to <- 2*pi
sd.eps <- .3
x <- seq(from=from, to=to, length=n)

shinyServer(function(input, output, clientData, session) {
    
    observeEvent(input$resetSP, { updateSliderInput(session, "sp", value=optimal.sp()) })
    
    f <- reactive({
        switch(as.numeric(input$functionchoice),
               function(x) { sin(x/2) },
               function(x) { sin(x) },
               function(x) { sin(2*x) },
               function(x) { sin(4*x) })
    })
    
    basic.function <- reactive({ f()(x) })
    
    test.set <- rep(FALSE, times=n)
    test.set[sample(n, size=floor(n/3))] <- TRUE
    
    df <- reactive({ data.frame(x, y = f()(x) + rnorm(n, sd=sd.eps), test.set ) })
    
    
    my.func <- reactive({
        function(sp) {
            ss <- smooth.spline( df()$x[!test.set], df()$y[!test.set], spar=sp)
                data.frame(sp        = sp,
                   rmse.train = sqrt(mean((df()$y[!test.set] - predict(ss, df()$x[!test.set])$y)^2)),
                   rmse.test  = sqrt(mean((df()$y[test.set] - predict(ss, df()$x[test.set])$y)^2)))
        }
    })
    
    # make data.frame of curve data. This tries a lot of different smoothing
    # parameters and collects the resulting training and test RMSE in a data frame.
    curves <- reactive({ adply(seq(0,1,length=201), 1, my.func() ) })
    # make a plot of the data:
    output$plot.model.curves <- renderPlot({
        ggplot(curves(), aes(x=sp)) +
            geom_line(aes(y=rmse.train),color='blue') +
            geom_line(aes(y=rmse.test),color='red') +
            annotate(geom="point", x=input$sp, y=rmse.train(), color='blue', size=3) +
            annotate(geom="point", x=input$sp, y=rmse.test(), color='red', size=3) +
            xlab("Smoothing parameter") + ylab("RMSE") +
            ggtitle("RMSE as Function of S.P.") +
            ylim(0,1.4)
        })

    ss <- reactive({ smooth.spline(df()$x[!df()$test], df()$y[!df()$test], spar=input$sp) })
    curvy <- reactive({ predict(ss(), seq(from=from, to=to, length=100)) })
    pred <- reactive({predict(ss(), df()$x)})

    output$plot.function.with.fit <- renderPlot({
    ggplot(df(), aes(x,y)) + geom_point(aes(color=test.set), size=2) +
            scale_color_manual(values=c('blue', 'red')) + ylim(-1.5, 1.5) +
            guides(color=FALSE) + annotate(geom='line', x=curvy()$x, y=curvy()$y, color='blue',size=1) +
            annotate(geom='segment', x=pred()$x[!df()$test], xend=pred()$x[!df()$test],
                  y=df()$y[!df()$test], yend=pred()$y[!df()$test], col='blue') +
            annotate(geom='segment', x=pred()$x[df()$test], xend=pred()$x[df()$test],
                  y=df()$y[df()$test], yend=pred()$y[df()$test], col='red') +
            annotate(geom='line', x=x, y=basic.function())
    })
    
    resids.train <- reactive({
        data.frame(x=1:(n-floor(n/3)), train=sort(abs(df()$y[!df()$test] - pred()$y[!df()$test])))
    })
    resids.test <- reactive({
        data.frame(x=1:floor(n/3), test=sort(abs(df()$y[df()$test] - pred()$y[df()$test])))
    })
    
    optimal.sp <- reactive({
        with(curves(), sp[which.min(rmse.test)])
    })
    
    rmse.train <- reactive({ sqrt(mean(resids.train()$train^2)) })
    rmse.test <- reactive({ sqrt(mean(resids.test()$test^2)) })
    
    output$plot.resid.train <- renderPlot({
        ggplot(resids.train(), aes(x, train)) +
            geom_segment(aes(xend=x, yend=0),color='blue') +
            geom_hline(yintercept=rmse.train(),linetype="dashed",color="blue") +
            annotate(geom="text", x=0, y=rmse.train(),
                     label=paste("RMSE =", round(rmse.train(),3)), hjust=0, vjust=-.3, color='blue') +
            ggtitle("Training residuals") + ylab('') + xlab('') + ylim(0,1.4) +
            theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                  axis.text.x=element_blank(), axis.ticks.x=element_blank())
    })
    
    output$plot.resid.test <- renderPlot({
        ggplot(resids.test(), aes(x, test)) +
            geom_segment(aes(xend=x, yend=0),color='red') +
            geom_hline(yintercept=rmse.test(),linetype="dashed", color="red") +
            annotate(geom="text", x=0, y=rmse.test(),
                     label=paste("RMSE =", round(rmse.test(),3)), hjust=0, vjust=-.3, color='red') +
            ggtitle("Test residuals") + ylab('') + xlab('') + ylim(0,1.4) +
            theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                  axis.text.x=element_blank(), axis.ticks.x=element_blank())
    })
})
