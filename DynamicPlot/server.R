library(shiny)
library(ggplot2)
theme_set(theme_minimal())
library(plyr)
shinyServer(function(input, output) {
    
    # set.seed(1000)
    f <- function(x) { sin(x) }
    n <- 150
    from <- 0
    to <- 4*pi
    sd.eps <- .3
    x <- seq(from=from, to=to, length=n)
    basic.function <- f(x)
    test <- rep(F, times=n)
    test[sample(n, size=floor(n/3))] <- T
    df <- data.frame(x, y = f(x) + rnorm(n, sd=sd.eps), test)
    
    
    my.func <- function(sp) {
        ss <- smooth.spline( df$x[!test], df$y[!test], spar=sp)
        data.frame(sp        = sp,
                   rmse.train = sqrt(mean((df$y[!test] - predict(ss, df$x[!test])$y)^2)),
                   rmse.test  = sqrt(mean((df$y[test] - predict(ss, df$x[test])$y)^2)))
    }
    
    # make data.frame of curve data:
    curves <- adply(seq(0,1,length=201), 1, my.func)
    # make a plot of the data:
    output$plot.model.curves <- renderPlot({
        ggplot(curves, aes(x=sp)) +
            geom_line(aes(y=rmse.train),color='blue') +
            geom_line(aes(y=rmse.test),color='red') +
            xlab("Smoothing parameter") + ylab("RMSE") +
            ggtitle("RMSE as Function of S.P.") +
            ylim(0,1.4)
        })

    ss <- reactive({ smooth.spline(df$x[!df$test], df$y[!df$test], spar=input$sp) })
    curvy <- reactive({ predict(ss(), seq(from=from, to=to, length=100)) })
    pred <- reactive({predict(ss(), df$x)})

    output$plot.function.with.fit <- renderPlot({
        ggplot(df, aes(x,y)) + geom_point(aes(col=test), size=2) +
            scale_color_manual(values=c('blue', 'red')) + ylim(-1.5, 1.5) +
            guides(color=F) + annotate(geom='line', x=curvy()$x, y=curvy()$y, color='blue',size=1) +
            annotate(geom='segment', x=pred()$x[!df$test], xend=pred()$x[!df$test],
                     y=df$y[!df$test], yend=pred()$y[!df$test], col='blue') +
            annotate(geom='segment', x=pred()$x[df$test], xend=pred()$x[df$test],
                     y=df$y[df$test], yend=pred()$y[df$test], col='red') +
            annotate(geom='line', x=x, y=basic.function)
    })
    
    resids.train <- reactive({
        data.frame(x=1:(n-floor(n/3)), train=sort(abs(df$y[!df$test] - pred()$y[!df$test])))
    })
    resids.test <- reactive({
        data.frame(x=1:floor(n/3), test=sort(abs(df$y[df$test] - pred()$y[df$test])))
    })
    
    output$plot.resid.train <- renderPlot({
        ggplot(resids.train(), aes(x, train)) +
            geom_segment(aes(xend=x, yend=0),color='blue') +
            ggtitle("Training residuals") + ylab('') + xlab('') + ylim(0,1.4) +
            theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                  axis.text.x=element_blank(), axis.ticks.x=element_blank())
    })
    
    output$plot.resid.test <- renderPlot({
        ggplot(resids.test(), aes(x, test)) +
            geom_segment(aes(xend=x, yend=0),color='red') +
            ggtitle("Test residuals") + ylab('') + xlab('') + ylim(0,1.4) +
            theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                  axis.text.x=element_blank(), axis.ticks.x=element_blank())
    })
})
