#!/usr/bin/env Rscript
# Runs all of Basingstoke's tests
# cd Basingstoke/tests
# ./run.R

options(warn=2)
library(Basingstoke)

AssertEqual <- function(a, b, ...) {
    res <- all.equal(a, b, ...)
    if(!isTRUE(res)) {
        stop(paste(res, collapse="\n"))
    }
}

AssertTrue <- function(v) {
    AssertEqual(TRUE, v)
}

AssertFalse <- function(v) {
    AssertEqual(FALSE, v)
}

AssertNull <- function(v) {
    AssertEqual(NULL, v)
}

AssertRaises <- function(ex) {
    # A function that expects an exception to be raise when ex is evalutated
    res <- tryCatch(eval(ex), error=function(e) e)
    if(!"error" %in% class(res)) {
        stop('Did not raise error\n')
    }
}

RunTests <- function(tests) {
    # tests should be a vector of function names
    options(error=function() {
                traceback(3)
                stop()
            }, 
            stringsAsFactors=FALSE)

    if(0==length(tests)) {
        stop('No tests to run! Is the working directory cheddar/tests ?')
    }
    else
    {
        for(test in tests) {
            cat(paste('Running [', test, ']\n', sep=''))
            do.call(test, args=list())
       }
    }
}

# Source all files in this dir except this one
files <- list.files(getwd(), pattern='*R$')
files <- setdiff(files, 'run.R')
junk <- sapply(file.path(getwd(), files), source)
tests <- commandArgs(trailingOnly=TRUE)
if(0==length(tests)) {
    tests <- ls(pattern=glob2rx('^Test*'))
}

RunTests(tests)
