#! /usr/bin/env Rscript

plumber::plumb(file="plumber.R")$run(
	host = "127.0.0.1", port = 8080, swagger = TRUE)
