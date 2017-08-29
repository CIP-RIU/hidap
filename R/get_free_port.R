get_free_port <- function(port = NULL){
  host = "localhost"
  if (is.null(port)) {
    for (i in 1:20) {
      port <- p_randomInt(3000, 8000)
      tmp <- try(startServer(host, port, list()), silent = TRUE)
      if (!inherits(tmp, "try-error")) {
        stopServer(tmp)
        .globals$lastPort <- port
        break
      }
    }
  }
  port
}
