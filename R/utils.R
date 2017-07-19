paste0_stop <- function(...) stop(paste0(...), call. = FALSE)

paste0_msg <- function(...) message(paste0(...))

asrt <- function (expr, ...) if (!expr) paste0_stop(...)