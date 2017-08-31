paste0_stop <- function(...) stop(paste0(...), call. = FALSE)

asrt <- function (expr, ...) if (!expr) paste0_stop(...)