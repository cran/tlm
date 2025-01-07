### welcome message
#' @importFrom utils packageDescription
.onAttach <- function(lib, pkg) {
  meta <- utils::packageDescription("tlm")
  attachmsg <- paste0("\nThis is tlm ",
                      meta$Version,
                      ". For details, use:\n",
                      "> help(package = 'tlm') and browseVignettes('tlm')\n\n",
                      "To cite the methods in the package use:\n",
                      "> citation('tlm')\n")
  packageStartupMessage(attachmsg, domain = NULL, appendLF = TRUE)
}
