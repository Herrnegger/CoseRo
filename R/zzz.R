.onAttach <- function(libname, pkgname) {
  msg <- paste0(
    "---------------------------------------------------------\n",
    " CoseRo: Hydrological Modelling Library (v", utils::packageVersion("CoseRo"), ")\n",

    " Run 'launch_cosero_app()' to start the GUI.\n",
    " Run 'help(CoseRo)' to get more information.\n",
    "---------------------------------------------------------"
  )
  
  packageStartupMessage(msg)
}