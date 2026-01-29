.onAttach <- function(libname, pkgname) {
  msg <- paste0(
    "---------------------------------------------------------\n",
    " COSERO: Hydrological Model Interface (v", utils::packageVersion("COSERO"), ")\n",

    " Run 'launch_cosero_app()' to start the GUI.\n",
    " Run 'help(COSERO)' to get more information.\n",
    "---------------------------------------------------------"
  )
  
  packageStartupMessage(msg)
}