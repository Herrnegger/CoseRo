.onAttach <- function(libname, pkgname) {
  msg <- paste0(
    "---------------------------------------------------------\n",
    " COSERO: Hydrological Model Interface (v", utils::packageVersion("COSERO"), ")\n",
    "---------------------------------------------------------\n",
    " Components Loaded Successfully:\n",
    "  [X] Core Run Interface\n",
    "  [X] Data Readers (read_cosero_output)\n",
    "  [X] Visualization & Shiny Helper Functions\n",
    "  [X] Sensitivity Analysis (Sobol framework)\n",
    "  [X] Optimized SPARTACUS Preprocessing\n",
    "---------------------------------------------------------\n",
    " Run 'launch_cosero_app()' to start the GUI.\n",
    "---------------------------------------------------------"
  )
  
  packageStartupMessage(msg)
}