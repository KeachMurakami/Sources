# Generalized
labeler <-
  function(text = "", units, dims, pre = "", abbrev = "", two_row = F){
    if(length(units) != length(dims)) stop("units and dims must be the same length")

    unit <-
      if_else(dims == 1, "", "^") %>%
      paste0(., dims) %>%
      if_else(. == "1", "", .) %>%
      paste0(units, .) %>%
      stringr::str_c(collapse = " ~ ") %>%
      paste0("'[' * ", pre, " * ", ., " * ']'")

    
    AbbrevList <-
      c(Pn = "italic(P)[n]",
        Pg = "italic(P)[g]",
        dPn = "Delta * italic(P)[n]",
        Gs = "italic(G)[s]",
        Ca = "italic(C)[a]",
        Ci = "italic(C)[i]",
        CO2 = "CO[2]"
      )
    
    abbreviated_quantity <-
      if_else(
        abbrev %in% names(AbbrevList),
        AbbrevList[abbrev],
        abbrev
      )
    
    if_else(two_row, ", ", " ~ ") %>%
      paste0("bquote(atop('", text, "' * ", abbreviated_quantity, ., unit, "))") %>%
      {eval(parse(text = .))}
  }

# fast access
u_um <- 
  function(Text) bquote(.(Text) ~ "[" * mu * "m]")
u_cm2 <- 
  function(Text) bquote(.(Text) ~ "[" * cm^2 * "]")
u_g_m2 <- 
  function(Text) bquote(.(Text) ~ "[g" * m^-2 * "]")
u_mmol_m2 <- 
  function(Text) bquote(.(Text) ~ "[mmol" ~ m^-2, "]")
u_SPFD <- 
  function(Text) bquote(.(Text) ~ "[" * mu * "mol" ~ m^-2 ~ s^-1 ~ nm^-1 * "]")

u_flux <- 
  function(Text = "", type = "", two_row = F){
  variables <-
    list(PFD = substitute(),
         Pn = substitute(expr = italic(P)[n]),
         Pg = substitute(expr = italic(P)[g]),
         dPn = substitute(expr = Delta * italic(P)[n]),
         Gs = substitute(expr = italic(G)[s]))
  
  
  if(type == 5){
    return(bquote(.(Text) ~ .(variables[[type]]) ~ "[mol" ~ m^-2 ~ s^-1 * "]")) 
  } else {
    return(bquote(.(Text) ~ .(variables[[type]]) ~ "[" * mu * "mol" ~ m^-2 ~ s^-1 * "]")) 
  }
}

u_CO2 <- 
  function(Text, type = "", mode = "mol"){
  variables <-
    list(Ca = substitute(expr = italic(C)[a]),
         Ci = substitute(expr = italic(C)[i]),
         CO2 = substitute(expr = CO[2]))
  
  if(mode == "mol"){
    return(bquote(.(Text) ~ .(variables[[type]]) ~ "[" * mu * "mol" ~ mol^-1 * "]"))
  } else {
    return(bquote(.(Text) ~ .(variables[[type]]) ~ "[Pa]"))
  }
}