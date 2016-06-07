# call my labels
u_um <- function(Text) bquote(.(Text) ~ "[" * mu * "m]")
u_cm2 <- function(Text) bquote(.(Text) ~ "[" * cm^2 * "]")
u_g_m2 <- function(Text) bquote(.(Text) ~ "[g" * m^-2 * "]")
u_mmol_m2 <- function(Text) bquote(.(Text) ~ "[mmol" ~ m^-2, "]")
u_SPFD <- function(Text) bquote(.(Text) ~ "[" * mu * "mol" ~ m^-2 ~ s^-1 ~ nm^-1 * "]")

u_flux <- function(Text, type = ""){
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


u_CO2 <- function(Text, type = "", mode = "mol"){
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

PAMyieldratios <- substitute(expr = italic(Y)[II] * "/" * italic(Y)[I])
