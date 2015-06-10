# call my labels

uPPFD <- expression(paste("PPFD [", mu, "mol", ~~ m^-2 ~~ s^-1,"]"))
uAbsPPFD <- expression(paste("absorbed photons [", mu, "mol", ~~ m^-2 ~~ s^-1,"]"))
uSPFD <-   expression(atop("spectral photon flux density",
                          paste("[", mu, "mol", ~~ m^-2 ~~ s^-1,"]")))
uSPFDlong <-   expression(paste("spectral photon flux density [", mu, "mol", ~~ m^-2 ~~ s^-1,"]"))

uPn <- expression(paste(italic(P)[n], " [", mu, "mol", CO[2] ~~ m^-2 ~~ s^-1,"]"))
udeltaPn <- expression(paste(Delta, italic(P)[n], " [", mu, "mol", CO[2] ~~ m^-2 ~~ s^-1,"]"))
uPg <- expression(paste(italic(P)[g], " [", mu, "mol", CO[2] ~~ m^-2 ~~ s^-1,"]"))
uGs <- expression(paste(italic(g)[s], " [mol", H[2], "O", ~~ m^-2 ~~ s^-1,"]"))
uCi <- expression(paste(italic(C)[i], " [Pa]"))
uCa <- expression(paste(italic(C)[a], " [Pa]"))

uLMA <- expression(paste("LMA [g ", m^-2, "]")) 
uLA <- expression(paste("leaf area [", cm^2, "]"))
uThick <- expression(paste("leaf thickness [", mu, "m]"))

uChl <- expression(paste("Chl content [mmol", ~~ m^-2, "]"))
uChlRatio <- expression(paste("Chl ", ~~ italic(a), "/", italic(b), " ratio [mol ", mol^-1, "]"))
uCarotenoid <- expression(paste("carotenoid content [g ", m^-2, "]"))
