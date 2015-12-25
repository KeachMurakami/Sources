# call my labels

uPFD <- paste("PFD [", mu, "mol", ~~ m^-2 ~~ s^-1,"]")
uPPFD <- paste("PPFD [", mu, "mol", ~~ m^-2 ~~ s^-1,"]")
uAbsPPFD <- paste("absorbed photons [", mu, "mol", ~~ m^-2 ~~ s^-1,"]")
uSPFD <- paste("spectral photon flux density [", mu, "mol", ~~ m^-2 ~~ s^-1,"]")

uPn <- paste(italic(P)[n], " [", mu, "mol", CO[2] ~~ m^-2 ~~ s^-1,"]")
udeltaPn <- paste(Delta, italic(P)[n], " [", mu, "mol", CO[2] ~~ m^-2 ~~ s^-1,"]")
uPg <- paste(italic(P)[g], " [", mu, "mol", CO[2] ~~ m^-2 ~~ s^-1,"]")
uGs <- paste(italic(g)[s], " [mol", H[2], "O", ~~ m^-2 ~~ s^-1,"]")
uCi <- paste(italic(C)[i], " [Pa]")
uCa <- paste(italic(C)[a], " [Pa]")

uLMA <- paste("LMA [g ", m^-2, "]")
uLA <- paste("leaf area [", cm^2, "]")
uThick <- paste("leaf thickness [", mu, "m]")

uChl <- paste("Chl content [mmol", ~~ m^-2, "]")
uChlRatio <- paste("Chl ", ~~ italic(a), "/", italic(b), " ratio [mol ", mol^-1, "]")
uCarotenoid <- paste("carotenoid content [g ", m^-2, "]")
