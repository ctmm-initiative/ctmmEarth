
Mvubu <- buffalo$Mvubu
Cillia <- buffalo$Cilla
Pepper <- buffalo$Pepper
Mvubu <- Mvubu[180:200,]
Cillia <- Cillia[180:200,]
Pepper <- Pepper[180:200,]
 GUESS <- ctmm.guess(Cillia, interactive = FALSE)
 FIT <- ctmm.fit(Cillia, GUESS, trace = TRUE)
 GUESS2 <- ctmm.guess(Mvubu, interactive = FALSE)
 FIT2 <- ctmm.fit(Mvubu, GUESS2, trace = TRUE)
as.kml(CTMM = list(FIT,FIT2),
       all_tour = FALSE,
       simulation_icons = TRUE,
       error_circle = TRUE,
       manual_cam = FALSE,
       pov_cam = FALSE,
       follow_cam = TRUE,
       duration = 60, 
       animals = list(Cillia,Mvubu), 
       num_simulations = 10)
