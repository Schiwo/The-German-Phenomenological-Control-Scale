
#=================================================================
# Variablennamen
#=================================================================

#SK01			Hand lowering
#SK02			Moving hands together
#SK03			Mosquito hallucination
#SK04			Sweet taste
#SK05			Sour taste
#SK06			Arm rigidity
#SK07			Arm immobilization
#SK08			Music hallucination
#SK09			Negative visual hallucination
#SK10			Amnesia 		
#SK12			Urge to press space bar
#SK13 			Space bar amnesia
#KK02_01		Items remembered before being asked to remember
#KK03_01	    Items remembered after being asked to remember again
#SO01			Gender
#SO02_01		Age
#SC02			Answered all the questions seriously
#TIME005		Time the participant needed to complete the experiment


#=================================================================
# Ausschlusskriterien
#=================================================================

# Alle Fragen sinnvoll beantwortet
dat <- dat[dat$SC02 == 1 ,]

# Alle ausschließen, die die Studie in unter 15 Min durchgeführt haben
dat <- dat[dat$TIME005 > 900 ,]


#=================================================================
# Mean PC Score ausrechnen
#=================================================================

# =================hand senken=================
dat$F1 <- factor(dat$SK01, labels = c(0, 1, 2, 3, 4, 5))

dat$F1 <- as.numeric(dat$F1)
dat$F1 <- dat$F1-1

mean(dat$F1, na.rm = TRUE)

sd(dat$F1, na.rm = TRUE)

# =================hand magnet=================
dat$F2 <- factor(dat$SK02, labels = c(0, 1, 2, 3, 4, 5))

dat$F2 <- as.numeric(dat$SK02)
dat$F2 <- dat$F2-1

mean(dat$F2, na.rm = TRUE)

sd(dat$F2, na.rm = TRUE)

# =================Moskito=================
dat$F3 <- factor(dat$SK03, labels = c(0, 1, 2, 3, 4, 5))

dat$F3 <- as.numeric(dat$SK03)
dat$F3 <- dat$F3-1

mean(dat$F3, na.rm = TRUE)

sd(dat$F3, na.rm = TRUE)

# =================Süß=================
dat$F4 <- factor(dat$SK04, labels = c(0, 1, 2, 3, 4, 5))

dat$F4 <- as.numeric(dat$SK04)
dat$F4 <- dat$F4-1

mean(dat$F4, na.rm = TRUE)

sd(dat$F4, na.rm = TRUE)

# =================Sauer=================
dat$F5 <- factor(dat$SK05, labels = c(0, 1, 2, 3, 4, 5))

dat$F5 <- as.numeric(dat$SK05)
dat$F5 <- dat$F5-1

mean(dat$F5, na.rm = TRUE)

sd(dat$F5, na.rm = TRUE)

# =================Süß & Sauer=================
taste <- rowMeans(dat[, c(24:25)], na.rm = TRUE)
mean(taste)

sd(taste)

# =================Steifer Arm=================
dat$F6 <- factor(dat$SK06, labels = c(0, 1, 2, 3, 4, 5))

dat$F6 <- as.numeric(dat$SK06)
dat$F6 <- dat$F6-1

mean(dat$F6, na.rm = TRUE)

sd(dat$F6, na.rm = TRUE)

# =================Schwerer Arm=================
dat$F7 <- factor(dat$SK07, labels = c(0, 1, 2, 3, 4, 5))

dat$F7 <- as.numeric(dat$SK07)
dat$F7 <- dat$F7-1

mean(dat$F7, na.rm = TRUE)

sd(dat$F7, na.rm = TRUE)

# =================Musik=================
dat$F8 <- factor(dat$SK08, labels = c(0, 1, 2, 3, 4, 5))

dat$F8 <- as.numeric(dat$SK08)
dat$F8 <- dat$F8-1

mean(dat$F8, na.rm = TRUE)

sd(dat$F8, na.rm = TRUE)

# =================Bälle=================
dat$F9 <- factor(dat$SK09, labels = c(0, 1, 2, 3, 4, 5))

dat$F9 <- as.numeric(dat$SK09)
dat$F9 <- dat$F9-1

mean(dat$F9, na.rm = TRUE)

sd(dat$F9, na.rm = TRUE)

# =================Amnesie=================
dat$F10 <- factor(dat$SK10, labels = c(0, 1, 2, 3, 4, 5))

dat$F10 <- as.numeric(dat$SK10)
dat$F10 <- dat$F10-1

mean(dat$F10, na.rm = TRUE)

sd(dat$F10, na.rm = TRUE)

# =================Leertaste urge=================
dat$F11 <- factor(dat$SK12, labels = c(0, 1, 2, 3, 4, 5))

dat$F11 <- as.numeric(dat$SK12)
dat$F11 <- dat$F11-1

# =================Leertaste amnesie=================
dat$F12 <- factor(dat$SK13, labels = c(0, 1, 2, 3, 4, 5))

dat$F12 <- as.numeric(dat$SK13)
dat$F12 <- dat$F12-1

library(psych)

# Geometrischen Mittelwert von den zwei Werten berechnen
dat$PHS <- apply(dat[, c(31:32)], 1, geometric.mean)
mean(dat$PHS)

sd(dat$PHS)

# Die Werte löschen damit rowMeans nachher richtig funktioniert
dat$F11 <- NULL
dat$F12 <- NULL

# =================PC Score per Person=================
PC <- rowMeans(dat[, c(21:31)], na.rm = TRUE)

mean(PC)

sd(PC)

#=================================================================
# demographische Daten 
#=================================================================
table(dat$SO01)

mean(dat$SO02_01)

sd(dat$SO02_01)

summary(dat$SO02_01)

