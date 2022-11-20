
# But calculer les valeurs pour rigging le nouveau bateau filippi

# Schéma géometrique construit sous https://www.geogebra.org/geometry

# Library

library(tidyverse)

# Body segments input
# On average, each of three body segments contributes approximately one third to the total length of
# the stroke arc (legs a bit more, trunk a bit less). The legs execute their work during the first half of
# the drive, when the force exertion is maximal. Therefore, the legs produce nearly half of the rowing
# power (46%); the trunk produces nearly one third (32%) and arms a bit more than one fifth (22%).
# As higher stroke rates the legs increase their percentage contribution power. Thus, trunk muscles
# utilize only about 55% of their work capacity during rowing. At the same time, the arms use about
# 75% and the legs up to 95% of their respective work capacity.

# (Rowing Biomechanics © 2006 Dr. Valery Kleshnev, p. 15)

# Kraft 

# Legs
legs <-  46*95/100

# Trunk
trunk <- 32*55/100

# Arms
arms <- 22*75/100

parts <- c(legs, trunk, arms)


# 

df <- tibble(parts = parts) %>% 
    mutate(total = sum(parts)) %>% 
    mutate(parts_pc = parts / total *100)

# Gearing proxy Stefano Ntouskos https://youtu.be/7_oGz7-A9QE?t=3754
# Spread 159 cm

oarlenght <- 283
inboard <- 87.5

gearing_ratio_proxy <- (oarlenght - inboard)/inboard
print(gearing_ratio_proxy)

# At a typical sculling inboard of 88cm, an oarlength of 288cm and blade 
# length of 45cm, the actual oar gearing is 2.089. 
# Whenaccounting for the measurement discrepancies outlined above it would be 2.273,which is 8.8% higher.

# oarlenght <- 288
# inboard <- 88
# 
# gearing_ratio_proxy <- (oarlenght - inboard)/inboard
# print(gearing_ratio_proxy)

oarlenght <- 288
inboard <- 88
outboard <- oarlenght - inboard
blade_length <- 45
pin <- 2
actual_outboard <- oarlenght- inboard - pin - blade_length/2
hands_from_grip <- 6
actual_inboard <- inboard - hands_from_grip + pin

# Overlap typical

spread <- 160
overlap <- 2*inboard - spread

gearing_ratio_precise <- actual_outboard/actual_inboard
print(gearing_ratio_precise)

# Gearing precises Stefano Ntouskos https://youtu.be/7_oGz7-A9QE?t=3754
# Spread 159 cm

oarlenght <- 283
inboard <- 87.5
blade_length <- 45
pin <- 2
actual_outboard <- oarlenght- inboard - pin - blade_length/2
hands_from_grip <- 6
actual_inboard <- inboard - hands_from_grip + pin

gearing_ratio_precise <- actual_outboard/actual_inboard
print(gearing_ratio_precise)

# Overlap Steafonos

spread <- 159
overlap <- 2*inboard - spread

# Application à michele: 

spread <- 156 # (presque) minimum spread possible sur le bateau (cf. tableau taille vs. spread de Kleshnev)
overlap <- 16 # comme Ntouskos

inboard <- (overlap + spread)/2
pin <- 2
hands_from_grip <- 6
blade_length <- 45
gearing_ratio_precise <- 2.048 # wie ntouskos

# gearing_ratio_precise = actual_outboard/actual_inboard
# gearing_ratio_precise = (oarlenght- inboard - pin - blade_length/2 )/(inboard - hands_from_grip + pin)
# gearing_ratio_precise*(inboard - hands_from_grip + pin) = (oarlenght- inboard - pin - blade_length/2 )
# gearing_ratio_precise*(inboard - hands_from_grip + pin) + inboard + pin + blade_length/2 = oarlenght

oarlenght <- gearing_ratio_precise*(inboard - hands_from_grip + pin) + inboard + pin + blade_length/2
print(paste("Michele oar lenght:", round(oarlenght, digits = 1)))

# Outboard 

outboard <- oarlenght - inboard

print(paste("MIchele outboard lenght:", round(outboard, digits = 1)))

# # Moiticé de l'angle total entre catch et finish - formule simple ####
# 
# half_alpha1_deg <- 100/2 # supposé
# half_alpha1_rad <- (half_alpha1_deg/180)*pi # en radiant
# 
# # distance entre Dolle et moitié du bateau
# half_spread <- 160 / 2 
# 
# # Moitié de longueur du coup (en ligne directe)
# 
# half_way <- (tan(half_alpha1_rad) * half_spread)
# 
# # Nouvel angle avec le même chemin 
# 
# half_alpha2_rad <- atan(half_way/(157/2))
# half_alpha2_deg <- half_alpha2_rad / pi * 180
# 
# # Augmentation de l'angle 
# 
# angle_diff <- half_alpha2_deg*2 - half_alpha1_deg*2
# print(paste("Old angle", round(half_alpha1_deg*2, digits = 1)))
# print(paste("New angle", round(half_alpha2_deg*2, digits = 1)))

# Application à michele: formule plus exacte (cf. dessin)  ####

spread <- 156 # cm minimum spread possible sur le bateau (cf. tableau taille vs. spread de Kleshnev)
half_spread <- spread/2
overlap <- overlap # comme Ntouskos
inboard <- (overlap + spread)/2
arm <- 70 # cm
clavicula <- 21 # cm

half_spread_net <- half_spread - clavicula

# Distance orthogonale Dolle schulter

#... en raison du corps penché en avant

body_angle_catch_deg <- 15

shoulder_femour_head <- 56 # cm
shoulder_ortogonal_1 <- shoulder_femour_head * cos((90-body_angle_catch_deg)/180*pi)

# ... en raison de la distance parcouru sur la coulisse

shoulder_ortogonal_2 <- 5 # cm

shoulder_ortogonal_tot <- shoulder_ortogonal_1 + shoulder_ortogonal_2

# Distances dolle_shoulder (pitagora)

dolle_shoulder <- (shoulder_ortogonal_tot^2 + half_spread_net^2)^(1/2)

# angle_hand_dolle_schulter
# lois des cosinus α = arccos [(b² + c² - a²)/(2bc)]

angle_hand_dolle_schulter <- acos((dolle_shoulder^2 + inboard^2 - arm^2)/(2*dolle_shoulder*arm))

# angle_schulter_dolle_orthogonal

angle_schulter_dolle_orthogonal <- asin(shoulder_ortogonal_tot/half_spread_net)

# angle_hand_dolle_orthogonal (alpha) = angle_hand_dolle_schulter + angle_schulter_dolle_orthogonal

alpha_rad <- angle_hand_dolle_schulter + angle_schulter_dolle_orthogonal
alpha_deg <- alpha_rad / pi * 180

print(alpha_deg)

# Angle au finish

beta_deg <- 40 # à vérifier

tot_deg <- alpha_deg + beta_deg

# Longeur du coup (circle)

stroke_length_new <- round(tot_deg/360*(outboard*2*pi))

print(paste("stroke length michele après changement:", stroke_length_new))

stroke_length_old <- round(98/360*(200*2*pi))
print(paste("stroke length michele avant changement:", stroke_length_old))

# Solution intermédiaire? ######
# Malheureusement la longueur minimal est de 284 cm

oarlenght <- 283.5 # minimum possible
spread <- 156 # Compris pour avoir un gearing ration de maximum 2.12 # Rowing Biomechanics © 2006 Dr. Valery Kleshnev
overlap <- 17 # presque comme Ntouskos

inboard <- (overlap + spread)/2

gearing_ratio_precise <-  (oarlenght- inboard - pin - blade_length/2 )/(inboard - hands_from_grip + pin)
