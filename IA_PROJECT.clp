(defclass MAIN::Cliente
   (is-a USER)
   (role concrete)
   (single-slot viaja_con_ninyos
      (type SYMBOL)
      (allowed-values FALSE TRUE)
      (create-accessor read-write))
   (single-slot num_acompanyantes
      (type INTEGER)
      (create-accessor read-write))
   (single-slot solicita
      (type INSTANCE)
      (create-accessor read-write))
   (single-slot Objetivo
      (type SYMBOL)
      (allowed-values Descanso Cultural Diversion Romantico)
      (create-accessor read-write))
   (single-slot tipo_de_acompanyantes
      (type SYMBOL)
      (allowed-values Mixto Familia Amigos Pareja)
      (create-accessor read-write))
   (single-slot viaje_especial
      (type SYMBOL)
      (allowed-values Boda Fin_de_curso)
      (create-accessor read-write))
   (single-slot edad
      (type INTEGER)
      (create-accessor read-write)))

(defclass MAIN::%3ACLIPS_TOP_LEVEL_SLOT_CLASS "Fake class to save top-level slot information"
   (is-a USER)
   (role abstract)
   (single-slot origen
      (type INSTANCE)
      (create-accessor read-write))
   (single-slot Objetivo
      (type SYMBOL)
      (allowed-values Descanso Cultural Diversion Romantico)
      (create-accessor read-write))
   (single-slot Data_inicio
      (type STRING)
      (create-accessor read-write))
   (single-slot num_max_dias
      (type INTEGER)
      (create-accessor read-write))
   (single-slot tiene
      (type INSTANCE)
      (create-accessor read-write))
   (single-slot puedes_visitar
      (type INSTANCE)
      (create-accessor read-write))
   (single-slot solicita
      (type INSTANCE)
      (create-accessor read-write))
   (single-slot num_max_diasxciudad
      (type INTEGER)
      (create-accessor read-write))
   (single-slot tipo_de_acompanyantes
      (type SYMBOL)
      (allowed-values Mixto Familia Amigos Pareja)
      (create-accessor read-write))
   (single-slot Data_final
      (type STRING)
      (create-accessor read-write))
   (single-slot Presupuesto_max
      (type INTEGER)
      (create-accessor read-write))
   (single-slot num_max_ciudades
      (type INTEGER)
      (create-accessor read-write))
   (single-slot viaje_especial
      (type SYMBOL)
      (allowed-values Boda Fin_de_curso)
      (create-accessor read-write))
   (single-slot num_min_dias
      (type INTEGER)
      (create-accessor read-write))
   (single-slot viaja_con_ninyos
      (type SYMBOL)
      (allowed-values FALSE TRUE)
      (create-accessor read-write))
   (single-slot num_acompanyantes
      (type INTEGER)
      (create-accessor read-write))
   (multislot incluye
      (type INSTANCE)
      (create-accessor read-write))
   (single-slot nombre
      (type STRING)
      (create-accessor read-write))
   (multislot dispone_de
      (type INSTANCE)
      (create-accessor read-write))
   (multislot restricciones_transporte
      (type SYMBOL)
      (allowed-values Tren Avion Autobus Coche Barco)
      (create-accessor read-write))
   (single-slot num_min_diasxciudad
      (type INTEGER)
      (create-accessor read-write))
   (single-slot edad
      (type INTEGER)
      (create-accessor read-write))
   (single-slot num_min_ciudades
      (type INTEGER)
      (create-accessor read-write)))

(defclass MAIN::Viaje
   (is-a USER)
   (role concrete)
   (multislot incluye
      (type INSTANCE)
      (create-accessor read-write))
   (single-slot origen
      (type INSTANCE)
      (create-accessor read-write))
   (single-slot Data_final
      (type STRING)
      (create-accessor read-write))
   (single-slot Data_inicio
      (type STRING)
      (create-accessor read-write))
   (single-slot Presupuesto_max
      (type INTEGER)
      (create-accessor read-write))
   (single-slot num_max_dias
      (type INTEGER)
      (create-accessor read-write))
   (single-slot num_max_ciudades
      (type INTEGER)
      (create-accessor read-write))
   (multislot restricciones_transporte
      (type SYMBOL)
      (allowed-values Tren Avion Autobus Coche Barco)
      (create-accessor read-write))
   (single-slot num_min_diasxciudad
      (type INTEGER)
      (create-accessor read-write))
   (single-slot num_min_ciudades
      (type INTEGER)
      (create-accessor read-write))
   (single-slot num_max_diasxciudad
      (type INTEGER)
      (create-accessor read-write))
   (single-slot num_min_dias
      (type INTEGER)
      (create-accessor read-write)))

(defclass MAIN::Ciudad
   (is-a USER)
   (role concrete)
   (single-slot tiene
      (type INSTANCE)
      (create-accessor read-write))
   (single-slot puedes_visitar
      (type INSTANCE)
      (create-accessor read-write))
   (single-slot nombre
      (type STRING)
      (create-accessor read-write))
   (multislot dispone_de
      (type INSTANCE)
      (create-accessor read-write)))

(defclass MAIN::Alojamiento
   (is-a USER)
   (role concrete))

(defclass MAIN::Sitios_Interesantes
   (is-a USER)
   (role concrete))

(defclass MAIN::Medios_de_Transporte
   (is-a USER)
   (role concrete))

(definstances MAIN::instancias
   (El_Cliente of Cliente
   )
   ([BCN] of Ciudad (nombre "Barcelona"))
   ([INS] of Ciudad (nombre "Instanbul"))
   ([ROM] of Ciudad (nombre "Roma"))
   ([LON] of Ciudad (nombre "Londres"))
   ([BEI] of Ciudad (nombre "Beijing"))
)

(defmessage-handler Cliente print primary ()
   (printout t "El Cliente: " crlf)
   (printout t "Edad: " ?self:edad crlf)
   (printout t "Objetivo: " ?self:Objetivo crlf)
)

(defrule MAIN::welcome
   ?cliente <- (object (is-a Cliente))
   =>
   (printout t "Bienvenido a el Proyecto de IA" crlf)
   (printout t "Este programa tendra como objetivo recomendar un Viaje" crlf)
   (printout t "a ciertas Ciudades dependiendo de las informaciones" crlf)
   (printout t "introducidas por un usuario" crlf)
   (printout t "----------------------------------" crlf)
   (printout t "Empezemos! Porfavor introduze tu edad: ")
   (bind ?edad (read))
   (send ?cliente put-edad ?edad)
   (printout t "Que principal objetivo tiene con este viaje? " crlf)
   (printout t "{Descanso, Cultural, Diversion, Romantico}:")
   (bind ?objetivo (read))
   (send ?cliente put-Objetivo ?objetivo)
   (assert (objetivo ?objetivo))
)

(defrule MAIN::if_romantico
   ?inst <- (objetivo Romantico)
   =>
   (make-instance El_Viaje of Viaje 
      (incluye [BCN] [ROM] [LON]))
   (retract ?inst)
)

(defrule MAIN::if_descanso
   ?inst <- (objetivo Descanso)
   =>
   (make-instance El_Viaje of Viaje 
      (incluye [BEI]))
   (retract ?inst)
)

(defrule MAIN::if_diversion
   ?inst <- (objetivo Diversion)
   =>
   (make-instance El_Viaje of Viaje 
      (incluye [BCN] [LON]))
   (retract ?inst)
)

(defrule MAIN::if_cultural
   ?inst <- (objetivo Cultural)
   =>
   (make-instance El_Viaje of Viaje 
      (incluye [ROM] [LON] [BEI]))
   (retract ?inst)
)


