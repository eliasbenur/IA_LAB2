(defrule welcome-message "Initial program message"
  (declare (salience 0))
  =>
  (printout t "----------------------------------" crlf)
  (printout t " Welcome to the IA Project at UPC" crlf)
  (printout t "----------------------------------" crlf)
)

(defrule days-ask "How many days do you have want to spend?"
   (declare (salience -1))
   =>
   (printout t "How many days do you have want to spend in the travel?" crlf)
   (bind ?days (read))
   (if (> ?days 0) then
      (if (<= ?days 2) then
         (assert (viaje dias corto))
      )
      (if (and (> ?days 2) (<= ?days 7)) then
         (assert (viaje dias normal))
      )
      (if (and (> ?days 7) (<= ?days 14)) then
         (assert (viaje dias largo))
      )
      (if (> ?days 14) then
         (assert (viaje dias muy-largo))
      )
   )
)


(defrule min-ciudades "How many cities do you want to visit at least?"
   (declare (salience -2))
   =>
   (printout t "How many cities do you want to visit at least?{0 for null Value}" crlf)
   (bind ?days (read))
   (if (> ?days 0) then
      (if (<= ?days 2) then
         (assert (viaje min-ciudades pocas))
      )
      (if (and (> ?days 2) (<= ?days 5)) then
         (assert (viaje min-ciudades normal))
      )
      (if (> ?days 5) then
         (assert (viaje min-ciudades muchas))
      )
      (if (= ?days 0) then
         (assert (viaje min-ciudades normal))
      )
   )
)

(defrule transportes "What type of transport do you want to use? "
   (declare (salience -3))
   =>
   (printout t "What type of transport do you want to use? " crlf)
   (printout t "{0 for null Value, Plane, Train, Car, Ship}" crlf)
   (bind $?res (readline))
   (bind $?trans (explode$ ?res))
   (loop-for-count (?i 1 (length$ $?trans)) do
      (assert (viaje transportes (nth$ ?i $?trans)))
   )
)

(defrule precio "How much money do you plan to spend more or less?"
   (declare (salience -4))
   =>
   (printout t "How much money do you plan to spend more or less?" crlf)
   (bind ?days (read))
   (if (> ?days 0) then
      (if (<= ?days 500) then
         (assert (viaje min-ciudades barato))
      )
      (if (and (> ?days 500) (<= ?days 1000)) then
         (assert (viaje min-ciudades normal))
      )
      (if (and (> ?days 1000) (<= ?days 1500)) then
         (assert (viaje min-ciudades caro))
      )
      (if (> ?days 1500) then
         (assert (viaje min-ciudades muy-caro))
      )
   )
)

(defrule hospedaje "Do you want a minimum level of accommodation? "
   (declare (salience -5))
   =>
   (printout t "Do you want a minimum level of accommodation?{1,2,3,4,5 starts}" crlf)
   (bind ?hosp (read))
   (if (and (> ?hosp 0) (<= ?hosp 5)) then
      (assert (viaje hospedaje ?hosp))
   )
   (assert (viaje filtrar))
)

;%%%%%
;%
;% FILTROS
;%
;%%%%%

(defrule filt-trans "Filtro de Ciudades x Transportes"
   (viaje filtrar)
   (viaje transportes ?trans)
   =>
   (bind ?ciudades (find-all-instances ((?ins Ciudad)) (collection-contains-a-element ?trans ?ins:transporte)))
   (loop-for-count (?j 1 (length$ ?ciudades)) do
      (assert (viaje ciudad (nth$ ?j ?ciudades)))
   )
)

(defrule filt-alojamiento "Filtro de Alojoamientos x Calidad del Alojamiento"
   (viaje filtrar)
   (viaje ciudad ?ciudad)
   (> ?star 0)
   =>
   (bind ?hospedajes (find-all-instances ((?ins Alojamiento)) (>= ?star ?ins:stars)))
   (loop-for-count (?j 1 (length$ (send ?ciudad get-dispone_de)) do
      (assert (viaje hospedaje (nth$ ?j ?ciudades)))
   )
)