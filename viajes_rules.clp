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
   (if (>= ?days 0) then
      (if (<= ?days 3) then
         (assert (viaje dias corto))
      )
      (if (and (> ?days 3) (<= ?days 7)) then
         (assert (viaje dias normal))
      )
      (if (and (> ?days 7) (<= ?days 14)) then
         (assert (viaje dias largo))
      )
      (if (> ?days 14) then
         (assert (viaje dias muy-largo))
      )
      (if (= ?days 0) then
         (assert (viaje dias normal))
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
      (if (and (> ?days 2) (<= ?days 4)) then
         (assert (viaje min-ciudades normal))
      )
      (if (> ?days 4) then
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
         (assert (viaje precio barato))
      )
      (if (and (> ?days 500) (<= ?days 1000)) then
         (assert (viaje precio normal))
      )
      (if (and (> ?days 1000) (<= ?days 1500)) then
         (assert (viaje precio caro))
      )
      (if (> ?days 1500) then
         (assert (viaje precio muy-caro))
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
   (declare (salience -6))
   (viaje filtrar)
   (viaje transportes ?trans)
   =>
   (bind ?ciudades (find-all-instances ((?ins Ciudad)) (collection-contains-a-element ?trans ?ins:transporte)))
   (loop-for-count (?j 1 (length$ ?ciudades)) do
      (assert (viaje ciudad (nth$ ?j ?ciudades)))
   )
)

(defrule filt-alojamiento "Filtro de Alojoamientos x Calidad del Alojamiento"
   (declare (salience -7))
   (viaje filtrar)
   (viaje hospedaje ?star)
   (viaje ciudad ?ciudad)
   =>
   (bind ?lista (create$))
   (loop-for-count (?j 1 (length$ (send ?ciudad get-dispone_de))) do
      (if (>= (send (nth$ ?j (send ?ciudad get-dispone_de)) get-star) ?star) then
         (bind ?lista (insert$ ?lista 1 (nth$ ?j (send ?ciudad get-dispone_de))))
      )
   )
   (send ?ciudad put-dispone_de ?lista)
   (assert (viaje filtrar-alo-ciu))
)

(defrule filt-alojamiento-pos "Filtro de Ciudades - Post eliminacion de Alojamientos"
   (declare (salience -8))
   (viaje filtrar)
   (viaje filtrar-alo-ciu)
   ?obs <- (viaje ciudad ?ciudad)
   =>
   (if (= (length$ (send ?ciudad get-dispone_de)) 0) then
      (retract ?obs)
   )
)

;Creacion de la estructura de num de ciudades y num de dias por ciudad

(defrule gen-prevision-dias-corto-pocas ""
   (declare (salience -9))
   (viaje dias corto)
   (viaje min-ciudades pocas)
   =>
   (bind ?ciudades (create$))
   (bind ?dias1 (create$ 1 1))
   (bind ?dias2 (create$ 2))
   (assert  (viaje1 (ciudades ?ciudades) (dias ?dias1)))
   (assert  (viaje2 (ciudades ?ciudades) (dias ?dias2)))
)

(defrule gen-prevision-dias-corto-normal ""
   (declare (salience -9))
   (viaje dias corto)
   (viaje min-ciudades normal)
   =>
   (bind ?ciudades (create$))
   (bind ?dias (create$ 1 1 1))
   (assert  (viaje1 (ciudades ?ciudades) (dias ?dias)))
   (assert  (viaje2 (ciudades ?ciudades) (dias ?dias)))
)

(defrule gen-prevision-dias-corto-muchas ""
   (declare (salience -9))
   (viaje dias corto)
   (viaje min-ciudades muchas)
   =>
   (printout t "You must select much days, or decrease the number of cities")
)

(defrule gen-prevision-dias-normal-pocas ""
   (declare (salience -9))
   (viaje dias normal)
   (viaje min-ciudades pocas)
   =>
   (bind ?ciudades (create$))
   (bind ?dias1 (create$ 2 2))
   (bind ?dias2 (create$ 3 3))
   (assert  (viaje1 (ciudades ?ciudades) (dias ?dias1)))
   (assert  (viaje2 (ciudades ?ciudades) (dias ?dias2)))
)

(defrule gen-prevision-dias-normal-normal ""
   (declare (salience -9))
   (viaje dias normal)
   (viaje min-ciudades normal)
   =>
   (bind ?ciudades (create$))
   (bind ?dias1 (create$ 2 2 2))
   (bind ?dias2 (create$ 2 2 2 1))
   (assert  (viaje1 (ciudades ?ciudades) (dias ?dias1)))
   (assert  (viaje2 (ciudades ?ciudades) (dias ?dias2)))
)

(defrule gen-prevision-dias-normal-muchas ""
   (declare (salience -9))
   (viaje dias normal)
   (viaje min-ciudades muchas)
   =>
   (bind ?ciudades (create$))
   (bind ?dias1 (create$ 2 2 1 1 1))
   (bind ?dias2 (create$ 2 1 1 1 1 1))
   (assert  (viaje1 (ciudades ?ciudades) (dias ?dias1)))
   (assert  (viaje2 (ciudades ?ciudades) (dias ?dias2)))
)

(defrule gen-prevision-dias-largo-pocas ""
   (declare (salience -9))
   (viaje dias largo)
   (viaje min-ciudades pocas)
   =>
   (bind ?ciudades (create$))
   (bind ?dias1 (create$ 4 4))
   (bind ?dias2 (create$ 5 5))
   (assert  (viaje1 (ciudades ?ciudades) (dias ?dias1)))
   (assert  (viaje2 (ciudades ?ciudades) (dias ?dias2)))
)

(defrule gen-prevision-dias-largo-normal ""
   (declare (salience -9))
   ?obj1 <-(viaje dias largo)
   ?obj2 <-(viaje min-ciudades normal)
   =>
   (bind ?ciudades (create$))
   (bind ?dias1 (create$ 2 2 2 2))
   (bind ?dias2 (create$ 3 3 3))
   (assert  (viaje1 (ciudades ?ciudades) (dias ?dias1)))
   (assert  (viaje2 (ciudades ?ciudades) (dias ?dias2)))
   (retract ?obj1)
   (retract ?obj2)
)

(defrule gen-prevision-dias-largo-muchas ""
   (declare (salience -9))
   (viaje dias largo)
   (viaje min-ciudades muchas)
   =>
   (bind ?ciudades (create$))
   (bind ?dias1 (create$ 2 2 2 2 2))
   (bind ?dias2 (create$ 2 2 2 2 2 2))
   (assert  (viaje1 (ciudades ?ciudades) (dias ?dias1)))
   (assert  (viaje2 (ciudades ?ciudades) (dias ?dias2)))
)

(defrule gen-prevision-dias-muy-largo-pocas ""
   (declare (salience -9))
   (viaje dias muy-largo)
   (viaje min-ciudades pocas)
   =>
   (bind ?ciudades (create$))
   (bind ?dias (create$ 8 8))
   (assert  (viaje1 (ciudades ?ciudades) (dias ?dias)))
   (assert  (viaje2 (ciudades ?ciudades) (dias ?dias)))
)

(defrule gen-prevision-dias-muy-largo-normal ""
   (declare (salience -9))
   (viaje dias muy-largo)
   (viaje min-ciudades normal)
   =>
   (bind ?ciudades (create$))
   (bind ?dias1 (create$ 5 5 4 4))
   (bind ?dias2 (create$ 5 5 5 5))
   (assert  (viaje1 (ciudades ?ciudades) (dias ?dias1)))
   (assert  (viaje2 (ciudades ?ciudades) (dias ?dias2)))
)

(defrule gen-prevision-dias-muy-largo-muchas ""
   (declare (salience -9))
   (viaje dias muy-largo)
   (viaje min-ciudades muchas)
   =>
   (bind ?ciudades (create$))
   (bind ?dias1 (create$ 3 3 3 3 3))
   (bind ?dias2 (create$ 4 4 4 4 4 4))
   (assert  (viaje1 (ciudades ?ciudades) (dias ?dias1)))
   (assert  (viaje2 (ciudades ?ciudades) (dias ?dias2)))
)

;Generacion de propuestas de viajes iniciales





