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
      (bind ?*list-trans* (insert$ ?*list-trans* (+ (length$ ?*list-trans*) 1) (nth$ ?i $?trans)))
   )
)

(defrule precio "How much money do you plan to spend more or less?"
   (declare (salience -4))
   =>
   (printout t "How much money do you plan to spend more or less?" crlf)
   (bind ?days (read))
   (if (> ?days 0) then
      (if (<= ?days 600) then
         (assert (viaje precio barato))
         (bind ?*tip-precio* barato)
      )
      (if (and (> ?days 600) (<= ?days 1200)) then
         (assert (viaje precio normal))
         (bind ?*tip-precio* normal)
      )
      (if (> ?days 1200) then
         (assert (viaje precio caro))
         (bind ?*tip-precio* caro)
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
   (bind ?lista-vacia (create$))
   (assert (lista-ciudades (ciudades ?lista-vacia)))
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
   (if (> (length$ (send ?ciudad get-dispone_de)) 0) then
      (bind ?*list-ciudades* (insert$ ?*list-ciudades* 1 ?ciudad))
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


(deffunction beta-gen-viajes-3 ()
   (bind ?viajex (create$))
   (loop-for-count (?j 1 3) do
      (loop-for-count (?x 1 (length$ ?*list-ciudades*)) do
         (bind ?viajex (insert$ ?viajex 1 (nth$ ?x ?*list-ciudades*)))
         (loop-for-count (?y ?x (length$ ?*list-ciudades*)) do
            (if (neq (nth$ ?y ?*list-ciudades*) (nth$ ?x ?*list-ciudades*)) then
               (bind ?viajex (insert$ ?viajex 2 (nth$ ?y ?*list-ciudades*)))
               (loop-for-count (?z ?y (length$ ?*list-ciudades*)) do
                  (if (and (neq (nth$ ?z ?*list-ciudades*) (nth$ ?x ?*list-ciudades*)) (neq (nth$ ?z ?*list-ciudades*) (nth$ ?y ?*list-ciudades*))) then
                     (bind ?viajex (insert$ ?viajex 3 (nth$ ?z ?*list-ciudades*)))
                     (make-instance of Viaje (incluye ?viajex))
                     (bind ?viajex (delete$ ?viajex 3 3))
                  )
               )
               (bind ?viajex (delete$ ?viajex 2 2))
            )
         )
         (bind ?viajex (delete$ ?viajex 1 1))
      )
   )
   TRUE
)

(deffunction gen-viajes-3 ()
   (bind ?viajex (create$))
   (loop-for-count (?j 1 3) do
      (bind ?length-ciudades (length$ ?*list-ciudades*))
      (loop-for-count (?x 1 ?length-ciudades) do
         (bind ?viajex (insert$ ?viajex 1 (nth$ ?x ?*list-ciudades*)))
         (loop-for-count (?y ?x ?length-ciudades) do
            ;(if (neq (nth$ ?y ?*list-ciudades*) (nth$ ?x ?*list-ciudades*)) then
            (if (not (collection-contains-a-element (nth$ ?y ?*list-ciudades*) ?viajex)) then
               (bind ?viajex (insert$ ?viajex 2 (nth$ ?y ?*list-ciudades*)))
               (loop-for-count (?z ?y ?length-ciudades) do
                  ;(if (and (neq (nth$ ?z ?*list-ciudades*) (nth$ ?x ?*list-ciudades*)) (neq (nth$ ?z ?*list-ciudades*) (nth$ ?y ?*list-ciudades*))) then
                  (if (not (collection-contains-a-element (nth$ ?z ?*list-ciudades*) ?viajex)) then
                     (bind ?viajex (insert$ ?viajex 3 (nth$ ?z ?*list-ciudades*)))
                     (make-instance of Viaje (incluye ?viajex))
                     (bind ?viajex (delete$ ?viajex 3 3))
                  )
               )
               (bind ?viajex (delete$ ?viajex 2 2))
            )
         )
         (bind ?viajex (delete$ ?viajex 1 1))
      )
   )
   TRUE
)



(defrule  gen-pos-viajes "Generar los posibles viajes"
   (declare (salience -10))
   (viaje2 (ciudades) (dias $?ciudades))
   =>
   (if (= (length$ ?ciudades) 3) then
      (gen-viajes-3)
   )
)

(defrule gen-precios "Calcular los precios de los viajes"
   (declare (salience -11))
   =>
   (bind ?viajes (find-all-instances ((?ins Viaje)) TRUE))
   (loop-for-count (?j 1 (length$ ?viajes)) do
      (bind ?ciudades (send (nth$ ?j ?viajes) get-incluye))
      (loop-for-count (?z 1 (length$ ?ciudades)) do
         (bind ?ciudad (nth$ ?z ?ciudades))
         (loop-for-count (?y 1 (length$ ?*list-trans*)) do
            (if (and (eq (nth$ ?y ?*list-trans*) Plane) (collection-contains-a-element Plane (send ?ciudad get-transporte)) (= ?*precio-trans* 0)) then 
               (bind ?*precio-trans* 0.00003)
            )
            (if (and (eq (nth$ ?y ?*list-trans*) Car) (collection-contains-a-element Car (send ?ciudad get-transporte)) (= ?*precio-trans* 0)) then 
               (bind ?*precio-trans* 0.00007)
            )
            (if (and (eq (nth$ ?y ?*list-trans*) Ship) (collection-contains-a-element Ship (send ?ciudad get-transporte)) (= ?*precio-trans* 0)) then 
               (bind ?*precio-trans* 0.00012)
            )
            (if (and (eq (nth$ ?y ?*list-trans*) Train) (collection-contains-a-element Train (send ?ciudad get-transporte)) (= ?*precio-trans* 0)) then 
               (bind ?*precio-trans* 0.00006)
            )
         )
         (if (= ?z 1) then
            (bind ?precio-trans-ciudad (cal-precio-trans ?*precio-trans* (send ?ciudad get-latitud) (send ?ciudad get-longitud) ?*lat-ori* ?*long-ori*))
         )
         (if (= ?z (length$ ?ciudades)) then
            (bind ?precio-trans-ciudad (+ 
                           (cal-precio-trans ?*precio-trans* (send ?ciudad get-latitud) (send ?ciudad get-longitud) ?*lat-ori* ?*long-ori*)
                           (cal-precio-trans ?*precio-trans* (send (nth$ (- ?z 1) ?ciudades) get-latitud) (send (nth$ (- ?z 1) ?ciudades) get-longitud) (send ?ciudad get-latitud) (send ?ciudad get-longitud))
                        )
            )
                           
         )
         (if (and (> ?z 1) (< ?z (length$ ?ciudades))) then
            (bind ?precio-trans-ciudad (cal-precio-trans ?*precio-trans* (send (nth$ (- ?z 1) ?ciudades) get-latitud) (send (nth$ (- ?z 1) ?ciudades) get-longitud) (send ?ciudad get-latitud) (send ?ciudad get-longitud)))
         )
         (send (nth$ ?j ?viajes) put-precio (+ (send (nth$ ?j ?viajes) get-precio) ?precio-trans-ciudad))
         (bind ?*precio-trans* 0)
      )
   )
)

(defrule gen-precios-aloj "Calcular los precios de los viajes"
   (declare (salience -12))
   (viaje2 (ciudades) (dias $?dias-ciudad))
   =>
   (bind ?viajes (find-all-instances ((?ins Viaje)) TRUE))
   (loop-for-count (?j 1 (length$ ?viajes)) do
      (bind ?ciudades (send (nth$ ?j ?viajes) get-incluye))
      (loop-for-count (?z 1 (length$ ?ciudades)) do
         (bind ?ciudad (nth$ ?z ?ciudades))
         (bind ?alojamiento-select (nth$ 1 (send ?ciudad get-dispone_de)))
         (bind ?precio-select (+ (send (nth$ ?j ?viajes) get-precio) (* (nth$ ?z ?dias-ciudad) (send ?alojamiento-select get-price))))
         (if (>= (length$ (send ?ciudad get-dispone_de)) 2) then
            (loop-for-count (?y 2 (length$ (send ?ciudad get-dispone_de))) do
               (bind ?alojamiento (nth$ ?y (send ?ciudad get-dispone_de)))
               (if (and (or (eq ?*tip-precio* barato) (eq ?*tip-precio* normal))
                  (> ?precio-select (+ (send (nth$ ?j ?viajes) get-precio) (* (nth$ ?z ?dias-ciudad) (send ?alojamiento get-price))))  
                  ) then
                  (bind ?alojamiento-select (nth$ ?y (send ?ciudad get-dispone_de)))
                  (bind ?precio-select (+ (send (nth$ ?j ?viajes) get-precio) (* (nth$ ?z ?dias-ciudad) (send ?alojamiento get-price))))
               )
               (if (and (eq ?*tip-precio* caro) 
                  (< ?precio-select (+ (send (nth$ ?j ?viajes) get-precio) (* (nth$ ?z ?dias-ciudad) (send ?alojamiento get-price))))  
                  ) then
                  (bind ?alojamiento-select (nth$ ?y (send ?ciudad get-dispone_de)))
                  (bind ?precio-select (+ (send (nth$ ?j ?viajes) get-precio) (* (nth$ ?z ?dias-ciudad) (send ?alojamiento get-price))))
               )
            )
         )
         (send (nth$ ?j ?viajes) put-precio (+ (send (nth$ ?j ?viajes) get-precio) ?precio-select))
      )
   )
)

(defrule select-viaje "Seleccionamos un Viaje"
   (declare (salience -13))
   =>
   (bind ?viajes (find-all-instances ((?ins Viaje)) TRUE))
   (bind ?viaje-definitivo (nth$ 1 ?viajes))
   (bind ?dif (abs (- (send ?viaje-definitivo get-precio) 1200)))
   (if (> (length$ ?viajes) 2) then
      (loop-for-count (?j 2 (length$ ?viajes)) do
         (if (< (abs (- (send (nth$ ?j ?viajes) get-precio) 1200)) ?dif) then
            (bind ?viaje-definitivo (nth$ ?j ?viajes))
            (bind ?dif (abs (- (send (nth$ ?j ?viajes) get-precio) 1200)))
         )
      )
   )
   (printout t ?viaje-definitivo "//" ?dif crlf)
)



