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
   (bind ?price (read))
   (bind ?*precio-cliente* ?price)
   (if (> ?price 0) then
      (if (<= ?price 600) then
         (assert (viaje precio barato))
         (bind ?*tip-precio* barato)
      )
      (if (and (> ?price 600) (<= ?price 1200)) then
         (assert (viaje precio normal))
         (bind ?*tip-precio* normal)
      )
      (if (> ?price 1200) then
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

(deffunction gen-viajes-1 (?tipo)
   (bind ?length-ciudades (length$ ?*list-ciudades*))
   (loop-for-count (?x 1 ?length-ciudades) do
      (make-instance of Viaje (incluye (nth$ ?x ?*list-ciudades*)) (tipo ?tipo))
   )
   TRUE
)

(deffunction gen-viajes-2 (?tipo)
   (bind ?viajex (create$))
   (loop-for-count (?j 1 2) do
      (bind ?length-ciudades (length$ ?*list-ciudades*))
      (loop-for-count (?x 1 ?length-ciudades) do
         (bind ?viajex (insert$ ?viajex 1 (nth$ ?x ?*list-ciudades*)))
            (loop-for-count (?z ?x ?length-ciudades) do
               (if (not (collection-contains-a-element (nth$ ?z ?*list-ciudades*) ?viajex)) then
                  (bind ?viajex (insert$ ?viajex 2 (nth$ ?z ?*list-ciudades*)))
                  (make-instance of Viaje (incluye ?viajex) (tipo ?tipo))
                  (bind ?viajex (delete$ ?viajex 2 2))
               )
            )
         (bind ?viajex (delete$ ?viajex 1 1))
      )
   )
   TRUE
)

(deffunction gen-viajes-3 (?tipo)
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
                     (make-instance of Viaje (incluye ?viajex) (tipo ?tipo))
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

(deffunction gen-viajes-4 (?tipo)
   (bind ?viajex (create$))
   (loop-for-count (?j 1 4) do
      (bind ?length-ciudades (length$ ?*list-ciudades*))
      (loop-for-count (?x 1 ?length-ciudades) do
         (bind ?viajex (insert$ ?viajex 1 (nth$ ?x ?*list-ciudades*)))
         (loop-for-count (?y ?x ?length-ciudades) do
            (if (not (collection-contains-a-element (nth$ ?y ?*list-ciudades*) ?viajex)) then
               (bind ?viajex (insert$ ?viajex 2 (nth$ ?y ?*list-ciudades*)))
               (loop-for-count (?a ?y ?length-ciudades) do
                  (if (not (collection-contains-a-element (nth$ ?a ?*list-ciudades*) ?viajex)) then
                     (bind ?viajex (insert$ ?viajex 3 (nth$ ?a ?*list-ciudades*)))
                     (loop-for-count (?z ?a ?length-ciudades) do
                        (if (not (collection-contains-a-element (nth$ ?z ?*list-ciudades*) ?viajex)) then
                           (bind ?viajex (insert$ ?viajex 4 (nth$ ?z ?*list-ciudades*)))
                           (make-instance of Viaje (incluye ?viajex) (tipo ?tipo))
                           (bind ?viajex (delete$ ?viajex 4 4))
                        )
                     )
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

(deffunction gen-viajes-5 (?tipo)
   (bind ?viajex (create$))
   (loop-for-count (?j 1 5) do
      (bind ?length-ciudades (length$ ?*list-ciudades*))
      (loop-for-count (?x 1 ?length-ciudades) do
         (bind ?viajex (insert$ ?viajex 1 (nth$ ?x ?*list-ciudades*)))
         (loop-for-count (?y ?x ?length-ciudades) do
            (if (not (collection-contains-a-element (nth$ ?y ?*list-ciudades*) ?viajex)) then
               (bind ?viajex (insert$ ?viajex 2 (nth$ ?y ?*list-ciudades*)))
               (loop-for-count (?a ?y ?length-ciudades) do
                  (if (not (collection-contains-a-element (nth$ ?a ?*list-ciudades*) ?viajex)) then
                     (bind ?viajex (insert$ ?viajex 3 (nth$ ?a ?*list-ciudades*)))
                     (loop-for-count (?b ?a ?length-ciudades) do
                        (if (not (collection-contains-a-element (nth$ ?b ?*list-ciudades*) ?viajex)) then
                           (bind ?viajex (insert$ ?viajex 4 (nth$ ?b ?*list-ciudades*)))
                           (loop-for-count (?z ?b ?length-ciudades) do
                              (if (not (collection-contains-a-element (nth$ ?z ?*list-ciudades*) ?viajex)) then
                                 (bind ?viajex (insert$ ?viajex 5 (nth$ ?z ?*list-ciudades*)))
                                 (make-instance of Viaje (incluye ?viajex) (tipo ?tipo))
                                 (bind ?viajex (delete$ ?viajex 5 5))
                              )
                           )
                           (bind ?viajex (delete$ ?viajex 4 4))
                        )
                     )
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

(deffunction gen-viajes-6 (?tipo)
   (bind ?viajex (create$))
   (loop-for-count (?j 1 6) do
      (bind ?length-ciudades (length$ ?*list-ciudades*))
      (loop-for-count (?x 1 ?length-ciudades) do
         (bind ?viajex (insert$ ?viajex 1 (nth$ ?x ?*list-ciudades*)))
         (loop-for-count (?y ?x ?length-ciudades) do
            (if (not (collection-contains-a-element (nth$ ?y ?*list-ciudades*) ?viajex)) then
               (bind ?viajex (insert$ ?viajex 2 (nth$ ?y ?*list-ciudades*)))
               (loop-for-count (?a ?y ?length-ciudades) do
                  (if (not (collection-contains-a-element (nth$ ?a ?*list-ciudades*) ?viajex)) then
                     (bind ?viajex (insert$ ?viajex 3 (nth$ ?a ?*list-ciudades*)))
                     (loop-for-count (?b ?a ?length-ciudades) do
                        (if (not (collection-contains-a-element (nth$ ?b ?*list-ciudades*) ?viajex)) then
                           (bind ?viajex (insert$ ?viajex 4 (nth$ ?b ?*list-ciudades*)))
                           (loop-for-count (?c ?b ?length-ciudades) do
                              (if (not (collection-contains-a-element (nth$ ?c ?*list-ciudades*) ?viajex)) then
                                 (bind ?viajex (insert$ ?viajex 5 (nth$ ?c ?*list-ciudades*)))
                                 (loop-for-count (?z ?c ?length-ciudades) do
                                    (if (not (collection-contains-a-element (nth$ ?z ?*list-ciudades*) ?viajex)) then
                                       (bind ?viajex (insert$ ?viajex 6 (nth$ ?z ?*list-ciudades*)))
                                       (make-instance of Viaje (incluye ?viajex) (tipo ?tipo))
                                       (bind ?viajex (delete$ ?viajex 6 6))
                                    )
                                 )
                                 (bind ?viajex (delete$ ?viajex 5 5))
                              )
                           )
                           (bind ?viajex (delete$ ?viajex 4 4))
                        )
                     )
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

(defrule  gen-pos-viajes-1 "Generar los posibles viajes"
   (declare (salience -10))
   (viaje1 (ciudades) (dias $?ciudades))
   =>
   (if (= (length$ ?ciudades) 1) then
      (gen-viajes-1 1)
   )
   (if (= (length$ ?ciudades) 2) then
      (gen-viajes-2 1)
   )
   (if (= (length$ ?ciudades) 3) then
      (gen-viajes-3 1)
   )
   (if (= (length$ ?ciudades) 4) then
      (gen-viajes-4 1)
   )
   (if (= (length$ ?ciudades) 5) then
      (gen-viajes-5 1)
   )
   (if (= (length$ ?ciudades) 6) then
      (gen-viajes-6 1)
   )
)

(defrule  gen-pos-viajes-2 "Generar los posibles viajes"
   (declare (salience -11))
   (viaje2 (ciudades) (dias $?ciudades))
   =>
   (if (= (length$ ?ciudades) 1) then
      (gen-viajes-1 2)
   )
   (if (= (length$ ?ciudades) 2) then
      (gen-viajes-2 2)
   )
   (if (= (length$ ?ciudades) 3) then
      (gen-viajes-3 2)
   )
   (if (= (length$ ?ciudades) 4) then
      (gen-viajes-4 2)
   )
   (if (= (length$ ?ciudades) 5) then
      (gen-viajes-5 2)
   )
   (if (= (length$ ?ciudades) 6) then
      (gen-viajes-6 2)
   )
)

;PRECIOS

(defrule gen-precios "Calcular los precios de los transportes"
   (declare (salience -12))
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
         
         (if (= ?*precio-trans* 0.00003) then
            (bind ?trans-to-ciudad Plane)
         )
         (if (= ?*precio-trans* 0.00007) then
            (bind ?trans-to-ciudad Car)
         )
         (if (= ?*precio-trans* 0.00012) then
            (bind ?trans-to-ciudad Ship)
         )
         (if (= ?*precio-trans* 0.00006) then
            (bind ?trans-to-ciudad Train)
         )

            (bind ?lista-trans (send (nth$ ?j ?viajes) get-transportes_utilizados))
            (bind ?lista-trans (insert$ ?lista-trans (+ (length$ ?lista-trans) 1) ?trans-to-ciudad))
            (send (nth$ ?j ?viajes) put-transportes_utilizados ?lista-trans)
         
         (bind ?*precio-trans* 0)
      )
   )
)

(defrule gen-precios-aloj-2 "Calcular los precios de los viajes"
   (declare (salience -13))
   (viaje2 (ciudades) (dias $?dias-ciudad))
   =>
   (bind ?viajes (find-all-instances ((?ins Viaje)) TRUE))
   (loop-for-count (?j 1 (length$ ?viajes)) do
      (if (= (send (nth$ ?j ?viajes) get-tipo) 2) then
         (bind ?ciudades (send (nth$ ?j ?viajes) get-incluye))
         (loop-for-count (?z 1 (length$ ?ciudades)) do
            (bind ?ciudad (nth$ ?z ?ciudades))
            (bind ?alojamiento-select (nth$ 1 (send ?ciudad get-dispone_de)))
            (bind ?precio-select 0)
            (bind ?precio-select (+ (send (nth$ ?j ?viajes) get-precio) (* (nth$ ?z ?dias-ciudad) (send ?alojamiento-select get-price))))
            (if (>= (length$ (send ?ciudad get-dispone_de)) 2) then
               (loop-for-count (?y 2 (length$ (send ?ciudad get-dispone_de))) do
                  (bind ?alojamiento (nth$ ?y (send ?ciudad get-dispone_de)))
                  (if (and (or (eq ?*tip-precio* barato) (eq ?*tip-precio* normal))
                     (> ?precio-select (+ (send (nth$ ?j ?viajes) get-precio) (* (nth$ ?z ?dias-ciudad) (send ?alojamiento get-price))))  
                     ) then
                     (bind ?alojamiento-select (nth$ ?y (send ?ciudad get-dispone_de)))
                     (bind ?precio-select 0)
                     (bind ?precio-select (+ (send (nth$ ?j ?viajes) get-precio) (* (nth$ ?z ?dias-ciudad) (send ?alojamiento-select get-price))))
                  )
                  (if (and (eq ?*tip-precio* caro) 
                     (< ?precio-select (+ (send (nth$ ?j ?viajes) get-precio) (* (nth$ ?z ?dias-ciudad) (send ?alojamiento get-price))))  
                     ) then
                     (bind ?alojamiento-select (nth$ ?y (send ?ciudad get-dispone_de)))
                     (bind ?precio-select 0)
                     (bind ?precio-select (+ (send (nth$ ?j ?viajes) get-precio) (* (nth$ ?z ?dias-ciudad) (send ?alojamiento-select get-price))))
                  )
               )
            )
            (send (nth$ ?j ?viajes) put-precio 0)
            (send (nth$ ?j ?viajes) put-precio (+ (send (nth$ ?j ?viajes) get-precio) ?precio-select))
            (bind ?precio-select 0)

            (bind ?lista-aloj (send (nth$ ?j ?viajes) get-alojamientos_utilizados))
            (bind ?lista-aloj (insert$ ?lista-aloj (+ (length$ ?lista-aloj) 1) ?alojamiento-select))
            (send (nth$ ?j ?viajes) put-alojamientos_utilizados ?lista-aloj)
         )
      )
   )
)

(defrule gen-precios-aloj-1 "Calcular los precios de los viajes"
   (declare (salience -14))
   (viaje1 (ciudades) (dias $?dias-ciudad))
   =>
   (bind ?viajes (find-all-instances ((?ins Viaje)) TRUE))
   (loop-for-count (?j 1 (length$ ?viajes)) do
      (if (= (send (nth$ ?j ?viajes) get-tipo) 1) then
         (bind ?ciudades (send (nth$ ?j ?viajes) get-incluye))
         (loop-for-count (?z 1 (length$ ?ciudades)) do
            (bind ?ciudad (nth$ ?z ?ciudades))
            (bind ?alojamiento-select (nth$ 1 (send ?ciudad get-dispone_de)))
            (bind ?precio-select 0)
            (bind ?precio-select (+ (send (nth$ ?j ?viajes) get-precio) (* (nth$ ?z ?dias-ciudad) (send ?alojamiento-select get-price))))
            (if (>= (length$ (send ?ciudad get-dispone_de)) 2) then
               (loop-for-count (?y 2 (length$ (send ?ciudad get-dispone_de))) do
                  (bind ?alojamiento (nth$ ?y (send ?ciudad get-dispone_de)))
                  (if (and (or (eq ?*tip-precio* barato) (eq ?*tip-precio* normal))
                     (> ?precio-select (+ (send (nth$ ?j ?viajes) get-precio) (* (nth$ ?z ?dias-ciudad) (send ?alojamiento get-price))))  
                     ) then
                     (bind ?alojamiento-select (nth$ ?y (send ?ciudad get-dispone_de)))
                     (bind ?precio-select 0)
                     (bind ?precio-select (+ (send (nth$ ?j ?viajes) get-precio) (* (nth$ ?z ?dias-ciudad) (send ?alojamiento get-price))))
                  )
                  (if (and (eq ?*tip-precio* caro) 
                     (< ?precio-select (+ (send (nth$ ?j ?viajes) get-precio) (* (nth$ ?z ?dias-ciudad) (send ?alojamiento get-price))))  
                     ) then
                     (bind ?alojamiento-select (nth$ ?y (send ?ciudad get-dispone_de)))
                     (bind ?precio-select 0)
                     (bind ?precio-select (+ (send (nth$ ?j ?viajes) get-precio) (* (nth$ ?z ?dias-ciudad) (send ?alojamiento get-price))))
                  )
               )
            )
            (send (nth$ ?j ?viajes) put-precio 0)
            (send (nth$ ?j ?viajes) put-precio (+ (send (nth$ ?j ?viajes) get-precio) ?precio-select))
            (bind ?precio-select 0)

            (bind ?lista-aloj (send (nth$ ?j ?viajes) get-alojamientos_utilizados))
            (bind ?lista-aloj (insert$ ?lista-aloj (+ (length$ ?lista-aloj) 1) ?alojamiento-select))
            (send (nth$ ?j ?viajes) put-alojamientos_utilizados ?lista-aloj)
         )
      )
   )
)

(defrule select-viaje-1 "Seleccionamos un Viaje"
   (declare (salience -15))
   (viaje1 (ciudades) (dias $?dias-ciudad))
   =>
   (bind ?viajes (find-all-instances ((?ins Viaje)) TRUE))
   (bind ?viaje-definitivo (nth$ 1 ?viajes))
   (bind ?dif (abs (- (send ?viaje-definitivo get-precio) ?*precio-cliente*)))
   (if (> (length$ ?viajes) 2) then
      (loop-for-count (?j 2 (length$ ?viajes)) do
         ;(printout t (send (nth$ ?j ?viajes) get-precio) crlf)
         (if (< (abs (- (send (nth$ ?j ?viajes) get-precio) ?*precio-cliente*)) ?dif) then
            (if (= (send (nth$ ?j ?viajes) get-tipo) 1) then 
               (bind ?viaje-definitivo (nth$ ?j ?viajes))
               (bind ?dif (abs (- (send (nth$ ?j ?viajes) get-precio) ?*precio-cliente*)))
            )
         )
      )
   )
   (bind ?dias-totales 0)
   (loop-for-count (?j 1 (length$ ?dias-ciudad)) do
      (bind ?dias-totales (+ (nth$ ?j ?dias-ciudad) ?dias-totales))
   )
   ;(printout t ?viaje-definitivo "//" (send ?viaje-definitivo get-precio) crlf)
   (printout t "------------------------------" crlf)
   (printout t "--------   VIAJE 1 -----------" crlf)
   (printout t "------------------------------" crlf)
   (printout t "- Precio: " (send ?viaje-definitivo get-precio) " €" crlf)
   (printout t "- Duracion del viaje: " ?dias-totales crlf)
   (printout t "- Ciudades: ")
   (loop-for-count (?j 1 (length$ ?dias-ciudad)) do
      (printout t "" (send (nth$ ?j (send ?viaje-definitivo get-incluye)) get-nombre) " (" (nth$ ?j ?dias-ciudad) " dias), " )
   )
   (printout t "" crlf)
   (printout t "- Alojamiento: ")
   (loop-for-count (?j 1 (length$ ?dias-ciudad)) do
      (printout t "" (send (nth$ ?j (send ?viaje-definitivo get-incluye)) get-nombre) " (" (send (nth$ ?j (send ?viaje-definitivo get-alojamientos_utilizados)) get-star) "*), " )
   )
   (printout t "" crlf)
   (printout t "- Viajes: ")
   (loop-for-count (?j 1 (length$ ?dias-ciudad)) do
      (if (= ?j 1) then
         (printout t "Origen->" (send (nth$ ?j (send ?viaje-definitivo get-incluye)) get-nombre) " (" (nth$ ?j (send ?viaje-definitivo get-transportes_utilizados)) "), " )
      )
      (if (and (> ?j 1) (< ?j (length$ ?dias-ciudad)))  then
         (printout t (send (nth$ (- ?j 1) (send ?viaje-definitivo get-incluye)) get-nombre) "->" (send (nth$ ?j (send ?viaje-definitivo get-incluye)) get-nombre) " (" (nth$ ?j (send ?viaje-definitivo get-transportes_utilizados)) "), " )
      )
      (if (= ?j (length$ ?dias-ciudad)) then
         (printout t (send (nth$ (- ?j 1) (send ?viaje-definitivo get-incluye)) get-nombre) "->" (send (nth$ ?j (send ?viaje-definitivo get-incluye)) get-nombre) " (" (nth$ ?j (send ?viaje-definitivo get-transportes_utilizados)) "), " )
         (printout t (send (nth$ ?j (send ?viaje-definitivo get-incluye)) get-nombre) "->Origen" " (" (nth$ ?j (send ?viaje-definitivo get-transportes_utilizados)) "), " )
      )
   )
   (printout t "" crlf)
)

(defrule select-viaje-2 "Seleccionamos un Viaje"
   (declare (salience -16))
   (viaje2 (ciudades) (dias $?dias-ciudad))
   =>
   (bind ?viajes (find-all-instances ((?ins Viaje)) TRUE))
   (bind ?viaje-definitivo (nth$ 1 ?viajes))
   (bind ?dif (abs (- (send ?viaje-definitivo get-precio) ?*precio-cliente*)))
   (if (> (length$ ?viajes) 2) then
      (loop-for-count (?j 2 (length$ ?viajes)) do
         (if (< (abs (- (send (nth$ ?j ?viajes) get-precio) ?*precio-cliente*)) ?dif) then
            (if (= (send (nth$ ?j ?viajes) get-tipo) 2) then 
               (bind ?viaje-definitivo (nth$ ?j ?viajes))
               (bind ?dif (abs (- (send (nth$ ?j ?viajes) get-precio) ?*precio-cliente*)))
            )
         )
      )
   )

   (bind ?dias-totales 0)
   (loop-for-count (?j 1 (length$ ?dias-ciudad)) do
      (bind ?dias-totales (+ (nth$ ?j ?dias-ciudad) ?dias-totales))
   )

   (printout t "------------------------------" crlf)
   (printout t "--------   VIAJE 2 -----------" crlf)
   (printout t "------------------------------" crlf)
   (printout t "- Precio: " (send ?viaje-definitivo get-precio) " €" crlf)
   (printout t "- Duracion del viaje: " ?dias-totales crlf)
   (printout t "- Ciudades: ")
   (loop-for-count (?j 1 (length$ ?dias-ciudad)) do
      (printout t "" (send (nth$ ?j (send ?viaje-definitivo get-incluye)) get-nombre) " (" (nth$ ?j ?dias-ciudad) " dias), " )
   )
   (printout t "" crlf)
   (printout t "- Alojamiento: ")
   (loop-for-count (?j 1 (length$ ?dias-ciudad)) do
      (printout t "" (send (nth$ ?j (send ?viaje-definitivo get-incluye)) get-nombre) " (" (send (nth$ ?j (send ?viaje-definitivo get-alojamientos_utilizados)) get-star) "*), " )
   )
   (printout t "" crlf)
   (printout t "- Viajes: ")
   (loop-for-count (?j 1 (length$ ?dias-ciudad)) do
      (if (= ?j 1) then
         (printout t "Origen->" (send (nth$ ?j (send ?viaje-definitivo get-incluye)) get-nombre) " (" (nth$ ?j (send ?viaje-definitivo get-transportes_utilizados)) "), " )
      )
      (if (and (> ?j 1) (< ?j (length$ ?dias-ciudad)))  then
         (printout t (send (nth$ (- ?j 1) (send ?viaje-definitivo get-incluye)) get-nombre) "->" (send (nth$ ?j (send ?viaje-definitivo get-incluye)) get-nombre) " (" (nth$ ?j (send ?viaje-definitivo get-transportes_utilizados)) "), " )
      )
      (if (= ?j (length$ ?dias-ciudad)) then
         (printout t (send (nth$ (- ?j 1) (send ?viaje-definitivo get-incluye)) get-nombre) "->" (send (nth$ ?j (send ?viaje-definitivo get-incluye)) get-nombre) " (" (nth$ ?j (send ?viaje-definitivo get-transportes_utilizados)) "), " )
         (printout t (send (nth$ ?j (send ?viaje-definitivo get-incluye)) get-nombre) "->Origen" " (" (nth$ ?j (send ?viaje-definitivo get-transportes_utilizados)) "), " )
      )
   )
   (printout t "" crlf)
)




