(deftemplate lista-ciudades 
   (multislot ciudades)
)

(deftemplate viaje1 
   (multislot ciudades)
   (multislot dias)
)

(deftemplate viaje2
   (multislot ciudades)
   (multislot dias)
)

(defglobal ?*list-ciudades* = (create$))
(defglobal ?*combinaciones-ciudades* = (create$))
(defglobal ?*list-trans* = (create$))
(defglobal ?*precio-trans* = 0)
(defglobal ?*pi* = 3.14159265359)
(defglobal ?*lat-ori* = 41.405753)
(defglobal ?*long-ori* = 2.150364)
(defglobal ?*tip-precio* = none)

;TEMPLATES

(defmessage-handler Cliente print primary ()
   (printout t "El Cliente: " crlf)
   (printout t "Edad: " ?self:edad crlf)
   (printout t "Objetivo: " ?self:Objetivo crlf)
)

(deffunction collection-contains-a-element (?element ?collection)
   (loop-for-count (?j 1 (length$ ?collection)) do
      (if (eq ?element (nth$ ?j ?collection))
      then (return TRUE))
   )
   FALSE
)

(deffunction cal-precio-trans (?precio ?lat1 ?long1 ?lat2 ?long2)
        (bind ?lat1 (/ (* ?lat1 ?*pi*) 180))
        (bind ?long1 (/ (* ?long1 ?*pi*) 180))
        (bind ?lat2 (/ (* ?lat2 ?*pi*) 180))
        (bind ?long2 (/ (* ?long2 ?*pi*) 180))
        (bind ?xlat (- ?lat2 ?lat1))
        (bind ?ylong (- ?long2 ?long1))
        (* ?precio (* 2 6371000 (asin(sqrt (+ (** (sin (/ ?xlat 2)) 2) (* (cos ?lat1) (cos ?lat2) (** (sin (/ ?ylong 2)) 2)))))))
)

