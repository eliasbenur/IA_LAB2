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
(defglobal ?*precio-cliente* = 0)
(defglobal
  ?*TRANSPORT_TYPES* = (create$ Plane Train Car Ship)
  ?*ACCOMMODATION_TYPES* = (create$ 1 2 3 4 5)
)


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

;Restricciones en entrada de valores

(deffunction es-num (?var)
  (bind ?ret (or (eq (type ?var) INTEGER) (eq (type ?var) FLOAT)))
  ?ret
)

(deffunction pregunta-num (?pregunta ?min ?max)
   (format t "%s (From %d to %d) " ?pregunta ?min ?max)
   (bind ?res (read))
   (while (not(and (es-num ?res) (>= ?res ?min)(<= ?res ?max))) do
      (format t "%s (From %d to %d) " ?pregunta ?min ?max)
      (bind ?res (read))
   )
   ?res
)

(deffunction pregunta-int (?pregunta ?min ?max)
   (format t "%s (From %d to %d) " ?pregunta ?min ?max)
   (bind ?res (read))
   (while (not(and (eq (type ?res) INTEGER) (>= ?res ?min)(<= ?res ?max))) do
      (format t "%s (From %d to %d) " ?pregunta ?min ?max)
      (bind ?res (read))
   )
   ?res
)

(deffunction pregunta-multivalor (?question ?allowed-values)
  (format t "%s? (%s) " ?question (implode$ ?allowed-values))
  (bind ?line (readline))
  (bind $?answer (explode$ ?line))
  (bind ?valid FALSE)
  (while (not ?valid) do
    (loop-for-count (?i 1 (length$ $?answer)) do
      (bind ?valid FALSE)
      (bind ?value-belongs FALSE)
      (loop-for-count (?j 1 (length$ $?allowed-values)) do
        (if (eq (nth$ ?i $?answer) (nth$ ?j $?allowed-values)) then
          (bind ?value-belongs TRUE)
          (break)
        )
      )
      (if (not ?value-belongs) then
        (printout t "| > " (nth$ ?i $?answer) " is not a valid option" crlf)
        (break)
      )
      (bind ?valid TRUE)
    )
    (if ?valid then (break))

    (printout t "| > " ?question crlf "| ")
    (bind ?line (readline))
    (bind $?answer (explode$ ?line))
  )
  $?answer
)


