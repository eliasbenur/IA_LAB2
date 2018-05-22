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
