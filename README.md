# IA_LAB2

# Resumen de commandos de Clips 

## Commandos Basicos

### Crear un Fact
~~~~
(assert (duck))
~~~~

### Listado de Facts
~~~~
(facts)
~~~~

### Limpiza de Facts y Rules
~~~~
(clear)
~~~~

### nil = NULL

### Eliminar un Fact
~~~~
(retract 3)
~~~~

### Debuging
~~~~
(watch facts)
(unwatch facts)
# facts/ instances/ activations /slots/ rules / etc ...

#Para que todo el output de los what se quede guardado en un fichero
# (watch all) para ver todo
(dribble-on <filename>) 
(dribble-off <filename>)
~~~~

### Breakpoints
~~~~
(set-break <rulename>)
(remove-break <rulename>)
(show-breaks)
~~~~

### Crear una Rule
~~~~
(defrule duck
 (animal-is duck)
	=>
 (assert (sound-is quack))
)
~~~~

### Ver la Agenda (Lista de Rules listas para activarse)
~~~~
(agenda)
~~~~

### Ejecutar
~~~~
(run)
~~~~

### Guardar las rules en un archivo
~~~~
(save "duck.clp")
(load "duck.clp") # To load the file
#save-facts y load-facts para guardar Facts
~~~~

### Ver una Rule
~~~~
(ppdeftule duck)
(rules) # Para ver la lista
~~~~

### PrintOut
~~~~
(defrule duck
 (animal-is duck)
	=>
 (printout t "quack" crlf)) #El crlf es para mejorar el aspecto del output
 # Output -> quack
~~~~

### Definir multiples Facts y Insertarlos
~~~~
(deffacts walk "Some facts about walking"
 (status walking)
 (walk-sign walk)
)

(reset) #Para insertarlos
		#Ademas reset no elimina las rules, solo los Facts

(undeffacts walk) #Para eliminar el conjuto de facts
~~~~

### Para ver que condicionales tiene activos una Rule
~~~~
(matches take-a-vacation)
#Output-> 
	Matches for Pattern 1
	f-1
	Matches for Pattern 2
	 None
	Matches for Pattern 3
	 None
~~~~

### Slots
~~~~
(deftemplate prospect
 (slot name
	 (type STRING)
	 (default ?DERIVE))
 (slot assets
	 (type SYMBOL)
	 (allowed-symbols
 	 	poor rich wealthy loaded)
	 (default rich))
 (slot age
	 (type NUMBER)
	 (default 80))
) 

#INSERTANDO
(assert(prospect))
(assert (prospect (age 99)
					(name "Dopey")))
~~~~

### Modificar un slot
~~~~
(modify ?prospect (assets poor))
~~~~

### Insertar por Teclado 
~~~~
(read)
(realine)
~~~~

### Elementos condicionales 
~~~~
The test conditional element provides a very powerful way by which to compare numbers, variables, and strings on the LHS. The (test) is used as a pattern on the LHS. A rule will only be triggered if the (test) is satisfied together with other patterns.
Many predefined functions are provided by CLIPS. Logical functions are:
	not: Boolean not
	and: Boolean and
	or: Boolean or
Arithmetic functions are:
	/: Division
	*: Multiplication
	+: Addition
	-: Subtraction
Comparison functions are:
	eq: Equal (any type). Compares type and magnitude.
	neq: Not equal (any type). 
	=: Equal (numeric type). Compares magnitude.
	<>: Not equal (numeric type).
	>=: Greater than or equal to
	>: Greater than.
	<=: Less than or equal to.
	<: Less than. 
~~~~

### Definir Classes
~~~~
(defclass UPPIE (is-a USER)) # User -> Padre, UPPIE -> hija
(browse-classes) # Ver lista de classes 
(browse-classes UPPIE) # Solo de classe UPPIE y abajo 

#Definiendo una Classe con attr
(defclass DUCK (is-a USER)
 (slot sound (default quack))
 (slot ID)
 (slot sex (default male))
)
#Creando una instancia	
(make-instance Dorky_Duck of DUCK)
#Viendo su contenido
(send [Dorky_Duck] print)

~~~~

### ID en clases
~~~~
(slot ID (default (gensym*))) 

(slot ID (default-dynamic (gensym*)))
~~~~

### Instancia
~~~~
(instances) # Ver instancias

(make-instance [Dorky] of DUCK) # Insertar una instances

(make-instance Dixie_Duck of DUCK
 (sound quack) (age 2))

(unmake-instance *) # Elimina todas las instancias

(unmake-instance [Dorky]) # Elimina una instancia

~~~~




