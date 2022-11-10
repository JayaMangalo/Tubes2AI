(deftemplate Description
    (slot ID (type SYMBOL) (default ?NONE))
    (multislot Features)
)
(deftemplate Classification
    (slot ID (type SYMBOL) (default ?NONE))
    (slot Class (type SYMBOL) (default ?NONE))
    (slot Flag (type INTEGER) (default 0))
)

(defrule ToRoot
    (Description (ID ?x))
    =>
    (assert (Classification (ID ?x)(Class DndClasses)))
)

(defrule D-Magic (declare (salience 10))
    (Classification (ID ?x) (Class DndClasses))  
    ?d <- (Description (ID ?x) (Features $?y))
    (not (Description (ID ?x) (Features $?l Magic $?r)))
    =>
    (printout t ?x ": Magic (Yes/No)? > ")
    (bind ?Magic (read))
    (if (eq ?Magic Yes) then (modify ?d (Features $?y Magic Yes)))
    (if (eq ?Magic No) then (modify ?d (Features $?y Magic No)))
)

(defrule ToFullCasters
    (Classification (ID ?x) (Class DndClasses))
    (Description (ID ?x) (Features $?s Magic Yes $?f))
    =>
    (assert (Classification (ID ?x)
            (Class FullCasters)))
)

(defrule ToNonMagic
   (Classification (ID ?x) (Class DndClasses))
   (Description (ID ?x)(Features $?s Magic No $?f))
    =>
   (assert (Classification (ID ?x)	
           (Class NonMagic)))
)

(defrule D-Civilized (declare (salience 8))
    (Classification (ID ?x) (Class NonMagic))  
    ?d <- (Description (ID ?x) (Features $?y))
    (not (Description (ID ?x) (Features $?l Civilized $?r)))
    =>
    (printout t ?x ": Civilized (Yes/No)? > ")
    (bind ?Civilized (read))
    (if (subsetp (create$ ?Civilized) (create$ Yes No)) then (modify ?d (Features $?y Civilized ?Civilized)))
)

(defrule ToOutdoors
    (Classification (ID ?x) (Class NonMagic))
    (Description (ID ?x)(Features $?s Civilized No $?f))
    =>
   (assert (Classification (ID ?x)
           (Class Outdoors)))
)

(defrule ToCity
    (Classification (ID ?x) (Class NonMagic))
    (Description (ID ?x)(Features $?s Civilized Yes $?f))
    =>
    (assert (Classification (ID ?x)
            (Class City)))
)

(defrule D-Stealth (declare (salience 6))
    (Classification (ID ?x) (Class City))  
    ?d <- (Description (ID ?x) (Features $?y))
    (not (Description (ID ?x) (Features $?l Stealth $?r)))
    =>
    (printout t ?x ": Stealth (Yes/No)? > ")
    (bind ?Stealth (read))
    (if (subsetp (create$ ?Stealth) (create$ Yes No)) then (modify ?d (Features $?y Stealth ?Stealth)))
)

(defrule ToRogue
    (Classification (ID ?x) (Class City))
    (Description (ID ?x)(Features $?s Stealth Yes $?f))
    =>
    (assert (Classification (ID ?x)
            (Class Rogue) (Flag 1)))
)

(defrule ToWarriors
    (Classification (ID ?x) (Class City))
    (Description (ID ?x)(Features $?s Stealth No $?f))
    =>
   (assert (Classification (ID ?x)
           (Class Warriors)))
)

(defrule D-Mercenary (declare (salience 4))
    (Classification (ID ?x) (Class Warriors))  
    ?d <- (Description (ID ?x) (Features $?y))
    (not (Description (ID ?x) (Features $?l Mercenary $?r)))
    =>
    (printout t ?x ": Mercenary (Yes/No)? > ")
    (bind ?Mercenary (read))
    (if (subsetp (create$ ?Mercenary) (create$ Yes No)) then (modify ?d (Features $?y Mercenary ?Mercenary)))
)

(defrule ToFighter
    (Classification (ID ?x) (Class Warriors))
    (Description (ID ?x)(Features $?s Mercenary Yes $?f))
    =>
    (assert (Classification (ID ?x)
            (Class Fighter) (Flag 1)))
)

(defrule ToPaladin
    (Classification (ID ?x) (Class Warriors))
    (Description (ID ?x)(Features $?s Mercenary No $?f))
    =>
    (assert (Classification (ID ?x)
            (Class Paladin) (Flag 1)))
)

(defrule D-Rage (declare (salience 6))
    (Classification (ID ?x) (Class Outdoors))  
    ?d <- (Description (ID ?x) (Features $?y))
    (not (Description (ID ?x) (Features $?l Rage $?r)))
    =>
    (printout t ?x ": Rage (Yes/No)? > ")
    (bind ?Rage (read))
    (if (subsetp (create$ ?Rage) (create$ Yes No)) then (modify ?d (Features $?y Rage ?Rage)))
)

(defrule ToBarbarian
    (Classification (ID ?x) (Class Outdoors))
    (Description (ID ?x)(Features $?s Rage Yes $?f))
    =>
    (assert (Classification (ID ?x)
            (Class Barbarian) (Flag 1)))
)

(defrule ToCalm
   (Classification (ID ?x) (Class Outdoors))
   (Description (ID ?x)(Features $?s Rage No $?f))
   =>
   (assert (Classification (ID ?x)
           (Class Calm)))
)

(defrule D-Strategy (declare (salience 4))
    (Classification (ID ?x) (Class Calm))  
    ?d <- (Description (ID ?x) (Features $?y))
    (not (Description (ID ?x) (Features $?l Strategy $?r)))
    =>
    (printout t ?x ": Strategy (Yes/No)? > ")
    (bind ?Strategy (read))
    (if (subsetp (create$ ?Strategy) (create$ Yes No)) then (modify ?d (Features $?y Strategy ?Strategy)))
)

(defrule ToRanger
    (Classification (ID ?x) (Class Calm))
    (Description (ID ?x)(Features $?s Strategy Yes $?f))
    =>
    (assert (Classification (ID ?x)
            (Class Ranger) (Flag 1)))
)

(defrule ToMonk
    (Classification (ID ?x) (Class Calm))
    (Description (ID ?x)(Features $?s Strategy No $?f))
    =>
    (assert (Classification (ID ?x)
            (Class Monk) (Flag 1)))
)

(defrule D-BornwithMagic (declare (salience 8))
    (Classification (ID ?x) (Class FullCasters))  
    ?d <- (Description (ID ?x) (Features $?y))
    (not (Description (ID ?x) (Features $?l BornwithMagic $?r)))
    =>
    (printout t ?x ": BornwithMagic (Yes/No)? > ")
    (bind ?BornwithMagic (read))
    (if (subsetp (create$ ?BornwithMagic) (create$ Yes No)) then (modify ?d (Features $?y BornwithMagic ?BornwithMagic)))
)

(defrule ToSorcerer
    (Classification (ID ?x) (Class FullCasters))
    (Description (ID ?x)(Features $?s BornwithMagic Yes $?f))
    =>
    (assert (Classification (ID ?x)
            (Class Sorcerer) (Flag 1)))
)

(defrule ToGainedMagic
   (Classification (ID ?x) (Class FullCasters))
   (Description (ID ?x)(Features $?s BornwithMagic No $?f))
   =>
   (assert (Classification (ID ?x)
           (Class GainedMagic)))
)

(defrule D-MagicSource (declare (salience 6))
    (Classification (ID ?x) (Class GainedMagic))  
    ?d <- (Description (ID ?x) (Features $?y))
    (not (test (subsetp (create$ MagicSource) $?y)))
    =>
    (printout t ?x ": MagicSource (Religion/Pact/Music/Nature/Books)? > ")
    (bind ?MagicSource (read))
    (if (subsetp (create$ ?MagicSource) (create$ Religion Pact Music Nature Books)) then 
        (modify ?d (Features $?y MagicSource ?MagicSource))
    )
)


(defrule ToCleric
    (Classification (ID ?x) (Class GainedMagic))
    (Description (ID ?x)(Features $?s MagicSource Religion $?f))
    =>
    (assert (Classification (ID ?x)
            (Class Cleric) (Flag 1)))
)

(defrule ToWarlock
    (Classification (ID ?x) (Class GainedMagic))
    (Description (ID ?x)(Features $?s MagicSource Pact $?f))
    =>
    (assert (Classification (ID ?x)
            (Class Warlock) (Flag 1)))
)

(defrule ToBard
    (Classification (ID ?x) (Class GainedMagic))
    (Description (ID ?x)(Features $?s MagicSource Music $?f))
    =>
    (assert (Classification (ID ?x)
            (Class Bard) (Flag 1)))
)

(defrule ToDruid
    (Classification (ID ?x) (Class GainedMagic))
    (Description (ID ?x)(Features $?s MagicSource Nature $?f))
    =>
    (assert (Classification (ID ?x)
            (Class Druid) (Flag 1)))
)

(defrule ToWizard
    (Classification (ID ?x) (Class GainedMagic))
    (Description (ID ?x)(Features $?s MagicSource Books $?f))
    =>
    (assert (Classification (ID ?x)
            (Class Wizard) (Flag 1)))
)

(defrule Print (declare (salience -1))
    (Classification (ID ?x) (Class ?y) (Flag 1))
    =>
    (printout t ?x " is a(n) " ?y crlf)
)