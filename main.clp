(deftemplate Diagnosis
    (slot Antibody (type SYMBOL) (default ?NONE))
    (slot Status (type SYMBOL) (default negative))
)
(deftemplate Result
    (slot Disease (default ?NONE))
)
; (deffacts initial-facts (Diagnosis (Antibody Start)))

(defrule D-HBsAg
    (Diagnosis (Antibody Start))
    =>
    (printout t ": HBsAg? > ")
    (bind ?HBsAg (read))
    (if (subsetp (create$ ?HBsAg) (create$ positive negative)) 
    then(assert(Diagnosis (Antibody HBsAg)(Status ?HBsAg))))
)

(defrule D-antiHDV
    (Diagnosis (Antibody HBsAg)(Status positive))
    =>
    (printout t ": anti-HDV? > ")
    (bind ?antiHDV(read))
    (if (subsetp (create$ ?antiHDV) (create$ positive negative)) 
    then (assert(Diagnosis (Antibody antiHDV)(Status ?antiHDV))))

)

(defrule D-antiHBc
    (or 
        (Diagnosis (Antibody antiHDV)(Status negative))
        (and 
            (Diagnosis (Antibody HBsAg)(Status negative))
            (Diagnosis (Antibody antiHBs)(Status positive))
        )
    )
    =>
    (printout t ": anti-HBc? > ")
    (bind ?antiHBc(read))
    (if (subsetp (create$ ?antiHBc) (create$ positive negative)) 
    then (assert(Diagnosis (Antibody antiHBc)(Status ?antiHBc))))
)

(defrule D-antiHBs
    (or 
        (and 
            (Diagnosis (Antibody HBsAg)(Status positive))
            (Diagnosis (Antibody antiHBc)(Status positive))
        )
        (Diagnosis (Antibody HBsAg)(Status negative))
    )
    =>
    (printout t ": anti-HBs? > ")
    (bind ?antiHBs(read))
    (if (subsetp (create$ ?antiHBs) (create$ positive negative)) 
    then (assert(Diagnosis (Antibody antiHBs)(Status ?antiHBs))))
)

(defrule D-IgM_antiHBc
    (Diagnosis (Antibody antiHBs)(Status negative))
    =>
    (printout t ": IgM anti-HBc? > ")
    (bind ?IgM_antiHBc(read))
    (if (subsetp (create$ ?IgM_antiHBc) (create$ positive negative)) 
    then (assert(Diagnosis (Antibody IgM_antiHBc)(Status ?IgM_antiHBc))))
)

(defrule ToHepatitisBD
    (Diagnosis (Antibody antiHDV)(Status positive))
    =>
    (assert (Result(Disease "Hepatitis B+D")))
)

(defrule ToUncertain
    (Diagnosis (Antibody antiHDV)(Status negative))
    (or 
    (Diagnosis (Antibody antiHBc)(Status negative)) 
    (Diagnosis (Antibody antiHBs)(Status positive))
    )
    =>
    (assert (Result(Disease "Uncertain configuration")))
)


(defrule ToAcuteInfection
    (Diagnosis (Antibody IgM_antiHBc)(Status positive))
    =>
    (assert (Result(Disease "Acute Infection")))
)

(defrule ToChronicInfection
    (Diagnosis (Antibody IgM_antiHBc)(Status negative))
    =>
    (assert (Result(Disease "Chronic Infection")))
)

(defrule ToCured
    (Diagnosis (Antibody HBsAg)(Status negative))
    (Diagnosis (Antibody antiHBs)(Status positive))
    (Diagnosis (Antibody antiHBc)(Status positive))
    =>
    (assert (Result(Disease "Cured")))
)

(defrule ToVaccinated
    (Diagnosis (Antibody HBsAg)(Status negative))
    (Diagnosis (Antibody antiHBs)(Status positive))
    (Diagnosis (Antibody antiHBc)(Status negative))
    =>
    (assert (Result(Disease "Vaccinated")))
)

(defrule ToUnclear
    (Diagnosis (Antibody HBsAg)(Status negative))
    (Diagnosis (Antibody antiHBs)(Status negative))
    (Diagnosis (Antibody antiHBc)(Status positive))
    =>
    (assert (Result(Disease "Unclear (possible resolved)")))
)

(defrule ToSuspicious
    (Diagnosis (Antibody HBsAg)(Status negative))
    (Diagnosis (Antibody antiHBs)(Status negative))
    (Diagnosis (Antibody antiHBc)(Status negative))
    =>
    (assert (Result(Disease "Healthy not vaccinated or suspicious")))
)


(defrule Print (declare (salience -1))
    (Result(Disease ?DiseaseName))
    =>
    (printout t "Hasil Prediksi = " ?DiseaseName crlf)
)