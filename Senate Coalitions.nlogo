; Senate Coalitions
; Max Feinleib
; CS 372, Northwestern University
; Final Project v3 (May 28)

extensions [ csv ]

globals [
  active-bill          ; the bill under consideration
  bill-done?           ; signal to start a new bill
  bills-passed-uc      ; number of bills passed by unanimous consent
  bills-passed-cloture ; number of bills passed using cloture
  bills-passed-simple  ; number of bills passed without cloture
  bills-failed         ; number of bills that failed
  passing-coalitions   ; list: number of proponents on passed bills
  minority-supports    ; list: % of minority senators in support of passed bills
  passed-bill-times    ; list: consideration time on passed bills
  bill-times           ; list: consideration time on all bills
]

breed [senators senator]
breed [bills bill]
breed [status-quos status-quo]

directed-link-breed [policy-movements policy-movement]
undirected-link-breed [supports support]

senators-own [
  lastname
  firstname
  home-state
  dwnom1         ; 1st-dimension DW-NOMINATE location
  dwnom2         ; 2nd-dimension DW-NOMINATE location
  party          ; ICPSR party code (independents are marked separately)
  maj-leader     ; 1 for majority-party leadership, 0 otherwise
  pbca           ; percent bipartisan cosponsors attracted (as bill sponsor)
  pbco           ; percent bipartisan cosponsorships offered (as non-sponsor)
  avg-cosponsors ; average number of cosponsors attracted (as bill sponsor)
  majority?      ; whether a senator is in the majority party
  coalition      ; "proponent", "opponent", "obstructionist", or "" (blank)
]

bills-own [
  dwnom1      ; 1st-dimension DW-NOMINATE location
  dwnom2      ; 2nd-dimension DW-NOMINATE location
  sponsor     ; sponsoring senator
  cosponsors  ; agentset of cosponsoring senators
  squo-policy ; the related status-quo policy
  active-time ; how long this bill has been active, in ticks
]

status-quos-own [
  dwnom1 ; 1st-dimension DW-NOMINATE location
  dwnom2 ; 2nd-dimension DW-NOMINATE location
]

to setup
  clear-all
  file-close-all ; close any (CSV) files open from last run
  ; turtle shapes
  set-default-shape senators "person"
  set-default-shape bills "circle 2"
  set-default-shape status-quos "square 2"
  set-default-shape policy-movements "movement"
  ; initialize senators
  setup-senators "data/senators_data_114.csv"
  ; initialize metrics
  setup-metrics
  reset-ticks
end

to go
  ; create a bill
  place-bill
  get-sponsor-and-cosponsors

  ; build coalitions
  create-coalitions
  ; loop until bill passes or dies
  while [not bill-done?] [
    advance-coalitions
    ask active-bill [ attempt-passage ]
  ]
  ; reset and move on to next bill
  clear-bills
  tick
end

;;; SETUP ;;;

to setup-senators [filename]
  ; set most senator properties from CSV data
  read-senator-data filename

  ; set senator location (by DW-NOMINATE), color (by party),
  ; majority status, and a blank coalition
  ask senators [
    set-dwnom-location
    set majority? (party = first modes [party] of senators)
    set color (
      ifelse-value
      party = 100 [blue]   ; Dem
      party = 328 [yellow] ; Ind
      party = 200 [red]    ; GOP
    )
    set coalition "" ; no initial coalition
  ]
end

; read senators data from CSV
; based on READ-TURTLES-FROM-CSV procedure in CSV Example model
to read-senator-data [filename]
  file-close-all ; close any open files

  if not file-exists? filename [
    user-message (word "No file '" filename "' exists!")
    stop
  ]

  file-open filename ; open the file with senator data

  ; read data from CSV
  while [ not file-at-end? ] [
    ; read a CSV line into a list
    let data csv:from-row file-read-line
    ; use that list to create a senator
    create-senators 1 [
      set lastname       item 0 data  ; last
      set firstname      item 1 data  ; first
      set home-state     item 2 data  ; state
      set dwnom1         item 4 data  ; dwnom1
      set dwnom2         item 5 data  ; dwnom2
      set party          item 10 data ; party
      set maj-leader     item 14 data ; maj_leader
      set avg-cosponsors item 22 data ; mean_cospon_spon_SN_nc
      set pbca           item 24 data ; mean_pct_cospon_opp_spon_SN_nc
      set pbco           item 26 data ; perc_co_bipart_nc
    ]
  ]

  file-close ; close the CSV file
end

; initialize metrics
to setup-metrics
  set bills-passed-uc 0
  set bills-passed-cloture 0
  set bills-passed-simple 0
  set bills-failed 0
  set passing-coalitions (list)
  set minority-supports (list)
  set passed-bill-times (list)
  set bill-times (list)
end

;;; CREATING A BILL ;;;

; create a new bill and associated status quo, and
; tick and increment active-time
to place-bill
  set bill-done? false
  ; placeholder to pair bill to status quo
  let new-status-quo 0
  create-status-quos 1 [
    set dwnom1 (random-float 2) - 1
    set dwnom2 random-dwnom2
    set-dwnom-location
    set color green + 1
    set new-status-quo self
  ]
  create-bills 1 [
    set dwnom1 (random-float 2) - 1
    set dwnom2 random-dwnom2
    set-dwnom-location
    set color green + 1
    set sponsor nobody
    set cosponsors no-turtles
    set active-time 1
    ; pair bills and status quos one-to-one
    set squo-policy new-status-quo
    create-policy-movement-from squo-policy [
      set color green - 2
      set thickness 0.2
    ]
    ; set this bill to be the active-bill
    set active-bill self
  ]
  tick
end

; clear bills, status quos, and (co)sponsorship links
to clear-bills
  ask bills [die]
  ask status-quos [die]
  ask senators [ set coalition "" ]
  clear-links
end

; call GET-SPONSOR and ATTRACT-COSPONSORS, and
; tick and increment active-time
to get-sponsor-and-cosponsors
  ask active-bill [
    get-sponsor
    attract-cosponsors
    set active-time active-time + 1
  ]
  tick
end

to get-sponsor ; bill procedure
  ; sponsor is the senator with maximum policy benefits
  set sponsor max-one-of senators [policy-benefits myself]
  create-support-with sponsor [
    set color green + 1
    set thickness 0.2
  ]
  ; add sponsor to proponent coalition
  ask sponsor [ set coalition "proponent" ]
end

to attract-cosponsors ; bill procedure
  ; number of cosponsors from Poisson distribution using sponsor's avg-cosponsors
  let n-cosponsors random-poisson [avg-cosponsors] of sponsor
  ; ask other senators to cosponsor bill
  ; order asks by senators' bill utility, PBCA/PBCO, home state
  let potential-cosponsor-list (
    reverse sort-on [cosponsor-likelihood myself]
    senators with [cosponsor-likelihood myself > 0] who-are-not sponsor
  )
  set cosponsors turtle-set first-n-from-list n-cosponsors potential-cosponsor-list
  ; create links between cosponsors and sponsor
  ; and add cosponsors to proponent coalition
  ask cosponsors [
    create-support-with [sponsor] of myself
    set coalition "proponent"
  ]
end

; likelihood of a senator to cosponsor a bill
; NOTE: not a calibrated probability, just used for ordering potential cosponsors
to-report cosponsor-likelihood [a-bill] ; senator procedure
  let sponsor-pbca [[pbca] of sponsor] of a-bill
  let sponsor-state [[home-state] of sponsor] of a-bill
  let sponsor-party [[party] of sponsor] of a-bill

  ; base benefits
  let benefits policy-benefits a-bill
  ; more likely to attract cosponsors from same party (according to PBCA)
  let party-factor (ifelse-value
    ; same party: (100 - PBCO) * (100 - sponsor PBCA)
    (party = sponsor-party) [(100 - pbco) * (100 - sponsor-pbca)]
    ; independents: 50 * 50 = 25
    (party = 328) [25]
    ; opposite party: PBCO * sponsor PBCA
    [pbco * sponsor-pbca]
  )
  ; bump cosponsor likelihood of senator from the same state as sponsor
  let state-bonus ifelse-value (home-state = sponsor-state) [10] [0]

  report benefits * party-factor + state-bonus
end

;;; GENERATING COALITIONS ;;;

; ask senators to find-initial-coalition, and
; tick and increment active-time
to create-coalitions
  ask non-sponsors [
    find-initial-coalition active-bill
  ]
  ask active-bill [ set active-time active-time + 1 ]
  tick
end

; set initial proponent/opponent coalitions
; because the general utility formulas need existing coalition sizes
to find-initial-coalition [a-bill] ; senator procedure
  ; NOTE: if policy-benefits is zero, initial coalition will be "opponent"
  set coalition ifelse-value (policy-benefits a-bill > 0) ["proponent"] ["opponent"]
end

; ask senators to find-coalition, and
; tick and increment active-time
to advance-coalitions
  ask non-sponsors [
    find-coalition active-bill
  ]
  ask active-bill [
    set active-time active-time + 1
  ]
  tick
end

; find a senator's coalition for the next tick
to find-coalition [a-bill] ; senator procedure
  let eu-prop proponent-utility a-bill
  let eu-obst obstructionist-utility a-bill
  ; tiebreaker goes to proponent over obstructionist coalition
  let better-coalition (
    ifelse-value (eu-prop >= eu-obst) ["proponent"] ["obstructionist"]
  )
  let better-coalition-utility max list eu-prop eu-obst
  ; but opponent still wins tiebreaker over the others
  set coalition (
    ifelse-value (better-coalition-utility > 0) [better-coalition] ["opponent"]
  )
end

; check passage conditions
to attempt-passage
  let bill-passed? false

  ; 4 bill end states
  ; each sets bill-done? to true and
  ; increments their associated counter
  (ifelse
    bill-failing? [
      set bill-done? true
      set bills-failed bills-failed + 1
    ]
    unanimous-consent? [
      set bill-done? true
      set bill-passed? true
      set bills-passed-uc bills-passed-uc + 1
    ]
    passage-vote? [
      set bill-done? true
      set bill-passed? true
      set bills-passed-simple bills-passed-simple + 1
    ]
    cloture-vote? [
      set bill-done? true
      set bill-passed? true
      set bills-passed-cloture bills-passed-cloture + 1
    ]
  ) ; else: bill continues

  ; record metrics for finished bills
  if bill-passed? [
    set passing-coalitions lput n-proponents passing-coalitions
    ; minority-supports records % of minority party in proponent coalition
    set minority-supports lput (
      100 * (count proponents with [not majority?]) / (count senators with [not majority?])
    ) minority-supports
    set passed-bill-times lput active-time passed-bill-times
  ]

  if bill-done? [ set bill-times lput active-time bill-times ]
end

;;; COALITION UTILITY CALCULATIONS ;;;

;; UTILITY ;;

to-report proponent-utility [a-bill] ; senator procedure
  report (exp-benefits-of-proponent a-bill) - (costs-of-proponent a-bill)
end

to-report obstructionist-utility [a-bill] ; senator procedure
  report (exp-benefits-of-obstructionist a-bill) - (costs-of-obstructionist a-bill)
end

;; EXPECTED BENEFITS ;;

; proponents' expected benefits = raw benefits * passage probability
to-report exp-benefits-of-proponent [a-bill] ; senator reporter
  report (benefits-of-proponent a-bill) * passage-probability
end

; obstructionists also get position-taking benefits regardless of success
to-report exp-benefits-of-obstructionist [a-bill] ; senator reporter
  report (
    (1 - passage-probability) * (obst-blocking-benefits a-bill)
    + (obst-position-taking-benefits a-bill)
  )
end

;; RAW BENEFITS ;;

; NOTE: benefits are shared among coalition members, so they are
; highest when the coalition is small, linearly declining to 0 if
; there are 100 proponents or 50 obstructionists.
; NOTE: opponents are defined to have 0 utility.

; raw policy benefits to a proponent
to-report benefits-of-proponent [a-bill] ; senator reporter
  report policy-benefits a-bill * ((100 - n-proponents) / 50)
end

; an obstructionist's raw benefits of blocking a bill:
; the inverse of their policy benefits, scaled by the number of obstructionists
to-report obst-blocking-benefits [a-bill] ; senator reporter
  report (- policy-benefits a-bill) * ((50 - n-obstructionists) / 50)
end

; an obstructionist's raw position-taking benefits
; position-taking benefits take the same sign as blocking benefits
to-report obst-position-taking-benefits [a-bill] ; senator reporter
  report (
    position-taking-benefits
    * (- sign policy-benefits a-bill)
    * ((50 - n-obstructionists) / 50)
  )
end

; probability of bill passage based on size of proponent coalition
; "pi" parameter in Wawro & Schickler (2006), Feinleib (2024)
to-report passage-probability
  let alpha-value ifelse-value (n-proponents >= cloture-threshold) [alpha-above-cloture] [alpha]
  ; if there are less than 50 proponents, pi = 0.01
  report ifelse-value (n-proponents >= 50) [((n-proponents - 50) / 50) ^ alpha-value] [0.01]
end

;; COSTS ;;

to-report costs-of-proponent [a-bill] ; senator reporter
  report floor-time-costs a-bill + proponent-debate-time-costs a-bill
end

to-report costs-of-obstructionist [a-bill] ; senator reporter
  report floor-time-costs a-bill + obstructionist-debate-time-costs a-bill
end

; floor time costs are higher for majority-party senators
to-report floor-time-costs [a-bill] ; senator reporter
  report (floor (([active-time] of a-bill) / 10))
  * ifelse-value majority? [majority-floor-time-costs] [minority-floor-time-costs]
end

; debate time costs may vary by coalition
to-report proponent-debate-time-costs [a-bill] ; senator reporter
  report [active-time] of a-bill * prop-debate-costs
end

to-report obstructionist-debate-time-costs [a-bill] ; senator reporter
  report [active-time] of a-bill * obst-debate-costs
end

;;; BILL END STATES ;;;

; bills fail when it is clear they don't have majority support
to-report bill-failing?
  report n-proponents < 47
end

; bills can pass by UC if about all senators support it
; and no senator is obstructing it
to-report unanimous-consent?
  report n-proponents > 90 and n-obstructionists = 0
end

; bills pass by simple majority vote
; if there is no organized obstruction
to-report passage-vote?
  report n-proponents > 50 and n-obstructionists < obsts-to-block-vote
end

; cloture is invoked with a supermajority vote
to-report cloture-vote?
  report n-proponents >= cloture-threshold
end

;;; COALITIONS AND THEIR SIZES ;;;

; flippable senators: everyone except sponsor and cosponsors
to-report non-sponsors
  report senators who-are-not (
    turtle-set ([sponsor] of active-bill) ([cosponsors] of active-bill)
  )
end

;; coalition turtle sets
to-report proponents
  report turtle-set senators with [coalition = "proponent"]
end

to-report opponents
  report turtle-set senators with [coalition = "opponent"]
end

to-report obstructionists
  report turtle-set senators with [coalition = "obstructionist"]
end

;; coalition sizes
to-report n-proponents
  report count proponents
end

to-report n-opponents
  report count opponents
end

to-report n-obstructionists
  report count obstructionists
end

;;; DW-NOMINATE HELPERS ;;;

; a senator's utility from a bill is the reduction in the senator's
; distance to the bill (versus its associated status quo)
to-report policy-benefits [a-bill] ; senator reporter
  let status-quo-dist dwnom-distance [squo-policy] of a-bill
  let bill-dist dwnom-distance a-bill
  report status-quo-dist - bill-dist
end

; distance from agent to another agent in DW-NOMINATE units
; weighted by DWNOM1-EMPHASIS
to-report dwnom-distance [other-agent] ; agent reporter
  ; sign of components doesn't matter because they're getting squared
  let dwnom1-distance
  ([dwnom1] of self) - ([dwnom1] of other-agent) * (dwnom1-emphasis / 50)
  let dwnom2-distance
  ([dwnom2] of self) - ([dwnom2] of other-agent) * ((100 - dwnom1-emphasis) / 50)

  report sqrt (dwnom1-distance ^ 2 + dwnom2-distance ^ 2)
end

; set xcor/ycor using DW-NOMINATE coordinates
to set-dwnom-location ; turtle procedure
  set xcor dwnom1 * 20
  set ycor dwnom2 * 10
end

; random dwnom2 coordinate within a turtle's possible dwnom2 range
to-report random-dwnom2 ; turtle reporter
  report (random-float (2 * dwnom2-range)) - dwnom2-range
end

; max absolute value of a turtle's dwnom2 coordinate
; so that total distance of their DW-NOMINATE coordinates is
; at most 1 unit from the origin
to-report dwnom2-range ; turtle reporter
  report sqrt (1 - dwnom1 ^ 2)
end

;;; SMALL GENERAL HELPERS ;;;

; first (up to) n elements in a list
to-report first-n-from-list [n lst]
  report sublist lst 0 min (list n length lst)
end

; mathematical sign function
to-report sign [number]
  report ifelse-value (number = 0) [ 0 ] [ number / abs number ]
end


; Credit: Some CSV code comes from the "CSV Example" model in the NetLogo Models Library.
; That model has been dedicated to the public domain.
@#$#@#$#@
GRAPHICS-WINDOW
210
10
874
355
-1
-1
16.0
1
10
1
1
1
0
0
0
1
-20
20
-10
10
1
1
1
ticks
30.0

BUTTON
15
15
80
48
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
350
385
530
418
dwnom1-emphasis
dwnom1-emphasis
0
100
90.0
1
1
%
HORIZONTAL

SLIDER
350
425
530
458
position-taking-benefits
position-taking-benefits
0
2
1.0
0.05
1
NIL
HORIZONTAL

SLIDER
15
215
195
248
alpha
alpha
0.01
1
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
15
255
195
288
alpha-above-cloture
alpha-above-cloture
0.01
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
15
175
195
208
cloture-threshold
cloture-threshold
51
100
60.0
1
1
votes
HORIZONTAL

BUTTON
15
55
80
88
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
15
365
97
410
proponents
n-proponents
0
1
11

MONITOR
190
365
297
410
obstructionists
n-obstructionists
17
1
11

MONITOR
105
365
182
410
opponents
n-opponents
17
1
11

PLOT
15
420
295
570
Coalitions
NIL
Senators
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"proponents" 1.0 0 -8732573 true "" "plot count proponents"
"opponents" 1.0 0 -1604481 true "" "plot count opponents"
"obstructionists" 1.0 0 -5298144 true "" "plot count obstructionists"

SLIDER
535
385
745
418
majority-floor-time-costs
majority-floor-time-costs
0
0.1
0.05
0.005
1
NIL
HORIZONTAL

SLIDER
535
425
745
458
minority-floor-time-costs
minority-floor-time-costs
0
0.1
0.01
0.005
1
NIL
HORIZONTAL

TEXTBOX
355
365
530
383
Utility parameters
11
0.0
1

SLIDER
350
465
530
498
prop-debate-costs
prop-debate-costs
0
0.1
0.01
0.005
1
NIL
HORIZONTAL

SLIDER
535
465
745
498
obst-debate-costs
obst-debate-costs
0
0.1
0.02
0.005
1
NIL
HORIZONTAL

TEXTBOX
20
155
195
181
Rules and environment
11
0.0
1

PLOT
885
15
1135
145
Passing coalitions
Senators
NIL
50.0
100.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -13840069 true "" "histogram passing-coalitions"

PLOT
885
200
1135
325
Minority support on passed bills
% of minority in support
NIL
0.0
100.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -13345367 true "" "histogram minority-supports"

MONITOR
920
505
1015
550
pass (simple)
bills-passed-simple / \n(bills-failed + bills-passed-uc \n+ bills-passed-cloture + bills-passed-simple) * 100
1
1
11

TEXTBOX
965
380
1065
398
Bill outcomes (%)
11
0.0
1

MONITOR
1035
405
1092
450
fail
bills-failed / \n(bills-failed + bills-passed-uc \n+ bills-passed-cloture + bills-passed-simple) * 100
1
1
11

MONITOR
920
405
1015
450
pass (UC)
bills-passed-uc / \n(bills-failed + bills-passed-uc \n+ bills-passed-cloture + bills-passed-simple) * 100
1
1
11

MONITOR
920
455
1015
500
pass (cloture)
bills-passed-cloture / \n(bills-failed + bills-passed-uc \n+ bills-passed-cloture + bills-passed-simple) * 100
1
1
11

MONITOR
885
145
942
190
mean
mean passing-coalitions
1
1
11

MONITOR
885
325
942
370
mean
mean minority-supports
1
1
11

SLIDER
15
295
195
328
obsts-to-block-vote
obsts-to-block-vote
1
50
3.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This is a model of legislative coalitions and the lawmaking process in the U.S. Senate. It shows the impact of different rules environments and member incentives on the operation of the Senate.

This model is based on the equation-based model of Senate coalitions in "Redesigning the Filibuster for More Effective Lawmaking in a Polarized Senate" by Max Feinleib (2024). That model in turn is a derivative of the model from the book *Filibuster* by Gregory J. Wawro and Eric Schickler (2006).


## HOW IT WORKS

### Setup

* Senators are initialized using data from Harbridge-Yong et al., "The Bipartisan Path to Effective Lawmaking" (2023). The data in this model is for the 114th Congress (2015-16).
   * Their locations correspond to their ideal points according to DW-NOMINATE.
   * Democrats are colored blue, Republicans red, and Independents yellow.
* The metrics on bill outcomes are reset.

### Go

The GO procedure runs a series of steps to consider a bill. For each bill:

**Step 1:** A bill is created at a random point in DW-NOMINATE policy space and associated with a status quo policy, which is also at a random point.

**Step 2a:** The bill attracts a sponsor.

* The sponsor will be the senator who receives the most policy benefits from the bill (because the bill moves policy toward their ideal point).
* The bill creates a green link with its sponsor.
* The sponsor permanently becomes a proponent of the bill.

**Step 2b:** The bill attracts cosponsors.

* The number of cosponsors (N-COSPONSORS) is randomly drawn from a Poisson distribution using the sponsor's average number of cosponsors on their bills.
* The 99 other senators calculate their "cosponsor likelihood" based on: 
	* Their own policy benefits from the bill, 
	* The party of the sponsor, how often the sponsor *attracts* bipartisan cosponsors, and how often the senator *offers* bipartisan cosponsorships, and 
	* A small bonus if they are from the same state as the sponsor.
* The cosponsors will be the N-COSPONSORS senators with the highest (positive) cosponsor likelihoods. If less than N-COSPONSORS senators have positive cosponsor likelihoods, the cosponsors will just be those senators with positive cosponsor likelihoods.
* The cosponsors create gray links with the sponsor.
* The cosponsors permanently become proponents of the bill.

**Step 3:** The senators (besides the sponsor and cosponsors) find their initial coalition: either **proponent** or **opponent**, depending on whether they receive positive policy benefits from the bill.

**Step 4a:** The senators (besides the sponsor and cosponsors) re-select their coalitions out of **proponent**, **opponent**, or **obstructionist**.

* The senators calculate their expected utility from joining the **proponent** or **obstructionist** coalitions. Expected utility refers to expected benefits minus costs.
	* The expected benefits for proponents start with their policy benefits. The benefits are downscaled based on the number of proponents, and upscaled based on the probability of passage (which depends on the number of proponents and the ALPHA parameters).
	* The expected benefits for obstructionists start with the opposite of their policy benefits. These benefits are downscaled based on the number of obstructionists and the probability of passage (which represents failure for obstructionists). Obstructionists also receive position-taking-benefits (which are similarly downscaled by the number of obstructionists) regardless of their probability of success.
	* The costs are a combination of:
		* The costs of consuming floor time, which are governed by MAJORITY-FLOOR-TIME-COSTS for majority-party senators, and MINORITY-FLOOR-TIME-COSTS for minority-party senators.
		* The costs of debate time, which increase by PROP-DEBATE-COSTS and OBST-DEBATE-COSTS for proponents and obstructionists, respectively, at each tick.
* The senator joins the coalition with the higher (positive) expected utility between **proponent** or **obstructionist**. If neither of those have positive expected utility, the senator joins the **opponent** coalition, which is defined to have zero utility.

**Step 4b:** The bill attempts to pass (or fails) based on the sizes of the different coalitions.

* **A bill fails** if there are less than 47 proponents.
* **A bill passes by unanimous consent (UC)** if there are more than 90 proponents and no obstructionists.
* **A bill passes with a simple majority vote** if there are more than 50 proponents and fewer than OBSTS-TO-BLOCK-VOTE obstructionsists.
* **A bill passes using cloture** if the number of proponents is greater than or equal to the CLOTURE-THRESHOLD.

Steps 4a and 4b repeat until the bill passes or fails. When that occurs, the outcome of the bill is recorded for the metrics in the interface monitors and plots.

**Step 5:** The bill is deleted (along with the associated status quo and links), and all senators clear their coalitions.



## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)


## THINGS TO NOTICE

(suggested things for the user to notice while running the model)


## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)


## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)


## NETLOGO FEATURES

Unlike most NetLogo models, ticks do not occur once per iteration of the GO procedure. Instead, ticks occur in several sub-procedures of the GO procedure (e.g. PLACE-BILL, GET-SPONSOR-AND-COSPONSORS, CREATE-COALITIONS, ADVANCE-COALITIONS). This design allows the model to describe how long it takes to consider each bill.


## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)


## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

movement
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 90 90 180
Line -7500403 true 150 90 210 180
Line -7500403 true 90 180 210 180
Polygon -7500403 true true 150 90 90 180 210 180 150 90
@#$#@#$#@
1
@#$#@#$#@
