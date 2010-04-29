(ns sheepy-magna
  (:require [clojure.contrib.math :as math])
  (:require [clojure.contrib.duck-streams :as output])
  (:require [clojure.contrib.str-utils :as string1])
  (:require (incanter core stats charts io))
  (:require [clojure.contrib.str-utils2 :as string2])
  (:import java.util.Date))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Sheepy Magna ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (c) Matt Burton. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ant sim ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;Set up some global parameters
;dimensions of square world
(def world-size 50)

;scale factor for food drawing
(def food-scale 500)

;number of inital homesteads
(def initial-homesteads 1)

;Cells will have a random possibilit
(def fertility-factor-max 10)

;parameter for how many friends someone has in their social network
(def num-friends 5)


;sleep time for the entire sim
(def simulation-sleep-time 10)


;a data structure for each cell in the world
(defstruct home :food :appetite)

;a data structure for the homestead agents
(defstruct homestead :fertility :friends )

;world is a 2d vector of refs to cells, at this stage the cells are empty
(def world 
     (apply vector 
            (map (fn [_] 
                   (apply vector (map (fn [_] (ref nil)) 
                                      (range world-size)))) 
                 (range world-size))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Misc flags
(def running true)
(def geographic true)

;So I can use this function before I define it
(declare create-homestead)


;Where to write files
(def baselocation "/home/mcburton/Dropbox/si/cscs530/simoutput/")

;Set up some global variables so I can track stuff
(def file (atom nil))
(def food-plot (atom nil))
(def food-data (atom nil))
(def homestead-data (atom nil))
(def homestead-plot (atom nil))

; global placeholder for homesteaders
(def homesteaders (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Variability Functions - This set of functions determine variation within the sim


;threshold before enlightenment
(def enlighten-threshold (atom -1))

;function to generate the random social networks
(defn gen-friends 
  "Generates n vectors of integer pairs based on world-size"
  [n]
  (take n (map #(apply vector %) (partition 2 (repeatedly #(rand-int world-size))))))


;lambda values for the poisson distrobution
;these could be adjusted for sweeping
(def fertility-lambda 10)
(def appetite-lambda 2)
(def max-fertility-lambda (atom nil))


;(incanter.stats/sample-poisson 10 :lambda 2)
(defn poisson-draw [x]
  (incanter.stats/sample-poisson 1 :lambda x))
(defn gen-fertility []
  (poisson-draw fertility-lambda))
(defn gen-max-fertility []
  (poisson-draw @max-fertility-lambda)
  @max-fertility-lambda)
(defn gen-appetite []
  (poisson-draw appetite-lambda))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;World Helper Functions

;from Clojure poetry
(defn rand-elt
  "Return a random element of this seq"
  [s]
  (nth s (rand-int (count s))))


;Rich Hickey's function to grab a reference from the world
(defn place [[x y]]
  (-> world (nth x) (nth y)))


;; Rich Hickey's torus world helper functions
(defn bound 
  "returns n wrapped into range 0-b"
  [b n]
    (let [n (rem n b)]
      (if (neg? n) 
        (+ n b) 
        n)))

(defn wrand 
  "given a vector of slice sizes, returns the index of a slice given a
  random spin of a roulette wheel with compartments proportional to
  slices."
  [slices]
  (let [total (reduce + slices)
        r (rand total)]
    (loop [i 0 sum 0]
      (if (< r (+ (slices i) sum))
        i
        (recur (inc i) (+ (slices i) sum))))))

;dirs are 0-7, starting at north and going clockwise
;these are the deltas in order to move one step in given dir
(def dir-delta {0 [0 -1]
                1 [1 -1]
                2 [1 0]
                3 [1 1]
                4 [0 1]
                5 [-1 1]
                6 [-1 0]
                7 [-1 -1]})

(defn delta-loc 
  "returns the location one step in the given dir. Note the world is a torus"
  [[x y] dir]
    (let [[dx dy] (dir-delta (bound 8 dir))]
      [(bound world-size (+ x dx)) (bound world-size (+ y dy))]))



; My function that leverages Rich's helper functions
(defn fetch-neighbors
  "returns a sequence of the homes around a location"
  [loc]
  (let [p (place loc)]) 
  (map #(delta-loc loc %) (range 0 8)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Agent Functions


(defn transfer-food 
  "this function increments the food at place p"
  [[dloc sloc]]
  (dosync
   (let [dest (place dloc)
	 source (place sloc)]
     (alter source assoc :food (dec (:food @source)))
     (alter dest assoc :food (inc (:food @dest)))))
  sloc)


(defn generate-recipients
  "Given a list of locations, sort those locations based upon food needs."
  [locations]
  (let [places (map #(place locations))]
    (sort-by #(:food (deref %)) places)))


(defn hungry? 
  "returns true if the incoming place has positive food"
  [place]
  (if (> 0 (:food @place))
    true
    false))

(defn share
  "share food from point source with points in destinations"
  [sloc dlocs]
  (let [source (place sloc)
	food (:food @source)
	dests (map place dlocs)]
    (doseq [needy-neighbor dests :when hungry?] ; only select hungry neighbors
      ;(when (> (:food @source) (:food @needy-neighbor)) 
	(dosync
	  (alter source assoc :food (+ (:food @source) (:food @needy-neighbor))) ;add the negative value of the neighbor to the source
	  (alter needy-neighbor assoc :food 0)) ; make the neighbor's food value zero
	;)
      )
    ))

 
; an "enlighten" function, this takes a list of locations ( geographic neighbors 
; or agent's social network) and creates new agents at these locations. 
; This also updates the main list of agents in case we need
; to access them at the REPL
; 
(defn enlighten
  "Given a list of neighbors randomly chooses one to become a homestead"
  [neighbors]
  (reset! homesteaders (conj @homesteaders (create-homestead (rand-elt neighbors)))))

; Main homestead agent function
(defn grow
  "Have the homesteader grow some food, share some food and enlighten a neigbor"
  [loc]
  (let  [p (place loc)
	 social-network (:friends (:homestead @p))
	 geo-network (fetch-neighbors loc)
	 current-food (:food @p)
	 fertility (:fertility (:homestead @p))]
    (dosync 
      (alter p assoc :food (+ fertility current-food )))
    (when (< 0 (:food @p))
      (if geographic
	 (share loc geo-network)
	 (share loc social-network)))
    ;(println enlighten-threshold)
    (when (> @enlighten-threshold (:food @p))
      (if geographic
	(enlighten geo-network)
	(enlighten social-network)))
    ))

  


; The-Hunger dynamic function
(defn eat
  "Step throught every home in the world and deplete each cells food value by thier appitite."
  []
  
   (doseq [x (range world-size) y (range world-size)]
     (dosync
      (let [p (place [x y])]
	(alter p assoc :food (- (:food @p) (:appetite @p))))))) 









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Measurement Functions - these are used to generate data and graphs


(defn get-col
  "quick helper funciton to grab columns from data"
  [data n]
  (let [col (incanter.core/sel @data :cols [n])]
    (if (seq? col)
      (vec col)
      (vector col))))


;(str (Date.))
(defn setup-metrix 
  "sets up the file-location for this run of the sim. Also initialzes the data sets"
  []
  (reset! file (str baselocation "Run-" (string2/replace (str (Date.)) #"\s" "-") ".csv"))
  (do
    ; set up the graph for measuring the amount of food at the homestead
    ;(reset! food-data (incanter.core/dataset (cons "step" (range initial-homesteads)) [[0 0]]))
    ;(reset! food-plot (incanter.charts/scatter-plot (get-col food-data 0) (get-col food-data 1) 
    ;:legend true 
    ;					       :series-label "Food"
    ;:x-label "steps"
    ;:y-label "Amount of food"))
   ; (incanter.charts/add-points @plot (get-col 0) (get-col 2) :series-label "two")
    ;(incanter.charts/add-points @plot (get-col 0) (get-col 3) :series-label "three")
    ;(incanter.core/view @food-plot)
    
    ;Set up a graph for homestead graph
    
    (reset! homestead-data (incanter.core/dataset (list "step" "num") [[0 0]]))
    (reset! homestead-plot (incanter.charts/scatter-plot (get-col homestead-data "step") 
							 (get-col homestead-data "num")
							 :x-label "Steps"
							 :y-label "Number of homesteads"))
    (incanter.core/view @homestead-plot)
    ))

(defn home-food-state
  "returns a data structure containing the state of the homesteads & their neighbors."
  [home-list]
  (let [results (map #(:food (deref (place %))) home-list)]
     results))

(defn output-pantry 
  "Spits out the food state of each homestead to a file."
  [file home-list step]
  (let [line (cons step (home-food-state home-list))
	newdata (incanter.core/conj-rows @food-data line)]
    (incanter.core/save newdata @file)
    (reset! food-data newdata))

)

 
(defn calc-homesteads
  "spits out the number of homesteads to a file."
  [file home-list step]
  (let [line (list step (count home-list))
	newdata (incanter.core/conj-rows @homestead-data line)]
    (incanter.core/save newdata @file)
    (reset! homestead-data newdata)))

(defn show-charts
  "graph the results of the last run"
  []                            
    (incanter.core/with-data @homestead-data
      
      (incanter.core/set-data @homestead-plot [(get-col homestead-data "step") (get-col homestead-data "num")] 0)
      ;(incanter.core/set-data @plot [(get-col "step") (get-col 1)] 1)
      ;(incanter.core/set-data @plot [(get-col "step") (get-col 2)] 2)
      ))



(defn metrixiate 
  "A master function for all the measurement tasks"
  [home-list step]
  ;(println step " " (count home-list))
  ;(output-pantry file home-list step) ; this measure is not useful w/ 700 homesteads
  (calc-homesteads file home-list step)
  (show-charts))





; Some misc code for running metrics
(comment 
(def x [ 1 2 3 4 5])
(def y [ 2 3 4 5 6])

(def data (incanter.io/read-dataset file))

;(def file "/Users/mcburton/Dropbox/si/cscs530/simoutput/Run-Thu-Apr-22-11:21:15-EDT-2010.csv")

(incanter.core/with-data data
  (incanter.core/set-data plot [(incanter.core/$ :col0) (incanter.core/$ :col1)] 0)
  (incanter.core/set-data plot [(incanter.core/$ :col0) (incanter.core/$ :col2)] 1)
  (incanter.core/set-data plot [(incanter.core/$ :col0) (incanter.core/$ :col3)] 2))

(incanter.core/set-data plot [[1][1]])
  


)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This is mostly rich hickey's code.
(import 
 '(java.awt Color Graphics Dimension)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))

;pixels per world cell
(def scale 5)
   


(defn fill-cell [#^Graphics g x y c]
  (doto g
    (.setColor c)
    (.fillRect (* x scale) (* y scale) scale scale)))



(defn render-place [g p x y]
  (if (pos? (:food p))
					; we have a positive amount of food, color the cell green
    (fill-cell g x y (new Color 0 255 0 
			  (int (min 255 (* 255 (/ 
						(:food p) 
						(max food-scale (:food p))))))))
					; we have a negative amount of food, color the cell red
    (fill-cell g x y (new Color 255 0 0
			  (int (min 255 (* 255 (/  
						(clojure.contrib.math/abs (:food p)) 
						(max food-scale (clojure.contrib.math/abs (:food p)))))))))))
  
(defn render [g]
  (let [v (dosync (apply vector (for [x (range world-size) y (range world-size)] 
				  @(place [x y]))))
	img (new BufferedImage (* scale world-size) (* scale world-size) 
		 (. BufferedImage TYPE_INT_ARGB))
	bg (. img (getGraphics))]
    (doto bg
      (.setColor (. Color white))
      (.fillRect 0 0 (. img (getWidth)) (. img (getHeight))))
    (dorun 
     (for [x (range world-size) y (range world-size)]
       (render-place bg (v (+ (* x world-size) y)) x y)))
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))

(def panel (doto (proxy [JPanel] []
		   (paint [g] (render g)))
	     (.setPreferredSize (new Dimension 
				     (* scale world-size) 
				     (* scale world-size)))))

(def frame (doto (new JFrame) (.add panel) .pack .show))

(defn animate []
  "Update the visual display"
  (. panel (repaint)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Setup Functions - these are for building the SIM

(defn create-homestead 
  "create an homestead at the location, returning an homestead agent on the location"
  [loc]
    (sync nil
      (let [p (place loc)
            stead (struct homestead (gen-max-fertility) (gen-friends num-friends))]
        (alter p assoc :homestead stead)
	loc))) 


(defn setup 
  "places initial homesteads, returns seq of homestead agents"
  []
  ;(setup-metrix)
  ;TODO: setup animation?
  ;populate the-world with homes that have a randome max fertility and appetite
  ;or
  ;clear the existing world and gen new cells
  (doseq [x (range world-size) y (range world-size)]
    (dosync
     (let [p (place [x y])
	   cell (struct home 0 (gen-appetite))]
       (ref-set p cell))))
    
    
    ; randomly seed world with homesteads, need to combine the dotimes & doall
    ; to have it randomly create homestead agents...
    (for [x (range initial-homesteads)]
      (create-homestead [(rand-int world-size) (rand-int world-size)])))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn not-stable?
  "check to see if homestead growth has stabilized, return true if it has"
  [tally]
  (if (< (count tally) 5)  ; make sure the tally has at least five entries
    true
    (let [n 50
	  n-avg (/ (reduce + (take n tally)) (first tally))]  ; calc the average of the last fifty entries
   ;   (println n n-avg)
      (if (= n n-avg)  ; if the average equals first entry, we are stable
	(do
	  (println "STABLE" (count tally))
	  false)
	true))))
	

(defn gogogo
  "Start executing the model in GUI mode"
  []
  (reset! homesteaders (setup))
  (setup-metrix)
  (println enlighten-threshold)
  (loop [i 0]
    (if running
      (do
	 (eat)			;execute the model dynamics
	;(println (count @homesteaders))
	(dorun (map grow (set @homesteaders)))	;execute agent actions, TODO: add shuffle
	(metrixiate (set @homesteaders) i)	;execute measurement functions
	(animate)			;Repaint the display
	(. Thread (sleep simulation-sleep-time))
	(recur (inc i))))))		;increment the steps by one and loop


(defn gogogo2
  "Start executing the model in GUI mode, stops running if sim is stable"
  []
  (reset! homesteaders (setup))
  (setup-metrix)
  (println enlighten-threshold)
  (loop [i 0
	 tally '(0)]
    (if (not-stable? tally)
      (do
	 (eat)			;execute the model dynamics
	;(println (count @homesteaders))
	(dorun (map grow (set @homesteaders)))	;execute agent actions, TODO: add shuffle
	(metrixiate (set @homesteaders) i)	;execute measurement functions
	(animate)			;Repaint the display
	(. Thread (sleep simulation-sleep-time))
	(recur (inc i) (cons (count (set @homesteaders)) tally))))))		;increment the steps by one and loop

   
(defn step
  "run a step of the simulation"
  []
  (do
    (eat)
    (dorun (map grow (set @homesteaders))))) ; there are duplicate homesteads, so I call a set operation


(defn run-til-stable
  "runs sim until a stable state is found or we reach max steps
   returns the stable number of homesteads or number at max"
  [max]
  (loop [i 0
	 tally '(0)]
    (if (and (< i max) (not-stable? tally))
      (do
	;(print "step " i " ")
	(step)
	(recur (inc i) (cons (count (set @homesteaders)) tally)))
      (count (set @homesteaders)))))
    
	
(defn repeat-sim
  "runs the sim n times, return a sequence of stable values"
  [n]
  (loop [i 0 
	 values nil]
    (if (< i n)
      (do
	(println "Repeating: " i values)
	(reset! homesteaders (setup))
	(recur (inc i) (cons (run-til-stable 300) values))) ; for high thresholds the sim takes forever to compute
      values)))


;;;;; Batch mode functions
(defn sweep-enlightenment
  "run the simulation sweeping the enlighten threshold
   starts at -100 and goes to -1. Returns an incanter dataset with results"
  [dataset]
  (loop [i -50
	 data nil]
    (println i)
    (if (> -1 i)
      (do 
	(reset! enlighten-threshold i)
	(println "sweeping: " enlighten-threshold data)
	(recur (inc i) (cons (repeat-sim 100) data)))
      (incanter.core/conj-cols dataset data))))

(comment
  (sweep (incanter.core/dataset '("step") (range 100)))
)

(defn sweep-fert
  "run the simulation sweeping the enlighten threshold
   starts at -100 and goes to -1. Returns an incanter dataset with results"
  [dataset]
  (loop [i 1
	 data nil]
    (println i)
    (if (< i 30)
      (do 
	(reset! max-fertility-lambda i)
	(println "sweeping: " max-fertility-lambda data)
	(recur (inc i) (cons (repeat-sim 100) data)))
      (incanter.core/conj-cols dataset data))))

(comment
  (sweep (incanter.core/dataset '("step") (range 1)))
)













;(println homesteaders)

	   
; filename (str baselocation "Run-threshold-sweep-" (string2/replace (str (Date.)) #"\s" "-") ".csv")
(comment
(defn batchrun
  "Executes a batch run of the sim, running function f after run of n steps"
  []
  (loop [n -50
	 data (incanter.core/dataset '("step") (range 10))
	 holder (atom nil)]        ; loop though varying enlighten-thresholds
      (loop [num 0
	     results nil]
	(if (< num 10)
	  (do
	    (reset! homesteaders (setup))
	    (loop [n 0
		 tally '(0)]
	      (if (and (< n 200) (not-stable? tally))
		(do
		  (eat)
		  (dorun (map grow @homesteaders))
		  (recur (inc n) (cons (count @homesteaders) tally)))))
	    (recur (inc num) (conj results (count @homesteaders))))
	  (reset! holder results))))	
     (recur (inc n) (incanter.core/conj-cols data results)))
)	
	  
