(ns sheepy-magna
  (:require clojure.contrib.math)
  (:require clojure.contrib.duck-streams)
  (:require clojure.contrib.str-utils))


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



    (comment
      (do
	(def homesteaders (setup))
	(send-off animator animation)
	(dorun (map #(send-off % grow) homesteaders))
	(send-off the-hunger eating)
	(send-off metrix output-pantry)
      )
      (map clear-agent-errors homesteaders)
      )




;dimensions of square world
(def world-size 10)

;scale factor for food drawing
(def food-scale 1000)

; number of inital homesteads
(def initial-homesteads 3)

;Cells will have a random possibility
(def fertility-factor-max 10)

; parameter for how many friends someone has in their social network
(def num-friends 5)

;default stubbornness TODO: make this a function
(def stubbornness-factor 1)

; default growth rate for homesteads
(def growth-rate 50)

;initial food value
(def initial-food 0)

; these determine how long the agents will sleep before cycling through their actions again
(def animation-sleep-ms 100)
(def homestead-sleep-ms 100) 
(def the-hunger-sleep-ms 100) 
(def output-sleep-ms 100)


(def running true)

;a data structure for each of the cells in the world
(defstruct home :fertility :food)


(defstruct homestead :friends )

; functino to generate the random social networks
(defn gen-friends 
"Generates n vectors of integer pairs based on world-size"
[n]
(take n (map #(apply vector %) (partition 2 (repeatedly #(rand-int world-size))))))


;create The Hunger, an agent that decrements the amount of food in every home in the world 
; not sure if this will work....
(def the-hunger (agent nil))

;world is a 2d vector of refs to cells
(def world 
     (apply vector 
            (map (fn [_] 
                   (apply vector (map (fn [_] (ref (struct home fertility-factor-max initial-food))) 
                                      (range world-size)))) 
                 (range world-size))))

(defn place [[x y]]
  (-> world (nth x) (nth y)))


(defn generate-social-network 
  "Function will return n x,y locations based from world-size"
  [num]
  )


(defn create-homestead 
  "create an homestead at the location, returning an homestead agent on the location"
  [loc]
    (sync nil
      (let [p (place loc)
            a (struct homestead (gen-friends num-friends))]
        (alter p assoc :homestead a)
	(agent loc)))) 


(defn setup 
  "places initial homesteads, returns seq of homestead agents"
  []
  (sync nil
    ; randomly seed world with homesteads, need to combine the dotimes & doall
    ; to have it randomly create homestead agents...
    (for [x (range initial-homesteads)]
      (create-homestead [(rand-int world-size) (rand-int world-size)])))) 


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





;agent functions, todo: consolidate into a single function, there is no need for sub functions



(defn transfer-food 
  "this function increments the food at place p"
  [[dloc sloc]]
  (dosync
   (let [dest (place dloc)
	 source (place sloc)]
     (alter source assoc :food (dec (:food @source)))
     (alter dest assoc :food (inc (:food @dest)))))
  sloc)

(defn grab-food
  "An agent function that executes the transfer food function if the
   model is running and source still has food."
  [[dloc sloc]]
  (let [source (place sloc)]
    (. Thread (sleep homestead-sleep-ms))
    (dosync
     (when (and running (< 0 (:food @source)))
       (send-off *agent* #'grab-food))
     (transfer-food [dloc sloc])
     [dloc sloc])))

(defn grow
  "Have the homesteader grow some food at their home."
  [loc]
  (let  [p (place loc)
	 social-network (:friends (:homestead @p))
	 geo-network (fetch-neighbors loc)
	 food (:food @p)]
    (. Thread (sleep homestead-sleep-ms))
    (dosync
     (when running
       (send-off *agent* #'grow))
     (alter p assoc :food (+ growth-rate (:food @p)))
     ; its strange to do this with agents, but I was banging my head
     ; until I realized this and my other methods were lazy eval
     ; but this is good b/c the enlightenment will work this way too
     (dorun (map #(send-off % grab-food) (map #(agent [% loc]) geo-network)))
     loc)))


(defn generate-recipients
  "Given a list of locations, sort those locations based upon food needs."
  [locations]
  (let [places (map #(place locations))]
    (sort-by #(:food (deref %)) places)))


  
; an "enlighten" function, this takes a list of locations ( geographic neighbors 
; or agent's social network) and creates new agents at these locations. 
; This also updates the main list of agents in case we need
; to access them at the REPL
; 


  (defn the-hunger-eat
    "This is the main action function for the hunger agent"
    []
    (dorun
     (for [x (range world-size) y (range world-size)]
       (dosync
	(let [p (place [x y])]
	  (alter p assoc :food (dec (:food @p)))))))) 

(defn eating [x]
    (when running
      (send-off *agent* #'eating))
    (the-hunger-eat)
    (. Thread (sleep the-hunger-sleep-ms))
    nil)



(def metrix (agent "/Users/mcburton/cscs/final-results/50-growth.txt"))

(defn output-pantry 
  "Spits out the state of the world to a file."
  [file]
  (when running
    (send-off *agent* #'output-pantry))
  (clojure.contrib.duck-streams/append-spit file (str (homesteads-state) "\n"))
  (. Thread (sleep output-sleep-ms))
  file)

(defn homesteads-state
  "returns a data structure containing the state of the homesteads & their neighbors."
  []
  (let [results (map #(:food (deref (place (deref %)))) homesteaders)]
    (str-join \, (map str results))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (import 
     '(java.awt Color Graphics Dimension)
     '(java.awt.image BufferedImage)
     '(javax.swing JPanel JFrame))

    ;pixels per world cell
    (def scale 50)
   


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

    (def animator (agent nil))

    (defn animation [x]
      (when running
	(send-off *agent* #'animation))
      (. panel (repaint))
      (. Thread (sleep animation-sleep-ms))
      nil)

