(ns prototurtle
  (:import (java.awt Color Graphics Graphics2D)
	   (java.awt.image BufferedImage)
	   (java.awt.event MouseListener)
	   (javax.swing JFrame JPanel)))

(defrecord Turtle [x y dir canvas panel])

(defprotocol PTurtle
  (move [this dist])
  (turn [this deg]))

(extend-protocol PTurtle Turtle
	(move [{:keys [x y dir canvas panel]} dist]
	      (let [dx (+ x (* dist (Math/cos (Math/toRadians dir))))
		    dy (+ y (* dist (Math/sin (Math/toRadians dir))))]
		(.drawLine canvas x y dx dy)
		(.repaint panel)
		(Turtle. dx dy dir canvas panel)))
	(turn [{:keys [x y dir canvas panel]} deg]
	      (Turtle. x y (+ dir deg) canvas panel)))

(defn turtle-motion [turtle reps motions]
  (-> (iterate #(reduce (fn [turtle [step angle]]
			  (-> turtle (move step) (turn angle))) % motions)
	       turtle)
       (nth reps)))

(defn render-scene [#^Graphics2D g #^BufferedImage bi]
  (. g (drawImage bi 0 0 nil)))

(defn figures [w h turtle gfx panel]
  (doseq [angle (range 180)]
    (turtle-motion turtle 4
		   [[90 (- angle)]
		    [30 (- angle)]
		    [60    angle]
		    [30    angle]
		    [60 (- angle)]])
    (Thread/sleep (if (or (= angle 90) (= angle 120)) 500 100))
    (doto gfx
      (.setColor Color/BLACK)
      (.fillRect 0 0 w h))
    (.repaint panel)
    (.setColor gfx Color/WHITE)))

(defn main [w h]
  (let [bi           (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
	gfx          (.getGraphics bi)
	scene        (atom gfx)
	panel        (proxy [JPanel] [] (paint [g] (render-scene g bi)))
	turtle       (Turtle. (/ w 2) (/ h 2) 0 gfx panel)]
    (doto (JFrame. "Turtles Playground")
      (.setSize w h) .pack .show (.add panel))
    (future (while true (.repaint panel) (Thread/sleep 50)))
    (.setColor gfx Color/WHITE)
    (figures w h turtle gfx panel)))
