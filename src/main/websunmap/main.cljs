(ns websunmap.main
  (:require  [websunmap.sunmap :as sm]
             [goog.object :as gobj]
             (goog.string.format) )    )

(enable-console-print!)

(def app-state (atom { :night-v []}) )
(def wd 800)  (def ht 400)

(defn locc "calc loc" [x y] (* 4 (+ x (* y wd)) ))

(defn copyLineSeg "copy partial row of pixels src->dest" [dest y src x0 w]
  ;; note 1 pixel (x) ==> 4 items: R G B A   " processing" :: 1 iter-per-pixel
  (doseq [x (range x0 (+ x0 w))]
      (aset dest (locc x y)  (aget src (locc x y) ))
      (aset dest (+ 1 (locc x y)) (aget src (+ 1 (locc x y))))
      (aset dest (+ 2 (locc x y)) (aget src (+ 2 (locc x y))))
      (aset dest (+ 3 (locc x y)) (aget src (+ 3 (locc x y))))    )  )

(defn cpy1 "" [[y xa wa] dest  srcni]  (copyLineSeg dest y  srcni  xa wa ) )

(defn cpy2 "" [[y xa wa  y xb wb] dest  srcni]
  (copyLineSeg dest y  srcni  xa wa) (copyLineSeg dest y  srcni  xb wb) )


(defn nowAsV "UTC time as a vector" []
  (let[now  (js/Date.)  ]
    [(. now getUTCFullYear) (inc (. now getUTCMonth))  (. now getUTCDate)
     (. now getUTCHours)    (. now getUTCMinutes)    (. now getUTCSeconds) ]
    )  )

(def secs-per-day 86400)    (def maxcnt 150)   (def ntrvl 10000) ;;in msec

(defn init-state "" []
  (let [now  (nowAsV )  j-d  (sm/get-jd now)  ]
    (swap! app-state assoc :d-t now :tJD j-d :countdown maxcnt
           :night-v (sm/get-night-v j-d)) ) )

(defn update-state "two-phase timer" []
  (let [{:keys [:d-t :tJD :countdown :night-v]} @app-state
        now-d-t     (nowAsV )
        now-JD      (sm/get-jd  now-d-t)
        nxt-cnt     (->> (- now-JD tJD) ( * secs-per-day  ) (- maxcnt  ) )
        nxt-app-st  (if (<= nxt-cnt 2) ;;was 0
 ;; then update  timestamp countdown _and_ image
                      {:d-t now-d-t   :tJD now-JD   :countdown maxcnt
                       :night-v (sm/get-night-v now-JD)}
 ;; else  update _only_ countdown
                      (assoc @app-state :countdown nxt-cnt) )          ]
    (reset! app-state nxt-app-st)    )  )


(defn draw-msg "" [ctx wd ht fmtdstr]
  (set! (. ctx -fillStyle) "red")
  (set! (. ctx -font) "16px monospace")
  (.fillText ctx fmtdstr (- wd 280) (- ht 20) )   )


(defn paint-only "" [ ]
  (let [ctx     (.getContext (.getElementById js/document "myCanvas") "2d")
        _       (.drawImage ctx (.getElementById js/document "dayImg") 0 0)
        imgOst  (.getImageData ctx 0 0  wd ht) ;; object
        imgOdat (gobj/get imgOst "data")  ;; raw array inside object
        
        ctxNit  (.getContext  (.getElementById js/document "cvsNit") "2d")
        imgNit  (.getElementById js/document "nightImg")
        _       (.drawImage ctxNit imgNit 0 0)
        imgNDd  (.getImageData ctxNit 0 0  wd ht)
        imgNDat (gobj/get imgNDd "data")

        _       (update-state )
        night-v (:night-v @app-state )
        [y m d h mt s]    (:d-t @app-state)
        countdown         (:countdown @app-state)        ]
    
    (doseq [y  (range ht)]
      (case (count (night-v y))
        0  0 ;; no-op
        3  (cpy1 (night-v y) imgOdat  imgNDat)        
        6  (cpy2 (night-v y) imgOdat  imgNDat)  )      )   
    
    (.putImageData ctx imgOst 0 0)  ;;s/imgOdat/imgOst/ !

    (->> (goog.string.format "UTC %4d-%02d-%02d %2d:%02d:%02d  %3d"
                             y m d h mt s  countdown)
         (draw-msg ctx wd ht ))       )  )


(defn main! "" []
  (println "main")
  (init-state )
  (paint-only)  (js/setInterval paint-only ntrvl)  )

(defn reload! "" []  (println "reloaded")
  (js/setInterval paint-only ntrvl)  )
