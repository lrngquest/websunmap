(ns websunmap.sunmap)
;; Sun map from current time, daytime, nighttime maps.  Started 2017Jun02

;;NOTE Code with CamelCase names should be considered translation (or
;; paraphrase) of Java code from indicated packages/classes of "Sky View Cafe" 
;; which carry the following notice:
;;   Copyright (C) 2000-2007 by Kerry Shetline, kerry@shetline.com.
;;   This code is free for public use in any non-commercial application. All
;;   other uses are restricted without prior consent of the author, Kerry
;;   Shetline. The author assumes no liability for the suitability of this
;;   code in any application.
;;   2007 MAR 31   Initial release as Sky View Cafe 4.0.36.


;; from org.shetline.math.MathUtil
(defn cos_deg "" [deg] (Math/cos (* deg (/ Math/PI 180.0))))
(defn ifloor "" [x]     (int (Math/floor x)) )
(defn ^double interpolate "" [ x0 x x1 y0 y1]
   (if (= x0 x1)   y0   (+ y0 (/ (* (- x x0) (- y1 y0)) (- x1 x0)) ) )   )

(defn iround "" [x]     (int (Math/round (double x))) )
(defn rmod   "" [ x y]  (- x (* (Math/floor (/ x y)) y) )  )
(defn sin_deg "" [deg] (Math/sin (* deg (/ Math/PI 180.0))))


;; from org.shetline.math.Angle
(defn atan2_nonneg "" [ y x]  (rmod (Math/atan2 y x) (* Math/PI 2.0)))


;; from org.shetline.math.SphericalPosition3D
(defn convertRectangular "" [x y z]
  {:longitude (atan2_nonneg y x)
   :latitude  (Math/atan2 z (Math/sqrt (+(* x x) (* y y))))
   :radius    (Math/sqrt (+ (* x x) (* y y) (* z z))) } )  ;; SP3D as map

(def D2Rad (/ Math/PI 180.0))          (def R2Deg (/ 180.0 Math/PI))

(defn getLongitudeDeg "" [pos]  (* R2Deg (:longitude pos)) )
(defn getLatitudeDeg  "" [pos]  (* R2Deg (:latitude  pos)) )
(defn getRightAscensionDeg "" [pos]  (getLongitudeDeg pos) )
(defn getDeclinationDeg "" [pos]     (getLatitudeDeg  pos) )


;; from org.shetline.astronomy.Ecliptic
(def JD_J2000 2451545.0)

(defn limitNegOneToOne "" [x]
  (cond (< x (- -1 0.01))  -1.0  ;; tolerance::0.01
        (> x (+  1 0.01))   1.0
        (< x -1.0)         -1.0
        (> x  1.0)          1.0
        :else               x )  )

(defn getNutation  "subset -- MEAN_OBLIQUITY::1 only !"  [timeJDE mode]
  (let [T      (/ (- timeJDE JD_J2000) 36525.0)
        coeff  [-4680.93 -1.55 1999.25 -51.38 -249.67
                  -39.05  7.12   27.87   5.79    2.45]
        [e U]  (reduce  (fn [[ae aU] v]
                          [ (+ ae (/ (* v aU ) 3600.0) )   (* aU aU) ]   )
                        [ 23.43929111  (/ T 100.0) ]    coeff)             ]
    [0.0  0.0  (* D2Rad e)]  )   )

(defn eclipticToEquatorial ""  [pos timeJDE mode]
  (let [nutation  (getNutation timeJDE mode)
        L         (:longitude pos)  ;; aka RightAscension
        B         (:latitude  pos)  ;; aka Declination
        E         (nutation 2)  ]   ;; 2::OBLIQUITY
    {:longitude (atan2_nonneg (- (* (Math/sin L) (Math/cos E))
                                 (* (Math/tan B) (Math/sin E)))
                            (Math/cos L) )
     :latitude  (Math/asin
                 (limitNegOneToOne (+ (* (Math/sin B) (Math/cos E))
                                      (* (Math/cos B) (Math/sin E) (Math/sin L))
                                      ) ) )
     :radius 0.0 }  )   )  ;; thus a SphericalPosition3D


;; from org.shetline.astronomy.SolarSystem
(defn getGreenwichMeanSiderealTime  ""  [ timeJDU]
  (let [t (- timeJDU JD_J2000)   T (/ t 36525)   T2 (* T T)   T3 (* T2 T)]
    (rmod
     (- (+ 280.46061837 (* 360.98564736629 t) (* 0.000387933 T2))
        (/ T3 38710000.0))
     360.0)  )    )


(defn getEclipticPosition
  "subset -- case planet:SUN flags:QUICK_SUN only! EV"  [planet timeJDE flags]
  (let [T   (/ (- timeJDE JD_J2000) 36525.0)
        T2  (* T T)
        e   (- 0.016708634 (* 0.000042037 T) (* 0.0000001267 T2) )
        L0  (+ 280.46646 (* 36000.76983 T) (* 0.0003032 T2) )
        M   (+ 357.52911 (* 35999.05029 T) (* -0.0001537 T2) )
        C   (+ (* (- 1.914602 (* 0.004817 T) (* 0.000014 T2)) (sin_deg M) )
               (* (- 0.019993 (* 0.000101 T))             (sin_deg (* 2.0 M)) )
               (* 0.000289  (sin_deg (* 3.0 M)) )   )
        L   (rmod (+ L0 C) 360.0)
        R   (/ (* 1.000001018 (- 1.0 (* e e)))
               (+ 1.0 (* e (cos_deg (+ M C)))) )  ]
    {:longitude (* D2Rad L)  :latitude 0.0  :radius R} )   ) ;; thus ret SP3D

(defn getEquatorialPosition "only MEAN_OBLIQUITY::1" [planet timeJDE obs flags]
  (let [eclipticPos  (getEclipticPosition planet timeJDE  flags)  ]
    (eclipticToEquatorial eclipticPos timeJDE 1) )  )


;; from org.shetline.astronomy.UTConvertor
(defn hDT "ala historicDeltaT" [y]
  (let [iy   (int y)     yx   (if (> iy 2014) 0  (- iy 2000))  ]
    ([63.82 64.09 64.30 64.47 64.57 64.68 64.84  65 65 66 66 66 67 69 71] yx)) )

(defn getDeltaTatJulianDate "table 2000..2014  calc to 2100" [ timeJDE]
  (let [year  (+ (/ (- timeJDE JD_J2000) 365.25) 2000.0)
        t     (/ (- year 2000) 100.0)
        dta   (+ 102.0 (* 102.0 t) (* 25.3 t t))
        dt3   (if (and (> year 2014) (< year 2100)) ;;off-end of table
                (+ dta (* 0.5161 (- year 2100.0)) )
                (hDT year) )  ]
   dt3)  )

(defn UT_to_TDB "" [timeJDU]
  (reduce
   (fn [timeJDE v]
     (+ timeJDU (/ (getDeltaTatJulianDate timeJDE) 86400.0))   )
   timeJDU  (range 5) )    )


(defn get-jd "convert date-time to julian (arity-overloaded)"
  ([v]
     (let [[y m d h mn s]  v]  (get-jd  y m d  h mn s)) )
  
  ([y m d h mn s]  ;; from J.Meeus via  T. Alonso Albi - OAN (Spain)
     (let [[Y M]  (if (< m 3)   [(dec y) (+ m 12)]   [y m] )
           A      (int (/ Y 100))
           B      (+ (- 2 A)  (/ A 4))
           dayFraction  (/ (+ h (/ (+ mn (/ s 60.0)) 60.0)) 24.0)
           jd     (+ dayFraction
                     (int (* 365.25 (+ Y 4716)))
                     (int (* 30.6001 (+ M 1)))
                     d  B  -1524.5)    ]
    jd) )    )     ;;Assert jd not in 2299150.0..2299160.0 !


;;from org.shetline.skyviewcafe.MapView  and ShadowedMap
(def imageWidth  800)        (def halfImageWidth  (/ imageWidth 2))
(def imageHeight 400)        (def equator (/ imageHeight 2)) ;;halfImageHeight
(def SUN 0)                  (def DAYLIGHT_EXPAND 1.0) ;;degrees
(def r  (cos_deg DAYLIGHT_EXPAND))     (def s_d_DE  (sin_deg DAYLIGHT_EXPAND))


(defn longit_to_x "" [ longit]
  (rmod (+ halfImageWidth (* (/ longit 360.0) imageWidth) ) imageWidth)  )


(defn ^double interpolateLongitude "" [ x0 x x1 y0 y1]
  (let [y1a (cond
             (< y1 (- y0 halfImageWidth))  (+ y1 imageWidth)
             (> y1 (+ y0 halfImageWidth))  (- y1 imageWidth)
             :else                         y1)   ] 
    (rmod (interpolate x0 x x1 y0 y1a)  imageWidth) )  )


(def poles-lit (atom {}) )     (def flL (atom {}) )
(def cols (atom {:dayStart  (make-array Double/TYPE   imageHeight)
                 :dayEnd    (make-array Double/TYPE   imageHeight)
                 :daySpan   (make-array Double/TYPE   imageHeight)
                 :iDayStart (make-array Integer/TYPE imageHeight)
                 :iDayEnd   (make-array Integer/TYPE imageHeight)  }) )

(defn m1asgn "for cols" [ a msinLat cosLat x_adj z_adj sunLon]
  (let [sindeg_a  (sin_deg a)
        cosdeg_a  (cos_deg a)
        x         (+ (* msinLat sindeg_a r) x_adj)
        y         (* cosdeg_a r)
        z         (+ (* cosLat sindeg_a r) z_adj)
        pos       (convertRectangular x y z)
        L         (getLongitudeDeg pos)        ;; aka daySpan below
        B         (getLatitudeDeg pos)
        yi  (max (min  (- equator (iround (* (/ B 180.0) imageHeight)))
                       (- imageHeight 1))
                 0)
        {:keys [dayStart dayEnd daySpan iDayStart iDayEnd]}  @cols
        daySt  (longit_to_x (- sunLon L))
        dayEn  (longit_to_x (+ sunLon L))    ]

    (when (> (Math/abs (double L)) 0.1)
      (aset dayStart  yi (double daySt ))
      (aset dayEnd    yi (double dayEn ))
      (aset daySpan   yi (double L))
      (aset iDayStart yi (ifloor daySt))
      (aset iDayEnd   yi (ifloor dayEn))  ) )   )


(defn m1A "using column-arrays" [timeJDU]
  (let [timeJDE (UT_to_TDB timeJDU)     ; from UTConvertor
        pos0    (getEquatorialPosition SUN timeJDE 0 32) ;; 32==> QUICK_SUN
        sdrlTm  (getGreenwichMeanSiderealTime timeJDU)
        sunLon  (- (getRightAscensionDeg pos0) sdrlTm)
        sunLat  (getDeclinationDeg pos0)
        cosLat  (cos_deg sunLat)
        x_adj   (* s_d_DE (- 0 cosLat))
        msinLat (- 0 (sin_deg sunLat) )
        z_adj   (* s_d_DE msinLat)  ]
    (doseq [i (range imageHeight)]  (aset (:iDayStart @cols) i -1) )
    (doseq [v (range -90 91)]  (m1asgn v msinLat cosLat x_adj z_adj sunLon) )   
    )  )


(defn m2 "ala orignal column-arrays" []
  (reset! flL {:firstLat -1  :lastLat -1})
  (let [{:keys [dayStart dayEnd daySpan iDayStart iDayEnd]}  @cols ]
    
    (doseq [i (range imageHeight)]
      (when (>= (aget ^ints iDayStart i) 0) ;;i.e. valid "row"
        
        (when (< (:firstLat @flL)0)  (swap! flL assoc :firstLat i)) ;;only once

        (let [{:keys [ lastLat]}  @flL]
          (when (and (> lastLat 0)  (> i (inc lastLat)))
            (doseq [j (range (inc lastLat) i)]
              (aset dayStart j
                    (interpolateLongitude   lastLat j i
                  (aget ^doubles dayStart lastLat) (aget ^doubles dayStart i)))
              (aset dayEnd   j
                    (interpolateLongitude  lastLat j i
                  (aget ^doubles dayEnd lastLat) (aget ^doubles dayEnd i) ))
              (aset daySpan  j
                    (interpolate  lastLat j i
                  (aget ^doubles daySpan lastLat) (aget ^doubles daySpan i) ))
              (aset iDayStart j (ifloor (aget ^doubles dayStart j)))
              (aset iDayEnd   j (ifloor (aget ^doubles dayEnd   j))) ) ;;doseq j
            ) ;; when and
          
          (swap! flL assoc :lastLat i) )  )  ) ;;let  ;;when >=    ;;doseq i
    
    (let [{:keys [firstLat lastLat]}  @flL]
      (reset! poles-lit {
              :north-pole-lit (> (aget ^doubles daySpan firstLat)
                                 (aget ^doubles daySpan (inc firstLat)) )
              :south-pole-lit (> (aget ^doubles daySpan lastLat)
                                 (aget ^doubles daySpan (dec lastLat))  ) } )
       ) )  )

      
(defn form-raster-v "" [ y]
  (let [{:keys [ iDayStart iDayEnd]}  @cols
        {:keys [north-pole-lit south-pole-lit]} @poles-lit
        daySt  (aget ^ints iDayStart y)    dayEn  (aget ^ints iDayEnd y)    ]
    (if (>= daySt 0)
      (if (< daySt dayEn)
        [y 0 (inc daySt) y dayEn (- imageWidth  dayEn) ]  ;; 2 night seg
        [                y dayEn (- (inc daySt) dayEn) ]) ;; 1
      (if (or (and (not north-pole-lit) (< y equator))
              (and (not south-pole-lit) (> y equator)) )
        [y 0 imageWidth] ;; full-width night line  else   full day line
        [] ) ) )   )


(defn get-night-v "for painting night rows" [timeJD]
  (m1A timeJD)
  (m2)
  (reduce (fn [a y] (conj a  (form-raster-v y)) ) [] (range imageHeight))  )
