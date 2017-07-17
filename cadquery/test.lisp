;;;; test.lisp

(in-package #:cadquery)

(defun test0 ()
  (cadquery (list (show> (.> (workplane :plane "front") (box 5 3 2) (faces "+Z") (shell 0.05))))))

(defun test2 ()
  (cadquery (list (show> (.> (workplane :plane "XY") (box 80 60 10) (faces ">Z") (workplane) (hole :dia 22))))))

(defun test3 ()
  (let* ((length 80.0)
	 (height 60.0)
	 (thickness 10.0)
	 (center-hole-dia 22.0)
	 (cbore-hole-diameter 2.4)
	 (cbore-diameter 4.4)
	 (cbore-depth 2.1)
	 (box1 (box length height thickness))
	 (hole1 (hole :dia center-hole-dia))
	 (rect1 (rect (- length 8.0) (- height 8.0) :for-construction t))
	 (cboreHole1 (cbore-hole :hole-dia cbore-hole-diameter :cbore-dia cbore-diameter :cbore-dep cbore-depth)))
    (cadquery (list (show> (.> (workplane :plane "XY") box1 (faces ">Z") (workplane) hole1 (faces ">Z") (workplane) rect1 (vertices) cboreHole1))))))

(defun test4 ()
  (let* ((circle-radius 50.0)
          (rectangle-width 13.0)
          (rectangle-length 19.0)
          (thickness 13.0))
    (cadquery (list (show> (.> (workplane :plane "front") (circle :r circle-radius) (rect rectangle-width rectangle-length) (extrude thickness)))))))

(defun test5 ()
  (let* ((width 2.0)
          (thickness 0.25))
    (cadquery (list (show> (.> (workplane :plane "front") (line-to width 0) (line-to width 1) (three-point-arc :p0 '(1.0 1.5) :p1 '(0.0 1.0)) (close>) (extrude thickness)))))))

(defun test6 ()
  (let* ((circle-radius 3.0)
          (thickness 0.25))
    (cadquery (list (show> (.> (workplane :plane "front") (circle :r circle-radius) (rect 0.5 0.5 :center '(1.5 0.0)) (circle :r 0.25 :center '(-1.5 1.5)) (extrude thickness)))))))

(defun test7 ()
  (let* ((plate-radius 2.0)
          (hole-pattern-radius 0.25)
          (thickness 0.125))
    (cadquery (list (show> (.> (workplane :plane "front") (circle :r plate-radius) (push-points :points '((1.5 0) (0 1.5) (-1.5 0) (0 -1.5))) (circle :r hole-pattern-radius) (extrude thickness)))))))

(defun test8 ()
  (let* ((width 3.1)
          (height 4.0)
          (thickness 0.25)
          (polygon_sides 6)
          (polygon_dia 1.0)
          (box0 (box width height thickness))
          (sketch0 (.> (push-points :points '((0 0.75) (0 -0.75))) (polygon :sides polygon_sides :dia polygon_dia))))
    (cadquery (list (show> (.> (workplane :plane "front") box0 sketch0 (cut-thru-all)))))))

(defun test9 ()
  (let* ((l 100) (h 20) (w 20) (t0 1)
	  (points (list (list 0 (/ H 2))
		    (list (/ W 2) (/ H 2))
		    (list (/ W 2) (- (/ H 2) t0))
		    (list (/ t0 2) (- (/ H 2) t0))
		    (list (/ t0 2) (- t0 (/ H 2)))
		    (list (/ W 2) (- t0 (/ H 2)))
		    (list (/ W 2) (/ H -2.0))
		    (list 0 (/ H -2.0))))
          (sketch0 (.> (polyline :points points) (mirror :dir "Y"))))
    (cadquery (list (show> (.> (workplane :plane "front") sketch0 (extrude l)))))))

;;spline curves
(defun test10 ()
  (let* ((points '((2.75 1.5) (2.5 1.75) (2.0 1.5) (1.5 1.0) (1.0 1.25) (0.5 1.0) (0 1.0)))
          (sketch0 (.> (line-to 3 0) (line-to 3 1) (spline :points points) (close>))))
    (cadquery (list (show> (.> (workplane :plane "XY") sketch0 (extrude 0.5)))))))

(defun test11 ()
  (let* ((sketch0 (.> (hline 1) (vline 0.5) (hline -0.25) (vline -0.25) (hline-to 0) (mirror :dir "Y"))))
    (cadquery (list (show> (.> (workplane :plane "front") sketch0 (extrude 0.25)))))))

(defun test12 ()
  (let* ((box0 (box 2 3 0.5)))
    (cadquery (list (show> (.> (workplane :plane "front") box0 (faces ">Z") (workplane) (hole :dia 0.5)))))))

(defun test13 ()
  (let* ((box0 (box 3 2 0.5)))
    (cadquery (list (show> (.> (workplane :plane "front") box0 (faces ">Z") (vertices :dir "<XY") (workplane) (circle :r 1) (cut-thru-all))))))) ;;"<XY" only one corner is cut

(defun test14 ()
  (let* ((box0 (box 3 2 0.5)))
    (cadquery (list (show> (.> (workplane :plane "front") box0 (faces "<X") (workplane :offset 0.75) (circle :r 1) (extrude 0.5)))))))

(defun test15 ()
  (let* ((box0 (box 4 4 0.25))
	  (sketch0 (.> (transform :offset '(0 -1.5 1.0) :rotate '(60 0 0)) (rect 1.5 1.5 :for-construction t))))
    (cadquery (list (show> (.> (workplane :plane "front") box0 (faces ">Z") (workplane) sketch0 (vertices) (hole :dia 0.25)))))))

(defun test16 ()
  (let* ((box0 (box 2 2 0.5)))
    (cadquery (list (show> (.> (workplane :plane "front") box0 (faces ">Z") (workplane) (rect 1.5 1.5 :for-construction t) (vertices) (hole :dia 0.125)))))))

(defun test17 ()
  (let* ((box0 (box 2 2 2)))
    (cadquery (list (show> (.> (workplane :plane "front") box0 (faces "+Z") (shell 0.05)))))))

(defun test18 ()
  (let ((box0 (box 4 4 0.25)))
    (cadquery (list (show> (.> (workplane :plane "front") box0 (faces ">Z") (circle :r 1.5) (workplane :offset 3.0) (rect 0.75 0.5) (loft :combine t)))))))

(defun test19 ()
  (let ((box0 (box 4 2 0.5)))
    (cadquery (list (show> (.> (workplane :plane "XY") box0 (faces ">Z") (workplane) (rect 3.5 1.5 :for-construction t) (vertices) (csk-hole :dia 0.125 :csk-dia 0.25 :csk-angle 82.0 :depth "None")))))))

(defun test20 ()
  (let ((box0 (box 3 3 0.5)))
    (cadquery (list (show> (.> (workplane :plane "XY") box0 (edges "|Z") (fillet 0.125)))))))

(defun test21 ()
  (let ((box0 (box 1 1 1)))
    (cadquery (list (show> (.> (workplane :plane "XY") box0 (faces ">Z") (workplane) (circle :r 0.25) (cut-thru-all) (faces ">Y") (workplane :offset -0.5) (split :keep-top t)))))))

;;bottle
(defun test22 ()
  (let* ((l 20) (w 6) (t0 3) ;;length, width, and thickness
	 ;;Draw half the profile of the bottle and extrude it
	 (p (.> (center (/ l -2) 0) (vline (/ w 2)) (three-point-arc :p0 (list (/ l 2) (+ (/ w 2) t0)) :p1 (list l (/ w 2))) (vline (/ w -2)) (mirror :dir "X") (extrude 30 :a "True")))
	 ;;Make the neck
	 (neck (.> (faces ">Z") (workplane) (circle :r 3) (extrude 2 :a "True")))
	 ;;Make a shell
	 (shell (.> (faces ">Z") (shell 0.3))))
    (cadquery (list (show> (.> (workplane :plane "XY") p neck shell))))))

(defun test23 ()
  (let* ((p_outerWidth 100) ;; Outer width of box enclosure
	  (p_outerLength 150) ;; Outer length of box enclosure
	  (p_outerHeight 50) ;; Outer height of box enclosure
	  (p_thickness 3) ;; Thickness of the box walls
	  (p_sideRadius 10) ;; Radius for the curves around the sides of the bo
	  (p_topAndBottomRadius 2) ;; Radius for the curves on the top and bottom edges
	  (p_screwpostInset 12) ;; How far in from the edges the screwposts should be
	  (p_screwpostID 4) ;; Inner diameter of the screwpost holes, should be roughly screw diameter not including threads
	  (p_screwpostOD 10) ;; Outer diameter of the screwposts. Determines overall thickness of the posts
	  (p_boreDiameter 8) ;; Diameter of the counterbore hole, if any
	  (p_boreDepth 1) ;; Depth of the counterbore hole, if
	  (p_countersinkDiameter 0) ;; Outer diameter of countersink. Should roughly match the outer diameter of the screw head
	  (p_countersinkAngle 90) ;; Countersink angle (complete angle between opposite sides, not from center to one side)
	  (p_lipHeight 1) ;; Height of lip on the underside of the lid. Sits inside the box body for a snug fit.
	  (oshell (.> (workplane :plane "XY") (rect p_outerWidth p_outerLength) (extrude (+ p_outerHeight p_lipHeight)))))
    ;;Weird geometry happens if we make the fillets in the wrong order
    (if (> p_sideRadius p_topAndBottomRadius)
      (setf oshell (.> oshell (edges "|Z") (fillet p_sideRadius) (edges "#Z") (fillet p_topAndBottomRadius)))
      (setf oshell (.> oshell (edges "#Z") (fillet p_topAndBottomRadius) (edges "|Z") (fillet p_sideRadius))))
    ;;Inner shell
    (let* ((ishell (.> oshell (faces "<Z") (workplane :offset p_thickness :a "True") (rect (- p_outerWidth (* 2 p_thickness)) (- p_outerLength (* 2 p_thickness)))
		     (extrude (- p_outerHeight (* 2 p_thickness)) :a "False") ;;Set combine false to produce just the new boss
		     (edges "|Z") (fillet (- p_sideRadius p_thickness))))
	    ;;Make the box outer box
	    (box1 (.> oshell (cut ishell)))
	    ;;Make the screwposts
	    (postwidth (- p_outerWidth (* 2.0 p_screwpostInset)))
	    (postlength (- p_outerLength (* 2.0 p_screwpostInset)))
	    (postCenters (.> box1 (faces ">Z") (workplane :offset (- p_thickness)) (rect postwidth postlength :for-construction t) (vertices) (circle :r (/ p_screwpostOD 2)) (circle :r (/ p_screwpostID 2)) (extrude (- (- (+ p_outerHeight p_lipHeight) (* 2 p_thickness))) :a "True")))
	    ;;Split lid into top and bottom parts (replace box1 with postCenters compared with py code)
	    (lid (.> postCenters (faces ">Z") (workplane :offset (- (- p_thickness) p_lipHeight)) (split :keep-top t)))
	    (bottom (.> postCenters (faces ">Z") (workplane :offset (- (- p_thickness) p_lipHeight)) (split :keep-bottom t)))
	    ;;Translate the lid, and subtract the bottom from it to produce the lid inset
	    (lowerLid (.> lid (translate 0 0 (- p_lipHeight))))
	    (cutlip (.> lowerLid (cut bottom) (translate (+ p_outerWidth p_thickness) 0 (+ (- p_thickness p_outerHeight) p_lipHeight))))
	    ;;Compute centers for counterbore/countersink or counterbore
	    (topOfLidCenters (.> cutlip (faces ">Z") (workplane) (rect postwidth postlength :for-construction t) (vertices)))
	    ;;Add holes of the desired type
	    (topOfLid ""))
      (cond
	((and (> p_boreDiameter 0) (> p_boreDepth 0))
	  (setf topOfLid (.> topOfLidCenters (cbore-hole :hole-dia p_screwpostID :cbore-dia p_boreDiameter :cbore-dep p_boreDepth :a (* 2 p_thickness)))))
	((and (> p_countersinkDiameter 0) (> p_countersinkAngle 0))
	  (setf topOfLid (.> topOfLidCenters (csk-hole :dia p_screwpostID :csk-dia p_countersinkDiameter :csk-angle p_countersinkAngle :depth (* 2 p_thickness)))))
	(t
	  (setf topOfLid (.> topOfLidCenters (hole :dia p_screwpostID :depth (* 2 p_thickness))))))
      (cadquery (list (show> (.> topOfLid (combine-solids bottom)))))
      )))

(defun test24 ()
  (let* ((new-doc "newDoc")
	  (initial-box "initialBox")
	  (cq-box "cqBox")
	  (new-thing "newThing")
	  (next-shape "nextShape")
	  (next-shape-shape (.> next-shape "Shape")))
    (cadquery
      (list
	(setf> new-doc (new-document))
	(setf> initial-box (add-object new-doc "Part::Box" "initialBox"))
	(recompute new-doc)
	(setf> cq-box (cq (solid (.> initial-box "Shape"))))
	(setf> new-thing (.> cq-box (faces ">Z") (workplane) (circle :r 0.5) (extrude 0.25)))
	(setf> next-shape (add-object new-doc "Part::Feature" "nextShape"))
	(setf> next-shape-shape (.> (val new-thing) "wrapped"))
	(recompute new-doc))
      :init (list (import> "cadquery") (import> "FreeCAD")))))

(defun test25 (&optional (switch 0))
  (let* ((rectangle_width 10)
	  (rectangle_length 10)
	  (angle_degrees 320)
	  (result "result"))
    (cadquery
      (list
	(cond
	  ((= switch 0)
	    (setf> result (.> (workplane :plane "XY") (rect rectangle_width rectangle_length :a "False") (revolve))))
	  ((= switch 1)
	    (setf> result (.> (workplane :plane "XY") (rect rectangle_width rectangle_length :a "False") (revolve angle_degrees))))
	  ((= switch 2)
	    (setf> result (.> (workplane :plane "XY") (rect rectangle_width rectangle_length) (revolve angle_degrees '(-5 -5)))))
	  ((= switch 3)
	    (setf> result (.> (workplane :plane "XY") (rect rectangle_width rectangle_length) (revolve angle_degrees '(-5 -5) '(-5 5)))))
	  ((= switch 4)
	    (setf> result (.> (workplane :plane "XY") (rect rectangle_width rectangle_length) (revolve angle_degrees '(-5 -5) '(-5 5) "False"))))
	  ((= switch 5)
	    (setf> result (.> (workplane :plane "XY") (rect rectangle_width rectangle_length :a "True") (revolve angle_degrees '(20 0) '(20 10)))))
	  (t
	    (setf> result (.> (workplane :plane "XY") (rect rectangle_width rectangle_length :a "False") (revolve)))))
	(show> result)))))

(defun test26 ()
  (let* ((lbumps 1) ;; number of bumps long
	  (wbumps 1) ;; number of bumps wide
	  (thin t) ;; True for thin, False for thick
	  (pitch 8.0)
	  (clearance 0.1)
	  (bumpDiam 4.8)
	  (bumpHeight 1.8)
	  (t0 (/ (- pitch (* 2 clearance) bumpDiam) 2))
	  (postDiam (- pitch t0))  ;; works out to 6.5
	  (total_length (- (* lbumps pitch) (* 2 clearance)))
	  (total_width (- (* wbumps pitch) (* 2 clearance)))
	  (height (gensym))
	  (s "s")
	  (tmp "tmp")
	  )
    (if thin
      (setf height 3.2)
      (setf height 9.6))
    (cadquery
      (list
	;;make the base
	(setf> s (.> (workplane :plane "XY") (box total_length total_width height)))
	;;shell inwards not outwards
	(setf> s (.> s (faces "<Z") (shell (* -1.0 t0))))
	;;make the bumps on the top
	(setf> s (.> s (faces ">Z") (workplane) (rarray pitch pitch lbumps wbumps :center "True") (circle :r (/ bumpDiam 2)) (extrude bumpHeight)))
	;; add posts on the bottom. posts are different diameter depending on geometry
	;; solid studs for 1 bump, tubes for multiple, none for 1x1
	(setf> tmp (.> s (faces "<Z") (workplane :invert t)))
	(cond
	  ((and (> lbumps 1) (> wbumps 1))
	    (setf> tmp (.> tmp (rarray pitch pitch (- lbumps 1) (- wbumps 1) :center "True") (circle :r (/ postDiam 2)) (circle :r (/ bumpDiam 2)) (extrude (- height t0)))))
	  ((> lbumps 1)
	    (setf> tmp (.> tmp (rarray pitch pitch (- lbumps 1) 1 :center "True") (circle :r t0) (extrude (- height t0)))))
	  ((> wbumps 1)
	    (setf> tmp (.> tmp (rarray pitch pitch 1 (- wbumps 1) :center "True") (circle :r t0) (extrude (- height t0)))))
	  (t
	    (setf> tmp s)))
	(show> tmp)))))

;;for test27: Defines a symetrical trapezoid in the XY plane.
(defun trapezoid (b1 b2 h xyplane)
  (let* ((y (/ h 2))
	  (x1 (/ b1 2))
	  (x2 (/ b2 2))
	  (pts (list (list (- x1) y) (list x1 y) (list x2 (- y)) (list (- x2) (- y)) (list (- x1) y))))
    (.> xyplane (polyline :points pts))
    ))
;;(trapezoid 1 2 3 "XY")

;;for test27: Defines the base shape: a box with fillets around the vertical edges.
(defun base (h width trapezoidFudge length height zFilletRadius xyplane)
  (.> (trapezoid width (* width trapezoidFudge) length xyplane) (extrude h) (translate 0 0 (/ height 2)) (edges "Z") (fillet zFilletRadius)))
;;(base 1 2 3 4 5 6 "XY")

;; source code issue: Fillets requires that edges be selected
(defun test27 ()
  (let* ((exploded nil)        ;; when true, moves the base away from the top so we see
	  (showTop t)          ;; When true, the top is rendered.
	  (showCover t)        ;; When true, the cover is rendered
	  (width 2.2)             ;; Nominal x dimension of the part
	  (height 0.5)            ;; Height from bottom top to the top of the top :P
	  (length 1.5)            ;; Nominal y dimension of the part
	  (trapezoidFudge 0.7)    ;; ratio of trapezoid bases. set to 1.0 for cube
	  (xHoleOffset 0.500)     ;; Holes are distributed symetrically about each axis
	  (yHoleOffset 0.500)
	  (zFilletRadius 0.50)    ;; Fillet radius of corners perp. to Z axis.
	  (yFilletRadius 0.250)   ;; Fillet readius of the top edge of the case
	  (lipHeight 0.1)         ;; The height of the lip on the inside of the cover
	  (wallThickness 0.06)    ;; Wall thickness for the case
	  (coverThickness 0.2)    ;; Thickness of the cover plate
	  (holeRadius 0.30)       ;; Button hole radius
	  (counterSyncAngle 100)  ;; Countersink angle.
	  (xyplane (workplane :plane "XY"))
	  (yzplane (workplane :plane "YZ"))
	  (top "top")
	  (cover "cover")
	  )
    (cadquery
      (list
	;;start with the base shape
	(setf> top (.> (base height width trapezoidFudge length height zFilletRadius xyplane) (edges ">Z") (fillet yFilletRadius) (faces "-Z") (shell (- wallThickness)) (faces "+Z") (workplane) (push-points :points (list '(0 0) (list (- xHoleOffset) 0) (list 0 (- yHoleOffset)) (list xHoleOffset 0) (list 0 yHoleOffset))) (csk-hole :dia holeRadius :csk-dia (* holeRadius 1.5) :csk-angle counterSyncAngle)))
	(setf> cover (.> (base coverThickness width trapezoidFudge length height zFilletRadius xyplane) (translate 0 0 (+ (- coverThickness) lipHeight)) (cut top) (edges "#Z") (fillet 0.02) (translate 0 0 (if exploded -0.5 0))))
	(when showTop (show> top))
	(when showCover (show> cover))))))

;; source code issure: list index out of range
(defun test28 ()
  (let* ((side 10)
	  (offset 5)
	  (pts "pts")
	  (result "result"))
    (cadquery
      (list
	(setf> pts (format nil "~a + ~a" (np-array (list '(0 0) (list side 0) (list side side) (list 0 side) '(0 0))) (coma (list offset offset) :bracket "[]")))
	(setf> result (.> (workplane :plane "XY") "polyline(pts)" (extrude 2) (faces "+Z") (workplane) (circle :r (/ side 2)) (extrude 1)))
	(show> result)
	)
      :init (list (import> "numpy" :as "np") (import> "cadquery") (import> "show" :from "Helpers")))))


;; The following functions for test29 ------------------------------------------------------------------------

(defun make-point (x y)
  (>> 'x x 'y y))

(defun braille-to-points (text cell_geometry)
  (let ((mask1  #*00000001)
	 (mask2 #*00000010)
	 (mask3 #*00000100)
	 (mask4 #*00001000)
	 (mask5 #*00010000)
	 (mask6 #*00100000)
	 (mask7 #*01000000)
	 (mask8 #*10000000)
	 (masks (list mask1 mask2 mask3 mask4 mask5 mask6 mask7 mask8))
	 (w (>> cell_geometry 'horizontal_interdot))
	 (h (>> cell_geometry 'vertical_interdot))
	 (pos1 (make-point 0 (* 2 h)))
	 (pos2 (make-point 0 h))
	 (pos3 (make-point 0 0))
	 (pos4 (make-point w (* 2 h)))
	 (pos5 (make-point w h))
	 (pos6 (make-point w 0))
	 (pos7 (make-point 0 (- h)))
	 (pos8 (make-point w (- h)))
	 (pos (list pos1 pos2 pos3 pos4 pos5 pos6 pos7 pos8))
	 (blank " ")
	 (points '())
	 (character_origin 0)
	 )
    (dolist (c text)
      (dotimes (i (length masks))
	(when (bit-and (nth i masks) (- (char-int c) (char-int #\ )))
	  (let ((p (nth i pos)))
	    (push (list (+ (>> p 'x) character_origin) (+ (>> p 'y) 0)) points))))
      (setf character_origin (+ character_origin (>> cell_geometry 'intercell))))
    (reverse points)))

(defun get-plate-height (text_lines cell_geometry)
  (+ (* 2 (>> cell_geometry 'vertical_interdot)) (* 2 (>> cell_geometry 'vertical_interdot)) (* (- (length text_lines) 1) (>> cell_geometry 'interline))))

(defun get-plate-width (text_lines cell_geometry)
  (let ((max_len (apply 'max (mapcar (lambda (x) (length x)) text_lines))))
    (+ (* 2 (>> cell_geometry 'horizontal_interdot)) (>> cell_geometry 'horizontal_interdot) (* (- max_len 1) (>> cell_geometry 'intercell)))))

(defun get-cylinder-radius (cell_geometry)
  (let ((h (>> cell_geometry 'dot_height))
	 (r (/ (>> cell_geometry 'dot_diameter) 2)))
    (/ (+ (expt r 2) (expt h 2)) 2 h)))

(defun get-base-plate-thickness (plate_thickness cell_geometry)
  (- (+ plate_thickness (get-cylinder-radius cell_geometry)) (>> cell_geometry 'dot_height)))

(defun make-base (text_lines cell_geometry plate_thickness)
  (let ((base_width (get-plate-width text_lines cell_geometry))
	 (base_height (get-plate-height text_lines cell_geometry))
	 (base_thickness (get-base-plate-thickness plate_thickness cell_geometry)))
    (.> (workplane :plane "XY") (box base_width base_height base_thickness :centered "(False, False, False)"))))

(defun make-embossed-plate (text_lines cell_geometry base_thickness)
  (let* ((result (gensym))
	  (base (make-base text_lines cell_geometry base_thickness))
	  (dot_pos '())
	  (base_width (get-plate-width text_lines cell_geometry))
	  (base_height (get-plate-height text_lines cell_geometry))
	  (y (- base_height (* 3 (>> cell_geometry 'vertical_interdot))))
	  (line_start_pos (make-point (>> cell_geometry 'horizontal_interdot) y)))
    (dolist (text text_lines)
      (let ((dots (braille-to-points text cell_geometry)))
	(setf dots (mapcar (lambda (p) (list (+ (car p) (>> line_start_pos 'x)) (+ (second p) (>> line_start_pos 'y)))) dots))
	(setf dot_pos (append dot_pos dots))
	(>> line_start_pos 'x (+ (>> line_start_pos 'x) 0))
	(>> line_start_pos 'y (+ (>> line_start_pos 'x) (- (>> cell_geometry 'interline))))))
    (let ((r (get-cylinder-radius cell_geometry))
	   (hidding_box (.> (workplane :plane "XY") (box base_width base_height base_thickness :centered "(False, False, False)"))))
      (setf base (.> base (faces ">Z") (vertices "<XY") (workplane) (push-points dot_pos) (circle :r r) (extrude r)))
      (setf base (.> base (faces ">Z") (edges) (fillet (- r 0.001))))
      (setf result (union> hidding_box base)))
    result))

;;TODO: solve the issue of representing ['⠠ ⠋ ⠗ ⠑ ⠑ ⠠ ⠉ ⠠ ⠁ ⠠ ⠙']
(defun test29 ()
  (let* ((text_lines '("'⠠ ⠋ ⠗ ⠑ ⠑ ⠠ ⠉ ⠠ ⠁ ⠠ ⠙'"))
	  (horizontal_interdot 2.5)
	  (vertical_interdot 2.5)
	  (horizontal_intercell 6)
	  (vertical_interline 10)
	  (dot_height 0.5)
	  (dot_diameter 1.3)
	  (base_thickness 1.5)
	  (BrailleCellGeometry "BrailleCellGeometry")
	  (_cell_geometry "_cell_geometry"))
    (cadquery
      (list
	(setf> BrailleCellGeometry (namedtuple "BrailleCellGeometry" '("horizontal_interdot" "vertical_interdot" "intercell" "interline" "dot_height" "dot_diameter")))
	(setf> _cell_geometry "BrailleCellGeometry(horizontal_interdot, vertical_interdot, horizontal_intercell, vertical_interline, dot_height, dot_diameter)")
	(show> (make-embossed-plate text_lines _cell_geometry)))
      :init (list (import> "unicode_literals, division" :from "__future__" ) (import> "namedtuple" :from "collections") (import> "cadquery") (import> "show" :from "Helpers")))))

(defun test31 ()
  (let* ((pts '((0 1) (1 2) (2 4)))	  
	  (path (.> (workplane :plane "XZ") (spline :points pts)))
	  (defaultSweep (.> (workplane :plane "XY") (circle :r 1.0) (sweep path)))
	  (frenetShell (.> (workplane :plane "XY") (circle :r 1.0) (sweep path :makeSolid "False" :isFrenet "True")))
	  (defaultRect (.> (workplane :plane "XY") (rect 1.0 1.0) (sweep path)))
	  (plineSweep (gensym))
	  (arcSweep (gensym)))
    (show "defaultSweep: {}" defaultSweep)
    (setf path (.> (workplane :plane "XZ") (polyline :points pts)))
    (setf plineSweep (.> (workplane :plane "XY") (circle :r 1.0) (sweep path)))
    (setf path (.> (workplane :plane "XZ") (three-point-arc :p0 '(1.0 1.5) :p1 '(0.0 1.0))))
    (setf arcSweep (.> (workplane :plane "XY") (circle :r 0.5) (sweep path)))
    (cadquery
      (list
	(show> defaultSweep)
	(show> (.> frenetShell (translate 5 0 0)))
	(show> (.> defaultRect (translate 10 0 0)))
	(show> (.> plineSweep (translate 15 0 0)))
	(show> (.> arcSweep (translate 20 0 0)))))))

(defun test33 ()
  (cadquery
    (list (show> (.> (workplane :plane "XY") (box 2 2 2) (faces ">Z") (shell -0.2) (faces ">Z") (edges "not(<X or >X or <Y or >Y)") (chamfer 0.125))))))



#|

|#
