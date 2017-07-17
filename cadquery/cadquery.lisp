;;;; cadquery.lisp

(in-package #:cadquery)

(defun merge-cad (l &key (link ""))
  (if (listp l)
    (reduce 
      (lambda (a b) (concatenate 'string a link b))
      (mapcar #'merge-cad l))
    l))

(defun import> (x &key (from nil) (as nil))
  (let ((result (gensym)))
    (if from
      (progn
	(setf result (format nil "from ~a import ~a" from x))
	(when as (setf result (format nil "~a as ~a" result as))))
      (progn
	(setf result (format nil "import ~a" x))
	(when as (setf result (format nil "~a as ~a" result as)))))
    result))
;;(import> "cadquery")
;;(import> "cadquery" :as "cq")
;;(import> "show" :from "Helpers")
;;(import> "show" :from "Helpers" :as "s")

(defun comment (x)
  (format nil "# ~a" x))
;;(comment "The dimensions of the box. These can be modified rather than changing the")

(defun show> (x)
  (format nil "show(~a)" x))
;;(show> "result")

(defun box (len0 len1 len2 &key (centered nil))
  (if centered
    (format nil "box(~a, ~a, ~a, centered=~a)" len0 len1 len2 centered)
    (format nil "box(~a, ~a, ~a)" len0 len1 len2)))
;;(box 3 2 1)
;;(box 3 2 1 :centered "(False, False, False)")

(defun rect (length height &key (center nil) (for-construction nil) (a nil))
  (let ((result nil))
    (if for-construction
      (setf result (format nil "rect(~a, ~a, forConstruction = True)" length height))
      (setf result (format nil "rect(~a, ~a)" length height)))
    (when center
      (setf result (concatenate 'string  (format nil "center(~a, ~a)." (car center) (second center)) result)))
    (when a (setf result (format nil "~a, ~a)" (subseq result 0 (- (length result) 1)) a)))
    result))
;;(rect 2 1)
;;(rect 2 1 :for-construction t) 
;;(rect 2 1 :a "False")

(defun circle (&key (r 1) (center nil))
  (if center
    (format nil "center(~a, ~a).circle(~a)" (car center) (second center) r)
    (format nil "circle(~a)" r)))

(defun coma (lst &key (bracket "()"))
  (let ((result (format nil (concatenate 'string "~{~A~^" ", " "~}") lst)))
    (cond
      ((equalp bracket "()") (format nil "(~a)" result))
      ((equalp bracket "[]") (format nil "[~a]" result))
      ((equalp bracket "{}") (format nil "{~a}" result))      
      (t (format nil "(~a)" result)))))
;;(coma '(1 2 3))
;;(coma '(1 2 3) :bracket "[]")
;;(coma '(1 2 3) :bracket "{}")
;;(coma '("a" 2 3))

(defun revolve (&optional (angle nil) (x nil) (y nil) (a nil))
  (if angle
    (if x
      (if y
	(if a
	  (format nil "revolve(~a, ~a, ~a, ~a)" angle (coma x) (coma y) a)
	  (format nil "revolve(~a, ~a, ~a)" angle (coma x) (coma y)))
	(format nil "revolve(~a, ~a)" angle (coma x)))
      (format nil "revolve(~a)" angle))
    (format nil "revolve()")
    ))
;;(revolve)
;;(revolve 360)
;;(revolve 360 '(-5 -5))
;;(revolve 360 '(-5 -5) '(-5 -3))
;;(revolve 360 '(-5 -5) '(-5 -3) "False")

(defun rarray (a0 a1 b0 b1 &key (center nil))
  (if center
    (format nil "rarray(~a, ~a, ~a, ~a, center=~a)" a0 a1 b0 b1 center)
    (format nil "rarray(~a, ~a, ~a, ~a)" a0 a1 b0 b1)))
;;(rarray 1 2 3 4)
;;(rarray 1 2 3 4 :center "True")

(defun np-array (lst)
  (let ((lst- (join (mapcar (lambda (x) (format nil "(~a, ~a)" (car x) (second x))) lst) :link ", ")))
    (format nil "np.array([~a])" lst-)))
;;(np-array '((0 0) (1 1) (2 3)))

(defun hole (&key (dia 2) (depth nil))
  (if depth
    (format nil "hole(~a, ~a)" dia depth)
    (format nil "hole(~a)" dia)))
;;(hole :dia 2)
;;(hole :dia 2 :depth 10)

(defun cbore-hole (&key (hole-dia 2.4) (cbore-dia 4.4) (cbore-dep 2.1) (a nil))
  (if a
    (format nil "cboreHole(~a, ~a, ~a, ~a)" hole-dia cbore-dia cbore-dep a)
    (format nil "cboreHole(~a, ~a, ~a)" hole-dia cbore-dia cbore-dep)))

(defun csk-hole (&key (dia 1) (csk-dia 1.5) (csk-angle 100) (depth nil))
  (if depth
    (format nil "cskHole(diameter=~a, cskDiameter=~a, cskAngle=~a, depth=~a)" dia csk-dia csk-angle depth)
    (format nil "cskHole(diameter=~a, cskDiameter=~a, cskAngle=~a)" dia csk-dia csk-angle)))
;;(csk-hole :dia 1 :csk-dia 1.5 :csk-angle 100 :depth nil)
;;(csk-hole :dia 1 :csk-dia 1.5 :csk-angle 100 :depth "None")

(defun line-to (x y)
  (format nil "lineTo(~a, ~a)" x y))

(defun three-point-arc (&key (p0 '(0 1)) (p1 '(2 3)))
  (format nil "threePointArc((~a, ~a), (~a, ~a))" (car p0) (second p0) (car p1) (second p1)))

(defun close> () (format nil "close()"))

(defun mirror (&key (dir "Y"))
  (format nil "mirror~a()" dir))

(defun extrude (thickness &key (a nil))
  (if a
    (format nil "extrude(~a, ~a)" thickness a)
    (format nil "extrude(~a)" thickness)))
;;(extrude 2 :a "True")

(defun cut (x)
  (format nil "cut(~a)" x))
;;(cut "x")

(defun union> (x y)
  (format nil "~a.union(~a)" x y))
;;(union> "x" "y")

(defun combine-solids (x)
  (format nil "combineSolids(~a)" x))
;;(combine-solids "x")

(defun cut-thru-all ()
  (format nil "cutThruAll()"))
;;(cut-thru-all)

(defun polygon (&key (sides 6) (dia 1))
  (format nil "polygon(~a, ~a)" sides dia))
;;(polygon :sides 6 :dia 1)

(defun push-points (&key (points '((1.5 0) (0 1.5) (-1.5 0) (0 -1.5))))
  (let ((points- (merge-cad (mapcar (lambda (x) (format nil "(~a, ~a)" (car x) (second x))) points) :link ", ")))
    (format nil "pushPoints([~a])" points-)))

(defun spline (&key (points '((2.75 1.5) (2.5 1.75) (2.0 1.5) (1.5 1.0) (1.0 1.25) (0.5 1.0) (0 1.0))))
  (let ((points- (merge-cad (mapcar (lambda (x) (format nil "(~a, ~a)" (car x) (second x))) points) :link ", ")))
    (format nil "spline([~a])" points-)))

(defun hline (dis)
  (format nil "hLine(~a)" dis))

(defun vline(dis)
  (format nil "vLine(~a)" dis))

;; hline-to allows using xCoordinate not distance
(defun hline-to (x)
  (format nil "hLineTo(~a)" x))

(defun edges (x)
  (format nil "edges('~a')" x))
;;(edges "|Z")

(defun fillet (x)
  (format nil "fillet(~a)" x))
;;(fillet 0.125)

(defun chamfer (x)
  (format nil "chamfer(~a)" x))
;;(chamfer 0.125)

(defun make-turtle (&key (coord (>> 'x 0 'y 0 'angle 0)) (point? t))
  (if point?
    (>> 'coord coord 'points (list (list (>> coord 'x) (>> coord 'y))))
    (>> 'coord coord 'points '())))

(defun goto (&key (a (make-turtle)) (x 1) (y 1) (angle nil) (point? t))
  (>> a `(coord x) x)
  (>> a `(coord y) y)
  (when (not (null angle))
    (>> a `(coord angle) angle))
  (when point?
    (let ((pts (>> a 'points)))
      (>> a 'points (push (list x y) pts))))
  a)

(defun fd (&key (a (make-turtle)) (dis 10))
  (let ((x (>> a `(coord x)))
	 (y (>> a `(coord y))))
    (>> a `(coord x) (+ x (* dis (cos (to-radian :angle (>> a `(coord angle)))))))
    (>> a `(coord y) (+ y (* dis (sin (to-radian :angle (>> a `(coord angle)))))))
    a))

(defun bk (&key (a (make-turtle)) (dis 10))
  (fd :a a :dis (- dis)))

(defun lt (&key (a (make-turtle)) (deg 45))
  (>> a `(coord angle) (+ (>> a `(coord angle)) deg))
  (when (< (>> a `(coord angle)) 0) (>> a `(coord angle) (+ (>> a `(coord angle)) 360)))
  (when (> (>> a `(coord angle)) 360) (>> a `(coord angle) (- (>> a `(coord angle)) 360)))
  a)

(defun rt (&key (a (make-turtle)) (deg 45))
  (lt :a a :deg (- deg)))

(defun polyline
  (&key
    (points (let ((l 100) (h 20) (w 20) (t0 1)) (list (list 0 (/ H 2))
						 (list (/ W 2) (/ H 2))
						 (list (/ W 2) (- (/ H 2) t0))
						 (list (/ t0 2) (- (/ H 2) t0))
						 (list (/ t0 2) (- t0 (/ H 2)))
						 (list (/ W 2) (- t0 (/ H 2)))
						 (list (/ W 2) (/ H -2.0))
						 (list 0 (/ H -2.0))))))
  (let ((pts (join (mapcar (lambda (x) (format nil "(~a, ~a)" (car x) (second x))) points) :link ", ")))
    (format nil "polyline([~a])" pts)))

(defun transform (&key (offset '(0 -1.5 1.0)) (rotate '(60 0 0)))
  (let ((off (join (mapcar (lambda (x) (format nil "~a" x)) offset) :link ", "))
	 (rot (join (mapcar (lambda (x) (format nil "~a" x)) rotate) :link ", ")))
    (format nil "transformed(offset = (~a), rotate = (~a))" off rot)))
;;(transform :offset '(0 -1.5 1.0) :rotate '(60 0 0))

(defun translate (x y z)
  (format nil "translate((~a, ~a, ~a))" x y z))
;;(translate 1 2 3)

(defun shell (thickness)
  (format nil "shell(~a)" thickness))

(defun vertices (&key (dir nil))
  (if dir
    (format nil "vertices('~a')" dir)
    (format nil "vertices()")))

(defun faces (axis)
  (format nil "faces('~a')" axis))
;;(faces "+Z")

;;offset = 0.75 'front' 'XY' nil 
(defun workplane (&key (plane nil) (offset nil) (a nil) (invert nil)) ;; a = "True" or "False"
  (let ((result ""))
    (if plane
      (setf result (format nil "cadquery.Workplane('~a')" plane))
      (progn
	(setf result (format nil "workplane()"))
	(if offset
	  (progn
	    (setf result (format nil "workplane(~a)" offset))
	    (when invert (setf result (format nil "~a, invert=True)" (subseq result 0 (- (length result) 1))))))
	  (when invert (setf result (format nil "workplane(invert=True)"))))
	(when a (setf result (format nil "~a, ~a)" (subseq result 0 (- (length result) 1)) a)))
	))
    result))
;;(workplane)
;;(workplane :plane "XY")
;;(workplane :offset 0.75)
;;(workplane :offset 0.75 :a "True")
;;(workplane :offset 0.75 :invert t)
;;(workplane :invert t)

(defun loft (&key (combine nil))
  (if combine
    (format nil "loft(combine=True)")
    (format nil "loft()")))

(defun sweep (path &key (makeSolid nil) (isFrenet nil))
  (let ((result (format nil "sweep(~a)" path)))
    (when makeSolid (setf result (format nil "~a, makeSolid=~a)" (subseq result 0 (- (length result) 1)) makeSolid)))
    (when isFrenet (setf result (format nil "~a, isFrenet=~a)" (subseq result 0 (- (length result) 1)) isFrenet)))
    result))
;;(sweep "path")
;;(sweep "path" :makeSolid "False" :isFrenet "True")

(defun split (&key (keep-top nil) (keep-bottom nil))
  (let ((result ""))
    (if keep-top
      (progn
	(setf result (format nil "split(keepTop=True)"))
	(when keep-bottom (setf result (format nil "~a, keepBottom=True)" (subseq result 0 (- (length result) 1))))))
      (progn
	(setf result (format nil "split()"))
	(when keep-bottom (setf result (format nil "~akeepBottom=True)" (subseq result 0 (- (length result) 1)))))))
    result))
;;(split :keep-top t)
;;(split :keep-top t :keep-bottom t)
;;(split :keep-bottom t)

(defun center (x y)
  (format nil "center(~a, ~a)" x y))

(defun setf> (x y)
  (format nil "~a = ~b" x y))
;; (=> "newDoc" "FreeCAD.newDocument()")

(defun new-document ()
  (format nil "FreeCAD.newDocument()"))
;;(new-document)

(defun add-object (document type object)
  (format nil "~a.addObject('~a', '~a')" document type object))
;;(add-object "newDoc" "Part::Box" "initialBox")

(defun recompute (document)
  (format nil "~a.recompute()" document))
;;(recompute "newDoc")

(defun solid (shape)
  (format nil "cadquery.Solid(~a)" shape))
;;(solid "initialBox.Shape")

(defun cq (object)
  (format nil "cadquery.CQ(~a)" object))
;;(cq (solid "initialBox.Shape"))

(defun val (object)
  (format nil "~a.val()" object))
;;(val "newThing")

(defun namedtuple (x y)
  (format nil "namedtuple('~a', (~a))" x (join (mapcar (lambda (n) (format nil "'~a'" n)) y) :link ", ")))
;;(namedtuple "BrailleCellGeometry" '("horizontal_interdot" "vertical_interdot" "intercell" "interline" "dot_height" "dot_diameter")) 

(defun .> (&rest data)
  (join data :link "."))
;;(.> "initialBox" "Shape")
;;(.> "cadquery" "Workplane('XY')" "rect(rectangle_width, rectangle_length, False)" "revolve()")

(defun cadquery (process &key
		  (init (list (import> "cadquery") (import> "show" :from "Helpers")))
		  (file-name "~/example_cadquery.py"))
  (let* ((result (append init process)))
    (extend-file :data "" :file file-name :new t :newline nil)
    (dolist (l result)
      (extend-file :data l :file file-name))))
