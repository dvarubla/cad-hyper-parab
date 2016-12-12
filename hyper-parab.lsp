; Only right branch of the east-west opening (horizontal) hyperbola
; (or right opening parabola) with apex at (0,0,0)
; is drawn, others are first converted to this form and then
; the resulting spline points are converted back

; useful link
; https://discourse.mcneel.com/t/how-to-create-hyperbola-with-c/8189

(setq hyper-parab-common
  (list
     ; read points for curves
    '(defun parab-read-points()
      (terpri)
      (setq apex (getpoint "Enter apex: "))
      (terpri)
      (setq first-point (getpoint "Enter point on parabola: "))
      (terpri)
      (setq second-point (getpoint apex "Enter point inside parabola: "))
      (list apex first-point second-point))

    '(defun hyper-read-points ()
      (terpri)
      (setq apex (getpoint "Enter apex: "))
      (terpri)
      (setq first-point (getpoint "Enter first point: "))
      (terpri)
      (setq second-point
        (getpoint "Enter second point on the same half of branch: "))
      (list apex first-point second-point))

    '(defun rect-hyper-read-points ()
      (terpri)
      (setq apex (getpoint "Enter apex: "))
      (terpri)
      (setq first-point (getpoint "Enter first point: "))
      (terpri)
      (setq second-point (getpoint apex "Enter point inside hyperbola: "))
      (list apex first-point second-point))

    ; subtract apex coordinates from every point coordinates
    '(defun process-subt(func points)
      (make-spline-wrap
        (post-process-subt
          (car points)
          (apply func (list points
            (subtract-from-apex (car points) (cdr points)))))) nil)

    ; add apex coordinates and translate from user cs to world cs
    '(defun post-process-subt(apex result)
       (cons (add-apex-and-trans apex (car result)) (cdr result)))

    '(defun subtract-from-apex(apex points)
      (mapcar
        '(lambda (pnt) (mapcar
          '(lambda (v1 v2)
            (abs (- v1 v2)))
             pnt apex))
          points))

    '(defun add-apex-and-trans(apex points)
      (mapcar '(lambda (pnt) (trans (list
      (+ (car pnt) (car apex))
      (+ (cadr pnt) (cadr apex))
      (+ (caddr pnt) (caddr apex))) 1 0)) points))

    ; helper function for chaining calls
    '(defun perform-next-call(next-call new-args)
      (apply (car next-call) (append (cdr next-call) new-args)))

    ; convert from north or south opening curve to east or west
    '(defun process-vertical(next-call subt-points)
        (if (is-vertical (car subt-points) (cadr subt-points))
            (post-process-vertical (perform-next-call next-call
              (list (mapcar
                '(lambda (pnt) (cons (cadr pnt) (cons (car pnt) (cddr pnt))))
              subt-points) T)))
          (perform-next-call next-call (list subt-points Nil))))

    ; convert from west or east opening curve back to north or south
    '(defun post-process-vertical(result)
      (cons
        (mapcar
          '(lambda (pnt) (cons (- (cadr pnt)) (cons (car pnt) (cddr pnt))))
           (car result))
        (cdr result)))

    ; find if a curve is north or south opening
    '(defun is-vertical(first second) (< 0
      (- (* (car first) (cadr second))
      (* (car second) (cadr first)))))

    ; for hyperbola — if the first point if more distant from apex
    ; than the second then swap points
    '(defun try-reverse(next-call subt-points)
       (perform-next-call next-call (list
         (if
           (> (car (car subt-points))
           (car (cadr subt-points)))
           (reverse subt-points)
           subt-points
         ))))

    ; convert from west opening to east opening
    '(defun process-left(next-call points subt-points vertical)
       (if (is-left (car points) (cadr points) vertical)
        (post-process-left
         (perform-next-call next-call (list subt-points)))
         (perform-next-call next-call (list subt-points))))

    ; convert from east opening back to west opening
    '(defun post-process-left(result)
       (cons
        (mapcar
          '(lambda (pnt) (cons (- (car pnt)) (cdr pnt)))
           (car result))
        (cdr result)))

    ; find if a curve is west opening
    '(defun is-left(apex first vertical)
      (if vertical
        (< (cadr first) (cadr apex))
        (< (car first) (car apex))))

    '(defun parab-calc-wrap (points)
       (parab-calc-result (car points)))

    ; calculate spline points for parabola
    ; and weights
    '(defun parab-calc-result (first)
      (list
       (list
        first
        (list (- (car first)) 0 0)
        (list (car first) (- (cadr first)) 0))
       (list 1.0 1.0 1.0)))

    ; main function for drawing parabola
    '(defun parab-func(points subt-points)
      (process-vertical
        (list 'process-left
          (list 'parab-calc-wrap)
           points)
         subt-points))

    ; main function for drawing hyperbola
    '(defun hyper-func (points subt-points)
      (try-reverse
        (list 'process-vertical
          (list 'process-left
            (list 'hyper-calc-wrap)
            points))
        subt-points))

    ; main function for drawing rectangular hyperbola
    '(defun rect-hyper-func (points subt-points)
      (process-vertical
        (list 'process-left
          (list 'rect-hyper-calc-wrap)
          points)
        subt-points))

    '(defun hyper-calc-wrap (points)
      (hyper-calc-xt
        (car (car points))
        (cadr (car points))
        (car (cadr points))
        (cadr (cadr points))))

    '(defun rect-hyper-calc-wrap (points)
      (rect-hyper-calc-xt
        (car (car points))
        (cadr (car points))))

    ; The formula is obtained using this Mathematica code:
    ; Reduce[{
    ;((x1-x0)^2/a^2-y1^2/b^2==1),
    ;((x2-x0)^2/a^2-y2^2/b^2==1),
    ;-y2/(t-x2)==b^2/a^2*(x2-x0)/y2,
    ; x2>x1, y2>y1, x2>0, y2>0, x1>0, y1>0, x0+a==0,a>0, b>0},
    ; {t}, {a, b, x0}, Reals]
    '(defun hyper-calc-xt(x1 y1 x2 y2)
      (hyper-form-result x2 y2 (/
        (- (* (expt x2 3) y1 y1) (* x1 x1 x2 y2 y2))
        (- (+ (* x2 x2 y1 y1) (* x1 x1 y2 y2)) (* 2 x1 x2 y2 y2)))))

    ; The formula is obtained using this Mathematica code:
    ; Reduce[{((x2 - x0)^2/a^2 - y2^2/a^2 == 1),
    ;-y2/(t - x2) == a^2/a^2*(x2 - x0)/y2,
    ;x2 > 0, y2 > 0, x0 + a == 0, a > 0},
    ;{t}, {a, x0}, Reals]
    '(defun rect-hyper-calc-xt(x2 y2)
      (hyper-form-result x2 y2 (/
        (- (expt x2 3) (* x2 y2 y2))
        (+ (* x2 x2) (* y2 y2)))))

    ; returns spline points for hyperbola
    ; and  weights
    '(defun hyper-form-result (x2 y2 xt)
      (print xt)
      (list
        (list (list x2 y2 0) (list xt 0 0) (list x2 (- y2) 0))
        (list 1.0 (- (/ x2 xt)) 1.0)))

    '(defun make-spline-wrap(result)
       (make-spline (car result) (cadr result)))

    ; create spline entity
    ; see http://www.autodesk.com/techpubs/autocad/acadr14/dxf/entities_section_al_u05_c.htm
    '(defun make-spline(points weights)
      (entmake (append
       '((0 . "SPLINE")
         (100 . "AcDbEntity"))

         (list (cons 8 (getvar "CLAYER")))
         (if (= "BYLAYER" (strcase (getvar "CECOLOR")))
          Nil
          (list (cons 62 (atoi (getvar "CECOLOR")))))
         (list (cons 6 (getvar "CELTYPE")))
         (list (cons 370 (getvar "CELWEIGHT")))

        '((100 . "AcDbSpline")
         (210 0.0 0.0 1.0)
         (70 . 12)
         (71 . 2)
         (72 . 6)
         (73 . 3)
         (74 . 0)
         (42 . 1e-010)
         (43 . 1e-010)
         (12 0.0 0.0 0.0)
         (13 0.0 0.0 0.0)
         (40 . 0.0)
         (40 . 0.0)
         (40 . 0.0)
         (40 . 1.0)
         (40 . 1.0)
         (40 . 1.0))

         (apply 'append (mapcar
          '(lambda (w p) (list (cons 10 p) (cons 41 w)))
          weights points)))))))

(defun C:parab()
  (mapcar 'eval hyper-parab-common)
  (process-subt 'parab-func (parab-read-points)))

(defun C:hyper()
  (mapcar 'eval hyper-parab-common)
  (process-subt 'hyper-func (hyper-read-points)))

(defun C:rhyper()
  (mapcar 'eval hyper-parab-common)
  (process-subt 'rect-hyper-func (rect-hyper-read-points)))

"parab, hyper, rhyper"
