(require 'gl 'glut 'glf)

(define a 0)
(define wind_w 640)
(define wind_h 480)

(define draw (lambda ()
	       (gl:Clear gl:COLOR_BUFFER_BIT)
	       (gl:Color3f 0 1 0)
	       (gl:PushMatrix)
	       (gl:Rotatef a 1 1 1)
	       (set! a (+ a 3))

	       (if (> a 359)
		 (set! a (- a 360)))

	       (gl:Translatef -1.5 0 0)
	       (gl:Scalef 0.5 0.5 0.5)

	       (glfDraw3DWiredString "3D Text")
	       (gl:PopMatrix)
	       (gl:Flush)
	       (glut:SwapBuffers)
	       ))

(define reshape (lambda (width height)
		  (gl:Viewport 0 0 width height)
		  (gl:MatrixMode gl:PROJECTION)
		  (gl:LoadIdentity)

		  (gl:Frustum -0.5 0.5 -0.5 0.5 2 14)
		  (gl:MatrixMode gl:MODELVIEW)
		  (gl:LoadIdentity)
		  (gl:Translatef 0.0 0.0 -9.0)

		  (set! wind_w width)
		  (set! wind_h height)
		  ))

(define timf (lambda (value)
	       (glut:PostRedisplay)
	       (glut:TimerFunc 40 timf 0)
	       ))

(define key (lambda (key x y)
	      (if (eq? 27 (char->integer key))
		(exit))
	      ))

(define main (lambda ()
	       (glfLoadFont "techno1.glf")
	       (glfSetSymbolSpace 0.2)

	       (glut:InitWindowSize wind_w wind_h)
	       (glut:InitDisplayMode (+ glut:RGB glut:DOUBLE))
	       (glut:CreateWindow "GLF Demo - 3D Text")

	       (glut:ReshapeFunc reshape)
	       (glut:DisplayFunc draw)
	       (glut:KeyboardFunc key)

	       (gl:ClearColor 0 0 0 0)
	       (glut:TimerFunc 40 timf 0)
	       (glfSetSymbolSpace 0.2)

	       (glut:MainLoop)
	       ))

(main)
