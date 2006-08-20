; Targa (TGA) file loader. Used for Latrunculi's textures.
; Michael McDermott, 2006

(require 'vector-lib 'srfi-13)
; SRFI-13 is a string library.

(define (logior x y)
   (cond ((= x y) x)
 	((zero? x) y)
 	((zero? y) x)
 	(else
 	 (+ (* (logior (quotient x 2) (quotient y 2)) 2)
 	    (if (and (even? x) (even? y)) 0 1)))))

(define (logand x y)
    (cond ((= x y) x)
  	((zero? x) 0)
  	((zero? y) 0)
  	(else
  	 (+ (* (logand (quotient x 2) (quotient y 2)) 2)
  	    (if (or (even? x) (even? y)) 0 1)))))
; Logical AND and Logical OR "borrowed" from http://okmij.org/ftp/Scheme/binary-parse.scm

(define left-shift (lambda (num bits)
		     (* num (expt 2 bits))
		     ))

(define read-bytes (lambda (port n)
		     (if (or (<= n 0)
			     (not (port? port)))
		       (void)
		       (begin
			 (define bytes (make-vector n))

			 (vector-for-each (lambda (i el)
					    (vector-set! bytes 
							 i 
							 (left-shift (char->integer (read-char port))
								     (* 8 i)
								     ))
					    )
					  bytes)
			 ; Reading all of the bytes to a vector was easy. Now, we have to OR them together.
			 ; Left shifting by n bits is the equivalent to multiplying the original number by 2^n.
			 ; After doing this to all n bytes, only one should have bits set to 1 in any particular
			 ; sequence of 8 bits, so adding should work.
			 ; TGA is written in little-endian format.

			 (vector-fold (lambda (i sum num)
					(+ sum num)
					)
				      0
				      bytes)
			 ; sums the entire list of bytes.
			 )
		       )
		     ))
; Read n bytes from port, ORing them into one long number.  If n <= 0 or an invalid port is passed, the results are undefined.

(define read-string (lambda (port n)
		      (define read-str-inner (lambda (port i n str)
					       (if (eq? i n)
						 str
						 (begin
						   (string-set! str i (read-char port))
						   (read-str-inner port
								   (+ i 1)
								   n
								   str
								   )
						 )
						 )
					       ))

		      (read-str-inner port 0 n (make-string n #\a))
		      ))
; Reads a string of n chars from the provided port.

(define tga-data (lambda (filename)
		   (define NO-IMG-DATA 0)
		   (define UNCOMPRESSED-COLOR-MAPPED 1)
		   (define UNCOMPRESSED-TRU-COLOR 2)
		   (define UNCOMPRESSED-B-W 3)
		   (define RLE-COLOR-MAPPED 9)
		   (define RLE-TRU-COLOR 10)
		   (define RLE-B-W 11)
		   ; This constants are defined by the TGA format and are used to denote the 
		   ; image type.

		   (define input-port (open-input-file filename))
		   ; input-port is a port created by opening the file named <filename>

		   (define id-length (char->integer (read-char input-port)))
		   ; The first byte in a TGA document is the id-length field, which describes
		   ; the length of the image id field (max. 255 chars).

		   (define color-map (char->integer (read-char input-port)))
		   ; a 1 indicates a color map, a 0 indicates the lack thereof.

		   (define image-type (char->integer (read-char input-port)))

		   (define first-entry-index (read-bytes input-port 2))
		   ; The starting point in the color table (allows you to use only a portion
		   ; of the table by starting somewhere in the middle). 2 bytes.
		   
		   (define color-map-length (read-bytes input-port 2))

		   (define color-map-entry-size (char->integer (read-char input-port)))
		   ; Now that we have done with the color map specs, we move on to the
		   ; image specs:

		   (define x-origin (read-bytes input-port 2))
		   (define y-origin (read-bytes input-port 2))

		   (define width (read-bytes input-port 2))
		   (define height (read-bytes input-port 2))

		   (define depth (char->integer (read-char input-port)))

		   (define image-descriptor (char->integer (read-char input-port)))

		   (define image-id "blah")
		   (if (> id-length 0)
		     (set! image-id (read-string input-port id-length))
		     (set! image-id "")
		     )

		   (define color-map-data '())
		   (if (eq? color-map 1)
		     (begin
		       )
		     )
		   ; a TGA file may or may not contain a color map. If it does not, there is no data
		   ; written to this section.

		   ;(display image-type)
		   ;(newline)
		   (define pixel-data '#()) 
		   (if (not (eq? image-type
				 NO-IMG-DATA))
		     (begin
		       (if (eq? image-type
				RLE-TRU-COLOR)
			 (begin
			   (define attrib-bytes 0)
			   ; A TGA defines how many attribute bytes precede the RGB data of a true-color
			   ; image in bits 0-3 of the image-descriptor.

			   (set! pixel-data (make-vector (* width height (+ 3 attrib-bytes))))
			   ; pixels will be returned in a 1D vector. The number of numbers we will need
			   ; to store is the number of pixels times three because each pixel will be represented
			   ; by three numbers (RGB).
			   )
			 )
		        (if (eq? image-type 
				 UNCOMPRESSED-TRU-COLOR)
			  (begin
			    (define load-inner (lambda (curr target i vec port)
						 (if (eq? curr target)
						   vec
						   (begin
						     (define blue-byte (char->integer (read-char input-port)))
						     (define green-byte (char->integer (read-char input-port)))
						     (define red-byte (char->integer (read-char input-port)))

						     (vector-set! vec i red-byte)
						     (vector-set! vec (+ i 1) green-byte)
						     (vector-set! vec (+ i 2) blue-byte)

						     (load-inner (+ curr 1) target (+ i 3) vec port)
						    )
						   )
						 ))
			    ; where curr is the current pixel, target is the number of pixels to load, i is the 
			    ; current vector's index, vec is the vector in which to store the data and port is
			    ; the port from which to read the data.

			    (set! pixel-data (load-inner 0 (* width height) 0 (make-vector (* width height 3)) input-port))
			    )
			  )
		       )
		     )

		   (vector height width depth pixel-data)

		   ;(define UNCOMPRESSED-COLOR-MAPPED 1)
		   ;(define UNCOMPRESSED-TRU-COLOR 2)
		   ;(define UNCOMPRESSED-B-W 3)
		   ;(define RLE-COLOR-MAPPED 9)
		   ;(define RLE-TRU-COLOR 10)
		   ;(define RLE-B-W 11)
		   ))
; This file reads in the TGA file _filename_ and returns RGBA values corresponding to the pixels
; of that file.
; Returns a vector of the format:
; #(H W D #(pixel data...))
