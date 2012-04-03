; author: Thomas Hintz
; email: t@thintz.com
; license: bsd

; Copyright (c) 2012, Thomas Hintz
; All rights reserved.

; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
;     * Neither the name of the <organization> nor the
;       names of its contributors may be used to endorse or promote products
;       derived from this software without specific prior written permission.

; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THOMAS HINTZ BE LIABLE FOR ANY
; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(module nunican
  (;; params
   css-unit

   ;; procs
   define-css

   css-html css-body css-a class id

   color background background-image background-color
   border-radius border-top-left-radius border-top-right-radius
   border-bottom-left-radius border-bottom-right-radius
   box-shadow font-family
   margin margin-top margin-right margin-left margin-bottom
   text-align line-height width height float
   padding padding-top padding-bottom padding-left padding-right
   font-size font-style font-weight text-decoration
   border border-top border-bottom border-right border-left
   position z-index css-display clear cursor)

(import scheme chicken data-structures)
(use srfi-1 spiffy intarweb srfi-69 uri-common)

(define (concat args #!optional (sep ""))
  (string-intersperse (map ->string args) sep))

;;; css resource handler

(define *css-resources* (make-hash-table equal?))
(define css-headers
  (lambda (res)
    `((content-type text/css) (content-length ,(string-length res)))))

(define (register-css-dispatcher)
  (handle-not-found
   (let ((old-handler (handle-not-found)))
     (lambda (_)
       (let* ((path-list (uri-path (request-uri (current-request))))
	      (path (if (null? (cdr path-list))
			(car path-list)
			(++ "/" (concat (cdr path-list) "/")))))
	 (let ((proc (css-resource-ref path)))
	   (if proc
	       (run-css-resource proc)
	       (old-handler path))))))))

(register-css-dispatcher)

(define (add-css-resource! path proc)
  (hash-table-set! *css-resources* path proc))

(define (css-resource-ref path)
  (hash-table-ref/default *css-resources* path #f))

(define (run-css-resource proc)
  (let ((res (proc)))
    (with-headers (css-headers res)
		  (lambda ()
		    (write-logged-response)
		    (request-method (current-request))
		    (display res (response-port (current-response)))))))

(define (define-css path proc)
  (add-css-resource! path proc))

;;;
;;; utils
;;;

(define ++ string-append)

(define css-unit (make-parameter "px"))

(define (att key value #!key (values #f))
  (++ key ": " (->string value) (if (number? value) (css-unit) "")
      (if values
	  (fold (lambda (v o) (++ o " " (->string v) (if (number? v) (css-unit) "")))
		""
		values)
	  "")
      "; "))

(define (css-html attributes)
  (++ "html { " attributes " } "))

(define (css-body attributes)
  (++ "body { " attributes " } "))

(define (css-a attributes)
  (++ "a { " attributes " } "))

(define (class class attributes)
  (++ "." class " { " attributes " } "))

(define (id id attributes)
  (++ "#" id " { " attributes " } "))

(define (color c) (att "color" c))
(define (background b) (att "background" b))
(define (background-image i) (att "background-image" i))
(define (background-color c) (att "background-color" c))
(define (border-radius r) (++ (att "border-radius" r) (att "-moz-border-radius" r)))
(define (border-top-left-radius r . s)
  (if (> (length s) 0)
      (let ((s-r (first s)))
	(++ (att "border-top-left-radius" r values: `(,s-r))
	    (att "-moz-border-radius-topleft" r values: `(,s-r))))
      (++ (att "border-top-left-radius" r)
	  (att "-moz-border-radius-topleft" r))))
(define (border-top-right-radius r . s)
  (if (> (length s) 0)
      (let ((s-r (first s)))
	(++ (att "border-top-right-radius" r values: `(,s-r))
	    (att "-moz-border-radius-topright" r values: `(,s-r))))
      (++ (att "border-top-right-radius" r)
	  (att "-moz-border-radius-topright" r))))
(define (border-bottom-left-radius r . s)
  (if (> (length s) 0)
      (let ((s-r (first s)))
	(++ (att "border-bottom-left-radius" r values: `(,s-r))
	    (att "-moz-border-radius-bottomleft" r values: `(,s-r))))
      (++ (att "border-bottom-left-radius" r)
	  (att "-moz-border-radius-bottomleft" r))))
(define (border-bottom-right-radius r . s)
  (if (> (length s) 0)
      (let ((s-r (first s)))
	(++ (att "border-bottom-right-radius" r values: `(,s-r))
	    (att "-moz-border-radius-bottomright" r values: `(,s-r))))
      (++ (att "border-bottom-right-radius" r)
	  (att "-moz-border-radius-bottomright" r))))
(define (box-shadow h-off v-off blur-r color #!key (inset #f) (spread-radius #f))
  (fold (lambda (a o)
	  (++ o (att a
		     (if inset "inset" h-off)
		     values: (cond ((and inset spread-radius) `(,h-off ,v-off ,spread-radius ,blur-r ,color))
				   (inset `(,h-off ,v-off ,blur-r ,color))
				   (spread-radius `(,v-off ,spread-radius ,blur-r ,color))
				   (#t `(,v-off ,blur-r ,color))))))
	""
	'("box-shadow" "-webkit-box-shadow" "-moz-box-shadow")))
(define (font-family f) (att "font-family" f))
(define (margin m) (att "margin" m))
(define (margin-top m) (att "margin-top" m))
(define (margin-bottom m) (att "margin-bottom" m))
(define (margin-left m) (att "margin-right" m))
(define (margin-right m) (att "margin-right" m))
(define (text-align a) (att "text-align" a))
(define (line-height h) (att "line-height" h))
(define (width w) (att "width" w))
(define (height h) (att "height" h))
(define (float f) (att "float" f))
(define (padding p) (att "padding" p))
(define (padding-top p) (att "padding-top" p))
(define (padding-bottom p) (att "padding-bottom" p))
(define (padding-left p) (att "padding-left" p))
(define (padding-right p) (att "padding-right" p))
(define (font-size s) (att "font-size" s))
(define (font-style s) (att "font-style" s))
(define (font-weight w) (att "font-weight" w))
(define (text-decoration d) (att "text-decoration" d))
(define (border s t c . r)
  (if (> (length r) 0)
      (att "border" s values: `(,t ,c ,(first r) ,(second r) ,(third r)))
      (att "border" s values: `(,t ,c))))
(define (border-top s t c) (att "border-top" s values: `(,t ,c)))
(define (border-bottom s t c) (att "border-bottom" s values: `(,t ,c)))
(define (border-left s t c) (att "border-left" s values: `(,t ,c)))
(define (border-right s t c) (att "border-right" s values: `(,t c)))
(define (position p) (att "position" p))
(define (z-index i) (att "z-index" i))
(define (css-display d) (att "display" d))
(define (clear c) (att "clear" c))
(define (cursor c) (att "cursor" c)))