;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname html) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

;; html-parser.rkt

;; Data definitions:

(define-struct elt (name data subs))
;; Element is (make-elt String String ListOfElement)
;; interp. An HTML element with tag name, text content (or empty string if none),
;;         and a list of sub-elements (for nested tags).

;; ListOfElement is one of:
;;  - empty
;;  - (cons Element ListOfElement)
;; interp. A list of HTML elements

;; Examples of HTML elements:

(define P1 (make-elt "p" "This is a <p> (div#1)." empty))
(define A1 (make-elt "a" "This is a <a> (div#1)." empty))
(define D1 (make-elt "div" "" (list P1 A1)))

(define P3 (make-elt "p" "This is is a <p> (div#2)." empty))
(define P4 (make-elt "p" "This is is a <p> (div#2 > div#3 !)." empty))
(define D3 (make-elt "div" "" (list P4)))
(define D2 (make-elt "div" "" (list P3 D3)))

(define P5 (make-elt "p" "This is a <p> (body)." empty))

(define HEAD1 (make-elt "head" "" empty))
(define BODY1 (make-elt "body" "" (list D1 D2 P5)))

(define HTML1 (make-elt "html" "" (list HEAD1 BODY1)))

;; Functions:

;; render-element : Element -> Image
;; Produces an image representation of the given HTML element
(define (render-element e)
  (cond
    [(string=? (elt-name e) "p")
     ;; Render paragraph as black text
     (text (elt-data e) 12 'black)]
    [(string=? (elt-name e) "a")
     ;; Render link as blue text
     (text/font (elt-data e) 12 'blue #f 'roman 'normal 'normal #t)]
    [(string=? (elt-name e) "div")
     ;; Render div by stacking sub-elements
     (render-elements (elt-subs e))]
    [(string=? (elt-name e) "body")
     ;; Render body by stacking sub-elements
     (render-elements (elt-subs e))]
    [else
     ;; Handle unknown or unsupported elements
     (text "Unsupported element" 12 'red)]))

;; render-elements : ListOfElement -> Image
;; Produces an image representation of a list of HTML elements
(define (render-elements elems)
  (cond
    [(empty? elems) empty-image]
    [else
     (above (render-element (first elems))
            (render-elements (rest elems)))]))

;; render-html : Element -> Image
;; Renders the given HTML structure as an image
(define (render-html html)
  (render-elements (elt-subs html)))

;; Example usage:
(render-html HTML1)