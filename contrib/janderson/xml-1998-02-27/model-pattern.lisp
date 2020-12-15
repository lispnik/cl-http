(in-package :xml-parser)

(defMethod pattern&datum->continue
           ((pattern dtd-element-declaration) (datum xml-datum) succeed fail result)
  (if (pattern&datum/types? pattern datum)
    (pattern&datum/relations->continue
          (dtd-element-declaration.model pattern)
          datum
          succeed
          fail
          result)
    (pattern-fail pattern datum succeed fail result 'pattern&datum->continue)))



(defMethod model&datum
           ((model dtd-model-group) (datum list))
  (model&datum-sequence datum
                       (first (dtd-model.content model))
                       (dtd-model.occurrence (first (dtd-model.content model)))
                       (or (dtd-model.connector model) *seq-marker*)
                       (rest (dtd-model.content model))))

(defMethod model&datum
           ((datum xml-element) (model dtd-model-group))
  (when (or (null (dtd-model.name model))
            (eq (dtd-model.name model) (xml-element.name datum)))
    (model&datum model (xml-element.content datum))))

(defMethod model&datum
           ((model t) (datum xml-element))
  (model&datum model (xml-element.content datum)))



(defMethod model&datum-element
           ((datum xml-element) (model dtd-element-model))
  (or (null (dtd-model.name model))
      (eq (dtd-model.name model) (xml-element.name datum))))

(defMethod model&datum-element
           ((datum xml-element) (model dtd-model-group))
  (if (or (null (dtd-model.name model))
          (eq (dtd-model.name model) (xml-element.name datum)))
    (model&datum-sequence (xml-element.content datum)
                         nil nil
                         (dtd-model.connector model)
                         (dtd-model.content model))))

(defMethod model&datum-sequence
           ((datum t)
            (element null) (occurrence null)
            (connector t) (rest cons))
  (setf element (first rest))
  (setf occurrence (dtd-model.occurrence element))
  (model&datum-sequence datum element occurrence connector (rest rest)))


;; one of four effects:
;; advance model, advance data, succeed, fail

(defMethod model&datum-sequence
           ((datum cons)
            (element dtd-element-model) (occurrence (eql *rep-marker*))
            (connector (eql *or-marker*)) (rest t))
  ; <x>*|... ? advance-data : advance-model
  (if (model&datum-element (first datum) element)
    (model&datum-sequence (rest datum) element occurrence connector rest)
    (model&datum-sequence datum (first rest) (first rest)
                         connector (rest rest))))

(defMethod model&datum-sequence
           ((datum cons)
            (element dtd-element-model) (occurrence (eql *opt-marker*))
            (connector (eql *or-marker*)) (rest t))
  ; <x>?|... ? advance-model&data : advance-model
  (if (model&datum-element (first datum) element)
    (model&datum-sequence (rest datum)
                         (first rest) (first rest)
                         connector (rest rest))
    (model&datum-sequence datum
                         (first rest) (first rest)
                         connector (rest rest))))

(defMethod model&datum-sequence
           ((datum cons)
            (element dtd-element-model) (occurrence (eql *plus-marker*))
            (connector (eql *or-marker*)) (rest t))
  ; <x>+|... ? advance-data : advance-model
  (if (model&datum-element (first datum) element)
    (model&datum-sequence (rest datum) element occurrence connector rest)
    (model&datum-sequence datum (first rest) (first rest)
                         connector (rest rest))))

(defMethod model&datum-sequence
           ((datum cons)
            (element dtd-element-model) (occurrence number)
            (connector (eql *or-marker*)) (rest t))
  ; <x>#|... ? ((# < 1) ? advance-data : succeed) : advance-model
  (if (model&datum-element (first datum) element)
    (if (plusp (decf occurrence))
      (model&datum-sequence (rest datum) element occurrence connector rest)
      t)
    (model&datum-sequence datum
                         (first rest) (first rest)
                         connector (rest rest))))

(defMethod model&datum-sequence
           ((datum cons)
            (element dtd-element-model) (occurrence (eql *rep-marker*))
            (connector (eql *seq-marker*)) (rest t))
  ; <x>*,... ? advance-data : advance-model
  (if (model&datum-element (first datum) element)
    (model&datum-sequence (rest datum) element occurrence connector rest)
    (model&datum-sequence datum (first rest) (first rest)
                         connector (rest rest))))

(defMethod model&datum-sequence
           ((datum cons)
            (element dtd-element-model) (occurrence (eql *opt-marker*))
            (connector (eql *seq-marker*)) (rest t))
  ; <x>?,... ? advance-model&data : advance-model
  (if (model&datum-element (first datum) element)
    (model&datum-sequence (rest datum)
                         (first rest) (first rest)
                         connector (rest rest))
    (model&datum-sequence datum
                         (first rest) (first rest)
                         connector (rest rest))))

(defMethod model&datum-sequence
           ((datum cons)
            (element dtd-element-model) (occurrence (eql *plus-marker*))
            (connector (eql *seq-marker*)) (rest t))
  ; <x>+,... ? advance-model&data : fail
  (if (model&datum-element (first datum) element)
    (or (model&datum-sequence (rest datum) element occurrence connector rest)
        (model&datum-sequence (rest datum)
                             (first rest) (first rest)
                             connector rest))
    nil))

(defMethod model&datum-sequence
           ((datum cons)
            (element dtd-element-model) (occurrence number)
            (connector (eql *seq-marker*)) (rest t))
  ; <x>#,... ? advance-model&data : fail
  (if (model&datum-element (first datum) element)
    (if (plusp (decf occurrence))
      (model&datum-sequence (rest datum) element occurrence connector rest)
      (model&datum-sequence (rest datum)
                           (first rest) (first rest)
                           connector (rest rest)))
    nil))

(defMethod model&datum-sequence
           ((datum cons)
            (element dtd-model-group) (occurrence number)
            (connector (eql *seq-marker*)) (rest t))
  ; <x>#,... ? advance-model&data : fail
  (if (model&datum-sequence datum
                           nil nil
                           (dtd-model.connector element)
                           (dtd-model.content element))
    (if (plusp (decf occurrence))
      (model&datum-sequence (rest datum) element occurrence connector rest)
      (model&datum-sequence (rest datum)
                           (first rest) (first rest)
                           connector (rest rest)))
    nil))

(defMethod model&datum-sequence
           ((datum null) (element null) (occurrence t)
            (connector (eql *seq-marker*)) (rest null))
  t)
(defMethod model&datum-sequence
           ((datum null) (element null) (occurrence t)
            (connector (eql *or-marker*)) (rest null))
  nil)

(defMethod model&datum-sequence
           ((datum t) (element dtd-model) (occurrence dtd-model)
            (connector t) (rest t))
  (model&datum-sequence datum element (dtd-model.occurrence occurrence)
                       connector rest))

(defMethod model&datum-sequence
           ((datum t) (element t) (occurrence t)
            (connector t) (rest t))
  nil)


#|

(model&datum #!m((a | c), (b | c)*) #!<test><a>...</a><b>b</b><c>c</c><b>b</b></test>)
(model&datum #!m((b* | c)*) #!<test><b>b</b><c>c</c><b>b</b></test>)

(model&datum #!m(a*, c) #!<test><a>...</a><c>---</c></test>)
(model&datum #!m(a*, c) #!<test><c>---</c></test>)
(trace model&datum model&datum-sequence)
|#
