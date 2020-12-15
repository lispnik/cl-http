;;; -*- package: "XML-PARSER"; -*-
;;;

"
<DOCUMENTATION>
 <DESCRIPTION>
  </DESCRIPTION>
 <COPYRIGHT HREF='defsystem.lisp|root().descendant(1,COPYRIGHT)'/>
 <CHRONOLOGY>
  <DELTA><DATE>19971125</DATE>
   XML-USER package </DELTA>
  <DELTA><DATE>19971203</DATE>
   method for <CODE>PARSE-POSITION</CODE> for use in error or
   other messages.</DELTA>
  <DELTA><DATE>19971205</DATE>jaa<BR/>
   added HANDLER-CASE to primary interface functions READ-PROCESS-PCDATA,
   READ-PROCESS-CDATA
  <DELTA><DATE>19971217</DATE>
   <UL><LI>'%' is a terminating character
   <LI>added read-entity-data to distinguish the effect of '%' from its
    behaviour in pcdata
   </UL>
   </DELTA>
  <DELTA><DATE>19980108</DATE>
   made READ-CONTENT-MODE robust in the presence of SGML syntax
   <DELTA>
  <DELTA><DATE>19980317</DATE>
         <MAILTO HREF='burke@cs.uchicago.edu'>rb</MAILTO>
   patch for allegro concatenated streams
   </DELTA>
  <DELTA><DATE>19980320</DATE>
   markup type readtable maps '|' to a token (even though xml-1.0 does not
   allow multiple names for attlists, etc.
   <BR>
   coerce -- tag to xml::--, otherwise comments arent't permitted in other
   packages.
  <DELTA><DATE>19980407</DATE>
   added package spec to read-markup-model</DELTA>
  <DELTA><DATE>19980618</DATE>
   export token</DELTA>
  <DELTA><DATE>19980808</DATE>
   wd-xml-names-19980802: added a token reader which supports single-colon symbol
   qualification. used as basis for a list reader (for models) and an attribute reader
   for elements etc. included checks for entities.
   changed INTERN-SYMBOL to search the dynamic bindings before the global package
   names.
   package creation generates a unique proper name and shifts the uri to a
   nickname
   </DELTA>
  <DELTA><DATE>19981024</DATE>
   previous-node -> processed-node to indicate the circumstances
   </DELTA>
  <DELTA><DATE>19981215</DATE>
   the enumeration forms specify a '|' separator for notation types and
   normal enumerated attribute values.</DELTA>
  <DELTA><DATE>19981218</DATE>
   fixed read-entity-value to include character references - it had been
   treating them as if they were general entity references and skipping
   them.<BR/>
   uniformly reduce all entity references: the respective method now checks
   the context and raises a condition or inserts the content in the input
   stream, as appropriate.</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")

"
<H3>READ-PRODUCTION</H3>
<P>
the READ-PRODUCTION methods for MARKUP and MARKUPDECL constitute the core
of the parser. they check for and parse the categories of markup permitted
in the respective contexts. each also checks for and expands the
respectively permitted entity references.</P>
<P>
the parse devolves through recursive calls to READ-PRODUCTION, which
determines the actual category of optional elements and evokes
PRODUCTION-READER-MACRO as appropriate. READ-PRODUCTION skips whitespace
where appropriate - outside of the primary element and within a dtd - and
always reads the first character of the entity itself. thus the name distinction:
PRODUCTION-READER-MACRO expects to be invoked as if a readtable macro,
whereby the dispatch character has already been read.</P>
<P>
the first three methods implement the distinction among
<UL>
 <LI>'documentMarkup', which must be a markup entity in the content of the primary
     document entity, and which precludes reference forms and skips whitespace</LI>
 <LI>'elementMarkup', which, as the content of the root element, accepts
     references in addition to the document markup entities, and is
     whitespace-sensitive, and</LI>
 <LI>'markupdecl' which recognizes entities in a document type declaration.</LI>
</UL>
</P>
"

(defMethod read-production
           ((production (eql 'xml-1.0::documentMarkup)) (stream t)
            &aux char entity)
  "effectively:
   [1] documentMarkup ::= (XMLDecl? Misc* (doctypedecl Misc*)?) element Misc*
   which we extend for general entities. this is, strictly speaking, too liberal,
   as a general entity could well expand into an entity from the prolog, which
   would not be quite right..."
  (setf char (peek-char t stream))
  (cond
   ;; a marked-up entity is read and returned
   ((char= char *tag-open-char*)
    (read-char stream)
    (setf entity (production-reader-macro 'xml-1.0::markup stream))
    (unless (typep entity 'document-markup)
      (xml-form-error production
                      "erroneous entity: ~s."
                      entity))
    entity)

   ;; otherwise the character is unrecognized, which, by default, is a terminal
   ;; error. if not just keep reading.
   (t
    (raise-xml-condition production *character-category-condition*
                         :character char
                         :category (list *tag-open-char* #\space))
    (read-production production stream))))


(defMethod read-production
           ((production (eql 'xml-1.0::elementMarkup)) (stream t)
            &aux char entity)
  "read 'parsed character data'. generate text- or element-node instances.
   note that, when so specified, the 'text-node' may be a string."
  (setf char (peek-char (not *preserve-whitespace*) stream))
  (cond
   ;; a marked-up entity is read and returned
   ((char= char *tag-open-char*)
    (read-char stream)
    ;; parse anything which is, in general, permitted in a document. the
    ;; context determines whether the result is legal
    (setf entity (production-reader-macro 'xml-1.0::markup stream))
    (unless (typep entity 'content-markup)
      (xml-form-error production
                      "erroneous entity: ~s."
                      entity))
    entity)

   ;; if it's either a general entity reference or some other legitimate character
   ;; data, then read it as a charactr node / string
   ((or (xml-char? char) (char= char *general-entity-reference-open-char*))
    (read-production 'xml-1.0::charData stream))

   ;; otherwise the character is unrecognized, which, by default, is a terminal
   ;; error. if not just keep reading.
   (t
    (raise-xml-condition production *character-category-condition*
                         :character char
                         :category (list *tag-open-char*
                                         *general-entity-reference-open-char*
                                         'xml-1.0::Char))
    (read-production production stream))))


(defMethod read-production
           ((production (eql 'xml-1.0::markupdecl)) stream
            &aux decl char)
  "strictly speaking:
  [29]  markupdecl ::= elementdecl | AttlistDecl
                       | EntityDecl | NotationDecl
                       | PI | Comment
  which we extend to include entity references directly
  and to terminate if a section end is read."

  (flet ((read-marker-string (&aux (buffer "   "))
           (dotimes (i 3) (setf (aref buffer i) (read-char stream)))
           buffer))
    ;; skip whitespace in declaration markup
    (setf char (peek-char t stream))
    (cond
     ;; if a declaration start, then read and return a declaration form.
     ((char= char *declaration-open-char*)
      (read-char stream)
      (setf decl (production-reader-macro 'xml-1.0::markupdecl stream))
      (cond
       ;; if parsing is suppressed, just keep reading
       (*parse-suppress*
        (read-production production stream))
       ;; otherwise, check that the resulting form conforms to the non-terminal
       ;; syntax. if so, return it.
       ((typep decl 'declaration-markup)
        decl)
       ;; anything else is an error
       (t
        (xml-form-error production
                        "erroneous entity present: ~s." decl)
        (read-production production stream))))

     ;; a parameter entity is handled by parsing it, which augments the
     ;; stream as a side-effect. just continue to parse with the
     ;; augmented stream
     ((maybe-parse-parameter-entity-reference stream char))

     ;; a section end raises the appropriate condition.
     ;; nb. in the external subset, the first ']' must start the end of a
     ;; conditional section, while in th einternal subset the single ']' ends
     ;; the internal subset and is simply skipped.
     ((char= char *end-section-char*)
      (if *in-external-subset*
        (unless (string-equal "]]>" (setf decl (read-marker-string)))
          (xml-form-error production
                          "erroneous section marker: ~s." decl))
        (read-char stream))
      (section-end))

     ;; otherwise the character is unrecognized, which, by default, is a terminal
     ;; error. if not just keep reading.
     (t
      (raise-xml-condition production *character-category-condition*
                           :character char
                           :category (list *tag-open-char*
                                           *parameter-entity-reference-open-char*
                                           *end-section-char*))
      (read-production production stream)))))




(defMethod production-reader-macro
           ((production (eql 'xml-1.0::markup)) (stream t)
            &aux char)
  (setf char (peek-char nil stream))
  (case char
    ;; if it looks like a declaration/section or a processing instruction
    ;; then parse it. 
    ((#\! #\?)
     (production-reader-macro 'xml-1.0::markupdecl stream))

    ;; otherwise check if it is an end tag
    (#\/ (production-reader-macro 'xml-1.0::Etag stream))

    ;; otherwise try to read it as an element
    (t (production-reader-macro 'xml-1.0::element stream))))

(defMethod production-reader-macro
       ((production (eql 'xml-1.0::markupdecl)) (stream t)
        &aux char name)
  "entity reader macro for declaration content:
   markup declarations / pi's / sections
   ... anomolously includes CDATA section, since the syntax is so close that
   it's easier to distinguish permissibility here."
  (setf char (peek-char nil stream))
  (case char
    ; either declaration form or section: skip the '!',
    ; check for a section marker; read the operator and dispatch on it
    (#\!                               
     (read-char stream)
     (setf char (peek-char nil stream))
     (case char
       ;; skip the section marker; read and dispatch on the
       ;; INCLUDE / IGNORE / CDATA
       (#\[
        (read-char stream)
        (setf name (read-name-token stream))
        (setf char (peek-char t stream))
        (unless (char= char #\[)
          (xml-form-error 'xml-1.0::ConditionalSect "erroneous character: ~s." char))
        (read-char stream)
        (cond ((and *in-external-subset* (string-equal name "INCLUDE"))
               (production-reader-macro 'xml-1.0::includeSect stream))
              ((and *in-external-subset* (string-equal name "IGNORE"))
               (production-reader-macro 'xml-1.0::ignoreSect stream))
              ((and (not (in-dtd?)) (string-equal name "CDATA"))
               (production-reader-macro 'XML-1.0::CDSect stream))
              (t
               (xml-form-error production
                               "erroneous conditional section: ~s." name))))

       ; otherwise it's a ENTITY, ELEMENT, etc
       (t                               
        (markup-declaration-reader-macro stream #\!))))

    ; a processing instruction (possibly reserved)
    (#\?                                
     (read-char stream)
     (processing-instruction-reader-macro stream))

    ;; otherwise the character is unrecognized, which, by default, is a terminal
    ;; error. if not just keep reading.
    (t
     (raise-xml-condition production *character-category-condition*
                          :character char
                          :category '(#\! #\?))
     (read-production production stream))))


(defMethod read-production
           ((production (eql 'xml-1.0::content)) (stream t)
            &aux
            element (elements nil))
  (flet ((collect-content (e)
           (unless *parse-suppress* (push e elements))))
    (loop (setf element (read-production 'xml-1.0::elementMarkup stream))
          (cond
           
           ;; comments are already cached in the document instance as a side
           ;; effect of parsing
           ((typep element 'comment) )
           
           ;; processing instructions are ignored
           ((typep element 'processing-instruction) )

           ;; two alternatives are supported to indicate that the content is
           ;; complete, either the start tag or an end tag appears.
           ((or (typep element 'end-tag) (eq element *parent-node*))
            (return))
           
           ;; simple content is simply included
           ((or (typep element 'string)
                (typep element 'content-markup))
            (collect-content element))           
           
           ;; otherwise it is an error
           (t
            (raise-xml-condition production
                                 *element-content-condition*
                                 "erroneous content: ~s." element))))
    
    ;; return the elements and 
    (values (nreverse elements) element)))
  


"
<H3>Entity References</H3>
<P>
once the #\& or #\% has been read, read the entity name and generate a
reference to that entity. in the case of character a entity, simply return
the intended entity.</P>
<P>
the conditional (MAYBE-PARSE-*-ENTITY) functions are intended to be used when
parsing a context where a reference MAY occur. several versions are provided -
one for each of the specified permissibilitiy combinations.
<PRE>
content:               maybe-parse-general-entity-or-char-reference
  included:            internal general, external general, character;
  not recognized:      parameter;
  forbidden:           external unparsed.
in attribute value:    maybe-parse-general-entity-or-char-reference
  the parsing per-se does not distinguish a reference to an entity which may turn
  out to be an unparsed external, but a check is performed when the reference
  is instantiated.
  included:            internal general, character;
  not recognized:      parameter;
  forbidden:           external general.
as attribute value:    initialize-instance (attribute)
  when an attribute with the appropriate declaration is instantiated,
  interned:            external unparsed;
  not recognized:      parameter, character;
  forbidden:           internal general, external general.
in entity value:       maybe-parse-parameter-entity-or-char-reference
  included:            parameter, character;
  not recognized:      internal general, external general;
  forbidden:           external unparsed.
in dtd:                maybe-parse-parameter-entity-reference
  the restriction is observed in that the '&' character precluded in declaration markup
  included:            parameter;
  not recognized:      ;
  forbidden:           internal general, external general, external unparsed, character.

</PRE>

all maybe-* functions return T, if the respective form of reference was present,
or, in the case of a character reference, the denoted character. if a named entity
was present, it has either been expanded into the stream, or a signal was raised to
indicate that it reference was not found.
</P>
"

(defMethod read-production
           ((production (eql 'xml-1.0::PEReference)) stream)
  "read the entity reference, resolve it to a value"
  (parameter-entity-ref-reader-macro stream (read-char stream)))

(defMethod read-production
           ((production (eql 'xml-1.0::Reference)) stream &aux char)
  (setf char (read-char stream))
  (if (char= (peek-char nil stream) *character-reference-open-char*)
    (character-ref-reader-macro stream char)
    (general-entity-ref-reader-macro stream char)))

(defMethod read-production
           ((production (eql 'xml-1.0::EntityRef)) stream)
  (general-entity-ref-reader-macro stream (read-char stream)))



(defun maybe-parse-parameter-entity-reference
       (stream char)
  (when (char= char *parameter-entity-reference-open-char*)
    (read-char stream)
    (parameter-entity-ref-reader-macro stream char)
    t))

(defun maybe-parameter-entity-or-char-reference-reader-macro
       (stream char)
  (cond ((char= char *parameter-entity-reference-open-char*)
         (parameter-entity-ref-reader-macro stream char)
         t)
        ((char= char *general-entity-reference-open-char*)
         (when (char= (peek-char nil stream) *character-reference-open-char*)
           (character-ref-reader-macro stream char)))))



(defun parameter-entity-ref-reader-macro
       (stream char
        &aux reference)
  "read the entity reference, generate a reference instance."
  (declare (ignore char))
  (flet ((name-error ()
           (xml-form-error 'xml-1.0::PEReference
                           "erroneous character name: ~s." reference)))
    (setf reference (read-entity-reference-name-string stream))
    (cond ((zerop (length reference))
           (name-error))
          (t
           (reduce-production (parameter-entity-reference reference)
                              :stream stream)))))


(defun character-ref-reader-macro
       (stream char &aux reference value)
  "reader macro for character references"
  (declare (ignore char))
  (flet ((name-error ()
           (xml-form-error 'xml-1.0::charRef
                           "erroneous character name: ~s." reference)))
    (setf reference (read-char-reference-name-string stream))
    (cond ((<= (length reference) 1)
           (name-error))
          ((char-equal *character-reference-open-char* (char reference 0))
           ;;; &#<number>; is a numeric entity reference
           (setf value (if (char-equal #\x (elt reference 1))
                         (parse-integer reference :start 2 :radix 16
                                        :junk-allowed t)
                         (parse-integer reference :start 1 :radix 10
                                        :junk-allowed t)))
           (unless value (name-error))
           (code-char value))
          (t
           (name-error)))))


(defun general-entity-ref-reader-macro
       (stream char &aux reference)
  "reader macro for restricted &-entities markup streams"
  (declare (ignore char))
  (flet ((name-error ()
           (xml-form-error 'xml-1.0::EntityRef
                           "erroneous character name: ~s." reference)))
    (setf reference (read-entity-reference-name-string stream))
    (cond ((zerop (length reference))
           (name-error))
          (t
           ;;; &<name>; is a named entity reference
           (reduce-production (general-entity-reference reference)
                              :stream stream)))))

;;;
;;; reader macros for various generic non-terminals

(defMethod markup-declaration-reader-macro
           ((stream t) (char t) &aux declaration-op)
  (setf declaration-op (read-name-string stream))
  (cond
   ;; if a comment appears, then a name could not be read and the comment
   ;; open ( "--" ) remaisn to be read.
   ((and (zerop (length declaration-op)) (char= (peek-char nil stream) #\-))
    (read-char stream)
    (if (char= (setf char (read-char stream)) #\-)
      (production-reader-macro 'xml-1.0::comment stream)
      (raise-xml-condition 'xml-1.0::markupdecl
                           *character-category-condition*
                           :character char :category "-")))

   ;; otherwise it must be a known operator
   ((string-equal declaration-op "ATTLIST")
    (production-reader-macro 'xml-1.0::AttlistDecl stream))
   ((string-equal declaration-op "DOCTYPE")
    (production-reader-macro 'xml-1.0::doctypedecl stream))
   ((string-equal declaration-op "ENTITY")
    (production-reader-macro 'xml-1.0::EntityDecl stream))
   ((string-equal declaration-op "ELEMENT")
    (production-reader-macro 'xml-1.0::ElementDecl stream))
   ((string-equal declaration-op "NOTATION")
    (production-reader-macro 'XML-1.0::NotationDecl stream))
   (t
    (xml-form-error 'xml-1.0::markupdecl
                    "erroneous declaration operator: ~s."
                    declaration-op))))

(defMethod processing-instruction-reader-macro
           ((stream t) &aux (target (read-markup-tag-type stream)))
  (cond ((string-equal target "xml")
         (production-reader-macro 'XML-1.0::XMLDecl stream))
        (t
         (pi-target-reader-macro target stream))))


;;;
;;; production-specific routines to parse the attributes present within the
;;; respective tag.


(defMethod read-entity-attributes
           ((production (eql 'xml-1.0::AttlistDecl)) stream
            &aux name attributes (element nil) (char nil))
  (handler-bind ;; augment error message for context
    ((error #'(lambda (condition)
                (xml-warn 'xml-1.0::AttDef* "name: ~s, attdef: ~s."
                          name attributes)
                condition)))
    (flet ((clause-error (clause value)
             (xml-form-error production
                             "erroneous declaration clause: ~s = ~s."
                             clause value))
           (collect-content (e)
             (unless *parse-suppress* (push e attributes))))
      (flet ((read-attribute ()
               (setf char (peek-char t stream))
               (cond ((char= *tag-close-char* char)
                      (read-char stream)
                      nil)
                     ((or (char= char #\') (char= char #\"))
                      (setf element (read-attribute-value #'xml-char? stream))
                      (if (stringp element)
                        (collect-content element)
                        (clause-error 'xml-1.0::AttributeValue element)))
                     ((reserved-name-indicator-p char)
                      (setf element (read-reserved-name-string stream))
                      (if (and (stringp element) (plusp (length element)))
                        (collect-content element)
                        (clause-error 'xml-1.0::DefaultDecl element)))
                     ((char= char #\()
                      (read-char stream)
                      (setf element (read-name-list #\) stream))
                      (collect-content element))
                     (t
                      (setf element (read-name-token stream))
                      (if (and element (symbolp element))
                        (collect-content element)
                        (clause-error 'xml-1.0::Name element))))))
        
        (setf name (read-name-token stream))
        (loop (unless (read-attribute) (return)))
        (values name (nreverse attributes))))))

(defMethod read-entity-attributes
           ((production (eql 'XML-1.0::NotationDecl)) stream
            &aux attribute-name name system-id public-id peek)
  (setf name (read-markup-tag-type stream))
  (loop
    (setf peek (peek-char t stream))
    (when (eql *tag-close-char* peek) (read-char stream) (return))
    (setf attribute-name (read-name-token stream))
    (cond ((null attribute-name) (return))
          ((and (string-equal attribute-name "SYSTEM")
                (not system-id) (not public-id))
           (setf system-id (read-system-literal stream)))
          ((and (string-equal attribute-name "PUBLIC")
                (not system-id) (not public-id))
           (setf public-id (read-public-literal stream))
           ;; notation permits an external id or a public id, which
           ;; makes the system literal optional here
           (setf peek (peek-char t stream))
           (when (or (char-equal peek #\') (char-equal peek #\"))
             (setf system-id (read-system-literal stream))))
          (t
           (xml-form-error production
                           "erroneous notation attribute: ~a."
                           attribute-name))))
  (values name system-id public-id))


(defMethod read-entity-attributes
           ((production (eql 'xml-1.0::doctypedecl)) stream
            &aux attribute-name name system-id public-id peek
            declarations)
  (setf name (read-markup-tag-type stream))
  (loop
    (setf peek (peek-char t stream))
    (cond ((eql *tag-close-char* peek)
           (read-char stream)
           (return))
          ((eql *start-section-char* peek)
           (read-char stream)
           (if declarations
             (xml-form-error production "duplicate declarations.")
             (setf declarations
                   (read-production 'xml-1.0::intSubset stream))))
          (t
           (setf attribute-name (read-name-token stream))
           (cond ((null attribute-name) (return))
                 ((and (string-equal attribute-name "SYSTEM")
                       (not system-id) (not public-id) (not declarations))
                  (setf system-id (read-system-literal stream)))
                 ((and (string-equal attribute-name "PUBLIC")
                       (not system-id) (not public-id) (not declarations))
                  (setf public-id (read-public-literal stream))
                  (setf system-id (read-system-literal stream)))
                 (t
                  (xml-form-error production
                                  "erroneous notation attribute: ~a."
                                  attribute-name))))))
  (values name system-id public-id declarations))

(defMethod read-entity-attributes
           ((production (eql 'xml-1.0::PEDecl)) stream
            &aux attribute-name name system-id public-id definition char)
  (setf name (read-markup-tag-type stream))
  (loop
    (setf char (peek-char t stream))
    (cond ((char= *tag-close-char* char)
           (read-char stream)
           (return))
          ((and (or (char= #\" char) (char= #\' char))
                (null definition)
                (null system-id)
                (null public-id))
           (setf definition (read-entity-value stream))
           (unless (and definition (typep definition 'sequence))
             (xml-form-error production
                             "erroneous entity definition: ~s."
                             definition)))
          ((null definition)
           (setf attribute-name (read-name-token stream))
           (cond ((and (string-equal attribute-name "SYSTEM")
                       (not system-id) (not public-id) (not definition))
                  (setf system-id (read-system-literal stream)))
                 ((and (string-equal attribute-name "PUBLIC")
                       (not system-id) (not public-id) (not definition))
                  (setf public-id (read-public-literal stream))
                  (setf system-id (read-system-literal stream)))
                 (t
                  (xml-form-error production
                                  "erroneous entity attribute: ~a."
                                  attribute-name))))
          (t
           (raise-xml-condition production
                                *character-category-condition*
                                :character char
                                :category (cond
                                           (definition
                                             (list *tag-close-char*))
                                           ((or system-id public-id)
                                            'xml-1.0::nameChar)
                                           (t
                                            '(#\" #\' xml-1.0::nameChar)))))))

  (values name definition system-id public-id))

(defMethod read-entity-attributes
           ((production (eql 'xml-1.0::GEDecl)) stream
            &aux
            attribute-name name definition system-id public-id notation char)
  (setf name (read-markup-tag-type stream))
  (loop
    (setf char (peek-char t stream))
    (cond ((char= *tag-close-char* char)
           (read-char stream)
           (return))
          ((and (or (char= #\" char) (char= #\' char))
                (null definition)
                (null system-id)
                (null public-id))
           (setf definition (read-entity-value stream))
           (unless (and definition (typep definition 'sequence))
             (xml-form-error production
                             "erroneous entity definition: ~s."
                             definition)))
          ((null definition)
           (setf attribute-name (read-name-token stream))
           (cond ((and (string-equal attribute-name "SYSTEM")
                       (not system-id) (not public-id) (not definition))
                  (setf system-id (read-system-literal stream)))
                 ((and (string-equal attribute-name "PUBLIC")
                       (not system-id) (not public-id) (not definition))
                  (setf public-id (read-public-literal stream))
                  (setf system-id (read-system-literal stream)))
                 ((and (string-equal attribute-name "NDATA")
                       (or system-id public-id))
                  (setf notation (read-notation-literal stream)))
                 (t
                  (xml-form-error production
                                  "erroneous entity attribute: ~a."
                                  attribute-name))))
          (t
           (raise-xml-condition production
                                *character-category-condition*
                                :character char
                                :category (cond
                                           (definition
                                             (list *tag-close-char*))
                                           ((or system-id public-id)
                                            'xml-1.0::nameChar)
                                           (t
                                            '(#\" #\' xml-1.0::nameChar)))))))
  (values name definition system-id public-id notation))
;(read-entity-attributes  'xml-1.0::GEDecl (encoded-stream "test 'asdf'"))

(defMethod read-entity-attributes
           ((production (eql 'XML-1.0::elementdecl)) (stream t)
            &aux name model nc)
  (setf name (read-markup-tag-type stream)
        model (read-markup-model stream))
  (setf nc (peek-char t stream))
  (unless (char= *tag-close-char* nc)
    (xml-form-error production
                    "erroneous character: ~s."
                    nc))
  (read-char stream)
  (values name model))


(defMethod read-entity-attributes
           ((production (eql 'xml-1.0::Stag)) (stream t)
            &aux name attributes char)
  (setf name (read-name-string stream))
  (multiple-value-setq (attributes char) (read-Stag-attributes stream))
  (values name attributes char))

(defun read-stag-attributes
       (stream
        &aux attributes attribute-name attribute-value (char nil)
        (production 'xml-1.0::attributes))
  (flet ((character-error (char category)
           (raise-xml-condition production
                                *character-category-condition*
                                :character char
                                :category category))
         (clause-error (clause value)
           (xml-form-error production
                           "erroneous tag clause: ~s = ~s."
                           clause value))
         (collect-content (e)
           (unless *parse-suppress* (push e attributes))))
    (flet ((read-attribute ()
             (setf char (peek-char t stream))
             (cond ((char= *tag-close-char* char)
                    (read-char stream)
                    nil)
                   ((char= *empty-tag-close-char* char)
                    ;; tag for empty element
                    (read-char stream)
                    (unless (char= *tag-close-char* (setf char (read-char stream)))
                      (character-error char (list *tag-close-char*)))
                    (setf char *empty-tag-close-char*)
                    nil)
                   ((xml-initial-namechar? char)
                    (setf attribute-name (read-name-string stream))
                    (unless (stringp attribute-name)
                      (clause-error 'xml-1.0::Name attribute-name))
                    (collect-content attribute-name)
                    (unless (char= *attribute-equals-char*
                                   (setf char (read-char stream)))
                      (character-error char (list *attribute-equals-char*)))
                    (setf char (peek-char t stream))
                    (unless (or (char= char #\') (char= char #\"))
                      (character-error char '(#\' #\")))
                    (unless (setf attribute-value
                                  (read-attribute-value #'xml-char? stream))
                      (clause-error 'xml-1.0::Attribute attribute-value))
                    (when (namespace-attribute-name? attribute-name)
                      (assert-namespace-binding
                       (namespace-attribute-name.prefix attribute-name)
                       attribute-value))
                    (collect-content attribute-value))
                   (t
                    (character-error char (list *tag-close-char*
                                                *empty-tag-close-char*
                                                'xml-1.0::nameChar))))))
      
      (loop (unless (read-attribute) (return)))
      
      ;; return the '>' or the '/' to indicate whether its an empty-element tag
      (values (nreverse attributes) char))))

(defMethod read-entity-attributes
           ((production (eql 'xml-1.0::XMLDecl)) (stream t)
            &aux encoding version standalone char attribute-name)
  (flet ((read-equals (&aux char)
           (unless (eql #\= (setf char (read-char stream)))
             (raise-xml-condition production
                                *character-category-condition*
                                :character char
                                :category '(#\=)))))
    (loop (setf char (peek-char t stream))
          (when (char= char *pi-close-char*)
            (read-char stream)
            (unless (char= (setf char (read-char stream)) *tag-close-char*)
              (xml-form-error production "erroneous character: ~s." char))
            (return))
          (setf attribute-name (read-name-token stream))
          (cond ((null attribute-name) (return))
                ((and (string-equal attribute-name "encoding") (not encoding))
                 (read-equals)
                 (setf encoding
                       (read-attribute-value #'xml-latinchar? stream)))
                ((and (string-equal attribute-name "standalone") (not standalone))
                 (read-equals)
                 (setf standalone 
                       (read-attribute-value #'xml-char? stream))
                 (unless (or (string-equal standalone "yes")
                             (string-equal standalone "no"))
                   (xml-form-error production
                                   "erroneous standalone value: ~s."
                                   standalone)))
                ((and (string-equal attribute-name "version") (not version))
                 (read-equals)
                 (setf version
                       (read-attribute-value #'xml-versionnumchar? stream)))
                (t
                 (xml-form-error production
                                 "erroneous entity attribute: ~a."
                                 attribute-name)))))
  (values encoding version standalone))


;;;
;;; miscellaneous readers

(defun read-name-list
       (delimiter stream
        &aux list char element)
  (flet ((read-element ()
           (cond ((maybe-parse-parameter-entity-reference stream char))
                 ((char= char #\()
                  (read-char stream)
                  (read-name-list #\)  stream))
                 (t
                  (read-name-token stream))))
         (character-error (char category)
           (raise-xml-condition 'xml-1.0::Names
                                *character-category-condition*
                                :character char
                                :category category)))
      (loop (setf char (peek-char t stream))
            (cond ((eql delimiter char)
                   (read-char stream)
                   (return))
                  ((setf element (read-element))
                   (unless (typep element '(or symbol list xml-marker))
                     (xml-form-error 'xml-1.0::Names
                                     "illegitimate name list constituent: ~a."
                                     element))
                   (unless *parse-suppress*
                     (push element list))
                   ;; skip to next non-whitspace
                   (setf char (peek-char t stream))
                   (cond ((char= char #\|) ;; skip it
                          (read-char stream))
                         ((eql delimiter char)
                          t)
                         (t
                          (character-error char (list #\| delimiter)))))
                  (t 
                   (character-error char (list delimiter
                                               'xml-1.0::nameChar
                                               #\#))))))
    (nreverse list))


(defun _read-markup-model
       (stream
        &aux
        (model nil) nc (production 'xml-1.0::contentspec))
  (handler-bind
    ((error #'(lambda (condition)
                (xml-warn production "model: ~s." model)
                condition)))
    (flet ((read-group (&aux group)
             (loop
               (cond ((char= #\) (peek-char t stream))
                      (read-char stream)
                      (return (nreverse group)))
                     (t
                      (push (_read-markup-model stream) group))))))
      (setf nc (peek-char t stream))
      (cond ((char-equal #\> nc)
             (xml-form-error production "missing content model."))
            ((char-equal #\( nc)
             (read-char stream)
             (setf model (read-group)))
            ((char= nc #\#)
             (setf model (read-restricted-name stream)))
            ((xml-model-op-char? nc)
             (setf model (read-model-operator stream)))
            ((maybe-parse-parameter-entity-reference stream nc)
             (return-from _read-markup-model (_read-markup-model stream)))
            ((xml-namechar? nc)
             (setf model (read-name-token stream)))
            (t
             (xml-form-error production
                             "erroneous character: ~s."
                             nc)))
      (unless (and model (or (symbolp model) (consp model)
                             (typep model 'model-marker)))
        (xml-form-error production "illegitimate content model: ~s." model))
      model)))

(defun read-markup-model
       (stream &aux model)
  (setf model (_read-markup-model stream))
  ;; allow a dangling occurrence token as the NEXT character in the stream
   ;; in a real stream this handler is superfluous - it's here for selection
   ;; or string streams
   (handler-case
     (when (xml-model-op-char? (peek-char nil stream))
       (setf model (list model (read-model-operator stream))))
     (end-of-file (condition) condition))
  model)


;;;
;;; various terminal readers

(defun read-model-operator
       (stream &aux (char (read-char stream)))
  (case char
    (#\* *rep-marker*)
    (#\, *seq-marker*)
    (#\| *or-marker*)
    (#\& *and-marker*)
    (#\? *opt-marker*)
    (#\+ *plus-marker*)))

(defun read-restricted-name
       (stream &aux string nc)
  ;; locate or intern the symbol in the markup package.
  ;; if it is new, then export it
  (unless (char= #\# (setf nc (read-char stream)))
    (xml-form-error 'restricted-name
                    "erroneous character: ~s." nc))
  (setf string (read-name-string stream))
  (intern (concatenate 'string "#" string) *xml-package*))

(defun read-char-to (char stream &aux (decoder (stream-decoder stream)) nc)
  (loop
    (setf nc (funcall decoder))
    (cond ((not nc) (error 'end-of-file :stream stream))
          ((eql char nc) (return char)))))

(defparameter *entity-char-buffer*
  (make-array 10 :fill-pointer 0 :adjustable t :element-type 'character))
(defun read-entity-reference-name-string
       (stream
        &aux 
        nc
        (name *entity-char-buffer*)
        (decoder (stream-decoder stream)))
  (handler-bind 
    ((error #'(lambda (condition)
                (xml-warn 'xml-1.0::Reference "name: ~s." name)
                condition)))
    (setf (fill-pointer name) 0)
    (loop (setf nc (funcall decoder))
          (cond ((not nc) (error 'end-of-file :stream stream))
                ((char= nc *entity-reference-close-char*) (return))
                ((xml-namechar? nc)
                 (vector-push-extend nc name))
                (t
                 (raise-xml-condition 'xml-1.0::Reference
                                      *character-category-condition*
                                      :character nc
                                      :category 'xml-1.0::nameChar))))
    (if *parse-suppress*
      name
      (subseq name 0))))

(defun read-char-reference-name-string
       (stream
        &aux 
        nc
        (name *entity-char-buffer*)
        (decoder (stream-decoder stream)))
  (handler-bind 
    ((error #'(lambda (condition)
                (xml-warn 'xml-1.0::charRef "name: ~s." name)
                condition)))
    (setf (fill-pointer name) 0)
    (loop (setf nc (funcall decoder))
          (cond ((not nc) (error 'end-of-file :stream stream))
                ((char= nc *entity-reference-close-char*) (return))
                ((or (xml-namechar? nc)
                     (and (zerop (fill-pointer name))
                          (char= nc #\#)))
                 (vector-push-extend nc name))
                (t
                 (raise-xml-condition 'xml-1.0::charRef
                                      *character-category-condition*
                                      :character nc
                                      :category 'xml-1.0::nameChar))))
    (if *parse-suppress*
      name
      (subseq name 0))))
;(read-entity/char-reference-name-string (encoded-stream "as%df;")) ;error


(defparameter *char-buffer*
  (make-array 10 :fill-pointer 0 :adjustable t :element-type 'character))
(defun read-char-string
       (predicate stream &optional (read-terminator nil)
                  &aux
            nc entity
            (output *char-buffer*)
            (decoder (stream-decoder stream)))
  "read the content of an attribute as a string;
   normalizes whitespace and trims before/after padding;
   expand character and general entities in-line "
  (handler-bind 
    ((error #'(lambda (condition)
                (xml-warn 'xml-1.0::AttValue "value: ~s." output)
                condition)))
    (flet ((put-result (c)
             (when (or *preserve-whitespace*
                       (not (xml-space? c))
                       (and (plusp (fill-pointer output))
                            (not (xml-space?
                                  (aref output (1- (fill-pointer output)))))))
               (vector-push-extend c output))))
      (setf (fill-pointer output) 0)
      (loop (setf nc (funcall decoder))
            (cond ((not nc) (error 'end-of-file :stream stream))
                  ((funcall predicate nc) (return))
                  ;; if it is a character entity, include it directly
                  ;; otherwise, either it was expanded and the parse proceeeds with the
                  ;; expanded stream, or it wasn't expanded (due to standalone) and will be ignored
                  ((char= nc *general-entity-reference-open-char*)
                   (cond ((char= (peek-char nil stream) *character-reference-open-char*)
                          (setf entity (character-ref-reader-macro stream nc))
                          (when (typep entity 'character)
                            (put-result entity)))
                         (t
                          (general-entity-ref-reader-macro stream nc))))
                  ;; otherwise add it to the content
                  (t
                   (put-result nc))))
      ;; nb. if whitespace handling is non-preserving it will strip even a
      ;; single space from the end
      (when (and (not *preserve-whitespace*)
                 (plusp (fill-pointer output))
                 (xml-space?  (aref output (1- (fill-pointer output)))))
        (decf (fill-pointer output)))
      )
  (values (if *parse-suppress* output (subseq output 0))
          (unless read-terminator (stream-untyi stream nc)))))

(defparameter *entity-buffer*
  (make-array 10 :fill-pointer 0 :adjustable t :element-type 'character))
(defun read-entity-string
       (predicate &optional (stream *standard-input*) (read-terminator nil)
                  &aux
                  nc entity
                  (output *entity-buffer*)
                  (decoder (stream-decoder stream)))
  "read the content of an entity as a string;
   expand character and parameter entities in-line;
   definition is padded with a space at each end."
  (handler-bind 
    ((error #'(lambda (condition)
                (xml-warn 'xml-1.0::AttValue "value: ~s." output)
                condition)))
    (flet ((put-result (c)
             (vector-push-extend c output)))
      (setf (fill-pointer output) 0)
      (put-result #\space)
      (loop (setf nc (funcall decoder))
            (cond ((not nc) (error 'end-of-file :stream stream))
                  ((funcall predicate nc) (return))
                  ((setf entity
                         (maybe-parameter-entity-or-char-reference-reader-macro stream nc))
                   ;; if it was a character entity, include it directly
                   ;; otherwise, either it was expanded and the parse proceeeds with the
                   ;; expanded stream, or it wasn't expanded (due to standalone) and will be ignored
                   (when (typep entity 'character)
                     (put-result entity)))
                  (t
                   (put-result nc))))
      (put-result #\space))
    (values (if *parse-suppress* output (subseq output 0))
            (unless read-terminator (stream-untyi stream nc)))))


;;; now the quoted forms

(defun read-attribute-value
       (predicate stream &aux char (*preserve-whitespace* nil))
  (setf char (peek-char t stream))
  (when (or (eql char #\') (eql char #\"))
    (read-char stream)
    (read-char-string
     #'(lambda (c)
         (cond ((eql c char))
               ((funcall predicate c) nil)
               (t
                ;; if it terminates prematurely, it's an error
                (raise-xml-condition 'xml-1.0::AttributeValue
                                     *character-category-condition*
                                     :character c
                                     :category (or (function-name predicate)
                                                   '?))
                ;; if it returns, skip to the terminator and let it be read
                ;; as normal
                (peek-char c stream)
                nil)))
     stream
     t)))

(defun read-entity-value
       (stream &aux char)
  (setf char (peek-char t stream))
  (when (or (eql char #\') (eql char #\"))
    (read-char stream)
    (read-entity-string
     #'(lambda (c)
         (cond ((eql c char))
               ((not (xml-char? c)))))
     stream
     t)))

(defun read-public-literal
       (stream)
  (read-attribute-value #'xml-pubidchar? stream))

(defun read-system-literal
       (stream)
  (read-attribute-value #'xml-char? stream))

(defun read-notation-literal
       (stream)
  (read-attribute-value #'xml-namechar? stream))


(defparameter *string-buffer*
  (make-array 10 :fill-pointer 0 :adjustable t :element-type 'character))
;;; read stream and return content up to or including a pattern string
(defun read-string-delimited-string
       (string
        &optional (stream *standard-input*) (include-terminator nil)
        (count 0) (length (length string))
        &aux
        nc
        (output *string-buffer*)
        (decoder (stream-decoder stream)))
  (flet ((at-end? (char)
           (if (char= char (char string count))
             (if (>= (incf count) length)
               count
               nil)
             (progn (setf count 0) nil))))
    (setf (fill-pointer output) 0)
    (loop (setf nc (funcall decoder))
          (unless nc (error 'end-of-file :stream stream))
          (unless (not (at-end? nc)) (return))
          (vector-push-extend nc output))
    (cond (include-terminator
           (vector-push-extend nc output))
          (t
           (decf (fill-pointer output) (1- length))))
    (if *parse-suppress*
      output
      (subseq output 0))))

(defun read-delimited-string
       (delimiter
        &optional (stream *standard-input*) (include-terminator nil)
        &aux
        nc
        (output *string-buffer*)
        (decoder (stream-decoder stream)))
  (flet ((at-end? (char)
           (char= char delimiter)))
    (setf (fill-pointer output) 0)
    (loop (setf nc (funcall decoder))
          (unless nc (error 'end-of-file :stream stream))
          (when (at-end? nc) (return))
          (vector-push-extend nc output))
    (when include-terminator
      (vector-push-extend nc output))
    (if *parse-suppress*
      output
      (subseq output 0))))

(defun read-markup-tag-type
       (stream)
  (read-name-token stream))

(defun read-name-token
       (stream &aux nc)
  (flet ((make-token (name)
           (cond ((and (> (length name) 0)
                       (xml-initial-namechar? (char name 0))
                       (< (count #\: name) 2))
                  (intern-name name))
                 (t 
                  (xml-form-error stream "illegitimate name: ~s." name)))))
    (setf nc (peek-char t stream))
    (cond ((and (in-dtd?)
                (maybe-parse-parameter-entity-reference stream nc))
           (read-name-token stream))
          (t
           ;; otherwise reed the token string and intern it. do not distinguish
           ;; different name forms (qualified or not) by context
           (make-token (read-name-string stream))))))

(defParameter *name-buffer*
  (make-array 10 :fill-pointer 0 :adjustable t :element-type 'character))
(defun read-name-string
       (stream
        &aux
        nc
        (name *name-buffer*)
        (decoder (stream-decoder stream)))
  ;; reads a simple string, does NOT check for entity references, these must
  ;; be filtered by the caller. by default raises an error for eof, since
  ;; names can't appear outside of tags
  (handler-bind 
    ((error #'(lambda (condition)
                (xml-warn 'xml-1.0::Name "name: ~s." name)
                condition)))
    (setf (fill-pointer name) 0)
    (loop (setf nc (funcall decoder))
          (cond ((not nc)
                 (error 'end-of-file :stream stream))
                ((or (xml-initial-namechar? nc)
                     (and (plusp (fill-pointer name))
                          (xml-namechar? nc)))
                 (vector-push-extend nc name))
                (t
                 (stream-untyi stream nc)
                 (return))))
    (if (zerop (fill-pointer name))
      nil
      (subseq name 0))))

(defparameter *reserved-name-buffer*
  (make-array 10 :fill-pointer 0 :adjustable t :element-type 'character))
(defun read-reserved-name-string
       (stream &optional (eof-error-p t) eof-value
        &aux
        nc
        (name *reserved-name-buffer*)
        (decoder (stream-decoder stream)))
  ;; reads a simple string, does NOT check for entity references, these must
  ;; be filtered by the caller. by default raises an error for eof, since
  ;; names can't appear outside of tags
  (handler-bind 
    ((error #'(lambda (condition)
                (xml-warn 'xml-1.0::ReservedName "name: ~s." name)
                condition)))
    (setf (fill-pointer name) 0)
    (loop (setf nc (funcall decoder))
          (cond ((not nc)
                 (if eof-error-p
                   (error 'end-of-file :stream stream)
                   (return)))
                ((or (reserved-name-indicator-p nc)
                     (and (plusp (fill-pointer name))
                          (xml-namechar? nc)))
                 (vector-push-extend nc name))
                (t
                 (stream-untyi stream nc)
                 (return))))
    (if (zerop (fill-pointer name))
      (if nc nil eof-value)
      (subseq name 0))))


;;;
;;; access to external entities:
;;; method depends on the access substrate which is present

(defMethod get-external-entity-content
           ((location pathname) &aux length buffer)
  (when (setf location (probe-file location))
    (with-open-file (stream location :direction :input
                            :element-type 'unsigned-byte)
      (setf length (file-length stream))
      (setf buffer (make-array length :element-type  'unsigned-byte))
      (multiple-value-bind (reader arg)
                           (stream-reader stream)
        (dotimes (i length)
          (setf (svref buffer i) (or (funcall reader arg) 0))))
    buffer)))

#+CL-HTTP
(defMethod get-external-entity-content
           ((location file-url) &aux length buffer)
  (when (setf location (probe-file (translated-pathname location)))
    (with-open-file (stream location :direction :input
                            :element-type 'unsigned-byte)
      (setf length (file-length stream))
      (setf buffer (make-array length :element-type  'unsigned-byte))
      (multiple-value-bind (reader arg)
                           (stream-reader stream)
        (dotimes (i length)
          (setf (svref buffer i) (or (funcall reader arg) 0))))
    buffer)))

#+CL-HTTP
(defMethod get-external-entity-content
           ((location http-url) &aux buffer byte length)
  (http:with-http-request (location :get)
    (setf length (http:get-header "Content-Length"))
    (or (when length (set length (ignore-errors (parse-integer length))))
        (setf length 128))
    (setf buffer (make-array length :element-type  'unsigned-byte
                             :adjustable t :fill-pointer 0))
    (loop (unless (setf byte (read-byte http::remote-stream nil)) (return))
          (vector-push-extend byte buffer)))
  buffer)



;;;
;;; a link must resolve to a well-formed document, thus simply load from the
;;; indicated source.


(defun get-link-content
           (location)
  (load-xml location
            :error-handler
            #'(lambda (condition)
                (warn "external link reference cannot be loaded: ~s: ~s."
                      location condition)
                (return-from get-link-content nil))))


;;;
;;; markup dispatch takes a postfix argument character which specifies
;;; how to interpret the subsequent data - and thus which reader to use.
;;; the readers are bound to properties on the function name:
;;; eg. #\! for dtd stuff
;;;     #\? for patterns
;;;     #\m for models
;;;     #\" for content wrapped in a string
;;;     #\> for xml pointers (see xptr.lisp)

;;; 19990223 made extensible

(defun markup-readtable-reader-macro
       (stream char arg
        &aux )
  "returns the first element read in the stream."
  (declare (ignore char arg))
  (markup-reader-macro stream (peek-char t stream)))

(defMethod markup-reader-macro
           ((stream t) (dispatch-readahead (eql #\!))
            &aux (*in-external-subset* t) (*document-type* *document-type*))
  (setf *document-type* (default-document-type))
  (read-char stream)
  (read-production 'xml-1.0::markupdecl (encoded-stream stream)))

(defMethod markup-reader-macro
           ((stream t) (dispatch-readahead (eql #\")))
  (setf stream (encoded-stream (read stream)))
  (markup-reader-macro stream (peek-char nil stream)))

(defMethod markup-reader-macro
           ((stream t) (dispatch-readahead (eql #\m))
            &aux (*document-type* *document-type*))
  (setf *document-type* (default-document-type))
  (read-char stream)
  (make-model (read-markup-model (encoded-stream stream))))

(defMethod markup-reader-macro
           ((stream t) (dispatch-readahead (eql #\M)))
  (markup-reader-macro stream #\m))

(defMethod markup-reader-macro
           ((stream t) (dispatch-readahead (eql #\<)))
  (read-production 'xml-1.0::documentMarkup (encoded-stream stream)))

(defun read-xml-element
       (stream)
  (markup-reader-macro stream #\<))


(set-dispatch-macro-character #\# #\! 'markup-readtable-reader-macro)
(defun patch-readtable-for-xml ()
  (set-dispatch-macro-character #\# #\! 'markup-readtable-reader-macro))

#|
;; test forms - in reverse order of calling structure, and thus also reverse lexical order

 (patch-readtable-for-xml)
 (defGeneralEntity XMLNS.1::|test| "&lt;test 1 2&gt;")
 (defGeneralEntity XMLNS.1::|quot| "'")
 (defGeneralEntity XMLNS.1::|QUOT| "&#39;")
 (defParameterEntity XMLNS.1::|parm| "one-two")
 (read-name-string (encoded-stream "asdf| "))
 (read-name-string (encoded-stream "asdf")) ;; -> ERROR
 (read-name-string (encoded-stream "1asdf ")) ;; -> NIL
 (read-name-token (encoded-stream "asdf;"))
 (with-default-document-type () (read-name-token (encoded-stream "%parm; "))) ;; -> XMLNS.1::|one-two|
 (read-markup-tag-type (encoded-stream "asdf "))
 (read-string-delimited-string ".." (encoded-stream "asdf.qwer... ") nil)
 (read-string-delimited-string ".." (encoded-stream "asdf.qwer... ") t) ;; -> "asdf.qwer.."
 (read-notation-literal (encoded-stream "'someNotationName'"))
 (read-system-literal (encoded-stream "'   AS  &#x27;&amp;&apos;&lt;&gt;&quot;  AS_  '")) ;; -> "AS '&'<>\" AS_" whitespace normalized, entities replaced
 (read-public-literal (encoded-stream "' -//testing//one /two   /three//  '"))
 (read-entity-value (encoded-stream "'sasf&test;&#x27;'")) ;; -> "sasf&test;<" including the general entity, and expanded char
 (read-attribute-value #'xml-char? (encoded-stream "'sasf&test;'")) ; -> "sasf<test 1 2>" which i think is right
 (read-attribute-value #'xml-char? (encoded-stream "'sasf&test;&quot;")) ;; -> "sasf<test 1 2>" also, which is not strictly
                                                                         ;; correct, as the quote should not be recognized in
                                                                         ;; replacement text...
 (read-attribute-value #'xml-char? (encoded-stream "'sasf&test;&QUOT;")) ;; -> "sasf<test 1 2>'" + ERROR, which suffices as an
 (read-attribute-value #'xml-char? (encoded-stream "'sasf&test;&QUOT;'")) ;; -> "sasf<test 1 2>'" equivalent alternative
                                                                          ;;                      encoding.

 (production-reader-macro 'xml-1.0::markupdecl (encoded-stream "![CDATA[ test 1 2 3 ]]>  "))
 (processing-instruction-reader-macro (encoded-stream "asdf qwqwe  ererwer wer?>"))
 (processing-instruction-reader-macro (encoded-stream "xml ?>"))
 (read-production 'xml-1.0::markupdecl (encoded-stream "  ]]> ")) ; -> should signal a section-end


(inspect *)
(inspect
 (list
  (read-production 'xml-1.0::markupdecl (encoded-stream "<?asdf qwqwe  <&#32ererwer wer?>"))
  (read-production 'xml-1.0::markupdecl (encoded-stream "<?xml version='1.0'?>"))
  (read-production 'xml-1.0::markupdecl (encoded-stream "<!ELEMENT asdf (ANY) >"))
  (read-production 'xml-1.0::markupdecl (encoded-stream "<!ATTLIST asdf att1 CDATA #REQUIRED >"))
  (read-production 'xml-1.0::markupdecl (encoded-stream "<!ATTLIST asdf att1 CDATA #IMPLIED >"))
  (read-production 'xml-1.0::markupdecl (encoded-stream "<!ATTLIST asdf att1 CDATA #FIXED 'ww/ss/xx'>"))
  (read-production 'xml-1.0::markupdecl (encoded-stream "<!ATTLIST asdf att1 CDATA 'ww/ss/xx'>"))
  (read-production 'xml-1.0::markupdecl (encoded-stream "<!ATTLIST asdf att1 NOTATION (notation| xyz) #REQUIRED >"))
  (read-production 'xml-1.0::markupdecl (encoded-stream "<!ATTLIST asdf att1 NOTATION (notation |xyz) #IMPLIED >"))
  (read-production 'xml-1.0::markupdecl (encoded-stream "<!ATTLIST asdf att1 NOTATION (notation|xyz) #FIXED 'notation' >"))
  (ignore-errors
   (read-production 'xml-1.0::markupdecl (encoded-stream "<!ATTLIST asdf att1 NOTATION (notation xyz) 'notation' >"))) ;; -> ERROR
  (read-production 'xml-1.0::markupdecl (encoded-stream "<!ATTLIST asdf att1 (yxcv |asdf) #REQUIRED >"))
  (read-production 'xml-1.0::markupdecl (encoded-stream "<!ATTLIST asdf att1 (yxcv| asdf) #IMPLIED >"))
  (read-production 'xml-1.0::markupdecl (encoded-stream "<!ATTLIST asdf att1 (yxcv|asdf) #FIXED 'asdf' >"))
  (ignore-errors
   (read-production 'xml-1.0::markupdecl (encoded-stream "<!ATTLIST asdf att1 (yxcv asdf) 'asdf' >"))) ;; -> ERROR
  (read-production 'xml-1.0::markupdecl (encoded-stream "<!ELEMENT SB (ANY) >"))
  (read-production 'xml-1.0::markupdecl (encoded-stream "<!ATTLIST SB p (y|n) 'y'>"))
  (read-production 'xml-1.0::markupdecl (encoded-stream "<!ELEMENT W (GR,GL?,HB?, ALT?, TRF*, XRF?, NT*)>"))
  (read-production 'xml-1.0::markupdecl (encoded-stream "<!ENTITY % OPTIONAL-FIELDS 'HB?, ALT?, TRF*, XRF?, NT*'>"))
  (with-default-document-type ()
    (read-production 'xml-1.0::markupdecl (encoded-stream "<!ELEMENT W (GR,GL?,%OPTIONAL-FIELDS;) >")))
  ))

(read-production 'xml-1.0::markupdecl
                 (encoded-stream "<!ATTLIST asdf
                                            att1 CDATA #REQUIRED
                                            att2 ID 'qwr'
                                            att3 IDREF #FIXED 'qwr'
                                            att4 IDREFS 'qwr rtz'
                                            att5 ENTITY #IMPLIED
                                            att6 ENTITIES 'qwr rtz'
                                            att7 NMTOKEN 'qwr rtz'
                                            att8 NMTOKENS 'qwr wer'
                                            att9 NOTATION (notation | xyz) #FIXED 'notation'
                                            att0 (yxcv | asdf) 'asdf' >"))

(read-production 'xml-1.0::markupdecl (encoded-stream "<!-- here is a comment -->"))
(read-production 'xml-1.0::markupdecl (encoded-stream "<!NOTATION asdf  SYSTEM 'ww/ss/xx'>"))
(read-production 'xml-1.0::markupdecl (encoded-stream "<!NOTATION asdf  PUBLIC '-()+,./:?;!*#@$_%' >"))
(read-production 'xml-1.0::markupdecl (encoded-stream "<!NOTATION asdf  PUBLIC '-()+,./:?;!*#@$_%' 'ww/ss/xx'>"))
(read-production 'xml-1.0::markupdecl (encoded-stream "<!ENTITY asdf 'this is a test'> "))
(read-production 'xml-1.0::documentMarkup (encoded-stream "<!DOCTYPE asdf SYSTEM 'ww/ss/xx' > "))
(with-default-document-type ()
  (read-production 'xml-1.0::documentMarkup
                   (encoded-stream "<!DOCTYPE asdf SYSTEM 'ww/ss/xx'
                                          [ <!NOTATION asdf  SYSTEM 'ww/ss/xx' > 
                                            <!ENTITY asdf 'this is a &#38;test'>
                                            <!ELEMENT qwer (ANY) >
                                            <!ATTLIST qwer att1 CDATA #REQUIRED >
                                          ] >")))


(read-production 'xml-1.0::markupdecl (encoded-stream "<!ELEMENT qwer (ANY) >"))
(read-production 'xml-1.0::markupdecl (encoded-stream "<!ELEMENT test (a | b)* >"))
(inspect *)

(read-production 'xml-1.0::ExtSubset (encoded-stream "<![IGNORE[ <!ELEMENT test (a | b)* > ]]>"))
(with-default-document-type ()
  (read-production 'xml-1.0::ExtSubset (encoded-stream "<![INCLUDE[
                                                          <!ELEMENT test (a | b | c)* > 
                                                          <!ELEMENT a ANY > 
                                                          <!ELEMENT b #PCDATA > 
                                                          <!ELEMENT b EMPTY > ]]>")))

(read-production 'xml-1.0::documentMarkup (encoded-stream "<![CDATA[ asdf >asas ]]>")) ;; -> ERROR
(read-production 'xml-1.0::elementMarkup (encoded-stream "<![CDATA[ asdf >asas ]]>"))
(read-production 'xml-1.0::charData (encoded-stream "asdd<"))
(read-production 'xml-1.0::documentMarkup (encoded-stream "<a:element xmlns:a='qwert'>asdd</a:element>"))

(read-production 'xml-1.0::document (encoded-stream " "))
(read-production 'xml-1.0::document (encoded-stream "<a:element xmlns:a='qwert'>asdd</a:element>"))
(read-production 'xml-1.0::document
                 (encoded-stream "<!DOCTYPE asdf2 SYSTEM 'ww/ss/xx'
                                          [ <?namespace xmlns:a=':ASDF' ?>
                                            <!NOTATION asdf  SYSTEM 'ww/ss/xx' > 
                                            <!ENTITY asdf 'this is a &#38;test;'>
                                            <!ENTITY %asdf2 'this is also a &#38;test;' >
                                            <!ELEMENT a:qwer ANY >
                                            <!ATTLIST a:qwer att1 CDATA #REQUIRED >
                                          ] >
                                  <a:qwer xmlns:a=':ASDF' att1='&asdf;'>asdd &asdf;</a:qwer>"))
(element-declaration 'xmlns.5::|qwer| t)

(with-default-document-type ()
  (read-production 'xml-1.0::ExtSubset (encoded-stream
                                        (make-concatenated-stream
                                         (make-string-input-stream "<![INCLUDE[")
                                         (make-string-input-stream "<!ELEMENT test (a | b | c)* > 
                                                                  <!ELEMENT a ANY > 
                                                                  <!ELEMENT b #PCDATA > 
                                                                  <!ELEMENT b EMPTY > ]]>")))))

(let ((*print-readably* t)) (print *))
|#

"XMLP"
