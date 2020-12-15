;;; -*- Package: ("XML-PARSER"); -*-

"<DOCUMENTATION>
 <DESCRIPTION>
  the least one has to do in order to handle RDF is to create the symbols in
  order that the reader (which is still central to the xml processor) permits
  single-colon package qualification.
  </DESCRIPTION>
 </DOCUMENTATION>
 "

(in-package "XML-PARSER")

(defPackage "http://www.w3.org/TR/WD-rdf-syntax/"
  (:use "XML")
  (:nicknames "RDF")
  (:export 
           "Description"
           "href"
           "instanceOf"
           "Ord"
           "PropertyType"
           "Resource"
           "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" ; not the best, but the xml spec doesn't allow this anyway
           ))


(defPackage "http://www.w3.org/TR/WD-rdf-schema/"
  (:use "XML")
  (:nicknames "RDFS")
  (:export "allowedPropertyType"
           "Class"
           "Collection"
           "Comment"
           "ConstraintPropertyType"
           "ExactlyOne"
           "necessity"
           "NecessityValue"
           "OneOrMore"
           "range"
           "String"
           "subClassOf"
           "ZeroOrMore"
           "ZeroOrOne"))

"XMLP"
