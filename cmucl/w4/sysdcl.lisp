;;;
;;;  CMUCL SYSTEM DEFINITION FOR W4 CONSTRAINT-GUIDED WEB WALKER
;;;

(in-package "CL-USER")

(defsystem w4-web-walker
  :source-pathname "HTTP:"
  :components
  ("client;w4-client"			; W4 client support methods
   "cmucl;client;w4-client"		; CMUCL W4 client support methods
   "w4;package"				; Package Definition
   "w4;variables"			; Variables
   "w4;utils"				; Utility functions and macros
   "w4;class"				; Class definitions and Print methods
   "w4;walker"				; Main Walker code
   "w4;constraints"			; Constraint Definitions
   "w4;actions"				; Action definitions
   "w4;activity"))			; Activity definitions

(defsystem w4-web-walker-demo
  :source-pathname "HTTP:"
  :components
  ("examples;configuration"             ; Standard configuration
   "w4;examples;trace"			; Standard examples
   "w4;examples;search"			; Salton style search example.
   "w4;examples;web-archive"		; Web Whacker
   "w4;examples;exports"))		; Basic examples
