
;;;
(defsystem lambda-ir
    :source-pathname "HTTP:"
    :components
    ("lambda-ir;package"
     "lambda-ir;ir-utils"
     "lambda-ir;variables"
     "lambda-ir;class"
     "cmucl;lambda-ir;bit-vectors"
     "lambda-ir;data-structures"
     "lambda-ir;computations"
     "lambda-ir;ir-base"
     "lambda-ir;contexts"
     "lambda-ir;constraints"
     "lambda-ir;bin-dump-utils"
     "lambda-ir;examples;lambdavista"
     "lambda-ir;examples;stemming"
     "lambda-ir;examples;lambdavista-exports"))
