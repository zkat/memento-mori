(cl:defpackage #:memento-mori.logger
  (:use #:cl #:alexandria #:memento-mori.utils)
  (:nicknames #:mori-log)
  (:shadow #:warn #:error #:debug)
  (:export

   #:ensure-logger
   
   #:emergency
   #:alert
   #:critical
   #:error
   #:warn
   #:notice
   #:info
   #:debug))
