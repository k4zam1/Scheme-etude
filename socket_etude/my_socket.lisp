
(defparameter my-socket (socket-server 4321))
(defparameter my-stream (socket-accept my-socket))

(defparameter my-stream (socket-connect 4321 "127.0.0.1"))