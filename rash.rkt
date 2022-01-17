#lang rash
(require relation/function)
(require racket/string)

xrandr | grep " connected" |>> (compose car (app string-split _ " "))
