(define-syntax endianness
   (syntax-rules (little big native)
      ((_ little)
       +little-endian+)
      ((_ big)
       +big-endian+)
      ((_ native)
       +native-endian+)
      ((_ other-type)
       (error "endianness" "unsupported endian type" other-type))
      ))

