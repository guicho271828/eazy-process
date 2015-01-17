
(in-package :eazy-process)

;; FIXME: referring to the internal symbol ...

(iolib.syscalls:defsyscall (execve "execve") :int
  (path iolib.syscalls::sstring)
  (argv :pointer)
  (envp :pointer))

(iolib.syscalls:defsyscall (execvpe "execvpe") :int
  (file iolib.syscalls::sstring)
  (argv :pointer)
  (envp :pointer))

