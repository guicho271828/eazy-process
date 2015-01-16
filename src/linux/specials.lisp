

(in-package :eazy-process)

#|

Define some special variables

|#

(defparameter +io-keywords+
  #(:rchar :wchar :syscr :syscw
    :read_bytes :write_bytes
    :cancelled_write_bytes))

(defparameter +statm-keywords+
  #(:size :resident :share :text :lib :data :dt))


(defparameter +stat-keywords+ 
  #(:pid :comm :state :ppid :pgrp :session :tty_nr :tpgid :lags
    :minflt :cminflt :majflt :cmajflt :utime :stime :cutime :cstime
    :priority :nice :num_threads :itrealvalue :starttime
    :vsize :rss :rsslim :startcode :endcode
    :startstack :kstkesp :kstkeip :signal :blocked :sigignore :sigcatch
    :wchan :nswap :cnswap :exit_signal :processor :rt_priority :policy
    :delayacct_blkio_ticks :guest_time :cguest_time))

(defvar *procfs-root* "/proc")
