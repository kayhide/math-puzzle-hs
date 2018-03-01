# q01

```
% stack exec -- q01 +RTS -s
Just 585
       1,665,672 bytes allocated in the heap
          22,688 bytes copied during GC
          44,504 bytes maximum residency (1 sample(s))
          29,224 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0         1 colls,     0 par    0.000s   0.000s     0.0000s    0.0000s
  Gen  1         1 colls,     0 par    0.000s   0.001s     0.0007s    0.0007s

  INIT    time    0.000s  (  0.005s elapsed)
  MUT     time    0.002s  (  0.009s elapsed)
  GC      time    0.000s  (  0.001s elapsed)
  EXIT    time    0.000s  (  0.001s elapsed)
  Total   time    0.002s  (  0.015s elapsed)

  %GC     time       4.8%  (4.5% elapsed)

  Alloc rate    1,010,723,300 bytes per MUT second

  Productivity  86.1% of total user, 63.1% of total elapsed
```
