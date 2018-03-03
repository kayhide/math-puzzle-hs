# q02

```
% stack exec -- q02 +RTS -s
5931 (5 * 9 * 31 = 1395)

   1,507,601,104 bytes allocated in the heap
       1,565,896 bytes copied during GC
          44,504 bytes maximum residency (2 sample(s))
          29,224 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      1454 colls,     0 par    0.008s   0.014s     0.0000s    0.0042s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0000s    0.0000s

  INIT    time    0.000s  (  0.005s elapsed)
  MUT     time    0.488s  (  0.495s elapsed)
  GC      time    0.008s  (  0.014s elapsed)
  EXIT    time    0.000s  (  0.004s elapsed)
  Total   time    0.497s  (  0.518s elapsed)

  %GC     time       1.7%  (2.7% elapsed)

  Alloc rate    3,089,473,142 bytes per MUT second

  Productivity  98.3% of total user, 96.3% of total elapsed
```
