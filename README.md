# Senex

Exposes Astrodientst's [Swiss Ephemeris](https://www.astro.com/swisseph/swephinfo_e.htm) C library via Haskell.

## Compiling the C libs

```
➜  csrc git:(master) ✗ make libswe.so
cc	 -c -g -Wall -fPIC  	   swedate.c     
cc	 -c -g -Wall -fPIC  	   swehouse.c     
cc	 -c -g -Wall -fPIC  	   swejpl.c     
cc	 -c -g -Wall -fPIC  	   swemmoon.c     
cc	 -c -g -Wall -fPIC  	   swemplan.c     
cc	 -c -g -Wall -fPIC  	   swepcalc.c     
cc	 -c -g -Wall -fPIC  	   sweph.c     
cc	 -c -g -Wall -fPIC  	   swepdate.c     
cc	 -c -g -Wall -fPIC  	   swephlib.c     
cc	 -c -g -Wall -fPIC  	   swecl.c     
cc	 -c -g -Wall -fPIC  	   swehel.c     
cc	 -shared -o libswe.so swedate.o swehouse.o swejpl.o swemmoon.o swemplan.o swepcalc.o sweph.o swepdate.o swephlib.o swecl.o swehel.o
➜  csrc git:(master) ✗ make libswe.a
ar r libswe.a	swedate.o swehouse.o swejpl.o swemmoon.o swemplan.o swepcalc.o sweph.o swepdate.o swephlib.o swecl.o swehel.o
ar: creating archive libswe.a

➜  csrc git:(master) ✗ cp libswe.* /usr/local/lib/
```
